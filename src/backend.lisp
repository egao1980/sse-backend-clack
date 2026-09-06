(in-package #:sse-backend-clack)

(defparameter +sse-headers+
  '(:content-type "text/event-stream; charset=utf-8"
    :cache-control "no-cache"
    :connection "keep-alive"
    :x-accel-buffering "no"))

(defclass clack-sse-backend (sse-protocol:sse-backend) ())

(defun make-clack-sse-backend ()
  (make-instance 'clack-sse-backend))

(defun use-clack-sse-backend ()
  (setf sse-protocol:*sse-backend* (make-clack-sse-backend)))

(defun sse-response-headers (&optional extra)
  (append (copy-list +sse-headers+) extra))

(defun request-last-event-id (env)
  "Last-Event-ID from a Clack ENV, or NIL."
  (let ((headers (getf env :headers)))
    (when headers
      (or (gethash "last-event-id" headers)
          (gethash :last-event-id headers)))))

(defun events-body (events)
  (mapcar #'sse-protocol:encode-sse-event events))

(defun %coerce-events (result)
  (cond
    ((null result) '())
    ((sse-protocol:sse-event-p result) (list result))
    ((and (listp result) (every #'sse-protocol:sse-event-p result)) result)
    (t (error 'sse-protocol:sse-error
              :message "SSE handler must return an sse-event, a list of them, or a writer"))))

(defun %invoke-handler (handler env)
  (if (functionp handler)
      (funcall handler env)
      handler))

(defun %keepalive-interval (keepalive)
  (etypecase keepalive
    ((eql t) sse-protocol:*sse-keepalive-interval*)
    (real keepalive)))

(defvar *sse-stream-hold* t
  "When T, :keepalive holds the Clack response until the stream closes.
   Tests bind this to NIL so draining a keepalive app does not block.")

(defclass chunk-output-stream
    (trivial-gray-streams:fundamental-character-output-stream)
  ((writer :initarg :writer :accessor chunk-stream-writer)
   (buffer :initform (make-string-output-stream) :accessor chunk-stream-buffer)
   (open :initform t :accessor chunk-stream-open-p)
   (hold :initarg :hold :initform t :accessor chunk-stream-hold-p)))

(defmethod trivial-gray-streams:stream-write-char ((s chunk-output-stream) char)
  (write-char char (chunk-stream-buffer s))
  char)

(defmethod trivial-gray-streams:stream-write-string
    ((s chunk-output-stream) string &optional start end)
  (write-string string (chunk-stream-buffer s)
                :start (or start 0) :end end)
  string)

(defun %flush-chunk (s &key close)
  (let ((chunk (get-output-stream-string (chunk-stream-buffer s)))
        (writer (chunk-stream-writer s)))
    (when (and writer (plusp (length chunk)))
      (funcall writer chunk))
    (when (and writer close)
      (funcall writer nil :close t))))

(defmethod trivial-gray-streams:stream-force-output ((s chunk-output-stream))
  (%flush-chunk s)
  nil)

(defmethod trivial-gray-streams:stream-finish-output ((s chunk-output-stream))
  (%flush-chunk s)
  nil)

(defun %close-chunk-stream (s)
  (when (chunk-stream-open-p s)
    (ignore-errors (%flush-chunk s :close t))
    (setf (chunk-stream-open-p s) nil))
  s)

(defun %make-chunk-stream (writer &key (hold *sse-stream-hold*))
  (make-instance 'chunk-output-stream :writer writer :hold hold))

(defun %should-hold-p (stream)
  (and *sse-stream-hold*
       (open-stream-p stream)
       (not (typep stream 'string-stream))
       (if (typep stream 'chunk-output-stream)
           (chunk-stream-hold-p stream)
           t)))

(defun %hold-while-open (stream)
  (loop
    (unless (open-stream-p stream)
      (return))
    (sleep 0.1)
    (handler-case (force-output stream)
      (error () (return)))))

(defun %write-event (stream event)
  (sse-protocol:write-sse-event stream event)
  (force-output stream)
  event)

(defun %make-stream-body (result &key keepalive)
  "Stream writer: write RESULT (events or writer), then keepalives."
  (lambda (stream)
    (let ((ka nil))
      (unwind-protect
           (progn
             (when keepalive
               (setf ka (sse-protocol:make-sse-keepalive
                         stream
                         :interval (%keepalive-interval keepalive)
                         :start t)))
             (if (functionp result)
                 (funcall result stream)
                 (dolist (ev result)
                   (%write-event stream ev)
                   (when ka (sse-protocol:note-sse-activity ka))))
             (when (and ka (%should-hold-p stream))
               (%hold-while-open stream)))
        (when ka (sse-protocol:stop-sse-keepalive ka))
        (ignore-errors (force-output stream))))))

(defun %adapt-stream-response (status headers stream-fn)
  "Hunchentoot ignores a function in (status headers body). Return a
   Clack response function that obtains the chunk writer and calls
   STREAM-FN with a character stream (force-output flushes)."
  (lambda (responder)
    (let* ((writer (funcall responder (list status headers)))
           (stream (%make-chunk-stream writer :hold *sse-stream-hold*)))
      (unwind-protect (funcall stream-fn stream)
        (%close-chunk-stream stream)))))

(defun call-sse-app (app env)
  "Invoke APP. Returns (status headers wire-string).
   Binds *SSE-STREAM-HOLD* to NIL so keepalive apps do not block."
  (let ((*sse-stream-hold* nil)
        (res (let ((*sse-stream-hold* nil))
               (funcall app env))))
    (cond
      ((functionp res)
       (let ((status nil) (headers nil) (chunks '()))
         (funcall res
                  (lambda (status-and-headers)
                    (setf status (first status-and-headers)
                          headers (second status-and-headers))
                    (lambda (body &key (start 0) end close)
                      (declare (ignore close))
                      (when body
                        (push (etypecase body
                                (string (subseq body start (or end (length body))))
                                ((vector (unsigned-byte 8))
                                 (babel:octets-to-string
                                  body :encoding :utf-8
                                       :start start
                                       :end (or end (length body)))))
                              chunks))
                      (values))))
         (list status headers (apply #'concatenate 'string (nreverse chunks)))))
      ((and (consp res) (functionp (third res)))
       (list (first res) (second res)
             (with-output-to-string (s) (funcall (third res) s))))
      ((consp res)
       (list (first res) (second res)
             (let ((body (third res)))
               (if (listp body)
                   (apply #'concatenate 'string body)
                   (or body "")))))
      (t (list 500 nil "")))))

(defun make-sse-stream-app (handler &key (path nil) headers (keepalive nil))
  "Clack app that writes via (lambda (stream) …).

   HANDLER is a list of SSE-EVENT, a single SSE-EVENT,
   (lambda (env) → events | writer), or a writer (lambda (stream) …)
   returned from that handler. PATH when set 404s other :path-info.
   KEEPALIVE T (or a positive interval in seconds) runs
   MAKE-SSE-KEEPALIVE / MAYBE-WRITE-SSE-KEEPALIVE on the live stream
   after events — not a prepended comment.

   Direct (funcall app env) returns either a 3-list (404) or a Clack
   response function so Hunchentoot/Woo stream chunks. Use CALL-SSE-APP
   in tests."
  (lambda (env)
    (block app
      (when (and path (not (string= (or (getf env :path-info) "/") path)))
        (return-from app
          '(404 (:content-type "text/plain; charset=utf-8") ("not found"))))
      (multiple-value-bind (result extra)
          (%invoke-handler handler env)
        (let ((payload (if (functionp result)
                           result
                           (%coerce-events result))))
          (%adapt-stream-response
           200
           (sse-response-headers (append extra headers))
           (%make-stream-body payload :keepalive keepalive)))))))

(defun make-sse-app (handler &key (path nil) headers (keepalive nil))
  "Clack app that emits HANDLER's events as text/event-stream.

   See MAKE-SSE-STREAM-APP. HANDLER is a list of SSE-EVENT, a single
   SSE-EVENT, or (lambda (env) → events | writer). Optional second
   value is extra response headers (Clack plist). PATH when set 404s
   other :path-info values. KEEPALIVE T runs keepalives on the live
   stream after events."
  (make-sse-stream-app handler :path path :headers headers :keepalive keepalive))

(defun %ensure-http-server-backend ()
  (or http-server-protocol:*http-server-backend*
      (progn
        (asdf:load-system "http-server-backend-hunchentoot")
        (funcall (find-symbol "USE-HUNCHENTOOT-BACKEND"
                              :http-server-backend-hunchentoot)))))

(defmethod sse-protocol:backend-serve-sse ((backend clack-sse-backend) handler
                                           &key (host "127.0.0.1") (port 8080)
                                             (path "/"))
  (declare (ignore backend))
  (%ensure-http-server-backend)
  (http-server-protocol:serve (make-sse-app handler :path path)
                              :host host :port port))

(defmethod sse-protocol:backend-open-sse ((backend clack-sse-backend) url
                                          &key last-event-id headers timeout
                                            method content
                                            &allow-other-keys)
  (declare (ignore url last-event-id headers timeout method content))
  (error 'sse-protocol:sse-error
         :message "sse-backend-clack is emit-only — use sse-backend-http for open-sse"))

(use-clack-sse-backend)
