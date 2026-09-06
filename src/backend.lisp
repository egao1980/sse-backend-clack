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

(defun %live-stream-p (stream)
  (and (open-stream-p stream)
       (not (typep stream 'string-stream))))

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
  "Clack body function: write RESULT (events or writer), then keepalives."
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
             (when (and ka (%live-stream-p stream))
               (%hold-while-open stream)))
        (when ka (sse-protocol:stop-sse-keepalive ka))))))

(defun make-sse-stream-app (handler &key (path nil) headers (keepalive nil))
  "Clack app whose body is (lambda (stream) …).

   HANDLER is a list of SSE-EVENT, a single SSE-EVENT,
   (lambda (env) → events | writer), or a writer (lambda (stream) …)
   returned from that handler. PATH when set 404s other :path-info.
   KEEPALIVE T (or a positive interval in seconds) runs
   MAKE-SSE-KEEPALIVE / MAYBE-WRITE-SSE-KEEPALIVE on the live stream
   after events — not a prepended comment."
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
          (list 200
                (sse-response-headers (append extra headers))
                (%make-stream-body payload :keepalive keepalive)))))))

(defun make-sse-app (handler &key (path nil) headers (keepalive nil))
  "Clack app that emits HANDLER's events as text/event-stream.

   Body is a stream function (see MAKE-SSE-STREAM-APP). HANDLER is a
   list of SSE-EVENT, a single SSE-EVENT, or (lambda (env) → events |
   writer). Optional second value is extra response headers (Clack
   plist). PATH when set 404s other :path-info values.
   KEEPALIVE T runs keepalives on the live stream after events."
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
