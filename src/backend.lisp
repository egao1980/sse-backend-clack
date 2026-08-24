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
              :message "SSE handler must return an sse-event or a list of them"))))

(defun %invoke-handler (handler env)
  (if (functionp handler)
      (funcall handler env)
      handler))

(defun make-sse-app (handler &key (path nil) headers (keepalive nil))
  "Clack app that emits HANDLER's events as text/event-stream.

   HANDLER is a list of SSE-EVENT, a single SSE-EVENT, or
   (lambda (env) → events). Optional second value is extra response headers
   (Clack plist). PATH when set 404s other :path-info values.
   KEEPALIVE T prepends one comment keepalive (does not start a timer)."
  (lambda (env)
    (block app
      (when (and path (not (string= (or (getf env :path-info) "/") path)))
        (return-from app
          '(404 (:content-type "text/plain; charset=utf-8") ("not found"))))
      (multiple-value-bind (result extra)
          (%invoke-handler handler env)
        (let ((events (%coerce-events result)))
          (when keepalive
            (setf events (cons (sse-protocol:make-sse-keepalive-event) events)))
          (list 200
                (sse-response-headers (append extra headers))
                (events-body events)))))))

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
                                            method content)
  (declare (ignore url last-event-id headers timeout method content))
  (error 'sse-protocol:sse-error
         :message "sse-backend-clack is emit-only — use sse-backend-http for open-sse"))

(use-clack-sse-backend)
