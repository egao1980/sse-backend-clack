(in-package #:sse-backend-clack/tests)

(defun ev (&rest args)
  (apply #'sse-protocol:make-sse-event args))

(defun %free-port ()
  (let* ((sock (usocket:socket-listen "127.0.0.1" 0 :reuseaddress t))
         (port (usocket:get-local-port sock)))
    (usocket:socket-close sock)
    port))

(defun %env (&key (path "/") last-event-id)
  (let ((headers (make-hash-table :test 'equal)))
    (when last-event-id
      (setf (gethash "last-event-id" headers) last-event-id))
    (list :path-info path :headers headers :request-method :get)))

(defun %body-string (body)
  (cond
    ((functionp body)
     (with-output-to-string (s) (funcall body s)))
    ((listp body) (apply #'concatenate 'string body))
    ((stringp body) body)
    (t "")))

(defun %bind-http ()
  (http-server-backend-hunchentoot:use-hunchentoot-backend)
  (setf http-protocol:*http-backend*
        (http-backend-dexador:make-dexador-backend)))

(deftest backend-class
  (ok (typep (sse-backend-clack:make-clack-sse-backend)
             'sse-backend-clack:clack-sse-backend)))

(deftest make-sse-app-finite
  (let* ((app (sse-backend-clack:make-sse-app
               (list (ev :id "1" :data "hello")
                     (ev :event "ping" :data "ok"))
               :path "/sse"))
         (res (funcall app (%env :path "/sse")))
         (status (first res))
         (headers (second res))
         (body (third res)))
    (ok (= 200 status))
    (ok (equal "text/event-stream; charset=utf-8"
               (getf headers :content-type)))
    (ok (functionp body))
    (let ((events (with-input-from-string (in (%body-string body))
                    (sse-protocol:collect-sse-events in))))
      (ok (= 2 (length events)))
      (ok (equal "hello" (sse-protocol:sse-event-data (first events))))
      (ok (equal "ping" (sse-protocol:sse-event-type (second events)))))))

(deftest make-sse-stream-app-same-body
  (let* ((app (sse-backend-clack:make-sse-stream-app
               (list (ev :data "hi")) :path "/sse"))
         (body (third (funcall app (%env :path "/sse")))))
    (ok (functionp body))
    (ok (search "data: hi" (%body-string body)))))

(deftest make-sse-app-keepalive-not-prepended
  (let* ((app (sse-backend-clack:make-sse-app (list (ev :data "hi"))
                                              :path "/sse" :keepalive t))
         (body (third (funcall app (%env :path "/sse")))))
    (ok (functionp body))
    (let ((wire (%body-string body)))
      (ok (search "data: hi" wire))
      (ng (search ":ping" wire)))))

(deftest make-sse-app-writer
  (let* ((app (sse-backend-clack:make-sse-app
               (lambda (env)
                 (declare (ignore env))
                 (lambda (stream)
                   (sse-protocol:write-sse-event stream (ev :data "from-writer"))
                   (force-output stream)))
               :path "/sse"))
         (res (funcall app (%env :path "/sse")))
         (evs (with-input-from-string (in (%body-string (third res)))
                (sse-protocol:collect-sse-events in))))
    (ok (functionp (third res)))
    (ok (= 1 (length evs)))
    (ok (equal "from-writer" (sse-protocol:sse-event-data (first evs))))))

(deftest make-sse-app-404
  (let* ((app (sse-backend-clack:make-sse-app (list (ev :data "x")) :path "/sse"))
         (res (funcall app (%env :path "/nope"))))
    (ok (= 404 (first res)))))

(deftest make-sse-app-last-event-id
  (let* ((app (sse-backend-clack:make-sse-app
               (lambda (env)
                 (list (ev :id "2"
                           :data (or (sse-backend-clack:request-last-event-id env)
                                     "none"))))
               :path "/sse"))
         (res (funcall app (%env :path "/sse" :last-event-id "1")))
         (body (%body-string (third res)))
         (evs (with-input-from-string (in body)
                (sse-protocol:collect-sse-events in))))
    (ok (equal "1" (sse-protocol:sse-event-data (first evs))))))

(deftest open-sse-rejected
  (let ((sse-protocol:*sse-backend* (sse-backend-clack:make-clack-sse-backend)))
    (ok (signals (sse-protocol:open-sse "http://127.0.0.1/sse")
                 'sse-protocol:sse-error))))

(deftest live-hunchentoot-dexador
  (%bind-http)
  (let* ((port (%free-port))
         (app (sse-backend-clack:make-sse-app
               (list (ev :id "1" :data "hello")
                     (ev :id "2" :event "ping" :data "ok"))
               :path "/sse")))
    (http-server-protocol:with-server (s app :host "127.0.0.1" :port port)
      (ok (http-server-protocol:running-p s))
      (sleep 0.2)
      (let* ((res (http:get (format nil "http://127.0.0.1:~a/sse" port)
                            :want-stream t
                            :accept-encoding nil
                            :decompress nil))
             (evs (sse-protocol:collect-sse-events
                   (http-protocol:body-stream res))))
        (ok (= 200 (http-protocol:response-status res)))
        (ok (= 2 (length evs)))
        (ok (equal "hello" (sse-protocol:sse-event-data (first evs))))
        (ok (equal "ok" (sse-protocol:sse-event-data (second evs))))))))

(deftest live-last-event-id
  (%bind-http)
  (let* ((port (%free-port))
         (app (sse-backend-clack:make-sse-app
               (lambda (env)
                 (if (equal "1" (sse-backend-clack:request-last-event-id env))
                     (list (ev :id "2" :data "resume"))
                     (list (ev :id "1" :data "first"))))
               :path "/sse")))
    (http-server-protocol:with-server (s app :host "127.0.0.1" :port port)
      (sleep 0.2)
      (let* ((url (format nil "http://127.0.0.1:~a/sse" port))
             (first (http:get url :want-stream t :accept-encoding nil :decompress nil))
             (evs1 (sse-protocol:collect-sse-events (http-protocol:body-stream first)))
             (again (http:get url
                              :want-stream t
                              :accept-encoding nil
                              :decompress nil
                              :headers '(("last-event-id" . "1"))))
             (evs2 (sse-protocol:collect-sse-events (http-protocol:body-stream again))))
        (ok (equal "first" (sse-protocol:sse-event-data (first evs1))))
        (ok (equal "resume" (sse-protocol:sse-event-data (first evs2))))))))

(deftest live-writer-hold
  (%bind-http)
  (let* ((port (%free-port))
         (app (sse-backend-clack:make-sse-app
               (lambda (env)
                 (declare (ignore env))
                 (lambda (stream)
                   (sse-protocol:write-sse-event stream (ev :data "held"))
                   (force-output stream)
                   (sleep 0.12)
                   (sse-protocol:write-sse-keepalive stream)
                   (sse-protocol:write-sse-event stream (ev :data "after"))
                   (force-output stream)))
               :path "/sse")))
    (http-server-protocol:with-server (s app :host "127.0.0.1" :port port)
      (sleep 0.2)
      (let* ((res (http:get (format nil "http://127.0.0.1:~a/sse" port)
                            :want-stream t
                            :accept-encoding nil
                            :decompress nil))
             (evs (sse-protocol:collect-sse-events
                   (http-protocol:body-stream res))))
        (ok (= 2 (length evs)))
        (ok (equal "held" (sse-protocol:sse-event-data (first evs))))
        (ok (equal "after" (sse-protocol:sse-event-data (second evs))))))))

(deftest live-keepalive-on-stream
  (%bind-http)
  (let* ((port (%free-port))
         (app (sse-backend-clack:make-sse-app
               (list (ev :data "hi"))
               :path "/sse" :keepalive 0.05)))
    (http-server-protocol:with-server (s app :host "127.0.0.1" :port port)
      (sleep 0.2)
      (let* ((res (http:get (format nil "http://127.0.0.1:~a/sse" port)
                            :want-stream t
                            :accept-encoding nil
                            :decompress nil))
             (reader (sse-protocol:make-sse-reader
                      (http-protocol:body-stream res)
                      :include-keepalives t))
             (ev1 (sse-protocol:read-sse-event reader :include-keepalives t))
             (ev2 (sse-protocol:read-sse-event reader :include-keepalives t)))
        (ok (equal "hi" (sse-protocol:sse-event-data ev1)))
        (ok (sse-protocol:sse-keepalive-p ev2))
        (ignore-errors (close (http-protocol:body-stream res)))))))
