;;;; Invoke make-sse-app as a Clack function (no listen) and decode the body.
;;;;   sbcl --load scripts/app-roundtrip.lisp

(setf *debugger-hook*
      (lambda (c h)
        (declare (ignore h))
        (format *error-output* "~&app-roundtrip failed: ~a~%" c)
        (uiop:quit 1)))

(defun %here ()
  (uiop:pathname-directory-pathname
   (or *load-truename* *compile-file-truename* (uiop:getcwd))))

(pushnew (uiop:pathname-parent-directory-pathname (%here))
         asdf:*central-registry* :test #'equal)
(asdf:load-system "sse-backend-clack")

(defun ev (&rest args)
  (apply #'sse-protocol:make-sse-event args))

(let* ((app (sse-backend-clack:make-sse-app
             (list (ev :id "1" :data "hello")
                   (ev :event "ping" :data "ok"))
             :path "/sse"))
       (env (list :path-info "/sse"
                  :headers (make-hash-table :test 'equal)
                  :request-method :get))
       (res (funcall app env))
       (body (apply #'concatenate 'string (third res)))
       (evs (with-input-from-string (in body)
              (sse-protocol:collect-sse-events in))))
  (unless (= 200 (first res))
    (format *error-output* "~&FAIL: status ~a~%" (first res))
    (uiop:quit 1))
  (unless (and (= 2 (length evs))
               (equal "hello" (sse-protocol:sse-event-data (first evs)))
               (equal "ok" (sse-protocol:sse-event-data (second evs))))
    (format *error-output* "~&FAIL: events ~a~%" evs)
    (uiop:quit 1)))

(format t "~&; sse-backend-clack app-roundtrip ok~%")
(uiop:quit 0)
