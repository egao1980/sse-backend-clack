(in-package #:sse-backend-clack)

(defclass clack-sse-backend (sse-protocol:sse-backend) ())

(defun make-clack-sse-backend ()
  (make-instance 'clack-sse-backend))

(defun use-clack-sse-backend ()
  (setf sse-protocol:*sse-backend* (make-clack-sse-backend)))
