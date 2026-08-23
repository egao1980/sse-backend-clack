(in-package #:sse-backend-clack/tests)

(deftest backend-class
  (ok (typep (sse-backend-clack:make-clack-sse-backend) 'sse-backend-clack:clack-sse-backend)))
