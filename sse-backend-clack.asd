(defsystem "sse-backend-clack"
  :version "0.1.1"
  :description "Clack/http-server-protocol emit backend for sse-protocol"
  :author "egao1980"
  :license "MIT"
  :depends-on ("sse-protocol" "http-server-protocol" "trivial-gray-streams")
  :properties (:cl-repo (:ci (:with ("dissect"))))
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "backend"))
  :in-order-to ((test-op (test-op "sse-backend-clack/tests"))))

(defsystem "sse-backend-clack/tests"
  :depends-on ("sse-backend-clack"
               "http-server-backend-hunchentoot"
               "http-backend-dexador"
               "rove"
               "usocket")
  :pathname "tests"
  :serial t
  :components ((:file "package")
               (:file "backend-test"))
  :perform (test-op (o c)
             (unless (symbol-call :rove :run c)
               (error "tests failed for ~A" (component-name c)))))
