# sse-backend-clack

Clack/http-server-protocol emit backend for sse-protocol.

Part of [cl-stack](https://github.com/egao1980/cl-stack) agent-wire ([brief](https://github.com/egao1980/cl-stack/blob/main/docs/capabilities/agent-wire.md)).

```lisp
(asdf:load-system "sse-backend-clack")

(sse-backend-clack:use-clack-sse-backend)
(http-server-backend-hunchentoot:use-hunchentoot-backend)

(http-server-protocol:with-server
    (s (sse-backend-clack:make-sse-app
        (list (sse-protocol:make-sse-event :data "hi"))
        :path "/sse")
       :host "127.0.0.1" :port 8080)
  ...)
```

`sbcl --load scripts/app-roundtrip.lisp`

CI: `setup-client` + `setup-roswell` + `scripts/ci-install.lisp` / `ci-test.lisp` (OCI only, no Quicklisp).

## License

MIT
