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

`make-sse-app` / `make-sse-stream-app` write each event through a stream lambda (`force-output`). Hunchentoot does not invoke a function in `(status headers body)`, so the app returns a Clack **response function** that flushes chunks. `:keepalive t` runs `make-sse-keepalive` on that live stream (not a prepended comment). A handler may return a writer `(lambda (stream) …)` instead of a finite event list.

`sbcl --load scripts/app-roundtrip.lisp`

CI: canned [`cl-repository`](https://github.com/egao1980/cl-repository) (`test-system.yml` / `setup-client` + `ci`). Deps from `ghcr.io/egao1980/cl-systems`.

## License

MIT
