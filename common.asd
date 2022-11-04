(pushnew "~/lisp/openrpc/" asdf:*central-registry*
         :test #'equal)
(pushnew "~/lisp/jsonrpc/" asdf:*central-registry*
         :test #'equal)
(pushnew "~/lisp/cl-json-web-tokens/" asdf:*central-registry*
         :test #'equal)
(pushnew "~/lisp/log4cl-extras/" asdf:*central-registry*
         :test #'equal)
(pushnew "~/lisp/gen-deps-system/" asdf:*central-registry*
         :test #'equal)
(pushnew "~/lisp/reblocks/" asdf:*central-registry*
         :test #'equal)

(defsystem common
  :class :package-inferred-system
  :pathname "common"
  :depends-on ())


(register-system-packages "mito" '(#:mito.class #:mito.db #:mito.dao #:mito.util))
(register-system-packages "dbd-postgres" '(#:dbd.postgres))
(register-system-packages "cl-dbi" '(#:dbi.cache.thread #:dbi.error))
(register-system-packages "log4cl" '(#:log))
(register-system-packages "slynk" '(#:slynk-api))
;; To prevent mito and clack loading these libraries in runtime
(register-system-packages "dbd-postgres" '(#:dbd.postgres))
(register-system-packages "clack-handler-hunchentoot" '(#:clack.handler.hunchentoot))
