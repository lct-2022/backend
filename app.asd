(defsystem app
  :class :package-inferred-system
  :pathname "src/"
  :depends-on ("app/server"))


(register-system-packages "mito" '(#:mito.class #:mito.db #:mito.dao #:mito.util))
(register-system-packages "dbd-postgres" '(#:dbd.postgres))
(register-system-packages "cl-dbi" '(#:dbi.cache.thread #:dbi.error))
(register-system-packages "log4cl" '(#:log))
(register-system-packages "slynk" '(#:slynk-api))
;; To prevent mito and clack loading these libraries in runtime
(register-system-packages "dbd-postgres" '(#:dbd.postgres))
(register-system-packages "clack-handler-hunchentoot" '(#:clack.handler.hunchentoot))

