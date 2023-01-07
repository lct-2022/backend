(when (probe-file ".local-config.lisp")
  (load (probe-file ".local-config.lisp")))

(pushnew "~/projects/openrpc/" asdf:*central-registry* :test #'equal)
(pushnew "~/projects/cl-schedule/" asdf:*central-registry* :test #'equal)
(pushnew "~/projects/action-list/" asdf:*central-registry* :test #'equal)
(pushnew "~/projects/log4cl-extras/" asdf:*central-registry* :test #'equal)
(pushnew "~/projects/reblocks/" asdf:*central-registry* :test #'equal)
(pushnew "~/projects/reblocks-websocket/" asdf:*central-registry* :test #'equal)
(pushnew "~/projects/reblocks-ui/" asdf:*central-registry*  :test #'equal)
(pushnew "~/projects/event-emitter/" asdf:*central-registry* :test #'equal)

(defsystem app
  :class :package-inferred-system
  :pathname "frontend"
  :depends-on ("common"
               "cl-postgres+local-time"
               "app/server"))


(asdf:register-system-packages "serapeum" '("SERAPEUM/BUNDLE"))
(asdf:register-system-packages "action-list" '("ORG.SHIRAKUMO.FRAF.ACTION-LIST"))
