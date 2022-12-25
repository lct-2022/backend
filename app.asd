(pushnew "~/projects/reblocks/" asdf:*central-registry* :test #'equal)
(pushnew "~/projects/reblocks-ui/" asdf:*central-registry*  :test #'equal)

(defsystem app
  :class :package-inferred-system
  :pathname "frontend"
  :depends-on ("common"
               "cl-postgres+local-time"
               "app/server"))


(asdf:register-system-packages "serapeum" '("SERAPEUM/BUNDLE"))
