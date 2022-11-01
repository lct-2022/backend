(defsystem app
  :class :package-inferred-system
  :pathname "frontend"
  :depends-on ("common"
               "app/server"))
