(defsystem passport
  :class :package-inferred-system
  :pathname "passport"
  :depends-on ("common"
               "passport/server"
               "passport/search"))
