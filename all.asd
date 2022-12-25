(when (probe-file ".local-config.lisp")
  (load (probe-file ".local-config.lisp")))

(defsystem all
  :class :package-inferred-system
  :pathname ""
  :depends-on ("app"
               ;; "platform"
               "passport"
               ;; "rating"
               "chat"
               "all/all"))
