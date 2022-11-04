(uiop:define-package #:common/logging
  (:use #:cl)
  (:import-from #:log4cl-extras/config))
(in-package #:common/logging)


(defun current-environment ()
  (or (uiop:getenv "ENVIRONMENT")
      "development"))


(defun setup ()
  (let ((appenders
          (cond
            ((string-equal (current-environment)
                           "development")
             (list '(this-console
                     :layout :plain
                     :filter :debug)))
            (t
             (list `(this-console
                     :stream ,*standard-output*
                     :layout :json
                     :filter :debug))))))

    (log4cl-extras/config:setup
     (list :level :warn
           :appenders appenders))))
