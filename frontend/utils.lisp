(uiop:define-package #:app/utils
  (:use #:cl)
  (:import-from #:reblocks/request
                #:get-cookie)
  (:import-from #:reblocks/session
                #:get-value)
  (:export
   #:get-user-token))
(in-package #:app/utils)


(defun get-user-token ()
  (or (get-cookie "auth_token")
      (get-value :auth-token)))
