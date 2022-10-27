(uiop:define-package #:common/token
  (:use #:cl)
  (:import-from #:cl-json-web-tokens)
  (:export
   #:get-jwt-secret))
(in-package #:common/token)


(defun decode (token)
  (cl-json-web-tokens:decode token :secret (get-jwt-secret)))


(defun get-jwt-secret ()
  (or (uiop:getenv "JWT_SECRET")
      "test-secret"))
