(uiop:define-package #:common/avatar
  (:use #:cl)
  (:import-from #:avatar-api
                #:gravatar)
  (:import-from #:dexador)
  (:import-from #:plump)
  (:import-from #:clss)
  (:import-from #:function-cache
                #:defcached)
  (:export
   #:get-avatar-url-for))
(in-package #:common/avatar)


(defparameter *default-avatar*
  "http://www.gravatar.com/avatar/501a6ae10e3fc3956ad1052cfc6d38d9?s=200")


(defun random-avatar ()
  (let* ((response (dex:get "https://randomavatar.com/"))
         (doc (plump:parse response))
         (images (clss:select "#icons > div > a > img" doc))
         (first (aref images 0)))
    (plump:attribute first "src")))


(defun get-content-length (url)
  (multiple-value-bind (response code headers)
      (dex:get url)
    (declare (ignore response code))
    (parse-integer
     (or (gethash "content-length" headers)
         "0"))))


(defcached (get-default-gravatar-length :timeout (* 15 60)) ()
  (get-content-length
   (gravatar "unknown-email@svetlyak.ru" 200)))


(defun default-gravatar-p (avatar-url)
  (= (get-content-length avatar-url)
     (get-default-gravatar-length)))


(defun get-avatar-url-for (email)

  (handler-case
      (let ((url (gravatar email 200)))
        (if (default-gravatar-p url)
            (random-avatar)
            url))
    (error (err)
      (log:error "Unable to retrieve avatar for ~A because of: ~A"
                 email err)
      *default-avatar*)))
