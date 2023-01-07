(uiop:define-package #:app/utils/user
  (:use #:cl)
  (:import-from #:function-cache
                #:defcached)
  (:import-from #:passport/client
                #:make-passport)
  (:import-from #:app/utils
                #:get-user-token))
(in-package #:app/utils/user)


(defcached (get-user-profile :timeout 15) (user-id)
  (let* ((api (passport/client::connect
               (make-passport)
               (get-user-token))))
    (passport/client:get-profile api user-id)))


(defcached (get-user-avatar :timeout 60) (user-id)
  (passport/client:user-avatar-url
   (get-user-profile user-id)))


(defcached (get-user-name :timeout 60) (user-id)
  (passport/client:user-nickname
   (get-user-profile user-id)))

