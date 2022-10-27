(uiop:define-package #:passport/server
  (:use #:cl)
  (:import-from #:openrpc-server
                #:define-rpc-method)
  (:import-from #:passport/user
                #:user-email
                #:user-password-hash
                #:user)
  (:import-from #:sha1
                #:sha1-hex)
  (:import-from #:common/server
                #:start
                #:stop)
  (:import-from #:common/token
                #:get-jwt-secret))
(in-package #:passport/server)


(defvar *users* nil)


(define-rpc-method login (email password)
  (:param email string)
  (:param password string)
  (:result string)
  (let* ((hash (sha1-hex password))
         (user (loop for user in *users*
                     when (and (string= (user-password-hash user)
                                        hash)
                               (string= (user-email user)
                                        email))
                       do (return user))))
    (cond
      (user (cl-json-web-tokens:issue (serapeum:dict "user-id" 100500)
                                      :algorithm :hs256
                                      :secret (get-jwt-secret)
                                      :issued-at (get-universal-time)
                                      ;; Если захотим, чтобы токены протухали через N минут
                                      ;; :expiration (+ (get-universal-time)
                                      ;;                (* 15 60))
                                      ))
      (t
       (openrpc-server:return-error "Неправильный email или пароль." :code 1)))))


(define-rpc-method create-user (email password)
  (:param email string)
  (:param password string)
  (:result user)
  (openrpc-server:return-error "Not implemented"))
