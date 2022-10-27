(uiop:define-package #:passport/server
  (:use #:cl)
  (:import-from #:openrpc-server
                #:return-error
                #:define-rpc-method)
  (:import-from #:passport/user
                #:is-email-available-p
                #:get-next-user-id
                #:user-email
                #:user-password-hash
                #:user)
  (:import-from #:sha1
                #:sha1-hex)
  (:import-from #:common/token
                #:get-jwt-secret)
  (:import-from #:openrpc-server/api
                #:define-api)
  (:import-from #:common/db
                #:with-connection)
  (:import-from #:common/server)
  (:import-from #:log4cl-extras/error
                #:with-log-unhandled))
(in-package #:passport/server)


(defvar *users* nil)

(define-api (passport-api :title "Passport API"))


(define-rpc-method (passport-api login) (email password)
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


(define-rpc-method (passport-api create-user) (email password fio)
  (:param email string)
  (:param password string)
  (:param fio string)
  (:result user)
  (with-connection ()
    (cond
      ((is-email-available-p email)
       (mito:create-dao 'user
                        :id (get-next-user-id)
                        :email email
                        :fio fio
                        :password-hash (sha1-hex password)))
      (t
       (return-error (format nil "Email ~A уже занят."
                             email)
                     :code 2)))))


(defun start-me ()
  (common/server::start passport-api 8000))

(defun stop-me ()
  (common/server::stop 8000))
