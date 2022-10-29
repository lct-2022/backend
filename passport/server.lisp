(uiop:define-package #:passport/server
  (:use #:cl)
  (:import-from #:openrpc-server
                #:return-error
                #:define-rpc-method)
  (:import-from #:passport/user
                #:issue-token-for
                #:get-user-by
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
                #:with-log-unhandled)
  (:import-from #:common/session
                #:with-session)
  (:import-from #:alexandria
                #:symbolicate)
  (:import-from #:common/rpc
                #:define-update-method)
  (:import-from #:serapeum
                #:fmt)
  (:import-from #:sxql
                #:limit
                #:order-by)
  (:import-from #:mito
                #:find-dao)
  (:import-from #:avatar-api
                #:gravatar))
(in-package #:passport/server)


(defparameter *default-avatar*
  "http://www.gravatar.com/avatar/501a6ae10e3fc3956ad1052cfc6d38d9?s=200")


(define-api (passport-api :title "Passport API"))


(define-rpc-method (passport-api login) (email password)
  (:param email string)
  (:param password string)
  (:result string)
  (with-connection ()
    (let* ((hash (sha1-hex password))
           (user (get-user-by email))
           (user-hash (when user
                        (user-password-hash user))))
      (cond
        ((equal user-hash hash)
         (issue-token-for user))
        (t
         (openrpc-server:return-error "Неправильный email или пароль." :code 1))))))


(define-rpc-method (passport-api signup) (email password fio)
  (:param email string)
  (:param password string)
  (:param fio string)
  (:result string)
  (log:info "Signup")
  (with-connection ()
    (cond
      ((is-email-available-p email)
       (let ((user (mito:create-dao 'user
                                    :id (get-next-user-id)
                                    :email email
                                    :fio fio
                                    :avatar-url (handler-case (gravatar email 200)
                                                  (error (err)
                                                    (log:error "Unable to retrieve avatar for ~A because of: ~A"
                                                               email err)
                                                    *default-avatar*))
                                    :password-hash (sha1-hex password))))
         (issue-token-for user)))
      (t
       (return-error (format nil "Email ~A уже занят."
                             email)
                     :code 2)))))


(define-rpc-method (passport-api my-profile) ()
  (:summary "Отдаёт профиль текущего залогиненого пользователя.")
  (:result user)
  (with-connection ()
    (with-session (user-id)
      (find-dao 'user
                :id user-id))))


(define-rpc-method (passport-api my-roles) ()
  (:summary "Отдаёт список строк с ролями текущего залогинового пользователя.")
  (:result (list-of string))
  (with-session ((roles))
    (map 'vector #'string-downcase roles)))


(define-update-method (passport-api update-profile user)
                      (fio birthday gender phone country city education job about)
  (with-session (user-id)
    (find-dao 'user
              :id user-id)))


(define-rpc-method (passport-api get-profile) (id)
  (:param id integer "ID пользователя")
  (:result user)
  (with-connection ()
    (let ((user (mito:find-dao 'user
                               :id id)))
      (if user
          user
          (return-error (fmt "Пользователь с id = ~A не найден."
                             id)
                        :code 4)))))


(define-rpc-method (passport-api popular-profiles) (&key (limit 10))
  (:param limit integer)
  (:result (list-of user))
  (with-connection ()
    (or (mito:select-dao 'user
          (order-by (:desc :created-at))
          (limit limit))
        #())))


(defun start-me ()
  (common/server::start passport-api 8000))

(defun stop-me ()
  (common/server::stop 8000))
