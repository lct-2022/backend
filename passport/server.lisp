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
                #:fmt))
(in-package #:passport/server)


(defvar *users* nil)

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
  (with-connection ()
    (cond
      ((is-email-available-p email)
       (let ((user (mito:create-dao 'user
                                    :id (get-next-user-id)
                                    :email email
                                    :fio fio
                                    :password-hash (sha1-hex password))))
         (issue-token-for user)))
      (t
       (return-error (format nil "Email ~A уже занят."
                             email)
                     :code 2)))))


(define-rpc-method (passport-api my-profile) ()
  (:result user)
  (with-connection ()
    (with-session (session :require t)
      (mito:find-dao 'user
                     :id (gethash "user-id" session)))))

(define-update-method (passport-api update-profile user)
                      (fio birthday gender phone country city education job about)
  (with-session (session :require t)
    (mito:find-dao 'user
                     :id (gethash "user-id" session))))


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


(defun start-me ()
  (common/server::start passport-api 8000))

(defun stop-me ()
  (common/server::stop 8000))
