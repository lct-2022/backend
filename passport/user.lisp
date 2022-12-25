(uiop:define-package #:passport/user
  (:use #:cl)
  (:import-from #:openrpc-server
                #:transform-result
                #:type-to-schema)
  (:import-from #:serapeum
                #:fmt
                #:soft-list-of
                #:dict)
  (:import-from #:cl-json-web-tokens)
  (:import-from #:mito
                #:object-id
                #:dao-table-class)
  (:import-from #:common/db
                #:sql-fetch-all)
  (:import-from #:common/token
                #:get-jwt-secret)
  (:import-from #:common/permissions
                #:assert-can-modify)
  (:import-from #:common/utils
                #:decode-json
                #:encode-json)
  (:import-from #:common/avatar
                #:get-avatar-url-for)
  (:import-from #:passport/nickname
                #:get-random-nickname))
(in-package #:passport/user)


(defclass user ()
  ((id :initarg :id
       :type integer
       :col-type :bigserial
       :primary-key t
       :accessor object-id)
   (nickname :initarg :nickname
             :type string
             :col-type :text
             :accessor user-nickname)
   (email :initarg :email
          :type string
          :col-type (or :null :text)
          :accessor user-email)
   (password-hash :initarg :password-hash
                  :type string
                  :col-type (or :null :text)
                  :reader user-password-hash)
   (avatar-url :initarg :avatar-url
               :type string
               :col-type (:or :null :text)
               :accessor avatar-url)
   (admin :initarg :admin
          :initform nil
          :type boolean
          :col-type :boolean
          :accessor adminp
          :documentation "Если этот признак True, то пользователь считается админом и может позволить себе больше, чем простые смертные."))
  (:table-name "passport.user")
  (:metaclass dao-table-class))


(defmethod print-object ((user user) stream)
  (print-unreadable-object (user stream :type t)
    (format stream "ID=~A EMAIL=~S"
            (object-id user)
            (user-email user))))



(defun get-next-user-id ()
  (let* ((rows (sql-fetch-all "select coalesce(max(id), 0) + 1 as next_id from passport.user")))
    (getf (first rows) :|next_id|)))


(defun is-email-available-p (email)
  (let* ((rows (sql-fetch-all "select 1 as value from passport.user where email = ?" email)))
    (null rows)))


(defun get-user-by (email)
  (mito:find-dao 'user :email email))


(defun issue-token-for (user)
  (let ((payload (dict "user-id" (object-id user)
                       "nickname" (user-nickname user)
                       ;; Пока у нас только одна роль. Но на будущее, роли отдаются списоком:
                       "roles" (when (adminp user)
                                 (list "admin")))))
    (cl-json-web-tokens:issue payload
                              :algorithm :hs256
                              :secret (get-jwt-secret)
                              :issued-at (get-universal-time)
                              ;; Если захотим, чтобы токены протухали через N минут
                              ;; :expiration (+ (get-universal-time)
                              ;;                (* 15 60))
                              )))


(defun make-unique-nickname (nickname)
  (loop for idx upfrom 0 to 10000
        for candidate = (if (zerop idx)
                            nickname
                            (fmt "~A-~A" nickname idx))
        unless (mito:find-dao 'user
                              :nickname candidate)
        do (return candidate)))

(defun make-anonymous-user ()
  (let ((nickname (make-unique-nickname
                   (get-random-nickname))))
    (mito:create-dao 'user
                     :nickname nickname
                     :avatar-url (fmt "https://robohash.org/~A.png"
                                      nickname))))


(defmethod assert-can-modify ((user-id integer) (obj user))
  ;; Пользователь может редактировать свой собственный профиль,
  ;; а админ может редактировать любой:
  (let* ((is-admin
           (mito:retrieve-by-sql
            "select 1 FROM passport.user as p
                 where p.id = ? AND p.admin"
            :binds (list user-id))))
    (or (= user-id (object-id obj))
        is-admin)))


(defun randomize-avatars ()
  (common/db::with-connection ()
    (loop for user in (mito:retrieve-dao 'user)
          do (setf (avatar-url user)
                   (get-avatar-url-for (user-email user)))
             (mito:save-dao user))))
