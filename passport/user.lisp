(uiop:define-package #:passport/user
  (:use #:cl)
  (:import-from #:openrpc-server
                #:transform-result
                #:type-to-schema)
  (:import-from #:serapeum
                #:dict)
  (:import-from #:cl-json-web-tokens)
  (:import-from #:mito
                #:object-id
                #:dao-table-class)
  (:import-from #:common/db
                #:sql-fetch-all)
  (:import-from #:common/token
                #:get-jwt-secret))
(in-package #:passport/user)


(defclass user ()
  ((id :initarg :id
       :type integer
       :col-type :integer
       :primary-key t
       :accessor object-id)
   (email :initarg :email
          :type string
          :col-type :text
          :accessor user-email)
   (password-hash :initarg :password-hash
                  :type string
                  :col-type :text
                  :reader user-password-hash)
   (avatar-url :initarg :avatar-url
               :type string
               :col-type :text
               :documentation "По-умолчанию, генерируем URL через Gravatar, по email пользователя. Часто это срабатывает."
               :accessor avatar-url)
   (fio :initarg :fio
        :initform nil
        :type (or null string)
        :col-type (or :null :text)
        :accessor user-fio)
   (birthday :initarg :birthday
             :initform nil
             :type (or null string)
             :col-type (or :null :text)
             :accessor user-birthday)
   (gender :initarg :gender
           :initform nil
           :type (or null string)
           :col-type (or :null :text)
           :accessor user-gender)
   (phone :initarg :phone
          :initform nil
          :type (or null string)
          :col-type (or :null :text)
          :accessor user-phone)
   (country :initarg :country
            :initform nil
            :type (or null string)
            :col-type (or :null :text)
            :accessor user-country)
   (city :initarg :city
         :initform nil
         :type (or null string)
         :col-type (or :null :text)
         :accessor user-city)
   (education :initarg :education
              :initform nil
              :type (or null string)
              :col-type (or :null :text)
              :accessor user-education
              :documentation "Название школы, ВУЗа, ПТУ. Научные степени.")
   (job :initarg :job
        :initform nil
        :type (or null string)
        :col-type (or :null :text)
        :accessor user-job)
   (about :initarg :about
          :initform nil
          :type (or null string)
          :col-type (or :null :text)
          :accessor user-about)
   (admin :initarg :admin
          :initform nil
          :type boolean
          :col-type :boolean
          :accessor adminp
          :documentation "Если этот признак True, то пользователь считается админом и может позволить себе больше, чем простые смертные."))
  (:table-name "passport.user")
  (:metaclass dao-table-class))


(defclass user-with-rating ()
  ((user :initarg :user
         :type user)
   (rating :initarg :rating
           :type integer)))


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
