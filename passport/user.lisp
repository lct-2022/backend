(uiop:define-package #:passport/user
  (:use #:cl)
  (:import-from #:openrpc-server
                #:transform-result
                #:type-to-schema)
  (:import-from #:serapeum
                #:dict)
  (:import-from #:mito
                #:dao-table-class)
  (:import-from #:common/db
                #:sql-fetch-all))
(in-package #:passport/user)


(defclass user ()
  ((id :initarg :id
       :type integer
       :col-type :integer
       :primary-key t
       :accessor user-id)
   (email :initarg :email
          :type string
          :col-type :text
          :accessor user-email)
   (password-hash :initarg :password-hash
                  :type string
                  :col-type :text
                  :reader user-password-hash)
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
              :accessor user-edication)
   (job :initarg :job
        :initform nil
        :type (or null string)
        :col-type (or :null :text)
        :accessor user-job)
   (about :initarg :about
          :initform nil
          :type (or null string)
          :col-type (or :null :text)
          :accessor user-about))
  (:table-name "passport.user")
  (:metaclass dao-table-class))


;; (defmethod transform-result ((obj user))
;;   (dict "id" (user-id obj)
;;         "email" (user-email obj)
;;         "fio" (user-fio obj)))


(defun get-next-user-id ()
  (let* ((rows (sql-fetch-all "select coalesce(max(id), 0) + 1 as next_id from passport.user")))
    (getf (first rows) :|next_id|)))


(defun is-email-available-p (email)
  (let* ((rows (sql-fetch-all "select 1 as value from passport.user where email = ?" email)))
    (null rows)))



