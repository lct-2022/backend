(uiop:define-package #:passport/user
  (:use #:cl)
  (:import-from #:openrpc-server
                #:transform-result
                #:type-to-schema)
  (:import-from #:serapeum
                #:dict))
(in-package #:passport/user)


(defclass user ()
  ((id :initarg :id
       :type integer
       :accessor user-id)
   (email :initarg :email
          :type string
          :accessor user-email)
   (password-hash :initarg :password-hash
                  :type string
                  :reader user-password-hash)
   (fio :initarg :fio
        :initform nil
        :type (or null string)
        :accessor user-fio)
   (birthday :initarg :birthday
             :initform nil
             :type (or null string)
             :accessor user-birthday)
   (gender :initarg :gender
           :initform nil
           :type (or null string)
           :accessor user-gender)
   (phone :initarg :phone
          :initform nil
          :type (or null string)
          :accessor user-phone)
   (country :initarg :country
            :initform nil
            :type (or null string)
            :accessor user-country)
   (city :initarg :city
         :initform nil
         :type (or null string)
         :accessor user-city)
   (education :initarg :education
              :initform nil
              :type (or null string)
              :accessor user-edication)
   (job :initarg :job
        :initform nil
        :type (or null string)
        :accessor user-job)
   (about :initarg :about
          :initform nil
          :type (or null string)
          :accessor user-about)))


(defmethod transform-result ((obj user))
  (dict "id" (user-id obj)
        "email" (user-email obj)
        "fio" (user-fio obj)))
