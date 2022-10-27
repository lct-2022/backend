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
                #:symbolicate))
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


(defvar *updatable-fields*
  '(fio birthday gender phone country city education job about)
  "Поля пользователя, которые можно обновлять в базе")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun slots-with-types (class-name)
    (let* ((class (closer-mop:ensure-finalized (find-class class-name)))
           (slots (closer-mop:class-slots class)))
      (loop for slot in slots
            for name = (closer-mop:slot-definition-name slot)
            for type = (closer-mop:slot-definition-type slot)
            collect (list name type)))))


(defmacro define-update-method ((api method-name model) fields &body body)
  (let* ((types (slots-with-types model))
         (fields-kwargs (loop for name in fields
                              for given-name = (symbolicate name "-GIVEN-P")
                              collect (list name nil given-name)))
         (update-code (loop for (name default given-name) in fields-kwargs
                            ;; In the TYPES alist, first item is a symbol,
                            ;; corresponding to the slot name
                            for slot-name = (first (assoc name types :test #'string-equal))
                            ;; TODO: use accessors here instead of slot-value
                            collect `(when ,given-name
                                       (setf (slot-value object ',slot-name)
                                             ,name))))
         (param-definitions (loop for name in fields
                                  for type = (second (assoc name types :test #'string-equal))
                                  unless type
                                    do (error "Unable to find type for field ~S."
                                              name)
                                  collect (list :param name type))))
    `(define-rpc-method (,api ,method-name) (&key ,@fields-kwargs)
       ,@param-definitions
       (:result ,model)
       (with-connection ()
         (let ((object (progn ,@body)))
           ,@update-code
           (mito:save-dao object)
           (values object))))))


(define-update-method (passport-api update-profile user)
                      (fio birthday gender phone country city education job about)
  (with-session (session :require t)
    (mito:find-dao 'user
                     :id (gethash "user-id" session))))


(defun start-me ()
  (common/server::start passport-api 8000))

(defun stop-me ()
  (common/server::stop 8000))
