(uiop:define-package #:passport/server
  (:use #:cl
        #:common/utils)
  (:import-from #:rating/client
                #:make-rating)
  (:import-from #:openrpc-server
                #:return-error
                #:define-rpc-method)
  (:import-from #:passport/user
                #:user-profession-id
                #:user-profession
                #:user-with-rating
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
                #:gravatar)
  (:import-from #:platform/client
                #:make-platform)
  (:import-from #:function-cache
                #:defcached))
(in-package #:passport/server)


(defparameter *default-avatar*
  "http://www.gravatar.com/avatar/501a6ae10e3fc3956ad1052cfc6d38d9?s=200")

(defparameter *default-profession-id* 42)


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


(define-rpc-method (passport-api my-profile) (&key additional-fields)
  (:summary "Отдаёт профиль текущего залогиненого пользователя.")
  (:description "В additional-fields можно передать \"projects\", чтобы в поле \"projects\" подтянулись проекты пользователя.")
  (:param additional-fields (list-of string)
          "В этом списке строк можно указывать только \"projects\".")
  (:result user)
  (with-connection ()
    (with-session (user-id)
      (let ((user (find-dao 'user
                            :id user-id)))
        (enrich-user user additional-fields)))))


(define-rpc-method (passport-api my-roles) ()
  (:summary "Отдаёт список строк с ролями текущего залогинового пользователя.")
  (:result (list-of string))
  (with-session ((roles))
    (map 'vector #'string-downcase roles)))


(define-update-method (passport-api update-profile user)
                      (fio
                       birthday
                       gender
                       phone
                       country
                       city
                       profession-id
                       skill-ids
                       education
                       job
                       about
                       looking-for-job
                       looking-for-hackathon)
  (with-session (user-id)
    (find-dao 'user
              :id user-id)))


(defcached (get-professions-map :timeout (* 15 60)) ()
  (loop with client = (platform/client::connect (make-platform))
        with items = (platform/client:get-professions client)
        with result = (make-hash-table)
        for item in items
        do (setf (gethash (platform/client::profession-id item) result)
                 (platform/client::profession-title item))
        finally (return result)))


(defcached (get-skills-map :timeout (* 15 60)) ()
  (loop with client = (platform/client::connect (make-platform))
        with items = (platform/client:get-skills client)
        with result = (make-hash-table)
        for item in items
        do (setf (gethash (platform/client:skill-id item) result)
                 (platform/client:skill-title item))
        finally (return result)))


(defun enrich-users (users additional-fields)
  (when (member "projects" additional-fields
                :test #'string-equal)
    (loop for user in users
          do (let* ((client (platform/client::connect
                             (make-platform))))
               (setf (passport/user::user-projects user)
                     (platform/client:user-projects client (object-id user)))
               user)))

  (loop with professions = (get-professions-map)
        with skills = (get-skills-map)
        for user in users
        do (setf (user-profession user)
                 (gethash (user-profession-id user) professions *default-profession-id*))
           (setf (passport/user::user-skills user)
                 (remove-if #'null
                            (mapcar (lambda (id)
                                      (gethash id skills ))
                                    (passport/user::user-skill-ids user)))))
  
  users)


(defun enrich-user (user additional-fields)
  (enrich-users (list user) additional-fields)
  user)


(define-rpc-method (passport-api get-profile) (id &key additional-fields)
  (:param id integer "ID пользователя")
  (:param additional-fields (list-of string)
          "Опциональный список полей, которые нужно заполнить для пользователя.
           Пока поддерживается только \"projects\".")
  (:result user)
  (with-connection ()
    (let ((user (mito:find-dao 'user
                               :id id)))
      (cond
        (user
         (enrich-user user additional-fields))
        (t
         (return-error (fmt "Пользователь с id = ~A не найден."
                            id)
                       :code 4))))))


(define-rpc-method (passport-api popular-profiles) (&key (limit 10))
  (:param limit integer)
  (:result (list-of user-with-rating))

  (with-connection ()
    ;; Сначала стучимся с микросервис рейтингов, и получаем top популярных профилей
    (let* ((client (rating/client:connect (make-rating)))
           (top (rating/client::get-top client "user" :limit limit))
           (top-ids (mapcar #'rating/client::top-item-subject-id top))
           (users
             ;; Теперь запросим их по id и отдадим уже проекты
             (select-dao-by-ids 'user
                                top-ids)))
      ;; Нам надо вернуть объект вместе с его рейтингом.
      (loop for top-item in top
            for user in users
            collect (make-instance 'user-with-rating
                                   :user user
                                   :rating (rating/client::top-item-rating top-item))))))


(defun start-me ()
  (common/server::start passport-api 8000))

(defun stop-me ()
  (common/server::stop 8000))
