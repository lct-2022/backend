(uiop:define-package #:app/pages/edit-profile
  (:use #:cl)
  (:import-from #:str)
  (:import-from #:reblocks-lass)
  (:import-from #:reblocks/widget
                #:update
                #:render
                #:defwidget)
  (:import-from #:reblocks-lass)
  (:import-from #:reblocks/html
                #:with-html)
  (:import-from #:app/forms
                #:with-html-form)
  (:import-from #:reblocks/dependencies
                #:get-dependencies)
  (:import-from #:serapeum
                #:fmt)
  (:import-from #:common/db
                #:with-connection)
  (:import-from #:platform/profession/model
                #:profession)
  (:import-from #:platform/skill/model
                #:skill)
  (:import-from #:alexandria
                #:remove-from-plistf)
  (:import-from #:common/permissions
                #:get-password-hash)
  (:import-from #:passport/user
                #:user)
  (:import-from #:common/avatar
                #:get-avatar-url-for)
  (:export
   #:make-edit-profile-widget))
(in-package #:app/pages/edit-profile)


(defwidget edit-profile ()
  ((added :initform nil
          :accessor user-added)
   (error :initform nil
          :accessor user-error)))


(defun make-edit-profile-widget ()
  (make-instance 'edit-profile))


(defun get-profession-id (text)
  (when text
    (with-connection ()
      (mito:object-id
       (or (mito:find-dao 'profession
                          :title (str:trim text))
           (mito:create-dao 'profession
                            :title (str:trim text)))))))

(defun get-skill-ids (text &key hard)
  (when text
    (let ((titles (mapcar #'str:trim (str:split ";" text))))
      (with-connection ()
        (loop for title in titles
              collect (mito:object-id
                       (or (mito:find-dao 'skill
                                          :title title)
                           (mito:create-dao 'skill
                                            :title title
                                            :hard hard))))))))


(defun parse-bool (text)
  (string-equal (str:trim text)
                "да"))


(defmethod render ((widget edit-profile))
  (flet ((create-user (&rest args &key id email fio birthday gender phone country city profession skills
                                    education job looking-for-job looking-for-hackathon about
                                    skill-ids
                                    profession-id
                                    avatar-url
                       &allow-other-keys)
           (declare (ignorable id email fio birthday gender phone country city profession skills
                               education job looking-for-job looking-for-hackathon about
                               avatar-url))
           (handler-case
               (progn
                 (setf args
                       (loop for (key value) on args by #'cddr
                             unless (or (null value)
                                        (string= value "")
                                        (string-equal key "action"))
                               append (list key value)))
           
                 (setf (getf args :looking-for-job)
                       (parse-bool looking-for-job))
                 (setf (getf args :profession-id)
                       (get-profession-id profession-id))
                 (setf (getf args :skill-ids)
                       (get-skill-ids skill-ids))
                 (setf (getf args :password-hash)
                       (get-password-hash "test"))
                 (setf (getf args :looking-for-hackathon)
                       (parse-bool looking-for-hackathon))
                 (setf (getf args :avatar-url)
                       (get-avatar-url-for email))

                 (log:info "Creating user" args)
           
                 (with-connection ()
                   (cond
                     ((and id
                           (not (string= id "")))
                      (let ((obj (mito:find-dao 'user
                                                :id id)))
                        (error "Обновление ~A пока не поддерживается." obj)))
                     (t
                      (apply #'mito:create-dao
                             'user
                             args))))
                 (setf (user-added widget) t))
             (error (c)
               (setf (user-error widget) c)))
           (update widget))
         (add-another (&rest args)
           (declare (ignore args))
           (setf (user-added widget) nil)
           (setf (user-error widget) nil)
           (update widget)))

    (with-html
      (:h1 "Добавление тестовой учётки")
      (:p "У добавленного пользователя будет пароль \"test\"."))
    
    (cond
      ((user-error widget)
       (with-html-form (:post #'add-another)
         (:p "При добавлении пользователя произошла ошибка:")
         (:code ("~A" (user-error widget)))
         
         (:p "Попробовать снова?")
         (:p (:button :type "submit"
                      :class "button success"
                      "А давай!"))))
      ((user-added widget)
       (with-html-form (:post #'add-another)
         (:p "Пользователь добавлен. Добавить ещё?")
         (:p (:button :type "submit"
                      :class "button success"
                      "А давай!"))))
      (t
       (with-html-form (:post #'create-user)
         (let* ((slots (get-editable-slots 'user)))
           (:ul
            (loop for slot in slots
                  do (render-slot 'user slot))
            (:button :type "submit"
                     :class "button success"
                     "Добавить"))))))))


(defgeneric get-editable-slots (class)
  (:method ((class symbol))
    (get-editable-slots (find-class class)))
  
  (:method ((class mito.class:table-class))
    (let ((slots-to-ignore (list "ID" "PASSWORD-HASH"
                                 "AVATAR-URL"
                                 "CREATED-AT"
                                 "UPDATED-AT")))
      (loop for slot in (mito.class:database-column-slots class)
            for name = (closer-mop:slot-definition-name slot)
            unless (progn (log:info "CEcheking ~S" name)
                          (member name slots-to-ignore :test #'string-equal))
              collect slot))))


(defgeneric render-slot (class slot)
  (:method (class (slot closer-mop:standard-direct-slot-definition
                   ;; mito.dao.column:dao-table-column-class
                   ))
    (let* ((slot-type (closer-mop:slot-definition-type slot))
           (simplified-slot-type (transform-type class slot-type))
           (slot-name (closer-mop:slot-definition-name slot)))
      (render-input class simplified-slot-type slot-name))))


(defgeneric render-input (class slot-type slot-name)
  (:method ((class t) (slot-type symbol) slot-name)
    (let ((field-id (gensym "FIELD-")))
      (with-html
        (:label :for field-id
                slot-name)
        (:input :id field-id
                :type :text
                :name slot-name))))

  (:method ((class t) (slot-type symbol) (slot-name (eql 'PASSPORT/USER::SKILL-IDS)))
    (let ((field-id (gensym "FIELD-")))
      (with-html
        (:label :for field-id
                "Навыки")
        (:textarea :id field-id
                   :name slot-name
                   :rows 5))))

  ;; TODO: выяснить, почему это не работает:
  (:method ((class t) (slot-type symbol) (slot-name (eql 'passport/user::about)))
    (let ((field-id (gensym "FIELD-")))
      (with-html
        (:label :for field-id
                slot-name)
        (:textarea :id field-id
                   :name slot-name
                   :rows 5)))))


(defgeneric transform-type (class slot-type)
  (:method (class slot-type)
    (cond
      ((or (equalp slot-type
                   '(or null string))
           (equalp slot-type
                   '(or string null)))
       :nullable-string)
      ((equalp slot-type
               '(serapeum:soft-list-of integer))
       :integers)
      ((equalp slot-type
               '(serapeum:soft-list-of string))
       :strings)
      (t
       slot-type))))


(defmethod get-dependencies ((widget edit-profile))
  (list
   (reblocks-lass:make-dependency
     '(.edit-profile
       :width 50%))))
