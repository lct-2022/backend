(uiop:define-package #:app/pages/profiles
  (:use #:cl)
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
  (:import-from #:app/vars
                #:*url-prefix*)
  (:import-from #:passport/client
                #:make-passport)
  (:import-from #:openrpc-client/error
                #:rpc-error
                #:rpc-error-message)
  (:import-from #:alexandria
                #:appendf))
(in-package #:app/pages/profiles)


(defwidget profiles ()
  ((query :initarg :query
          :initform ""
          :accessor search-query)
   (users :initarg :users
          :initform nil
          :accessor users-list)
   (next-page-func :initarg :next-page
                   :initform nil
                   :accessor next-page-func)))


(defwidget user-profile ()
  ((fio :initarg :fio
        :reader user-fio)
   (avatar-url :initarg :avatar-url
               :reader user-avatar)
   (raw-user :initarg :raw
             :reader raw-user)))


(defun make-profiles-widget ()
  (let ((widget (make-instance 'profiles
                               :next-page (make-first-page-retriever "*"))))
    (retrieve-next-page widget)
    (values widget)))


(defun make-first-page-retriever (query)
  (flet ((retrieve-first-page ()
           (let* ((query (cond
                           ((string= query "") "*")
                           (t query)))
                  (client (passport/client::connect (make-passport))))
             (passport/client:search-users client query))))
    #'retrieve-first-page))


(defun change-query (widget new-query)
  (setf (search-query widget) new-query
        (users-list widget) nil
        (next-page-func widget) (make-first-page-retriever new-query))
  (retrieve-next-page widget))


(defun retrieve-next-page (widget)
  (when (next-page-func widget)
    (multiple-value-bind (users next-page-func)
        (funcall (next-page-func widget))
      (appendf (users-list widget)
               (loop for user in users
                     collect (make-instance 'user-profile
                                            :raw user
                                            :avatar-url (passport/client:user-avatar-url user)
                                            :fio (passport/client:user-fio user))))
      (setf (next-page-func widget)
            next-page-func))))


(defmethod render ((widget user-profile))
  (with-html
    (:img :class "avatar"
          :src (user-avatar widget))
    (:span (user-fio widget))))


(defmethod get-dependencies ((widget user-profile))
  (list
   (reblocks-lass:make-dependency
     '(.user-profile
       :margin-bottom 1em
       (.avatar
        :width 50px
        :display inline)))))


(defmethod render ((widget profiles))
  (flet ((do-search (&key query &allow-other-keys)
           (change-query widget query)
           (update widget))
         (retrieve-more-results (&rest args)
           (declare (ignore args))
           (retrieve-next-page widget)
           (update widget)))
    (with-html
      (with-html-form (:post #'do-search)
        (:input :type "text"
                :name "query"
                :value (search-query widget))
        (:input :type "submit"
                :class "button"
                :value "Найти"))
      (cond
        ((users-list widget)
         (:ul (loop for user in (users-list widget)
                    do (render user)))
         (when (next-page-func widget)
           (with-html-form (:post #'retrieve-more-results)
             (:input :type "submit"
                     :class "button"
                     :value "Ещё"))))
        (t
         (:p "Нет пользователей по такому запросу"))))))
