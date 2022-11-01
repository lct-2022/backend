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
                #:appendf)
  (:import-from #:rating/client
                #:make-rating))
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
  ((id :initarg :id
       :reader user-id)
   (fio :initarg :fio
        :reader user-fio)
   (avatar-url :initarg :avatar-url
               :reader user-avatar)
   (raw-user :initarg :raw
             :reader raw-user)
   (rating :initarg :rating
           :reader user-rating)))


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
      (let* ((ids (mapcar #'passport/client:user-id users))
             (rating-client (rating/client::connect (make-rating)))
             (ratings (rating/client:get-ratings rating-client "user" ids))
             (new-profiles
               (loop for user in users
                     for rating in ratings
                     collect (make-instance 'user-profile
                                            :raw user
                                            :id (passport/client:user-id user)
                                            :avatar-url (passport/client:user-avatar-url user)
                                            :fio (passport/client:user-fio user)
                                            :rating rating))))
        (appendf (users-list widget)
                 new-profiles))
      (setf (next-page-func widget)
            next-page-func))))


(defmethod render ((widget user-profile))
  (with-html
    (:img :class "avatar"
          :src (user-avatar widget))
    (:span :class "fio" (user-fio widget))
    (:span :class "invite-button"
           (:input :class "button success"
                   :value "Позвать"))
    (:span :class "rating"
           (:span :class "rating-value"
                  ("~A" (user-rating widget)))
           (:span :class "rating-label"
                  "в рейтинге"))))


(defmethod get-dependencies ((widget user-profile))
  (list
   (reblocks-lass:make-dependency
     '(.user-profile
       :margin-bottom 1em
       :border 1px solid gray
       :display flex
       :flex-direction row
       :justify-content space-between
       :align-items center
       (.avatar
        :display flex
        :width 50px
        :height 50px)
       (.fio
        :display flex
        :flex-grow 10
        :padding-left 1rem)
       (.invite-button
        :display flex
        :margin-left 1rem
        :margin-right 1rem
        (input :margin 0))
       (.rating
        :display flex                   
        :flex-direction row             
        :align-items center             
        :padding 3px 10px               
        :gap 10px                       

        :width 149px                    
        :height 36px                    
        
        :background "#F9F5FF"             
        :border-radius 8px
        (.rating-value :color "#6941C6")
        (.rating-label :color "#344054"))))))


(defmethod render ((widget profiles))
  (flet ((do-search (&key query &allow-other-keys)
           (change-query widget query)
           (update widget))
         (retrieve-more-results (&rest args)
           (declare (ignore args))
           (retrieve-next-page widget)
           (update widget)))
    (with-html
      (with-html-form (:post #'do-search :class "search-form")
        (:input :type "text"
                :class "search-input"
                :name "query"
                :value (search-query widget))
        (:input :type "submit"
                :class "button"
                :value "Найти"))
      (cond
        ((users-list widget)
         (loop for user in (users-list widget)
               do (render user))
         (when (next-page-func widget)
           (with-html-form (:post #'retrieve-more-results
                                  :class "more-form")
             (:input :type "submit"
                     :class "button"
                     :value "Ещё"))))
        (t
         (:p "Нет пользователей по такому запросу"))))))


(defmethod get-dependencies ((widget profiles))
  (list
   (reblocks-lass:make-dependency
     '(.profiles
       :width 80%
       :margin-top 2rem
       :margin-left auto
       :margin-right auto
       (.search-form
        :display flex
        :align-items center
        (.search-input :margin-right 1rem))
       (.more-form
        :display flex
        :justify-content center)))))
