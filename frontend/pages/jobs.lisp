(uiop:define-package #:app/pages/jobs
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
                #:ellipsize
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
  (:import-from #:platform/client
                #:make-platform))
(in-package #:app/pages/jobs)


(defparameter *job-description-limit* 200)


(defwidget jobs ()
  ((query :initarg :query
          :initform ""
          :accessor search-query)
   (jobs :initarg :jobs
         :initform nil
         :accessor jobs-list)
   (next-page-func :initarg :next-page
                   :initform nil
                   :accessor next-page-func)))


(defwidget job ()
  ((id :initarg :id
       :reader job-id)
   (title :initarg :title
          :reader job-title)
   (description :initarg :description
                :reader job-description)
   (raw-job :initarg :raw
            :reader raw-job)))


(defun make-jobs-widget ()
  (let ((widget (make-instance 'jobs
                               :next-page (make-first-page-retriever "*"))))
    (retrieve-next-page widget)
    (values widget)))


(defun make-first-page-retriever (query)
  (flet ((retrieve-first-page ()
           (let* ((query (cond
                           ((string= query "") "*")
                           (t query)))
                  (client (platform/client::connect (make-platform))))
             (platform/client:search-jobs client query))))
    #'retrieve-first-page))


(defun change-query (widget new-query)
  (setf (search-query widget) new-query
        (jobs-list widget) nil
        (next-page-func widget) (make-first-page-retriever new-query))
  (retrieve-next-page widget))


(defun retrieve-next-page (widget)
  (when (next-page-func widget)
    (multiple-value-bind (jobs next-page-func)
        (funcall (next-page-func widget))
      (let* (;; (ids (mapcar #'passport/client:user-id jobs))
             ;; (rating-client (rating/client::connect (make-rating)))
             ;; (ratings (rating/client:get-ratings rating-client "user" ids))
             (new-jobs
               (loop for job in jobs
                     ;; for rating in ratings
                     collect (make-instance 'job
                                            :raw job
                                            :id (platform/client:job-id job)
                                            :title (platform/client:job-title job)
                                            :description (platform/client:job-description job)
                                            ;; :rating rating
                                            ))))
        (appendf (jobs-list widget)
                 new-jobs))
      (setf (next-page-func widget)
            next-page-func))))


(defmethod render ((widget job))
  (with-html
    (:div :class "title" (job-title widget))
    (:div :class "description"
          (ellipsize (job-description widget)
                     *job-description-limit*))))


(defmethod get-dependencies ((widget job))
  (list
   (reblocks-lass:make-dependency
     '(.job
       :display flex
       :flex-direction column
       :background "#FFF"
       :border 1px solid "#98A2B3"
       :border-radius 5px
       :margin-bottom 1rem
       :padding 0.5ch))))


(defmethod render ((widget jobs))
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
        ((jobs-list widget)
         (loop for job in (jobs-list widget)
               do (render job))
         (when (next-page-func widget)
           (with-html-form (:post #'retrieve-more-results
                                  :class "more-form")
             (:input :type "submit"
                     :class "button"
                     :value "Ещё"))))
        (t
         (:p "Нет вакансий по такому запросу"))))))


(defmethod get-dependencies ((widget jobs))
  (list
   (reblocks-lass:make-dependency
     '(.jobs
       :width 100%
       :margin-top 2rem
       (.search-form
        :display flex
        :align-items center
        (.search-input :margin-right 1rem))
       (.more-form
        :display flex
        :justify-content center)))))
