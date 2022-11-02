(uiop:define-package #:app/widgets/header
  (:use #:cl)
  (:import-from #:reblocks-lass)
  (:import-from #:reblocks/widget
                #:render
                #:defwidget)
  ;; (:import-from #:app/widgets/login
  ;;               #:get-username)
  (:import-from #:reblocks-ui/form
                #:render-form-and-button)
  (:import-from #:reblocks/dependencies
                #:get-dependencies)
  (:export
   #:make-page-with-header))
(in-package #:app/widgets/header)


(defwidget page-with-header ()
  ((content :initarg :content
            :reader content)))


(defun make-page-with-header (content)
  (make-instance 'page-with-header :content content))


(defmethod render ((widget page-with-header))
  (flet ((logout (&rest rest)
           (declare (ignore rest))
           (reblocks/session:reset)
           (reblocks/response:redirect "/alternative/logout/"))
         (login (&rest rest)
           (declare (ignore rest))
           (reblocks/response:redirect "/alternative/login/")))
    (reblocks/html:with-html
      (:header
       (:div :class "logo"
             (:img :src "http://placekitten.com/100/100"))
       (:div :class "main-menu"
             (:a :href "/alternative/profiles/"
                 "Эксперты")
             (:a :href "/alternative/jobs/"
                 "Вакансии")
                    (cond
         ;; ((get-username)
         ;;  (:a :href "/feedback"
         ;;      "Обратная связь")
          
         ;;  (render-form-and-button "Выйти"
         ;;                          #'logout
         ;;                          :method :post
         ;;                          :button-class "button secondary logout"))
         (t
          (render-form-and-button "Войти"
                                  #'login
                                  :method :post
                                  :button-class "button secondary login")))))

      (render (content widget)))))



(defmethod get-dependencies ((widget page-with-header))
  (list
   (reblocks-lass:make-dependency
     '(.page-with-header
       :display flex
       :flex-direction column
       :align-items center
       :background "D0D5DD"
       
       :width 80%
       :margin-left auto
       :margin-right auto

       
       (header
        :width 100%
        :display flex
        :flex-direction row
        :align-items center
        :justify-content space-between
        :border-bottom 1px solid "#CCCCCC"
        :margin-top 0.5rem
        :padding-bottom 1rem

        (.main-menu :display flex
                    :align-items center
         (a :margin-right 1rem))

        
        ;; (h1 :margin-left 1em
        ;;     :width 100%
        ;;     :text-align center)
        
        ;; (form
        ;;  :justify-self end
        ;;  :margin-right 1em)

        (input
         :margin 0)
        )))))
