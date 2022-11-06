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
  (:import-from #:app/utils
                #:get-user-token)
  (:import-from #:passport/client
                #:make-passport)
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
    (let* ((api (passport/client::connect
                 (make-passport)
                 (get-user-token)))
           (profile (ignore-errors
                     (passport/client::my-profile api)))
           (avatar-url (when profile
                         (passport/client::user-avatar-url profile))))
      (reblocks/html:with-html
        (:header
         (:link :href "https://ideahunt.ru/static/css/main.a005c85d.css"
                :rel "stylesheet")
         (:div :class "navbar"
               (:img :class "navbar-main-logo"
                     :src "https://sportishka.com/uploads/posts/2022-03/1646088782_53-sportishka-com-p-gori-kirgizstana-turizm-krasivo-foto-60.jpg")
               (:div :class "navbar-right-block"
                     (:nav :class "navbar-menu"
                           (:a :class "navbar-menu-point"
                               :href "/profiles"
                               "Эксперты")
                           (:a :class "navbar-menu-point active"
                               :href "/projects"
                               "Проекты")
                           (:a :class "navbar-menu-point"
                               :href "/services"
                               "Сервисы")
                           (:a :class "navbar-menu-point"
                               :href "/events"
                               "Мероприятия")
                           (:a :class "navbar-menu-point"
                               :href "/vacancies"
                               "Вакансии"))
                     ;; Иконка профиля
                     (if avatar-url
                         (:img :class "navbar-user-icon"
                               :src avatar-url)
                         (:a :class "login-link"
                             :href "/login"
                             "Войти"))))
        
         ;; (:div :class "logo"
         ;;       (:img :src "http://placekitten.com/100/100"))
         ;; (:div :class "main-menu"
         ;;       (:a :href "/alternative/profiles/"
         ;;           "Эксперты")
         ;;       (:a :href "/alternative/jobs/"
         ;;           "Вакансии")
         ;;       (cond
         ;;         ;; ((get-username)
         ;;         ;;  (:a :href "/feedback"
         ;;         ;;      "Обратная связь")
        
         ;;         ;;  (render-form-and-button "Выйти"
         ;;         ;;                          #'logout
         ;;         ;;                          :method :post
         ;;         ;;                          :button-class "button secondary logout"))
         ;;         (t
         ;;          (render-form-and-button "Войти"
         ;;                                  #'login
         ;;                                  :method :post
         ;;                                  :button-class "button secondary login"))))
         )

        (:div :class "page-content"
              (render (content widget)))))))



(defmethod get-dependencies ((widget page-with-header))
  (list
   (reblocks-lass:make-dependency
     `(.page-with-header
       :display flex
       :flex-direction column
       :align-items center

       ((:or .navbar-user-icon
             .login-link)
        :margin-left 3em)
       
       (.page-content
        :width 80%
        :margin-left auto
        :margin-right auto)
       
       (header
        :width 100%
        ;; :display flex
        ;; :flex-direction row
        ;; :align-items center
        ;; :justify-content space-between
        ;; :border-bottom 1px solid "#CCCCCC"
        ;; :margin-top 0.5rem
        ;; :padding-bottom 1rem

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
