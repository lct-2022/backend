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
  (:import-from #:app/vars
                #:*text-color*
                #:*dark-background*)
  (:export
   #:make-page-with-header))
(in-package #:app/widgets/header)


(defwidget page-with-header ()
  ((content :initarg :content
            :reader content)
   (wide :initarg :wide
         :initform nil
         :reader widep)))


(defun make-page-with-header (content &key wide)
  (make-instance 'page-with-header
                 :content content
                 :wide wide))


(defmethod render ((widget page-with-header))
  (flet ((logout (&rest rest)
           (declare (ignore rest))
           (reblocks/session:reset)
           (reblocks/response:redirect "/logout/"))
         (login (&rest rest)
           (declare (ignore rest))
           (reblocks/response:redirect "/login/")))
    (let* ((api (passport/client::connect
                 (make-passport)
                 (get-user-token)))
           (profile (ignore-errors
                     (passport/client::my-profile api)))
           (avatar-url (when profile
                         (passport/client::user-avatar-url profile))))
      (reblocks/html:with-html
        (:raw "
<!-- Yandex.Metrika counter -->
<script type=\"text/javascript\" >
   (function(m,e,t,r,i,k,a){m[i]=m[i]||function(){(m[i].a=m[i].a||[]).push(arguments)};
   m[i].l=1*new Date();
   for (var j = 0; j < document.scripts.length; j++) {if (document.scripts[j].src === r) { return; }}
   k=e.createElement(t),a=e.getElementsByTagName(t)[0],k.async=1,k.src=r,a.parentNode.insertBefore(k,a)})
   (window, document, \"script\", \"https://mc.yandex.ru/metrika/tag.js\", \"ym\");

   ym(91843672, \"init\", {
        clickmap:true,
        trackLinks:true,
        accurateTrackBounce:true,
        webvisor:true
   });
</script>
<noscript><div><img src=\"https://mc.yandex.ru/watch/91843672\" style=\"position:absolute; left:-9999px;\" alt=\"\" /></div></noscript>
<!-- /Yandex.Metrika counter -->
")
        (:header
         (:div :class "navbar"
               (:div :class "main-logo"
                     (:div :class "title"
                           (:a :href "/"
                               "ChitChat")))
               (:div :class "right-block"
                     ;; (:nav :class "navbar-menu"
                     ;;       (:a :class "navbar-menu-point"
                     ;;           :href "/"
                     ;;           "TV программа"))
                     ;; Иконка профиля
                     (if avatar-url
                         (:img :class "user-icon"
                               :src avatar-url)
                         (:a :class "login-link"
                             :href "/login"
                             "Войти")))))

        (:div :class (if (widep widget)
                         "wide-page-content"
                         "page-content")
              (render (content widget)))

        (:div :class "footer"
              (when (string-equal (reblocks/request:get-path)
                                  "/")
                (:p :class "contacts"
                    "По любым вопросам обращаться в "
                    (:a :href "https://t.me/ivanodigital"
                        (:img :src "https://altezza-store.ru/images/telegram.png")))))))))



(defmethod get-dependencies ((widget page-with-header))
  (list*
   (reblocks-lass:make-dependency
     `(body
       :background ,*dark-background*
       :color ,*text-color*
       
       (.page-with-header
        :display flex
        :flex-direction column
        :align-items center

        (.navbar
         :display flex
         :justify-content space-between
         :padding-left 1rem
         :padding-right 1rem
         (.main-logo
          :display flex
          :flex-direction column
          :flex-grow 10
          :text-align center
          (a :color ,*text-color*)

          (.title
           :font-size 3rem
           :font-weight bold))

         (.user-icon
          :width 64px)
         ((:or .user-icon
               .login-link)
          :margin-left 3em))
       
        (.page-content
         :width 80%
         :margin-left auto
         :margin-right auto)
        
        (.wide-page-content
         :width 100%)
       
        (header
         :width 100%
         (.main-menu :display flex
                     :align-items center
                     (a :margin-right 1rem))
        
         (input
          :margin 0))

        (.footer
         (.contacts
          :margin-bottom 7rem
          (img
           :height 1em)
          (a :color ,*text-color*))))))

   (reblocks-lass:make-dependency
           `(:media "(max-width: 600px)"
                    (body
                     (.page-with-header
                      (.navbar
                       (.user-icon
                        :width 32px
                        :height 32px)
                       (.main-logo
                        (.motto :display none)))

                      (.page-content
                       :width 100%
                       :margin 0
                       :padding-left 1rem
                       :padding-right 1rem)))))
   
   (call-next-method)))
