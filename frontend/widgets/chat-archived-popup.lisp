(uiop:define-package #:app/widgets/chat-archived-popup
  (:use #:cl)
  (:import-from #:reblocks/widget
                #:defwidget)
  (:import-from #:reblocks-ui/popup
                #:render-popup-content
                #:hide-popup
                #:popup-widget)
  (:import-from #:serapeum
                #:fmt)
  (:import-from #:reblocks/html
                #:with-html)
  (:import-from #:reblocks-ui/form
                #:render-form-and-button)
  (:import-from #:reblocks-websocket
                #:websocket-widget)
  (:import-from #:reblocks/dependencies
                #:get-dependencies)
  (:import-from #:app/vars
                #:*light-background*))
(in-package #:app/widgets/chat-archived-popup)


(defwidget chat-archived-popup (popup-widget websocket-widget)
  ((next-chat-id :initform nil
                 :accessor next-chat-id)
   (next-programme-title :initform nil
                         :accessor next-programme-title)))



(defmethod render-popup-content ((widget chat-archived-popup))
  (with-html
    (:p "Программа закончилась и чат закрыт.")

    (when (next-chat-id widget)
      (:p "Продолжить общение в чате следующей передачи:")
      (:p (:a :href (fmt "/chat/~A"
                         (next-chat-id widget))
              (next-programme-title widget))))
    
    (:p (render-form-and-button
         :close 
         (lambda (&rest rest)
           (declare (ignore rest))
           (hide-popup widget))
         :method :post
         :value "Закрыть"
         :button-class "button secondary"))))


(defmethod get-dependencies ((widget chat-archived-popup))
  (list*
   (reblocks-lass:make-dependency
     `(.popup.chat-archived-popup
       :z-index 1000
       
       (.popup-content
        :width inherit
        :max-width 80%
        :background ,*light-background*
          
        (.button
         :margin-right 1rem
         :margin-bottom 0))))
   
   (reblocks-lass:make-dependency
     `(:media "(max-width: 600px)"
              (.popup.chat-archived-popup
               (.popup-content
                :max-width 95%))))
   (call-next-method)))
