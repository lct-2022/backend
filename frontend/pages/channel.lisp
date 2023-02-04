(uiop:define-package #:app/pages/channel
  (:use #:cl)
  (:import-from #:reblocks/widget
                #:render
                #:defwidget)
  (:import-from #:reblocks/html
                #:with-html)
  (:import-from #:common/db
                #:with-connection)
  (:import-from #:function-cache
                #:defcached)
  (:import-from #:app/program
                #:programme-start
                #:programme-stop
                #:programme-title
                #:programme
                #:channel-name
                #:channel)
  (:import-from #:serapeum
                #:fmt)
  (:import-from #:mito
                #:object-id)
  (:import-from #:reblocks/dependencies
                #:get-dependencies)
  (:import-from #:reblocks-lass)
  (:import-from #:app/vars
                #:*current-source-id*
                #:*light-background*
                #:*text-color*)
  (:import-from #:group-by
                #:child-groupings)
  (:import-from #:reblocks/request
                #:get-path)
  (:import-from #:sxql
                #:where)
  (:import-from #:local-time
                #:now
                #:format-timestring
                #:timestamp<=
                #:timestamp<)
  (:import-from #:app/utils
                #:get-user-timezone)
  (:import-from #:reblocks/response
                #:redirect)
  (:import-from #:app/pages/landing
                #:create-chat-for-program)
  (:import-from #:reblocks-ui/form
                #:render-form-and-button))
(in-package #:app/pages/channel)


(defwidget channel-widget ()
  ((id :initform nil
       :accessor channel-id)
   (channel :initform nil
            :accessor channel)
   (schedule :initform nil
             :accessor schedule)))


(defun make-channel-widget ()
  (make-instance 'channel-widget))


(defmethod render ((widget channel-widget))
  (cl-ppcre:register-groups-bind (channel-id)
      ("/channels/(.*)" (get-path))
    (unless (string-equal (channel-id widget)
                          channel-id)
      (with-connection () 
        (setf (channel-id widget)
              channel-id)
        (setf (channel widget)
              (mito:find-dao 'channel
                             :channel-id channel-id
                             :source-id *current-source-id*))
        (setf (schedule widget)
              (mito:select-dao 'programme
                (where (:and (:= :channel-id channel-id)
                             (:= :source-id *current-source-id*)
                             (:raw "start BETWEEN current_date AND current_date + '1 day'::interval"))))))))
  (with-html
    (let ((channel (channel widget))
          (schedule (schedule widget)))
      (:h1 (channel-name channel))
      (:div :class "schedule"
            (loop with now = (now)
                  for programme in schedule
                  for passed = (timestamp< (programme-stop programme) now)
                  for current = (timestamp<= (programme-start programme)
                                             now
                                             (programme-stop programme))
                  do (render-programme programme passed current))))))


(defun render-programme (programme passed current)
  (with-html
    (let ((classes (append
                    (list "programme")
                    (when passed
                      (list "passed"))
                    (when current
                      (list "current"))))
          (pr-title (programme-title programme)))
      (flet ((open-chat (&rest args)
               (declare (ignore args))
               (let ((chat-id (create-chat-for-program pr-title)))
                 (redirect (fmt "/chat/~A" chat-id)))))
        (:div :class (str:join " " classes)
              (:div :class "interval"
                    (fmt "~A -> ~A"
                         (format-timestring nil (programme-start programme)
                                            :format '((:hour 2) #\: (:min 2))
                                            :timezone (get-user-timezone))
                         (format-timestring nil (programme-stop programme)
                                            :format '((:hour 2) #\: (:min 2))
                                            :timezone (get-user-timezone))))
              (:div :class "title"
                    (programme-title programme)
                    (if current
                        (render-form-and-button :open-chat
                                                #'open-chat
                                                :value "Открыть чат"))))))))


(defmethod get-dependencies ((widget channel-widget))
  (list* (reblocks-lass:make-dependency
           `(.channel-widget
             (.schedule
              :display flex
              :flex-direction column
              :gap 1rem
              (.programme
               :background ,*light-background*
               :padding 1rem
               :border-radius 0.5rem
               (.title
                :display flex
                (form
                 :margin-left 1rem
                 :margin-top -0.5rem
                 (input
                  :margin 0))))
              ((:and .programme .passed)
               (.title
                :color "#949393"))
              ((:and .programme .current)
               (.title
                :color "#ff7d7d")))))

         (reblocks-lass:make-dependency
           `(:media "(max-width: 600px)"
                    (.channel
                     )))
         (call-next-method)))
