(uiop:define-package #:app/pages/channels
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
                #:*light-background*
                #:*text-color*)
  (:import-from #:group-by
                #:child-groupings))
(in-package #:app/pages/channels)


(defwidget channels ()
  ())


(defun make-channels-widget ()
  (make-instance 'channels))


(defcached (get-all-channels :timeout 60) ()
  (with-connection ()
    (mito:select-dao 'channel
      (sxql:order-by :name))))



(defmethod render ((widget channels))
  (with-html
    (:h1 "Все каналы")
    (loop with groups = (group-by:make-grouped-list (get-all-channels)
                                                    :keys (list (lambda (channel)
                                                                  (elt (channel-name channel)
                                                                       0)))
                                                    :tests (list #'char-equal))
          for group in (reverse (child-groupings groups))
          for idx upfrom 1
          do (:h2 :class "channels-group-header"
                  (fmt "~A" (group-by:key-value group)))
             (:div :class "channels-list"
                   (loop for channel in (group-by:items-in-group group)
                         do (render-channel channel))))))


(defun render-channel (channel)
  (with-html
    (:div :class "channel"
          (:a :href (fmt "/channels/~A"
                         (object-id channel ))
              (channel-name channel)))))


(defmethod get-dependencies ((widget channels))
  (list* (reblocks-lass:make-dependency
           `(.channels
             (.channels-group-header
              :margin-top 2rem)

             (.channels-list
              :display flex
              :flex-direction row
              :flex-wrap wrap
              :gap 1rem

              (.channel
               :white-space nowrap
               :font-size 2rem
               :background ,*light-background*
               :padding 1rem
               :border-radius 5px
               (a :color ,*text-color*)))))

         (reblocks-lass:make-dependency
           `(:media "(max-width: 600px)"
                    (.channels
                     :width 100%
                     (.channels-list
                      :width 100%
                      (.channel
                       :width 100%
                       :overflow hidden
                       :text-overflow ellipsis)))))
         (call-next-method)))
