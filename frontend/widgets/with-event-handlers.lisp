(uiop:define-package #:app/widgets/with-event-handlers
  (:use #:cl)
  (:import-from #:reblocks/widget
                #:render
                #:defwidget)
  (:import-from #:alexandria
                #:plist-hash-table)
  (:import-from #:app/bus
                #:add-event-handler
                #:remove-event-handler))
(in-package #:app/widgets/with-event-handlers)


(defwidget with-event-handlers-mixin ()
  ((handlers :initform (make-hash-table)
             :reader handlers))
  (:documentation "Вызывает INIT-HANDLERS метод внутри INITIALIZE-INSTANCE,
                   а затем устанавливает их каждый раз когда UPDATE вызывается
                   не при обработке вебсокета."))


(defgeneric init-handlers (widget)
  (:documentation "Должен вернуть plist где ключами являются имена событий а значениями - обработчики."))


(defmethod initialize-instance :after ((widget with-event-handlers-mixin) &rest args)
  (declare (ignore args))
  (setf (slot-value widget 'handlers)
        (plist-hash-table (init-handlers widget))))


(defmethod render :after ((widget with-event-handlers-mixin))
  (loop for event-name being the hash-key of (handlers widget)
        using (hash-value handler)
        ;; Так как event-emitter не поддерживает проверку того, есть
        ;; хэндлер уже или нет, то сначала удаляем его а потом добавляем:
        do (remove-event-handler event-name handler)
           (add-event-handler event-name handler)))
