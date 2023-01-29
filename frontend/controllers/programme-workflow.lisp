(uiop:define-package #:app/controllers/programme-workflow
  (:use #:cl)
  (:import-from #:org.shirakumo.fraf.action-list
                #:action-list)
  (:import-from #:app/program
                #:programme
                #:programme-stop
                #:programme-title
                #:programme-chat-id
                #:programme-start
                #:programme-channel-id
                #:get-programs-for-landing)
  (:import-from #:org.shirakumo.fraf.action-list
                #:push-back)
  (:import-from #:org.shirakumo.fraf.action-list
                #:delay)
  (:import-from #:common/db
                #:with-connection)
  (:import-from #:chat/client
                #:make-chat-api)
  (:import-from #:serapeum
                #:random-in-range)
  (:import-from #:org.shirakumo.fraf.action-list
                #:actions)
  (:import-from #:local-time-duration
                #:timestamp-duration+
                #:duration
                #:timestamp-duration-)
  (:import-from #:app/actions/wait-till
                #:wait-till)
  (:import-from #:org.shirakumo.fraf.action-list
                #:lane-limited-action)
  (:import-from #:org.shirakumo.fraf.action-list
                #:clone-into)
  (:import-from #:org.shirakumo.fraf.action-list
                #:stop)
  (:import-from #:sxql
                #:limit
                #:where
                #:order-by)
  (:import-from #:mito
                #:object-id)
  (:import-from #:app/bus
                #:send-message-to-the-bus
                #:remove-event-handler
                #:make-event-handler
                #:add-event-handler
                #:emit-with-continue)
  (:import-from #:org.shirakumo.fraf.action-list
                #:update)
  (:import-from #:org.shirakumo.fraf.action-list
                #:finished-p)
  ;; (:import-from #:app/controllers/programme-workflow
  ;;               #:channel)
  (:import-from #:log4cl-extras/error
                #:with-log-unhandled)
  (:import-from #:app/utils
                #:get-user-token)
  (:import-from #:org.shirakumo.fraf.action-list
                #:blocking-p)
  (:import-from #:org.shirakumo.fraf.action-list
                #:finished-p))
(in-package #:app/controllers/programme-workflow)


(defvar *workflows* nil
  "Список workflow для программ, которые идут прямо сейчас или скоро начнутся.")


;; EVENTS, которые кидает в bus обновление workflow:
;; (:program-was-started workflow)
;; (:chat-was-archived workflow)


;; (unless (string-equal text "")
;;              (log:info "Posting" text)

;;              (let* ((api (chat/client::connect
;;                           (make-chat-api)
;;                           (get-user-token)))
;;                     (message (chat/client:post api (chat-id widget) text)))
;;                ;; Сбросим состояние окна для ввода сообщения
;;                (reblocks/widget:update widget)
               
;;                (send-message-to-the-bus message)))


(defclass programme-workflow (action-list)
  ((channel :initarg :channel
            :reader channel)
   (programme :initarg :programme
              :reader programme)
   (next-workflow :initform nil
                  :accessor next-workflow)
   (chat-id :initform nil
            :accessor chat-id)
   (latest-message :initform nil
                   :accessor latest-chat-message)
   (started :initform nil
            :accessor started-p)
   (archived :initform nil
             :accessor archived-p)))


(defmethod print-object ((object programme-workflow) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "~A ~A"
            (channel object)
            (programme object))))


(defun current-p (workflow)
  (and (started-p workflow)
       (not (archived-p workflow))))


(defun get-chat-for-program (programme)
  (with-connection ()
    (let ((chat (mito:find-dao 'app/program::programme-chat
                               :channel-id (programme-channel-id programme)
                               :start (programme-start programme))))
      (when chat
        (programme-chat-id chat)))))


(defun create-chat-for-program (programme)
  (let ((program-title (programme-title programme)))
    (or (get-chat-for-program programme)
        (progn
          (log:info "Creating chat for" program-title)
    
          (let* ((client (chat/client::connect (make-chat-api)))
                 (chat (chat/client:create-chat client
                                                :title program-title))
                 (chat-id (chat/client:chat-id chat)))
            ;; (chat/client:create-fake-messages
            ;;  client
            ;;  chat-id
            ;;  (random-in-range 3 10))

            (with-connection ()
              (mito:create-dao 'app/program::programme-chat
                               :channel-id (programme-channel-id programme)
                               :start (programme-start programme)
                               :stop (programme-stop programme)
                               :title program-title
                               :chat-id chat-id))
            (values chat-id))))))


(defclass immediate-action (lane-limited-action)
  ())


(defmethod update ((action immediate-action) dt)
  (setf (finished-p action)
        t))


(defclass start-new-programme (immediate-action)
  ())


(defmethod stop ((action start-new-programme))
  (let* ((workflow (action-list action))
         (channel (channel workflow))
         (programme (programme workflow)))
    (log:debug "New program was started" channel programme)
    (setf (started-p workflow)
          t)
    (emit-with-continue :program-was-started workflow)))


(defclass post-random-messages (org.shirakumo.fraf.action-list:action)
  ((max-delay :initform 15
              :accessor max-delay)
   (max-delay-limit :initform (* 10 60)
                    :reader max-delay-limit)
   (next-message-at :initform (+ (get-universal-time)
                                 (random 15))
                    :accessor next-message-at)))


(defmethod update ((action post-random-messages) dt)
  (log:debug "Checking if new message should be posted")
  (let ((workflow (action-list action)))
    (cond
      ((archived-p workflow)
       (log:debug "Chat was archived, removing action post-random-messages from the list")
       (setf (finished-p action)
             t))
      
      ((>= (get-universal-time)
           (next-message-at action))
       (log:debug "Posting new message")

       (let* ((client (chat/client::connect (make-chat-api)))
              (chat-id (chat-id workflow))
              (messages (chat/client:create-fake-messages client chat-id 1))
              (message (first messages)))
         (when message
           (send-message-to-the-bus message)))
       
       (setf (next-message-at action)
             (+ (get-universal-time)
                (random (max-delay action))))
       (setf (max-delay action)
             (min
              (max-delay-limit action)
              (* (max-delay action)
                 2)))))))


(defmethod blocking-p ((action post-random-messages))
  nil)


(defclass create-chat-for-next-program (immediate-action)
  ())


(defun get-next-programme (channel)
  (with-connection ()
    (first
     (mito:select-dao 'programme
       (where (:and (:= :channel-id
                        (object-id channel))
                    (:> :start :current_timestamp)))
       (order-by (:asc :start))
       (limit 1)))))


(defun get-current-and-next-programmes (channel &optional (n 5))
  "Для отладки, возвращает текущую и N-1 последующих программ"
  (with-connection ()
    (mito:select-dao 'programme
      (where (:and (:= :channel-id
                       (object-id channel))
                   (:> :start :current_timestamp)))
      (order-by (:asc :start))
      (limit n))))


(defmethod stop ((action create-chat-for-next-program))
  (let* ((workflow (action-list action))
         (channel (channel workflow))
         (next-programme (get-next-programme channel)))
    (log:debug "Creating a workflow for the next-programe" next-programme)

    (cond
      (next-programme
       (let* ((new-workflow (make-instance 'programme-workflow
                                           :channel channel
                                           :programme next-programme)))
         (setf (next-workflow workflow)
               new-workflow)
         (push new-workflow *workflows*)))
      (t
       (log:error "Unable to find next programme for" channel)))))


(defclass archive-chat (immediate-action)
  ())


(defmethod stop ((action archive-chat))
  (let* ((workflow (action-list action))
         (programme (programme workflow))
         (chat-id (chat-id workflow)))
    (log:debug "Archiving the chat" chat-id "because programme" programme "was finished")

    (setf (archived-p workflow)
          t)

    (let* ((api (chat/client::connect
                 ;; TODO: надо добавить какой-то server-side токен,
                 ;; чтобы выполнять операции от имени сервиса а не пользователя:
                 (make-chat-api))))
      (chat/client::archive-chat api chat-id))
    
    (emit-with-continue :chat-was-archived workflow)))


(defun init-actions (workflow)
  (let* ((programme (programme workflow))
         (actions (list
                   (make-instance 'wait-till
                                  :time (programme-start programme))
                   (make-instance 'start-new-programme)
                   ;; Дальше время от времени будем постить рандомные сообщения от имени роботов
                   (make-instance 'post-random-messages)
                   
                   ;; За 5 минут до окончания программы
                   ;; создаём чат для следующей:
                   (make-instance 'wait-till
                                  :time (timestamp-duration- (programme-stop programme)
                                                             (duration :minute 5)))
                   (make-instance 'create-chat-for-next-program)
                   ;; А через 5 минут после окончания программы закрываем её чат
                   (make-instance 'wait-till
                                  :time (timestamp-duration+ (programme-stop programme)
                                                             (duration :minute 5)))
                   (make-instance 'archive-chat))))
    (loop for action in actions
          do (setf (action-list action)
                   workflow))
    (setf (actions workflow)
          actions)))


(defmethod initialize-instance :after ((obj programme-workflow) &rest args)
  (declare (ignore args))

  (setf (chat-id obj)
        (create-chat-for-program (programme obj)))

  (init-actions obj))


(defun make-workflows-for-current-programmes ()
  (setf *workflows*
        (loop for (channel programme) in (get-programs-for-landing)
              collect (make-instance 'programme-workflow
                                     :channel channel
                                     :programme programme))))


(defvar *last-update-time* nil)


(defun update-workflows ()
  (unless *last-update-time*
    (setf *last-update-time*
          (get-universal-time)))

  (unless *workflows*
    (make-workflows-for-current-programmes))

  (let* ((dt (- (get-universal-time)
                *last-update-time*)))
    (loop for workflow in *workflows*
          do (with-simple-restart (continue "Ignore error and proceed to the next workflow.")
               (update workflow dt)))
    (setf *workflows*
          (remove-if #'archived-p
                     *workflows*))
    (setf *last-update-time*
          (get-universal-time))
    
    (values)))


(defun get-workflows-for-channels (channels)
  (loop with by-id = (make-hash-table)
        for workflow in *workflows*
        for channel-id = (mito:object-id (channel workflow))
        do (setf (gethash channel-id by-id)
                 workflow)
        finally (return (loop for channel in channels
                              for workflow = (gethash (mito:object-id channel) by-id)
                              when workflow
                              collect workflow))))


(defun on-new-minute ()
  (ignore-errors
   (with-log-unhandled ()
     (update-workflows))))


(defun schedule-regular-workflow-update ()
  (remove-event-handler :new-minute 'on-new-minute)
  (add-event-handler :new-minute 'on-new-minute)
  (values))
