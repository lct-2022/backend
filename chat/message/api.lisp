(uiop:define-package #:chat/message/api
  (:use #:cl
        #:common/utils)
  (:import-from #:csv)
  (:import-from #:openrpc-server
                #:define-rpc-method)
  (:import-from #:chat/api
                #:chat-api)
  (:import-from #:chat/message/model
                #:message)
  (:import-from #:mito
                #:object-created-at
                #:object-id
                #:create-dao)
  (:import-from #:common/session
                #:with-session)
  (:import-from #:alexandria
                #:lastcar)
  (:import-from #:function-cache
                #:defcached)
  (:import-from #:local-time
                #:timestamp-)
  (:import-from #:serapeum
                #:fmt
                #:dict)
  (:import-from #:log4cl-extras/error
                #:with-log-unhandled)
  (:import-from #:serapeum/bundle
                #:random-elt)
  (:import-from #:chat/chat/model
                #:chat-archived-p))
(in-package #:chat/message/api)

(declaim (optimize (space 3) (debug 1)))


(define-rpc-method (chat-api post) (chat-id message)
  (:summary "Добавляет в чат сообщение от текущего пользователя.")
  (:param chat-id string)
  (:param message string)
  (:result message)

  (with-session (user-id)
    (with-connection ()
      
      (let ((chat (find-dao 'chat/chat/model::chat
                            :id chat-id)))
        (unless chat
          (openrpc-server:return-error (fmt "Чат с id ~S не найден." chat-id)
                                       :code 10))
        
        (when (chat-archived-p chat)
          (openrpc-server:return-error (fmt "Чат с id ~S в архиве и недоступен на запись." chat-id)
                                       :code 11))
        
        (create-dao 'message
                    :chat-id chat-id
                    :user-id user-id
                    :message message)))))


(defvar *next-phrases*
  nil)

(defvar *next-phrases-lock* (bt:make-recursive-lock))


(defparameter *starting-phrases*
  (list "Привет"
        "Как жизнь?"
        "Вот всё у нас так"
        "Прикольная передача"
        "Интересно, как они это делают?"
        ))


(defun first-sentence-from (text)
  (first (cl-ppcre:split "[.?!\]" text)))


(defun generate-phrase-from (text)
  (handler-case
      (multiple-value-bind (response status-code headers)
          (dex:post "https://yandex.ru/lab/api/yalm/text3"
                    :headers '((:content-type . "application/json"))
                    :content (yason:with-output-to-string* ()
                               (yason:encode
                                (dict
                                 "query" text
                                 "intro" 0
                                 "filter" 1)))
                    :read-timeout 10)
        (declare (ignore status-code))
        
        ;; Иногда страничка отдаёт Captcha и тогда не надо пытаться парсить её
        (unless (string-equal (gethash "content-type" headers)
                              "application/json")
          (return-from generate-phrase-from text))
        
        (let* ((data (yason:parse response))
               (text (gethash "text" data)))
          (first-sentence-from text)))
    (dexador.error:http-request-forbidden ()
      text)))


(defun fill-next-phrases ()
  (with-log-unhandled ()
    (loop repeat 15
          for phrase = (generate-phrase-from (random-elt *starting-phrases*))
          do (bt:with-lock-held (*next-phrases-lock*)
               (when phrase
                 (push phrase *next-phrases*)))
             (sleep 5))))


;; (defun generate-random-message ()
;;   (bt:with-lock-held (*next-phrases-lock*)
;;     (prog1
;;         (if *next-phrases*
;;             (pop *next-phrases*)
;;             (generate-phrase-from (random-elt *starting-phrases*)))
;;       (unless *next-phrases*
;;         (bt:make-thread 'fill-next-phrases
;;                         :name "Next Phrases Filler")))))

(defparameter *phrases* nil)


(defun load-phrases ()
  (with-open-file (f (asdf:system-relative-pathname :app "phrases.csv"))
    (loop for line in (uiop:slurp-stream-lines f)
          for phrase = (string-trim '(#\Newline #\Tab #\Space #\ZERO_WIDTH_NO-BREAK_SPACE) line)
          unless (string= phrase "")
          collect phrase into phrases
          and count 1 into phrases-count
          finally (return (make-array phrases-count
                                      :initial-contents phrases)))))


(defun generate-random-message ()
  (bt:with-lock-held (*next-phrases-lock*)
    (unless *phrases*
      (setf *phrases*
            (load-phrases)))

    (random-elt *phrases*)))


(defcached (get-all-robot-ids :timeout 60) ()
  (loop for row in (mito.db:retrieve-by-sql "SELECT id FROM passport.user WHERE robot = true")
        collect (getf row :id)))


(defun get-random-user-ids (num-messages)
  (let ((all-robot-ids (get-all-robot-ids)))
    (loop repeat num-messages
          collect (serapeum/bundle:random-elt all-robot-ids))))


(defun shift-creation-date (message seconds)
  (let ((new-ts (timestamp- (object-created-at message)
                            seconds :sec)))
    (setf (mito:object-created-at message)
          new-ts)
    (setf (mito:object-updated-at message)
          new-ts)
    (mito:save-dao message)
    (values message)))


(define-rpc-method (chat-api create-fake-messages) (chat-id num-messages)
  (:summary "Добавляет в чат NUM-MESSAGES сообщений от рандомных пользователей, помеченных как robot.")
  (:param chat-id string)
  (:param num-messages integer)
  (:result (list-of message))

  (with-connection ()
    (loop with user-ids = (get-random-user-ids num-messages)
          with max-minutes = 15
          for user-id in user-ids
          for random-shift in (sort (loop repeat num-messages
                                          collect (random (* max-minutes 60)))
                                    #'>)
          for text = (generate-random-message)
          for message = (create-dao 'message
                                    :chat-id chat-id
                                    :user-id user-id
                                    :message text)
          collect (shift-creation-date message random-shift))))


(define-rpc-method (chat-api get-messages) (chat-id &key (limit 100) page-key)
  (:summary "Отдаёт сообщения из чата, с пейджинацией.")
  (:description "После того, как метод отдал пустую страницу, можно периодически
вызывать его с тем же page-key, чтобы дождаться появления новых сообщений.

По-хорошему, в проде надо будет вместо поллинга использовать websocket или server-side-events.")
  (:param chat-id string)
  (:param limit integer)
  (:param page-key integer)
  (:result (paginated-list-of message))

  (with-connection ()
    (let ((results
            (select-dao 'message
              (if page-key
                  (where (:and (:= :chat_id chat-id)
                               (:> :id page-key)))
                  (where (:= :chat_id chat-id)))
              (order-by :id)
              (limit limit))))
      (when results
        (let ((last-message (lastcar results)))
          (values results
                  (object-id last-message)))))))

;; Эксперимент по книге
;; (locally
;;     (declaim (optimize (debug 3) (speed 0))))

(defmacro do-vector ((var vector &key (start 0) (end nil)) &body body)
  `(block nil
     (map-vector #'(lambda (,var) ,@body)
                 ,vector :start ,start :end ,end)))

;; (declaim (inline map-vector))
(declaim (notinline map-vector))
(defun map-vector (fn vector &key (start 0) end)
  (loop for index from start below (or end (length vector))
        do (funcall fn (aref vector index))))

(defun foo (v)
  (let ((cnt 0))
    (do-vector (item v)
      (incf cnt item))
    cnt))


;; Alternative
(defmacro do-vector2 ((var vector &key (start 0) (end nil)) &body body)
  `(let ((v ,vector))
     (loop for index from ,start below (or ,end (length v))
           for ,var = (aref v index)
           do (progn ,@body))))

(defun foo2 (v)
  (let ((cnt 0))
    (do-vector2 (item v)
      (incf cnt item))
    cnt))


(defun show-policy ()
  #.(with-output-to-string (*standard-output*)
      (sb-ext:describe-compiler-policy)))
