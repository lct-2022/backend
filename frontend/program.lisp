(uiop:define-package #:app/program
  (:use #:cl)
  (:import-from #:function-cache
                #:defcached)
  (:import-from #:cl-cookie)
  (:import-from #:fxml)
  (:import-from #:local-time
                #:timestamp-difference
                #:now
                #:unix-to-timestamp
                #:timestamp)
  (:import-from #:serapeum
                #:->
                #:fmt)
  (:import-from #:gzip-stream
                #:gzip-input-stream)
  (:import-from #:mito.dao
                #:select-by-sql)
  (:import-from #:sxql
                #:limit
                #:order-by
                #:where
                #:join)
  (:import-from #:plump)
  (:import-from #:clss)
  (:import-from #:common/db
                #:with-connection)
  (:import-from #:alexandria
                #:assoc-value))
(in-package #:app/program)


;; curl 'https://tv.yandex.ru/api/213' \
;;   -H 'Cookie: yandexuid=2496322901669495048; yuidss=2496322901669495048; is_gdpr=0; is_gdpr_b=CIy8DhDXlwEoAg==; _yasc=HT6z8bSGxymqNx08FGbAerfz1Bjx2i/P2ypMX30cygHmPRdw7MlJc9E68rMboQ==; i=dq6N/deeUusvTm7issbaVTELzWH2tBie+vWd/alhVfAKCIIeNKapoVq/41YlWUpp0sClcCVayX6CALFmEAOcoNwdfg4=' \
;;   -H 'X-Requested-With: XMLHttpRequest' \
;;   -H 'X-TV-SK: e40d23bed00e873c701579c9660905316f8c0929:1669495059476' \
;;   --compressed

;; X-TV-SK:
;;  window.__INITIAL_SK__ = {"key":"83bbd91e6e03ddc7705672dc90d79310302884ec:1669495376914","expire":1669581776914};

;; Это не работает - яндекс редиректит на Captche
;; (defun get-yandex-program ()
;;   (let* ((cookies (cl-cookie:make-cookie-jar))
;;          ;; (index-page (dex:get "https://tv.yandex.ru/" :cookie-jar cookies))
;;          )
;;     (dex:get "https://tv.yandex.ru/" :cookie-jar cookies
;;                                      :headers '(("User-Agent" . "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/104.0.5112.114 YaBrowser/22.9.1.1146 Yowser/2.5 Safari/537.36")))
;;     ;; index-page
;;     ;; (dex:get "https://tv.yandex.ru/api/213" :cookie-jar cookies
;;     ;;                                         :headers '(("X-TV-SK" . "e40d23bed00e873c701579c9660905316f8c0929:1669495059476")
;;     ;;                                                    ("X-Requested-With" . "XMLHttpRequest")))
;;     ))


;; (defclass program ()
;;   ((begin :initarg :begin
;;           :type timestamp
;;           :reader program-begin)
;;    (end :initarg :end
;;         :type timestamp
;;         :reader program-end)
;;    (title :initarg :title
;;           :type string
;;           :reader program-title)))


;; (defmethod print-object ((obj program) stream)
;;   (print-unreadable-object (obj stream :type t)
;;     (format stream "[~A -> ~A] ~A"
;;             (program-begin obj)
;;             (program-end obj)
;;             (program-title obj))))


;; (defcached (get-program :timeout (* 15 60)) ()
;;   (let* ((response (dex:get "https://stream.1tv.ru/api/schedule.json"))
;;          (data (yason:parse response))
;;          (channel (gethash "channel" data))
;;          (schedule (gethash "schedule" channel))
;;          (program (gethash "program" schedule)))
;;     (loop for item in program
;;           collect (make-instance 'program
;;                                  :begin (unix-to-timestamp (gethash "begin" item))
;;                                  :end (unix-to-timestamp (gethash "end" item))
;;                                  :title (gethash "title" item)))))


(defvar *file* nil)

(defun download-file (source-id)
  (let* ((source (with-connection ()
                   (mito:find-dao 'channel-source
                                  :id source-id)))
         (source-name (channel-source-name source))
         (source-url (channel-source-url source)))
    (log:info "Downloading file for" source-name)
    (setf *file* (dex:get source-url
                          :keep-alive nil))
    (values)))


(defclass channel-source ()
  ((name :initarg :name
         :initform nil
         :col-type :text
         :accessor channel-source-name)
   (url :initarg :url
        :initform nil
        :col-type :text
        :accessor channel-source-url))
  (:table-name "tv.channel_source")
  (:metaclass mito:dao-table-class))


(defclass channel ()
  ((source-id :initarg :source-id
              :initform 0
              :col-type :bigint
              :accessor channel-source-id)
   (channel-id :initarg :channel-id
               :type string
               :col-type :text
               :accessor channel-id)
   (name :initarg :name
         :initform nil
         :col-type :text
         :accessor channel-name)
   (rating :initarg :rating
           :initform nil
           :col-type (or :null :bigint)
           :accessor channel-rating)
   (url :initarg :url
        :initform nil
        :col-type (or :null :text)
        :accessor channel-url)
   (image-url :initarg :image-url
              :initform nil
              :col-type (or :null :text)
              :accessor channel-image-url))
  (:table-name "tv.channel")
  (:primary-key source-id channel-id)
  (:metaclass mito:dao-table-class))


(defclass programme ()
  ((channel-id :initarg :channel-id
               :initform nil
               :col-type :bigint
               :type integer
               :accessor programme-channel-id)
   (source-id :initarg :source-id
              :initform 0
              :col-type :bigint
              :accessor channel-source-id)
   (start :initarg :start
          :initform nil
          :col-type :timestamptz
          :type timestamp
          :accessor programme-start)
   (stop :initarg :stop
         :initform nil
         :col-type :timestamptz
         :type timestamp
         :accessor programme-stop)
   (title :initarg :title
          :col-type :text
          :type string
          :accessor programme-title)
   (description :initarg :description
                :col-type :text
                :type string
                :accessor programme-description))
  (:primary-key source-id channel-id start stop)
  (:table-name "tv.programme")
  (:metaclass mito:dao-table-class))


(defclass programme-chat ()
  ((channel-id :initarg :channel-id
               :initform nil
               :col-type :bigint
               :type integer
               :accessor programme-channel-id)
   (source-id :initarg :source-id
              :col-type :bigint
              :accessor channel-source-id)
   (start :initarg :start
          :initform nil
          :col-type :timestamptz
          :type timestamp
          :accessor programme-start)
   (stop :initarg :stop
         :initform nil
         :col-type :timestamptz
         :type timestamp
         :accessor programme-stop)
   (title :initarg :title
          :col-type :text
          :type string
          :accessor programme-title)
   (chat-id :initarg :chat-id
            :col-type :uuid
            :reader programme-chat-id))
  (:primary-key source-id channel-id start)
  (:table-name "tv.programme_chat")
  (:metaclass mito:dao-table-class))


(defmethod print-object ((obj channel) stream)
  (print-unreadable-object (obj stream :type t)
    (handler-case
        (format stream "~A"
                (channel-name obj))
      (error (err)
        (format stream "ERROR: ~A" err)))))


(defmethod print-object ((obj programme) stream)
  (print-unreadable-object (obj stream :type t)
    (handler-case
        (format stream "~A [~A -> ~A]"
                (programme-title obj)
                (programme-start obj)
                (programme-stop obj))
      (error (err)
        (format stream "~A" err)))))


(defmethod print-object ((obj programme-chat) stream)
  (print-unreadable-object (obj stream :type t)
    (handler-case
        (format stream "~A [~A -> ~A]"
                (programme-title obj)
                (programme-start obj)
                (programme-stop obj))
      (error (err)
        (format stream "~A" err)))))


;; Надо придумать, как распределять программы по дням недели внутри канала
(defun programme-id (programme)
  (unless (programme-start programme)
    (error "Unable to make id for programme: ~A" programme))
  (fmt "~A-~A-~A"
       (channel-source-id programme)
       (programme-channel-id programme)
       (local-time:timestamp-to-unix (programme-start programme))))


(defmethod print-object ((channel channel) stream)
  (print-unreadable-object (channel stream :type t)
    (format stream "source=~A id=~A name=~A"
            (channel-source-id channel)
            (channel-id channel)
            (channel-name channel))))


(defmethod print-object ((programme programme) stream)
  (print-unreadable-object (programme stream :type t)
    (format stream "channel-id=~A title=~A"
            (programme-channel-id programme)
            (programme-title programme))))


(defvar *channels*
  (make-hash-table :test 'equal :synchronized t))


(defun channel-exists (source-id channel-id)
  (mito:find-dao 'channel
                 :source-id source-id
                 :channel-id channel-id))

(defun programme-exists (source-id channel-id program-start)
  (let ((rows (mito:retrieve-by-sql "SELECT 1 FROM tv.programme WHERE source_id = ? AND channel_id = ? AND start = ?"
                                    :binds (list source-id channel-id program-start))))
    (not (null rows))))


(defun parse-timestamp (text)
  "Parses datetime in format \"20221211081000 +0300\"."
  ;; local-time:parse-timestring does not support
  ;; NIL as date and time delimiters.
  (cl-ppcre:register-groups-bind
      (year month day hour minute second
            first-part-of-tz
            second-part-of-tz)
      ("(\\d{4})(\\d{2})(\\d{2})(\\d{2})(\\d{2})(\\d{2}) (.*)(\\d{2})" text)
    (let ((ts (format nil "~A-~A-~AT~A:~A:~A~A:~A"
                      year month day
                      hour minute second
                      first-part-of-tz
                      second-part-of-tz)))
      (local-time:parse-rfc3339-timestring ts))))


(defun process-file (source-id &key (num-programs-to-process nil)
                                    (force nil)
                                    (progress t))
  (declare (ignorable num-programs-to-process))
  
  (when (or force
            (null *file*))
    (download-file source-id))
  
  (let* ((ids-to-params
           '(("rossia1" 1 "https://vgtrk.ru/russiatv")
             ("pervy" 2 "http://www.1tv.ru")
             ("tnt" 3 "http://tnt-online.ru")
             ("ntv" 4 "http://www.ntv.ru")
             ("sts" 5 "https://ctc.ru")
             ("rentv" 6 "https://ren.tv/live")
             ("rossia-24" 7 "http://www.vesti.ru")
             ("piatnica" 8 "https://friday.ru/live")
             ("tvcentr" 9 "https://www.tvc.ru/channel/onair")
             ("domashny" 10 "https://domashniy.ru/online")
             ("zvezda" 11 "https://tvzvezda.ru/schedule/")
             ("yu" 12 "https://www.u-tv.ru/online/")))
         (channel-ids (mapcar #'car ids-to-params))
         (programme-count 0)
         (progress-every-n-programms 100))
    (common/db:with-connection ()
      (block xml-processing
        (flex:with-input-from-sequence (s *file*)
          (let ((gzip-stream (make-instance 'gzip-input-stream
                                            :understream s))
                (text-stream (make-string-output-stream))
                channel-id
                programme-params
                inside-programme
                inside-channel
                inside-first-channel-desc
                first-channel-desc-processed
                image-url)
            (labels ((getattr (attrs name)
                       (let ((attr (fxml.sax:find-attribute name attrs)))
                         (when attr
                           (fxml.sax:attribute-value attr))))
                     (get-text ()
                       (string-trim '(#\Newline #\Space)
                                    (get-output-stream-string text-stream)))
                     (on-start (ns lname qname attrs)
                       (declare (ignore ns lname))
                       (cond
                         ((string-equal qname "channel")
                          (setf channel-id
                                (getattr attrs "id"))
                          (setf inside-channel t)
                          (setf first-channel-desc-processed nil)
                          (setf inside-first-channel-desc nil)
                          (setf image-url nil))
                         
                         ((string-equal qname "display-name")
                          (when (and inside-channel
                                     (not first-channel-desc-processed))
                            (setf inside-first-channel-desc t)))
                         
                         ((string-equal qname "icon")
                          (setf image-url
                                (getattr attrs "src")))
                         
                         ((string-equal qname "programme")
                          (setf programme-params nil)
                          (setf inside-programme t)
                          (setf (getf programme-params :channel-id)
                                (getattr attrs "channel"))
                          (setf (getf programme-params :source-id)
                                source-id)
                          (setf (getf programme-params :start)
                                (parse-timestamp (getattr attrs "start")))
                          (setf (getf programme-params :stop)
                                (parse-timestamp (getattr attrs "stop"))))
                         
                         (t
                          (log:info "Element start" qname attrs)))
                       (values))
                     (on-characters (data)
                       (when (or inside-programme
                                 inside-first-channel-desc)
                         (write-string data text-stream))
                       (values))
                     (on-end (ns lname qname)
                       (declare (ignore ns lname))
                       (cond
                         ((string-equal qname "channel")
                          (let* ((channel-name
                                   (get-text))
                                 (params (assoc channel-id
                                                ids-to-params
                                                :test #'string=))
                                 (channel-rating (second params))
                                 (channel-url (third params)))
                            (when (and (member channel-id channel-ids
                                               :test #'string=)
                                       (not (channel-exists source-id channel-id)))
                              (mito:create-dao 'channel
                                               :source-id source-id
                                               :channel-id channel-id
                                               :rating channel-rating
                                               :image-url image-url
                                               :url channel-url
                                               :name channel-name))
                            (setf channel-id nil)
                            (setf inside-channel nil)))
                         ((string-equal qname "display-name")
                          (when inside-channel
                            (when inside-first-channel-desc
                              (setf first-channel-desc-processed t)
                              (setf inside-first-channel-desc nil))))
                         ((string-equal qname "title")
                          (when programme-params
                            (setf (getf programme-params :title)
                                  (get-text))))
                         ((string-equal qname "desc")
                          (when programme-params
                            (setf (getf programme-params :description)
                                  (get-text))))
                         ((string-equal qname "programme")
                          (when programme-params
                            (let ((channel-id (getf programme-params :channel-id)))
                              (when (member channel-id channel-ids
                                            :test #'string=)
                                (unless (programme-exists
                                         (getf programme-params :source-id)
                                         channel-id
                                         (getf programme-params :start))
                                  (apply #'mito:create-dao
                                         'programme
                                         programme-params))
                                (when progress
                                  (incf programme-count)
                                  (when (zerop (mod programme-count
                                                    progress-every-n-programms))
                                    (log:warn "Already" programme-count "were processed")))
                                
                                ;; Для отладки, чтобы парсить только часть
                                (when num-programs-to-process
                                  (decf num-programs-to-process)
                                  (when (zerop num-programs-to-process)
                                    (return-from xml-processing))))))
                          (setf inside-programme nil))
                         
                         (t
                          (log:info "Element ended" qname)))
                       (values))
                     (resolve-entity (some-arg url)
                       (declare (ignore some-arg))
                       (log:info "Downloading" url)
                       (let ((data (dex:get url
                                            :force-binary t)))
                         (crypto:make-octet-input-stream data))))
              (declare (dynamic-extent #'getattr
                                       #'get-text
                                       #'on-start
                                       #'on-end
                                       #'on-characters
                                       #'resolve-entity))
              
              (fxml:parse gzip-stream
                          (fxml.sax:make-callback-handler
                           :start-element #'on-start
                           :characters #'on-characters
                           :end-element #'on-end)
                          :forbid-external nil
                          :entity-resolver #'resolve-entity))
            (sb-ext:gc :full t)))))))



(defun get-channel-images ()
  (let* ((response (dex:get "https://www.powernet.com.ru/channels-stat"))
         (data (plump:parse response))
         (rows (clss:select "table tr" data)))
    (loop for row across rows
          for tds = (clss:select "td" row)
          for num-tds = (length tds)
          for rating = (unless (zerop num-tds)
                         (plump:text (aref tds 0)))
          for image-and-title = (unless (zerop num-tds)
                                  (aref tds 1))
          for image-url = (when image-and-title
                            (plump:attribute (aref (clss:select "img" image-and-title)
                                                   0)
                                             "src"))
          for title = (when image-and-title
                        (str:trim (plump:text image-and-title)))
          when rating
          collect (list rating
                        title
                        (fmt "https://www.powernet.com.ru/~A"
                             image-url)))))


(defun update-channel-image-and-rating (title rating image-url)
  (common/db:with-connection ()
    (let ((channel (mito:find-dao 'channel :name title)))
      (when channel
        (setf (channel-rating channel) rating)
        (setf (channel-image-url channel) image-url)
        (mito:save-dao channel)))))


(defun update-urls ()
  (common/db:with-connection ()
    (loop for (rating title image-url) in (get-channel-images)
          do (update-channel-image-and-rating title rating image-url))))


(defcached (get-channel-by-id :timeout 60) (source-id channel-id)
  (common/db:with-connection ()
    (mito:find-dao 'channel :source-id source-id
                            :channel-id channel-id)))


(defparameter *source-id* 2)

(defcached (get-programs-for-landing :timeout 60) ()
  (common/db:with-connection ()
    (loop for programme in (mito:select-dao 'programme
                             (join 'tv.channel
                                   :on (:and (:= :tv.programme.channel_id
                                              :tv.channel.channel_id)
                                             (:= :tv.programme.source_id
                                              :tv.channel.source_id)))
                             (where (:and (:= :tv.channel.source_id
                                              *source-id*)
                                          (:not-null :tv.channel.rating)
                                          (:<=
                                           :tv.programme.start
                                           :current_timestamp)
                                          (:<=
                                           :current_timestamp
                                           :tv.programme.stop)))
                             (order-by (:asc :tv.channel.rating))
                             (limit 11))
          for channel = (get-channel-by-id (channel-source-id programme)
                                           (programme-channel-id programme))
          collect (list channel programme))))


(defcached (get-channels-for-landing :timeout 60) ()
  (common/db:with-connection ()
    (mito:select-dao 'channel
      (where (:and (:not-null :rating)
                   (:= :source-id
                       *source-id*)))
      (order-by (:asc :tv.channel.rating))
      (limit 11))))



(defun min-left (programme)
  (let* ((now (now))
         (stop (programme-stop programme))
         (difference (timestamp-difference stop
                                           now))
         (title (programme-title programme)))
    (log:debug "Calculating MIN-LEFT for"
               title
               stop
               difference)
    (max 0
         (coerce
          (floor
           (/ difference
              60))
          'integer))))


(defvar *channel-title-to-url* nil)


(defun get-channel-urls (&optional (page 1))
  (flet ((first-elt (array)
           (unless (zerop (length array))
             (elt array 0))))
    (let* ((response (dex:get (fmt "https://podryad.tv/vladivostok/tv-online/channels?page=~A"
                                   page)))
           (doc (plump:parse response))
           (items (clss:select ".tv-online__channels-list__item" doc))
           ;; Элемент A есть всегда, но на последней странице у него нет HREF
           (next-page-link (first-elt
                            (clss:select ".paging__next" doc)))
           (has-next-page (when (and next-page-link
                                     (plump:attribute next-page-link
                                                      "href"))
                            t)))
      (append
       (loop for item across items
             for a = (first-elt
                      (clss:select ".tv-online__channels-list__item__details__item__description a"
                        item))
             for url = (when a
                         (plump:attribute a
                                          "href"))
             for title-node = (first-elt
                               (clss:select ".tv-online__channels-list__item__description__title__name"
                                 item))
             for title = (when title-node
                           (string-trim '(#\Newline #\Space)
                                        (plump:text title-node)))
             collect (cons title url))
       (when has-next-page
         (get-channel-urls (1+ page)))))))


(defun update-channel-urls ()
  (unless *channel-title-to-url*
    (setf *channel-title-to-url*
          (get-channel-urls)))

  (with-connection ()
    ;; Из 214 выкачаных урлов, title совпал только у 98. Почему?
    (loop for (title . url) in *channel-title-to-url*
          do (mito:execute-sql "UPDATE tv.channel SET url = ? WHERE name = ?"
                               (list url title)))))


(defcached get-channel-url (source-id channel-id)
  (with-connection ()
    (let* ((rows (mito:retrieve-by-sql "SELECT url FROM tv.channel WHERE source_id = ? AND channel_id = ?"
                                       :binds (list source-id channel-id))))
      (when rows
        (getf (first rows) :url)))))


(defcached %get-programme-chat-url (source-id channel-id start)
  (with-connection ()
    (let ((chat (mito:find-dao 'programme-chat
                               :source-id source-id
                               :channel-id channel-id
                               :start start)))
      (when chat
        (let ((chat-id (programme-chat-id chat)))
          (fmt "/chat/~A" chat-id))))))


(-> get-programme-chat-url
    (programme)
    (values (or null string) &optional))
(defun get-programme-chat-url (programme)
  (%get-programme-chat-url (channel-source-id programme)
                           (programme-channel-id programme)
                           (programme-start programme)))
