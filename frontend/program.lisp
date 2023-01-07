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
                #:with-connection))
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

(defun download-file ()
  (setf *file* (dex:get "http://www.teleguide.info/download/new3/xmltv.xml.gz"))
  (values))


(defclass channel ()
  ((name :initarg :name
         :initform nil
         :col-type :text
         :accessor channel-name)
   (rating :initarg :rating
           :initform nil
           :col-type (or :null :bigint)
           :accessor channel-rating)
   (image-url :initarg :image-url
              :initform nil
              :col-type (or :null :text)
              :accessor channel-image-url))
  (:table-name "tv.channel")
  (:metaclass mito:dao-table-class))


(defclass programme ()
  ((channel-id :initarg :channel-id
               :initform nil
               :col-type :bigint
               :type integer
               :accessor programme-channel-id)
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
  (:primary-key channel-id start)
  (:table-name "tv.programme")
  (:metaclass mito:dao-table-class))


(defclass programme-chat ()
  ((channel-id :initarg :channel-id
               :initform nil
               :col-type :bigint
               :type integer
               :accessor programme-channel-id)
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
  (:primary-key channel-id start)
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
  (fmt "~A-~A"
       (channel-id programme)
       (local-time:timestamp-to-unix (programme-start programme))))


(defmethod print-object ((channel channel) stream)
  (print-unreadable-object (channel stream :type t)
    (format stream "id=~A name=~A"
            (mito:object-id channel)
            (channel-name channel))))


(defmethod print-object ((programme programme) stream)
  (print-unreadable-object (programme stream :type t)
    (format stream "channel-id=~A title=~A"
            (programme-channel-id programme)
            (programme-title programme))))


(defvar *channels*
  (make-hash-table :test 'equal :synchronized t))


(defun channel-exists (channel-id)
  (mito:find-dao 'channel :id channel-id))

(defun programme-exists (channel-id program-start)
  (let ((rows (mito:retrieve-by-sql "SELECT 1 FROM tv.programme WHERE channel_id = ? AND start = ?"
                                    :binds (list channel-id program-start))))
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


(defun process-file (&key (num-programs-to-process nil)
                          (force nil))
  (declare (ignorable num-programs-to-process))

  (when (or force
            (null *file*))
    (download-file))
  
  (common/db:with-connection ()
    (block xml-processing
      (flex:with-input-from-sequence (s *file*)
        (let ((gzip-stream (make-instance 'gzip-input-stream
                                          :understream s))
              (channel-id nil)
              (programme-params nil)
              (text-stream (make-string-output-stream)))
          (flet ((getattr (attrs name)
                   (let ((attr (fxml.sax:find-attribute name attrs)))
                     (when attr
                       (fxml.sax:attribute-value attr))))
                 (get-text ()
                   (string-trim '(#\Newline #\Space)
                                (get-output-stream-string text-stream))))
            (fxml:parse gzip-stream
                        (fxml.sax:make-callback-handler
                         :start-element (lambda (ns lname qname attrs)
                                          (declare (ignore ns lname))
                                          (cond
                                            ((string-equal qname "channel")
                                             (setf channel-id
                                                   (parse-integer (getattr attrs "id"))))
                                            ((string-equal qname "programme")
                                             (setf programme-params nil)
                                             (setf (getf programme-params :channel-id)
                                                   (parse-integer (getattr attrs "channel")))
                                             (setf (getf programme-params :start)
                                                   (parse-timestamp (getattr attrs "start")))
                                             (setf (getf programme-params :stop)
                                                   (parse-timestamp (getattr attrs "stop"))))
                                            ((string-equal qname "display-name")
                                             ;; Just ignore to not log
                                             nil)
                                            (t
                                             (log:info "Element start" qname attrs)))
                                          (values))
                         :characters (lambda (data)
                                       (write-string data text-stream)
                                       (values))
                         :end-element (lambda (ns lname qname)
                                        (declare (ignore ns lname))
                                        (cond
                                          ((string-equal qname "channel")
                                           (let* ((channel-name
                                                    (get-text)))
                                             (unless (channel-exists channel-id)
                                               (mito:create-dao 'channel
                                                                :id channel-id
                                                                :name channel-name))
                                             (setf channel-id nil)))
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
                                             (unless (programme-exists
                                                      (getf programme-params :channel-id)
                                                      (getf programme-params :start))
                                               (apply #'mito:create-dao
                                                      'programme
                                                      programme-params)))
                                           ;; Раскомментировать для отладки, чтобы парсить только часть
                                           (when num-programs-to-process
                                             (decf num-programs-to-process)
                                             (when (zerop num-programs-to-process)
                                               (return-from xml-processing))))
                                          
                                          ((string-equal qname "display-name")
                                           ;; Just ignore to not log
                                           nil)
                                          (t
                                           (log:info "Element ended" qname)))
                                        (values)))
                        :forbid-external nil
                        :entity-resolver (lambda (some-arg url)
                                           (declare (ignore some-arg))
                                           (log:info "Downloading" url)
                                           (let ((data (dex:get url
                                                                :force-binary t)))
                                             (crypto:make-octet-input-stream data)))))
          (sb-ext:gc :full t))))))



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


(defcached (get-channel-by-id :timeout 60) (channel-id)
  (common/db:with-connection ()
    (mito:find-dao 'channel :id channel-id)))


(defcached (get-programs-for-landing :timeout 60) ()
  (common/db:with-connection ()
    (loop for programme in (mito:select-dao 'programme
                             (join 'tv.channel
                                   :on (:= :tv.programme.channel-id
                                        :tv.channel.id))
                             (where (:and (:not-null :tv.channel.rating)
                                          (:<=
                                           :tv.programme.start
                                           :current_timestamp)
                                          (:<=
                                           :current_timestamp
                                           :tv.programme.stop)))
                             (order-by (:asc :tv.channel.rating))
                             (limit 11))
          for channel = (get-channel-by-id (programme-channel-id programme))
          collect (list channel programme))))


(defcached (get-channels-for-landing :timeout 60) ()
  (common/db:with-connection ()
    (mito:select-dao 'channel
      (where (:and (:not-null :rating)))
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
