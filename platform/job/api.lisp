(uiop:define-package #:platform/job/api
  (:use #:cl)
  (:import-from #:openrpc-server
                #:define-rpc-method)
  (:import-from #:platform/api
                #:platform-api)
  (:import-from #:platform/job/model
                #:current-user-job-application
                #:job)
  (:import-from #:common/db
                #:map-by-id
                #:with-connection)
  (:import-from #:sxql
                #:limit
                #:order-by
                #:where)
  (:import-from #:mito
                #:object-id
                #:retrieve-dao
                #:select-dao)
  (:import-from #:platform/team/model
                #:get-team-id)
  (:import-from #:platform/job-application/model
                #:job-application)
  (:import-from #:common/session
                #:with-session))
(in-package #:platform/job/api)


(defmethod get-team-id ((obj job))
  (slot-value obj 'platform/job/model::team-id))


(define-rpc-method (platform-api create-job) (team-id title description)
  (:param team-id integer)
  (:param title string)
  (:param description string)
  (:result job)

  (with-connection ()
    ;; TODO: добавить проверку прав
    (mito:create-dao 'job
                     :title title
                     :description description
                     :team-id team-id)))


(defun enrich-jobs (jobs additional-fields)
  (with-session (user-id :require nil)
    ;; Отклики заполняются только если пользователь залогинен
    (when (and user-id
               (member "job-application" additional-fields
                       :test #'string-equal))
      (let ((applications
              (with-connection ()
                (map-by-id
                 (retrieve-dao 'job-application
                               :user-id user-id)))))
        (loop for job in jobs
              do (setf (current-user-job-application job)
                       (gethash (object-id job) applications))))))
  jobs)


(defun enrich-job (job additional-fields)
  (enrich-jobs (list job) additional-fields)
  job)


(define-rpc-method (platform-api get-job) (id &key additional-fields)
  (:summary "Возвращает вакансию.")
  (:description "В additional-fields можно передать список из \"job-application\",
                 чтобы в результате было заполнено это поле. Но это работает лишь если пользователь залогинен.")
  (:param id integer)
  (:param additional-fields (list-of string))
  (:result job)

  (with-connection ()
    (enrich-job (mito:find-dao 'job :id id)
                additional-fields)))


(define-rpc-method (platform-api get-team-jobs) (team-id &key only-open additional-fields)
  (:summary "Возвращает вакансии команды.")
  (:description "По-умолчанию возвращаются все, а если указан параметр only-open, то только открытые.
                 В additional-fields можно передать список из \"job-application\",
                 чтобы в результате было заполнено это поле. Но это работает лишь если пользователь залогинен.")
  (:param team-id integer)
  (:param only-open boolean)
  (:param additional-fields (list-of string))
  (:result (list-of job))

  (with-connection ()
    (or (enrich-jobs
         (select-dao 'job
           (if only-open
               (where (:and (:= :team-id team-id)
                            (:= :open t)))
               (where (:= :team-id team-id))))
         additional-fields)
        #())))

(define-rpc-method (platform-api popular-jobs) (&key (limit 5) additional-fields)
  (:summary "Возвращает последние предложения вступить в команду.")
  (:description "В additional-fields можно передать список из \"job-application\",
                 чтобы в результате было заполнено это поле. Но это работает лишь если пользователь залогинен.")
  (:param limit integer)
  (:param additional-fields (list-of string))
  (:result (list-of job))

  (with-connection ()
    (or (enrich-jobs
         (select-dao 'job
           (order-by (:desc :created-at))
           (limit limit))
         additional-fields)
        #())))
