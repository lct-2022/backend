(uiop:define-package #:platform/job/api
  (:use #:cl)
  (:import-from #:openrpc-server
                #:define-rpc-method)
  (:import-from #:platform/api
                #:platform-api)
  (:import-from #:platform/job/model
                #:job)
  (:import-from #:common/db
                #:with-connection)
  (:import-from #:sxql
                #:limit
                #:order-by
                #:where)
  (:import-from #:mito
                #:select-dao)
  (:import-from #:platform/team/model
                #:get-team-id))
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


(define-rpc-method (platform-api get-team-jobs) (team-id &key only-open)
  (:summary "Возвращает вакансии команды.")
  (:description "По-умолчанию возвращаются все, а если указан параметр only-open, то только открытые.")
  (:param team-id integer)
  (:param only-open boolean)
  (:result (list-of job))

  (with-connection ()
    (when only-open
      (log:error "Параметр only-open пока не поддерживается."))

    (or (select-dao 'job
          (if only-open
              (where (:and (:= :team-id team-id)
                           (:= :open t)))
              (where (:= :team-id team-id))))
        #())))

(define-rpc-method (platform-api popular-jobs) (&key (limit 5))
  (:summary "Возвращает последние предложения вступить в команду.")
  (:param limit integer)
  (:result (list-of job))

  (with-connection ()
    (or (select-dao 'job
          (order-by (:desc :created-at))
          (limit limit))
        #())))
