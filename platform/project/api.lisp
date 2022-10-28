(uiop:define-package #:platform/project/api
  (:use #:cl)
  (:import-from #:openrpc-server
                #:define-rpc-method)
  (:import-from #:platform/api
                #:platform-api)
  (:import-from #:platform/project/model
                #:project)
  (:import-from #:common/db
                #:with-connection)
  (:import-from #:mito
                #:object-id)
  (:import-from #:platform/team/api
                #:create-team))
(in-package #:platform/project/api)


(define-rpc-method (platform-api create-project) (title description &rest rest &key url contests)
  (:param title string)
  (:param description string)
  (:param url string)
  (:param contests string)
  (:result project)
  (declare (ignore url contests))

  (with-connection ()
    (let* ((project (apply #'mito:create-dao
                           'project
                           :title title
                           :description description
                           rest)))
      (create-team (object-id project)
                   "Команда проекта")
      project)))


(define-rpc-method (platform-api top-projects) ()
  (:result (list-of project))
  (with-connection ()
    ;; Пока тупо выдаём все, позже будем делать запрос в систему рейтингов
    ;; и потом запрашивать по ID из своей базы.
    (values (mito:select-dao 'project
              (sxql:limit 10)))))
