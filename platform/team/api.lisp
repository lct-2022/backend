(uiop:define-package #:platform/team/api
  (:use #:cl)
  (:import-from #:openrpc-server
                #:define-rpc-method)
  (:import-from #:platform/api
                #:platform-api)
  (:import-from #:platform/team/model
                #:team)
  (:import-from #:common/db
                #:with-connection)
  (:import-from #:platform/job/api
                #:create-job))
(in-package #:platform/team/api)


(defparameter *default-jobs*
  (list (list "Бэкендер" "Требуются навыки разработки микросервисов + немного devop. Знание баз данных.")
        (list "Фронтендер" "Необходимо владеть React и Typescript.")
        (list "Дизайнер" "UX & UI.")
        (list "Маркетолог" "Тут должны быть разные маркетинговые баззворды.")))


(define-rpc-method (platform-api create-team) (project-id title)
  (:summary "Создаёт ещё одну команду внутри проекта.")
  (:description "Пока нам это не нужно, для каждого проекта создаётся дефолтная команда. Однако в будущем, может быть будет необходимо создавать по несколько команд для работы над большими проектами.

Чтобы создать команду, надо быть владельцем проекта или админом.")
  (:param title string)
  (:param project-id integer)
  (:result team)

  (with-connection ()
    (let* ((team (mito:create-dao 'team
                                  :title title
                                  :project-id project-id))
           (team-id (mito:object-id team)))
      (loop for (title description) in *default-jobs*
            do (create-job team-id title description))
      team)))


(define-rpc-method (platform-api get-project-teams) (project-id)
  (:summary "Отдаёт список команд для проекта.")
  (:param project-id integer)
  (:result (list-of team))

  (with-connection ()
    (mito:select-dao 'team
      ;; Сейчас это не работает для get-project-team - проект не раскрывается CL клиентом, но на фронте должно быть OK.
      ;; Пока не нужно. поэтому закомментировано
      ;; (mito:includes 'platform/project/model::project)
      (sxql:where (:= :project-id project-id)))))
