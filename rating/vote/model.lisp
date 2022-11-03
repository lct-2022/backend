(uiop:define-package #:rating/vote/model
  (:use #:cl)
  (:import-from #:mito
                #:dao-table-class
                #:object-id)
  (:import-from #:alexandria
                #:lastcar))
(in-package #:rating/vote/model)


(defparameter *supported-subject-types*
  '("project" "user" "job" "service" "team"))


(defclass vote ()
  ((id :initarg :id
       :type integer
       :col-type :bigserial
       :primary-key t
       :accessor object-id)
   (user-id :initarg :user-id
            :type integer
            :col-type :integer
            :reader user-id)
   (subject-type :initarg :subject-type
                 :type string
                 :col-type :text
                 :reader subject-type
                 :documentation "Тип объекта, за который оставлен голос. В базе это ENUM (project, user)")
   (subject-id :initarg :subject-id
               :type integer
               :col-type :integer
               :reader subject-id
               :documentation "ID объекта, за который оставлен голос."))
  (:documentation "Запись о голосе конкретного участника за какую-то сущность в нашей системе.")
  (:table-name "rating.vote")
  (:metaclass dao-table-class))


(defclass top-item (mito:dao-class)
  ((subject-type :initarg :subject-type
                 :type string
                 :col-type :text
                 :reader subject-type
                 :documentation "Тип объекта, за который оставлен голос. В базе это ENUM (project, user)")
   (subject-id :initarg :subject-id
               :type integer
               :col-type :integer
               :reader subject-id
               :documentation "ID объекта, из TOP.")
   (rating :initarg :rating
           :type integer
           :col-type :integer
           :reader rating
           :documentation "Рейтинг объекта (пока это просто количество голосов за него)."))
  (:table-name "rating.top_items_view")
  (:as "SELECT subject_type, subject_id, count(*) as rating
          FROM rating.vote
      GROUP BY subject_type, subject_id
      ORDER BY rating DESC")
  (:documentation "Представляет собой ссылку на объект из топа + его рейтинг.")
  (:metaclass mito:dao-table-view))
