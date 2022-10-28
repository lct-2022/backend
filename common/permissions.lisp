(uiop:define-package #:common/permissions
  (:use #:cl)
  (:import-from #:openrpc-server
                #:return-error)
  (:import-from #:serapeum
                #:fmt))
(in-package #:common/permissions)


(defgeneric assert-can-modify (user-id object)
  (:documentation "Если пользователь с USER-ID не админ и не владелец объекта, то выполнение метода будет прервано
и API вернёт ошибку.

Методы должны возвращать T, если пользователь может изменять объект. А :around метод уже вернёт ошибку.")
  
  (:method ((user-id integer) (object t))
    "По умолчанию, доступ запрещён."
    nil)
  
  (:method :around ((user-id integer) (object t))
    (unless (call-next-method)
      (return-error (fmt "Пользователь с id = ~A не может изменять объект ~A."
                         user-id object)))))
