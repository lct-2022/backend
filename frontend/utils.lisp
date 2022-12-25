(uiop:define-package #:app/utils
  (:use #:cl)
  (:import-from #:reblocks/request
                #:get-cookie)
  (:import-from #:reblocks/session
                #:get-value)
  (:import-from #:local-time
                #:format-timestring
                #:find-timezone-by-location-name)
  (:import-from #:function-cache
                #:defcached)
  (:import-from #:passport/client
                #:anonymous-login
                #:make-passport)
  (:export #:get-user-token
           #:get-user-timezone
           #:format-time))
(in-package #:app/utils)


(defun get-user-token ()
  (or (get-cookie "auth_token")
      (get-value :auth-token)
      ;; Если ничего нет, то создадим пользователя
      (let* ((client (passport/client::connect (make-passport)))
             (token (anonymous-login client)))
        (setf (get-value :auth-token)
              token))))


(defcached get-user-timezone ()
  (find-timezone-by-location-name "Europe/Moscow"))


(defparameter +time-format+
  '((:hour 2) #\: (:min 2) #\: (:sec 2)))


(defun format-time (ts)
  (format-timestring nil ts
                     :format +time-format+
                     :timezone (get-user-timezone)))
