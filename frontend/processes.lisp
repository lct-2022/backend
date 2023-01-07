(uiop:define-package #:app/processes
  (:use #:cl)
  (:import-from #:bordeaux-threads
                #:make-thread
                #:make-lock)
  (:import-from #:log4cl-extras/error
                #:with-log-unhandled)
  (:import-from #:event-emitter
                #:emit)
  (:import-from #:app/bus
                #:emit-with-continue
                #:*bus*)
  (:export
   #:ensure-every-minute-thread-is-running))
(in-package #:app/processes)


(defvar *every-minute-thread* nil)
(defvar *every-minute-thread-lock* (make-lock))


;; EVENTS: new-minute срабатывает каждую минуту

(defun every-minute-updater ()
  "Запускает по шине событие примерно раз в минуту, чтобы разные виджеты могли себя обновить."
  (loop do (ignore-errors
            (with-log-unhandled ()
              (emit-with-continue :new-minute)))
           (sleep 60)))


(defun ensure-every-minute-thread-is-running ()
  (bt:with-lock-held (*every-minute-thread-lock*)
    (when (or (null *every-minute-thread*)
              (not (bt:thread-alive-p *every-minute-thread*)))
      (log:debug "Starting every minute thread")
      (setf *every-minute-thread*
            (make-thread 'every-minute-updater
                         :name "Every Minute Updater")))))


(defun stop-thread ()
  (bt:with-lock-held (*every-minute-thread-lock*)
    (when (and (not (null *every-minute-thread*))
               (bt:thread-alive-p *every-minute-thread*))
      (bt:destroy-thread *every-minute-thread*)
      (setf *every-minute-thread*
            nil))))
