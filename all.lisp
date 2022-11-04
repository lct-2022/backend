(uiop:define-package #:all/all
  (:use #:cl)
  (:import-from #:app/server)
  (:import-from #:chat/server)
  (:import-from #:passport/server)
  (:import-from #:rating/server)
  (:import-from #:platform/server)
  (:export
   #:start-all))
(in-package #:all/all)


(defun start-all ()
  (app/server::start)
  (chat/server::start-me)
  (passport/server::start-me)
  (rating/server::start-me)
  (platform/server::start-me)
  (values))
