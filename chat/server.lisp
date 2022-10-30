(uiop:define-package #:platform/server
  (:use #:cl)
  (:import-from #:common/server)
  (:import-from #:chat/api
                #:chat-api)
  (:import-from #:chat/chat/api)
  (:import-from #:chat/chat-member/api)
  (:import-from #:chat/message/api))
(in-package #:platform/server)



(defun start-me ()
  (common/server::start chat-api 8003))

(defun stop-me ()
  (common/server::stop 8003))
