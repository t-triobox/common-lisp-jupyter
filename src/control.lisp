(in-package #:jupyter)

#|

# The Control channel #

|#

(defclass control-channel (request-channel)
  ()
  (:documentation "Control channel class."))

#|

# Message sending functions

|#

(defun send-shutdown-reply (ch parent-msg restart)
  (message-send ch
                (make-message (channel-session ch) "shutdown_reply"
                              (json-new-obj
                                ("restart" (if restart t :f)))
                              :parent parent-msg)))

(defun send-interrupt-reply (ch parent-msg)
  (message-send ch
                (make-message (channel-session ch) "interrupt_reply"
                              (json-new-obj)
                              :parent parent-msg)))

                            
(defun send-debug-reply (ch parent-msg &optional body)
  (inform :info ch "Sending debug_reply ~S" body)
  (message-send ch
                (make-message (channel-session ch) "debug_reply"
                              (json-new-obj
                                ("type" "response")
                                ("request_seq" (json-getf (message-content parent-msg) "seq"))
                                ("success" :true)
                                ("command" (json-getf (message-content parent-msg) "command"))
                                ("body" (or body (json-empty-obj))))
                              :parent parent-msg)))

(defun send-debug-reply-failure (ch parent-msg message)
  (inform :info ch "Sending debug_reply ~S" message)
  (message-send ch
                (make-message (channel-session ch) "debug_reply"
                              (json-new-obj
                                ("type" "response")
                                ("request_seq" (json-getf (message-content parent-msg) "seq"))
                                ("success" :false)
                                ("command" (json-getf (message-content parent-msg) "command"))
                                ("message" message))
                              :parent parent-msg)))

