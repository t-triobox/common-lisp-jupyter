(in-package #:jupyter)

#|

# The shell router socket #

|#

(defclass shell-channel (request-channel)
  ()
  (:documentation "SHELL channel class."))

#|

# Message sending functions

|#

(defun send-is-complete-reply (shell parent-msg status)
  (message-send shell
                (make-message (channel-session shell) "is_complete_reply"
                              `(:object
                                 ("status" . ,status)
                                 ("indent" . ""))
                              :parent parent-msg)))

(defun send-execute-reply-ok (shell parent-msg execution-count payload)
  (message-send shell
                (make-message (channel-session shell) "execute_reply"
                              `(:object
                                 ("status" . "ok")
                                 ("execution_count" . ,execution-count)
                                 ("user_expressions" . ,:empty-object)
                                 ("payload" . ,payload))
                              :parent parent-msg)))

(defun send-execute-reply-error (shell parent-msg execution-count ename evalue)
  (declare (ignore execution-count))
  (message-send shell
                (make-message (channel-session shell) "execute_reply"
                              `(:object
                                 ("status" . "error")
                                 ("ename" . ,ename)
                                 ("evalue" . ,evalue)
                                 ("traceback" . nil))
                              :parent parent-msg)))

(defun send-inspect-reply-error (shell parent-msg ename evalue)
  (message-send shell
                (make-message (channel-session shell) "inspect_reply"
                              `(:object
                                 ("status" . "error")
                                 ("ename" . ,ename)
                                 ("evalue" . ,evalue)
                                 ("traceback" . nil))
                              :parent parent-msg)))

(defun send-inspect-reply-ok (shell parent-msg data)
  (message-send shell
                (make-message (channel-session shell) "inspect_reply"
                              `(:object
                                 ("status" . "ok")
                                 ("found" . ,(and data t))
                                 ("data" . ,(or data :empty-object))
                                 ("metadata" . :empty-object))
                              :parent parent-msg)))

(defun send-complete-reply-error (shell parent-msg ename evalue)
  (message-send shell
                (make-message (channel-session shell) "complete_reply"
                              `(:object
                                 ("status" . "error")
                                 ("ename" . ,ename)
                                 ("evalue" . ,evalue)
                                 ("traceback" . nil))
                              :parent parent-msg)))

(defun send-complete-reply-ok (shell parent-msg matches start end &optional metadata)
  (message-send shell
                (make-message (channel-session shell) "complete_reply"
                              `(:object
                                 ("status" . "ok")
                                 ("matches" . ,matches)
                                 ("cursor_start" . ,start)
                                 ("cursor_end" . ,end)
                                 ("metadata" . ,(or metadata :empty-object)))
                              :parent parent-msg)))

(defun send-comm-info-reply (shell parent-msg comms)
  (message-send shell
                (make-message (channel-session shell) "comm_info_reply"
                              (list :object
                                    (cons "comms" (or (mapcar (lambda (p)
                                                                (cons
                                                                  (car p)
                                                                  (list :object
                                                                        (cons "target_name" (cdr p)))))
                                                              comms)
                                                      :empty-array)))
                              :parent parent-msg)))

(defun send-history-reply (shell parent-msg history)
  (message-send shell
                (make-message (channel-session shell) "history_reply"
                              `(:object
                                 ("history" . ,history))
                              :parent parent-msg)))
