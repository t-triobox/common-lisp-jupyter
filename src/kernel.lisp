(in-package #:jupyter)

(defvar *kernel* nil)
(defvar *message* nil)
(defvar *payload* nil)
(defvar *debugger* nil)

(defvar *page-output* nil
  "Output stream sent to Jupyter pager. Available during calls to evaluate-code.")

(defclass kernel (source)
  ((name
     :initarg :name
     :initform ""
     :reader kernel-name
     :documentation "Kernel name. Used as a unique identifier in kernel description.")
   (version
     :initarg :version
     :initform ""
     :reader kernel-version
     :documentation "Kernel version.")
   (banner
     :initarg :banner
     :initform ""
     :reader kernel-banner
     :documentation "Banner text used to describe kernel. Used in kernel_info_reply messages.")
   (language-name
     :initarg :language-name
     :initform ""
     :reader kernel-language-name
     :documentation "Display name of implementation language. Used in kernel_info_reply messages.")
   (language-version
     :initarg :language-version
     :initform ""
     :reader kernel-language-version
     :documentation "Version of implementation language. Used in kernel_info_reply messages.")
   (mime-type
     :initarg :mime-type
     :initform ""
     :reader kernel-mime-type
     :documentation "Default MIME type for source files. Used in kernel_info_reply messages.")
   (file-extension
     :initarg :file-extension
     :initform ""
     :reader kernel-file-extension
     :documentation "Default file extension for source files. Used in kernel_info_reply messages.")
   (pygments-lexer
     :initarg :pygments-lexer
     :initform ""
     :reader kernel-pygments-lexer
     :documentation "Name of Pygments lexer for source files. Used in kernel_info_reply messages.")
   (codemirror-mode
     :initarg :codemirror-mode
     :initform ""
     :reader kernel-codemirror-mode
     :documentation "CodeMirror mode for source files. Used in kernel_info_reply messages.")
   (help-links
     :initarg :help-links
     :initform nil
     :reader kernel-help-links
     :documentation "An association list of help links. The car is the description and the cdr is
       URL. Used in kernel_info_reply messages.")
   (package
     :initarg :package
     :accessor kernel-package
     :documentation "The package in which evaluate-code, code-is-complete and others are called.")
   (connection-file
     :initarg :connection-file
     :reader kernel-connection-file
     :documentation "Pathname of connection file.")
   (transport
     :accessor kernel-transport
     :type string
     :documentation "Transport protocol from connection file.")
   (ip
     :accessor kernel-ip
     :type string
     :documentation "IP address from connection file.")
   (shell-port
     :accessor kernel-shell-port
     :type fixnum
     :documentation "SHELL port from connection file.")
   (stdin-port
     :accessor kernel-stdin-port
     :type fixnum
     :documentation "STDIN port from connection file.")
   (iopub-port
     :accessor kernel-iopub-port
     :type fixnum
     :documentation "IOPUB port from connection file.")
   (control-port
     :accessor kernel-control-port
     :type fixnum
     :documentation "CONTROL port from connection file.")
   (hb-port
     :accessor kernel-hb-port
     :type fixnum
     :documentation "HB port from connection file.")
   (signature-scheme
     :accessor kernel-signature-scheme
     :type string
     :documentation "Signature scheme from connection file.")
   (key
     :accessor kernel-key
     :documentation "Signing key from connection file.")
   (prompt-prefix
     :initarg :prompt-prefix
     :initform (coerce '(#\Escape #\X) 'string)
     :reader kernel-prompt-prefix
     :documentation "String prefix using in *standard-output* to indicate the start of prompt.")
   (prompt-suffix
     :initarg :prompt-suffix
     :initform (coerce '(#\Escape #\\) 'string)
     :reader kernel-prompt-suffix
     :documentation "String suffix using in *standard-output* to indicate the end of prompt.")
   (ctx
     :initform nil
     :accessor kernel-ctx
     :documentation "pzmq ctx handle.")
   (mac
     :initform nil
     :accessor kernel-mac
     :documentation "Message authentification.")
   (hb
     :initform nil
     :accessor kernel-hb
     :documentation "Heartbeat channel.")
   (shell
     :initform nil
     :accessor kernel-shell
     :documentation "SHELL channel.")
   (stdin
     :initform nil
     :accessor kernel-stdin
     :documentation "STDIN channel.")
   (control
     :initform nil
     :accessor kernel-control
     :documentation "CONTROL channel.")
   (iopub
     :initform nil
     :accessor kernel-iopub
     :documentation "IOPUB channel.")
   (session
     :initform nil
     :accessor kernel-session
     :documentation "Session identifier.")
   (request-queue
     :initarg :input-queue
     :initform (make-instance 'queue)
     :reader kernel-request-queue
     :documentation "Message queue for SHELL and CONTROL channel.")
   (input-queue
     :initarg :input-queue
     :initform (make-instance 'queue)
     :reader kernel-input-queue
     :documentation "Input queue used to feed values into execute_result payloads.")
   (history
     :initform nil
     :accessor kernel-history
     :documentation "Kernel history manager.")
   (execution-count
     :initform 0
     :accessor history-execution-count
     :documentation "Kernel execution count.")
   (comms
     :initform (make-hash-table :test #'equal)
     :reader kernel-comms
     :documentation "Currently open comms.")
   (control-thread
     :accessor kernel-control-thread
     :initarg :control-thread
     :initform nil
     :documentation "Control thread")
   (shell-thread
     :accessor kernel-shell-thread
     :initform nil
     :documentation "Shell thread")
   (tmp-file-prefix
     :accessor kernel-tmp-file-prefix
     :documentation "Prefix for temporary debugger files")
   (tmp-file-suffix
     :accessor kernel-tmp-file-suffix
     :documentation "Suffix for temporary debugger files")
   (hash-seed
     :accessor kernel-hash-seed
     :documentation "Hash seed for temporary debugger files")
   (breakpoints
     :accessor kernel-breakpoints
     :initform nil
     :documentation "Currently set breakpoints.")
   (debugger-started
     :accessor kernel-debugger-started
     :initform nil
     :documentation "Whether the debugger has been started")
   (thread-control-queues
     :accessor kernel-thread-control-queues
     :initform (make-hash-table)
     :documentation "CONTROL message queues for threads")
   (debug-objects
     :reader kernel-debug-objects
     :initform (make-hash-table)
     :documentation "Kernel frames")
   (debug-object-id
     :accessor kernel-debug-object-id
     :initform 0))
  (:documentation "Kernel state representation."))

(defgeneric evaluate-code (kernel code)
  (:documentation "Evaluate code along with paged output. Kernel implementations
  must return a list of evaluated results. Each result should be wrapped with an
  appropriate `result` class instance. Sending the results to the client will be
  handled by the calling method."))

(defmethod evaluate-code (kernel code))

(defgeneric code-is-complete (kernel code)
  (:documentation "Check code for completeness. Kernel implementations should
  result one of the permitted values of complete, incomplete, unknown or
  invalid."))

(defmethod code-is-complete (kernel code)
  "unknown")

(defgeneric inspect-code (kernel code cursor-pos detail-level)
  (:documentation "Inspect code at cursor-pos with detail-level. Successful
  inspection should return a single wrapped result."))

(defmethod inspect-code (kernel code cursor-pos detail-level))

(defgeneric complete-code (kernel match-set code cursor-pos)
  (:documentation "Complete code at cursor-pos. Successful matches should be added to match-set
  via match-set-add."))

(defmethod complete-code (kernel match-set code cursor-pos))

(defgeneric debug-initialize (kernel)
  (:documentation ""))

(defgeneric debug-continue (kernel environment)
  (:documentation ""))

(defgeneric debug-in (kernel environment)
  (:documentation ""))

(defgeneric debug-out (kernel environment)
  (:documentation ""))

(defgeneric debug-next (kernel environment)
  (:documentation ""))

(defgeneric debug-stack-trace (kernel environment)
  (:documentation ""))

(defgeneric debug-scopes (kernel environment frame)
  (:documentation ""))

(defgeneric debug-set-breakpoint (kernel source line column condition hit-condition log-message)
  (:documentation ""))


(defun add-debug-object (kernel object &key (thread-id 1))
  (incf (kernel-debug-object-id kernel))
  (setf (gethash (kernel-debug-object-id kernel) (kernel-debug-objects kernel))
        (list object thread-id))
  (kernel-debug-object-id kernel))


(defun remove-debug-object (kernel id)
  (remhash id (kernel-debug-objects kernel)))


(defun get-debug-object (kernel id)
  (values-list (gethash id (kernel-debug-objects kernel))))


(defun debug-stop (kernel reason environment &key (thread-id 1))
  (let ((msg-queue (make-instance 'queue)))
    (setf (gethash thread-id (kernel-thread-control-queues kernel)) msg-queue)
    (send-execute-reply-ok (kernel-shell kernel) *message* (history-execution-count kernel) nil)
    (send-debug-event (kernel-iopub kernel) "stopped"
                      (json-new-obj
                        ("reason" reason)
                        ("threadId" thread-id)))
    (prog (msg)
     wait
      (setq msg (dequeue msg-queue))
      (send-status-update (kernel-iopub kernel) msg "busy")
      (switch ((json-getf (message-content msg) "command") :test #'equal)
        ("continue"
          (handle-debug-request/continue kernel msg environment))
        ("disconnect"
          (return))
        ("stepIn"
          (handle-debug-request/step-in kernel msg environment))
        ("stepOut"
          (handle-debug-request/step-out kernel msg environment))
        ("next"
          (handle-debug-request/next kernel msg environment))
        ("scopes"
          (handle-debug-request/scopes kernel msg environment))
        ("stackTrace"
          (handle-debug-request/stack-trace kernel msg environment)))
      (send-status-update (kernel-iopub kernel) msg "idle")
      (go wait))))


;; Start all channels.
(defmethod start ((k kernel))
  (with-slots (connection-file control-port ctx hb hb-port history iopub request-queue
               iopub-port ip key language-name mac name prompt-prefix prompt-suffix
               session shell shell-port signature-scheme sink stdin stdin-port control
               transport tmp-file-prefix tmp-file-suffix hash-seed file-extension)
              k
    (setf sink (make-instance 'sink
                              :path (uiop:xdg-runtime-dir
                              (make-pathname :directory '(:relative "common-lisp-jupyter")
                                             :name (pathname-name connection-file)
                                             :type "log"))))
    (start sink)
    (inform :info k "Starting ~A kernel" name)
    (inform :info k "Parsing connection file ~A" connection-file)
    (let* ((config-js (jsown:parse (read-file-into-string connection-file)))
           (encoded-key (json-getf config-js "key")))
           (inform :info k "~A" config-js)
      (setf transport (json-getf config-js "transport")
            ip (json-getf config-js "ip")
            shell-port (json-getf config-js "shell_port")
            stdin-port (json-getf config-js "stdin_port")
            iopub-port (json-getf config-js "iopub_port")
            control-port (json-getf config-js "control_port")
            hb-port (json-getf config-js "hb_port")
            key (if (string= encoded-key "")
                  nil
                  (babel:string-to-octets encoded-key :encoding :ASCII))
            signature-scheme (json-getf config-js "signature_scheme")))
    (setf *uuid-random-state* (make-random-state t)
          session (make-uuid)
          hash-seed (random #xffffffff *uuid-random-state*)
          tmp-file-prefix (namestring (merge-pathnames (uiop:default-temporary-directory) (format nil "~A-~A-" name session)))
          tmp-file-suffix file-extension
          ctx (pzmq:ctx-new)
          mac (make-instance 'mac
                             :sink sink
                             :key key
                             :signature-scheme signature-scheme)
          hb (make-instance 'hb-channel
                            :sink sink
                            :session session
                            :mac mac
                            :socket (pzmq:socket ctx :rep)
                            :transport transport
                            :ip ip
                            :port hb-port)
          iopub (make-instance 'iopub-channel
                               :sink sink
                               :session session
                               :mac mac
                               :socket (pzmq:socket ctx :pub)
                               :transport transport
                               :ip ip
                               :port iopub-port)
          shell (make-instance 'shell-channel
                               :sink sink
                               :session session
                               :mac mac
                               :request-queue request-queue
                               :socket (pzmq:socket ctx :router)
                               :transport transport
                               :ip ip
                               :port shell-port)
          stdin (make-instance 'stdin-channel
                               :sink sink
                               :session session
                               :mac mac
                               :socket (pzmq:socket ctx :dealer)
                               :transport transport
                               :ip ip
                               :port stdin-port)
          control (make-instance 'control-channel
                                 :sink sink
                                 :session session
                                 :mac mac
                                 :request-queue request-queue
                                 :socket (pzmq:socket ctx :router)
                                 :transport transport
                                 :ip ip
                                 :port control-port)
          history (make-instance 'history
                                 :sink sink
                                 :path (uiop:xdg-data-home
                                         (make-pathname :directory '(:relative "common-lisp-jupyter")
                                                        :name language-name
                                                        :type "history"))))
    (start mac)
    (start hb)
    (start iopub)
    (start shell)
    (start stdin)
    (start control)
    (start history)
    (send-status iopub "starting")
    (send-status iopub "idle")
    (setf (kernel-shell-thread k)
          (bordeaux-threads:make-thread (lambda ()
                                          (run-shell k))
                                        :name "SHELL Thread"))))


;; Stop all channels and destroy the control.
(defmethod stop ((k kernel))
  (with-slots (sink ctx hb iopub shell stdin control history mac name) k
    (inform :info k "Stopping ~A kernel" name)
    (when (bordeaux-threads:thread-alive-p (kernel-shell-thread k))
      (bordeaux-threads:destroy-thread (kernel-shell-thread k)))
    (stop hb)
    (stop iopub)
    (stop shell)
    (stop stdin)
    (stop control)
    (stop mac)
    (stop history)
    (stop sink)
    (pzmq:ctx-destroy ctx)))


(defun run-shell (kernel)
  (catch 'kernel-shutdown
    (prog (msg)
     read-next
      (catch 'kernel-interrupt
        (progn
          (setq msg (message-recv (kernel-shell kernel)))
          (unless (handle-shell-message kernel msg)
            (bordeaux-threads:interrupt-thread
              (kernel-control-thread kernel)
              (lambda ()
                (throw 'kernel-shutdown nil)))
            (return))))
      (go read-next))))


(defun run-control (kernel)
  (catch 'kernel-shutdown
    (prog (msg)
     read-next
      (inform :info kernel "Waiting for control message")
      (setq msg (message-recv (kernel-control kernel)))
      (when (handle-control-message kernel msg)
        (go read-next)))))


(defun run-kernel (kernel-class connection-file)
  "Run a kernel based on a kernel class and a connection file."
  (unless (stringp connection-file)
    (error "Wrong connection file argument (expecting a string)"))
  (let ((kernel (make-instance kernel-class
                               :connection-file connection-file
                               :control-thread (bordeaux-threads:current-thread))))
    (start kernel)
    (unwind-protect
        (run-control kernel)
      (stop kernel))))


(defun make-eval-error (err msg &key (quit nil))
  (make-error-result (symbol-name (class-name (class-of err))) msg :quit quit))

(define-condition quit-condition (error)
  ()
  (:documentation "A condition for identifying a request for kernel shutdown.")
  (:report (lambda (c stream) (declare (ignore c stream)))))

(defun choose ()
  (write-string "Choice: " *query-io*)
  (finish-output *query-io*)
  (read))

; Trim restarts to only include ones from our debugger. If our debugger's exit restart cannot be
; found then don't present any restarts.
(defmethod initialize-instance :after ((instance dissect:environment) &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (do ((restarts (dissect:environment-restarts instance) (cdr restarts)))
      ((null restarts) (setf (slot-value instance 'dissect:restarts) nil))
    (when (equal 'exit (dissect:name (car restarts)))
      (rplacd restarts nil)
      (return)))
  instance)

(defun my-debugger (condition me-or-my-encapsulation)
  (declare (ignore me-or-my-encapsulation))
  (let* ((restarts (compute-restarts condition))
         (applicable-restarts (subseq restarts 0 (1+ (position 'exit restarts :key #'restart-name :test #'equal)))))
    (dissect:present condition *error-output*)
    (finish-output *error-output*)
    (terpri)
    (terpri)
    (finish-output)
    (do ((choice (choose) (choose)))
        ((and (integerp choice)
              (< -1 choice (length applicable-restarts)))
         (invoke-restart-interactively (nth choice applicable-restarts))))))


(defmacro handling-errors (&body body)
  "Macro for catching any conditions including quit-conditions during code
  evaluation."
  `(handler-case
       (handler-bind
           ((warning
              (lambda (wrn)
                (format t "[~S] ~A~%" (type-of wrn) wrn)
                (muffle-warning)))
            (serious-condition
              (lambda (err)
                (dissect:present err *error-output*))))
         (progn ,@body))
     (quit-condition (err)
       (make-eval-error err (format nil "~A" err) :quit t))
     (simple-error (err)
       (make-eval-error err
                        (apply #'format nil (simple-condition-format-control err)
                               (simple-condition-format-arguments err))))
     (simple-type-error (err)
       (make-eval-error err
                        (apply #'format nil (simple-condition-format-control err)
                               (simple-condition-format-arguments err))))
     (serious-condition (err)
       (make-eval-error err (format nil "~A" err)))))


(defmacro debugging-errors (&body body)
  `(let ((#+sbcl sb-ext:*invoke-debugger-hook* #-sbcl *debugger-hook* #'my-debugger))
       (with-simple-restart (abort "Exit debugger, returning to top level.")
         ,@body)))
  ; `(if *debugger*
  ;    (let ((*debugger-hook* #'my-debugger))
  ;      (with-simple-restart (exit "Exit debugger, returning to top level.")
  ;        ,@body))
  ;    (handling-errors ,@body)))

(defmacro handling-comm-errors (&body body)
  "Macro for catching any conditions including quit-conditions during code
  evaluation."
  `(handler-case
       (handler-bind
           ((serious-condition
              (lambda (err)
                (inform :error *kernel* (dissect:present err nil))))
            (warning
              (lambda (wrn)
                (inform :warn *kernel* (dissect:present wrn nil))
                (muffle-warning))))
         (progn ,@body))
     ;(quit-condition (err)
     ;  (make-eval-error err (format nil "~A" err) :quit t))
     (serious-condition (err)
       (declare (ignore err)))))


#|

### Message type: Handle control messages ###

|#

(defun handle-control-message (kernel msg)
  (let ((msg-type (format nil "~A~@[/~A~]" (json-getf (message-header msg) "msg_type")
                          (json-getf (message-content msg) "command")))
        (*kernel* kernel)
        (*message* msg))
    (cond
      ((member msg-type '("debug_request/continue" "debug_request/next" "debug_request/stackTrace"
                          "debug_request/stepIn" "debug_request/stepOut")
               :test #'equal)
        (let ((msg-queue (gethash (json-getf (json-getf (message-content msg) "arguments") "threadId") (kernel-thread-control-queues kernel))))
          (enqueue msg-queue msg))
        t)
      ((member msg-type '("debug_request/scopes") :test #'equal)
        (let* ((thread-id (nth-value 1 (get-debug-object kernel (json-getf (json-getf (message-content msg) "arguments") "frameId"))))
               (msg-queue (gethash thread-id (kernel-thread-control-queues kernel))))
          (enqueue msg-queue msg))
        t)
      (t
        (unwind-protect
            (progn
              (send-status-update (kernel-iopub kernel) msg "busy")
              (switch (msg-type :test #'equal)
                ("interrupt_request"
                  (handle-interrupt-request kernel msg)
                  t)
                ("shutdown_request"
                  (handle-shutdown-request kernel msg)
                  nil)
                ("debug_request/attach"
                  (handle-debug-request/attach kernel msg))
                ("debug_request/configurationDone"
                  (handle-debug-request/configuration-done kernel msg))
                ("debug_request/debugInfo"
                  (handle-debug-request/debug-info kernel msg))
                ("debug_request/disconnect"
                  (handle-debug-request/disconnect kernel msg))
                ("debug_request/dumpCell"
                  (handle-debug-request/dump-cell kernel msg))
                ("debug_request/initialize"
                  (handle-debug-request/initialize kernel msg))
                ("debug_request/setBreakpoints"
                  (handle-debug-request/set-breakpoints kernel msg))
                ("debug_request/source"
                  (handle-debug-request/source kernel msg))
                (otherwise
                  (inform :warn kernel "Ignoring ~A message since there is no appropriate handler." msg-type)
                  t)))
          (send-status-update (kernel-iopub kernel) msg "idle"))))))

#|

### Message type: Handle shell messages ###

|#

(defun handle-shell-message (kernel msg)
  (let ((msg-type (json-getf (message-header msg) "msg_type"))
        (*kernel* kernel)
        (*message* msg))
    (unwind-protect
        (progn
          (send-status-update (kernel-iopub kernel) msg "busy")
          (switch (msg-type :test #'equal)
            ("comm_close"
              (handle-comm-close kernel msg))
            ("comm_info_request"
              (handle-comm-info-request kernel msg))
            ("comm_msg"
              (handle-comm-message kernel msg))
            ("comm_open"
              (handle-comm-open kernel msg))
            ("complete_request"
              (handle-complete-request kernel msg))
            ("execute_request"
              (handle-execute-request kernel msg))
            ("history_request"
              (handle-history-request kernel msg))
            ("inspect_request"
              (handle-inspect-request kernel msg))
            ("is_complete_request"
              (handle-is-complete-request kernel msg))
            ("kernel_info_request"
              (handle-kernel-info-request kernel msg))
            (otherwise
              (inform :warn kernel "Ignoring ~A message since there is no appropriate handler." msg-type)
              t)))
      (send-status-update (kernel-iopub kernel) msg "idle"))))

#|

### Message type: kernel_info_request ###

|#

(defun handle-kernel-info-request (kernel msg)
  (inform :info kernel "Handling kernel_info_request message")
  (with-slots (name version language-name language-version mime-type session
               file-extension pygments-lexer codemirror-mode help-links banner
               shell)
              kernel
    (message-send shell
      (make-message session "kernel_info_reply"
        (json-new-obj
          ("protocol_version" +KERNEL-PROTOCOL-VERSION+)
          ("implementation" name)
          ("implementation_version" version)
          ("banner" banner)
          ("debugger" :true)
          ("help_links"
            (mapcar
              (lambda (p)
                (list :obj (cons "text" (car p)) (cons "url" (cdr p))))
              help-links))
          ("language_info"
            (json-new-obj
              ("name" language-name)
              ("version" language-version)
              ("mimetype" mime-type)
              ("file_extension" file-extension)
              ("pygments_lexer" pygments-lexer)
              ("codemirror_mode" codemirror-mode))))
      :parent msg)))
  t)

#|

### Message type: debug_request / attach ###

|#

(defun handle-debug-request/attach (kernel msg)
  (inform :info kernel "Handling debug_request/attach message ~S" (message-content msg))
  (send-debug-reply (kernel-control kernel) msg)
  t)


#|

### Message type: debug_request / configurationDone ###

|#

(defun handle-debug-request/configuration-done (kernel msg)
  (inform :info kernel "Handling debug_request/configurationDone message ~S" (message-content msg))
  (send-debug-reply (kernel-control kernel) msg)
  t)


#|

### Message type: debug_request / continue ###

|#

(defun handle-debug-request/continue (kernel msg environment)
  (inform :info kernel "Handling debug_request/continue message ~S" (message-content msg))
  (send-debug-reply (kernel-control kernel) msg)
  (send-debug-event (kernel-iopub kernel) "continued")
  (debug-continue kernel environment)
  t)


#|

### Message type: debug_request / debugInfo ###

|#

(defun handle-debug-request/debug-info (kernel msg)
  (inform :info kernel "Handling debug_request/debugInfo message")
  (send-debug-reply (kernel-control kernel) msg
    (json-new-obj
      ("isStarted" (if (kernel-debugger-started kernel) :true :false))
      ("hashMethod" "Murmur2")
      ("hashSeed" (kernel-hash-seed kernel))
      ("tmpFilePrefix" (kernel-tmp-file-prefix kernel))
      ("tmpFileSuffix" (kernel-tmp-file-suffix kernel))
      ("breakpoints" (kernel-breakpoints kernel))))
  t)


#|

### Message type: debug_request / disconnect ###

|#

(defun handle-debug-request/disconnect (kernel msg)
  (inform :info kernel "Handling debug_request/disconnect message")
  (when (json-getf (json-getf (message-content msg) "arguments") "terminateDebuggee")
    (setf (kernel-debugger-started kernel) nil)
    (maphash (lambda (thread-id msg-queue)
               (enqueue msg-queue msg))
             (kernel-thread-control-queues kernel))
    (clrhash (kernel-thread-control-queues kernel)))
  (send-debug-reply (kernel-control kernel) msg)
  t)


#|

### Message type: debug_request / dumpCell ###

|#

(defun handle-debug-request/dump-cell (kernel msg)
  (inform :info kernel "Handling debug_request/dumpCell message")
  (let* ((data (babel:string-to-octets (json-getf (json-getf (message-content msg) "arguments") "code")))
         (hash (murmur-hash-2 data (kernel-hash-seed kernel)))
         (source-path (format nil "~A~A~A" (kernel-tmp-file-prefix kernel) hash (kernel-tmp-file-suffix kernel))))
    (with-open-file (stream source-path :direction :output :if-exists :supersede :element-type '(unsigned-byte 8))
      (write-sequence data stream))
    (send-debug-reply (kernel-control kernel) msg
      (json-new-obj
        ("sourcePath" source-path))))
  t)


#|

### Message type: debug_request / initialize ###

|#

(defun handle-debug-request/initialize (kernel msg)
  (inform :info kernel "Handling debug_request/initialize message ~S" (message-content msg))
  (send-debug-reply (kernel-control kernel) msg
    (debug-initialize kernel))
  (setf (kernel-debugger-started kernel) t)
  (send-debug-event (kernel-iopub kernel) "initialized")
  t)


#|

### Message type: debug_request / next ###

|#

(defun handle-debug-request/next (kernel msg environment)
  (inform :info kernel "Handling debug_request/next message ~S" (message-content msg))
  (send-debug-reply (kernel-control kernel) msg)
  (debug-next kernel environment)
  t)


#|

### Message type: debug_request / scopes ###

|#

(defun handle-debug-request/scopes (kernel msg environment)
  (inform :info kernel "Handling debug_request/scopes message ~S" (message-content msg))
  (send-debug-reply (kernel-control kernel) msg
                    (json-new-obj
                      ("scopes" (debug-scopes kernel environment
                                              (get-debug-object kernel (json-getf (json-getf (message-content msg) "arguments") "frameId"))))))
  t)


#|

### Message type: debug_request / setBreakpoints ###

|#

(defun handle-debug-request/set-breakpoints (kernel msg)
  (inform :info kernel "Handling debug_request/setBreakpoints message ~S" (message-content msg))
  (let* ((nbp (json-getf (message-content msg) "arguments"))
         (source-path (json-getf (json-getf nbp "source") "path"))
         (breakpoints (mapcar (lambda (breakpoint)
                                (debug-set-breakpoint kernel source-path
                                                      (json-getf breakpoint "line")
                                                      (json-getf breakpoint "column")
                                                      (json-getf breakpoint "condition")
                                                      (json-getf breakpoint "hitCondition")
                                                      (json-getf breakpoint "logMessage")))
                              (json-getf nbp "breakpoints"))))
    (setf (kernel-breakpoints kernel)
          (append breakpoints
                  (delete-if (lambda (breakpoint)
                               (equal source-path
                                      (json-getf (json-getf breakpoint "source") "path")))
                             (kernel-breakpoints kernel))))
    (send-debug-reply (kernel-control kernel) msg
      (json-new-obj
        ("breakpoints" breakpoints))))
  t)


#|

### Message type: debug_request / source ###

|#

(defun handle-debug-request/source (kernel msg)
  (inform :info kernel "Handling debug_request/source message ~S" (message-content msg))
  (handler-case
      (send-debug-reply (kernel-control kernel) msg
        (json-new-obj
          ("content" (alexandria:read-file-into-string (json-getf (json-getf (json-getf (message-content msg) "arguments") "source") "path")))
          ("mimeType" (kernel-mime-type kernel))))
    (error (err)
      (send-debug-reply-failure (kernel-control kernel) msg "Unable to load source")))
  t)


#|

### Message type: debug_request / stackTrace ###

|#

(defun handle-debug-request/stack-trace (kernel msg environment)
  (inform :info kernel "Handling debug_request/stackTrace message ~S" (message-content msg))
  (send-debug-reply (kernel-control kernel) msg
                    (json-new-obj
                      ("stackFrames" (debug-stack-trace kernel environment))))
  t)


#|

### Message type: debug_request / stepIn ###

|#

(defun handle-debug-request/step-in (kernel msg environment)
  (inform :info kernel "Handling debug_request/stepIn message ~S" (message-content msg))
  (send-debug-reply (kernel-control kernel) msg)
  (send-debug-event (kernel-iopub kernel) "stopped")
  (debug-in kernel environment)
  t)


#|

### Message type: debug_request / stepOut ###

|#

(defun handle-debug-request/step-out (kernel msg environment)
  (inform :info kernel "Handling debug_request/stepOut message ~S" (message-content msg))
  (send-debug-reply (kernel-control kernel) msg)
  (send-debug-event (kernel-iopub kernel) "stopped")
  (debug-out kernel environment)
  t)


#|

### Message type: execute_request ###

|#

(defun handle-execute-request (kernel msg)
  (inform :info kernel "Handling execute_request message")
  (with-slots (execution-count history iopub package prompt-prefix prompt-suffix shell stdin input-queue)
              kernel
    (let* ((code (json-getf (message-content msg) "code"))
           (results (list (make-error-result "interrupt" "Execution interrupted")))
           (*payload* (make-array 16 :adjustable t :fill-pointer 0))
           (*page-output* (make-string-output-stream))
           (*query-io* (make-stdin-stream stdin msg))
           (*standard-input* *query-io*)
           (*error-output* (make-iopub-stream iopub msg "stderr"
                                              prompt-prefix prompt-suffix))
           (*standard-output* (make-iopub-stream iopub msg "stdout"
                                                 prompt-prefix prompt-suffix))
           (*debug-io* *standard-output*)
           (*trace-output* *standard-output*))
      (setq execution-count (1+ execution-count))
      (add-cell history execution-count code)
      (unwind-protect
          (setq results (let* ((*package* package)
                               (r (evaluate-code kernel code)))
                          (setf package *package*)
                          r))
        (dolist (result results)
          (send-result result))
        ;broadcast the code to connected frontends
        (send-execute-code iopub msg execution-count code)
        ;; send any remaining stdout
        (finish-output *standard-output*)
        ;; send any remaining stderr
        (finish-output *error-output*)
        ;; send reply (control)
        (let ((errors (remove-if-not #'eval-error-p results)))
          (if errors
            (let ((ename (format nil "~{~A~^, ~}" (mapcar #'error-result-ename errors)))
                  (evalue (format nil "~{~A~^, ~}" (mapcar #'error-result-evalue errors))))
              (send-execute-reply-error shell msg execution-count ename evalue))
            (let ((p (get-output-stream-string *page-output*)))
              (unless (queue-empty-p input-queue)
                (set-next-input (dequeue input-queue)))
              (unless (zerop (length p))
                (page (make-inline-result p)))
              (send-execute-reply-ok shell msg execution-count (coerce *payload* 'list))))))
      ;; return t if there is no quit errors present
      (notany #'quit-eval-error-p results))))

#|

### Message type: shutdown_request ###

|#

(defun handle-shutdown-request (kernel msg)
  (inform :info kernel "Handling shutdown_request message")
  (let* ((control (kernel-control kernel))
         (content (message-content msg))
         (restart (json-getf content "restart")))
    (send-shutdown-reply control msg restart)
    (bordeaux-threads:interrupt-thread (kernel-shell-thread kernel) (lambda () (throw 'kernel-shutdown nil)))
    nil))

#|

### Message type: interrupt_request ###

|#

(defun handle-interrupt-request (kernel msg)
  (inform :info kernel "Handling interrupt_request message")
  (let* ((control (kernel-control kernel))
         (content (message-content msg)))
    (bordeaux-threads:interrupt-thread (kernel-shell-thread kernel) (lambda () (throw 'kernel-interrupt nil)))
    (sleep 1)
    (send-interrupt-reply control msg)
    t))

#|

### Message type: is_complete_request ###

|#

(defun handle-is-complete-request (kernel msg)
  (inform :info kernel "Handling is_complete_request message")
  (let* ((shell (kernel-shell kernel))
         (content (message-content msg))
         (code (json-getf content "code"))
         (status (code-is-complete kernel code)))
    (send-is-complete-reply shell msg status)
    t))

#|

### Message type: inspect_request ###

|#

(defun handle-inspect-request (kernel msg)
  (inform :info kernel "Handling inspect_request message")
  (with-slots (shell package) kernel
    (let* ((content (message-content msg))
           (code (json-getf content "code"))
           (result (let ((*package* package))
                     (inspect-code kernel
                              code
                              (min (1- (length code)) (json-getf content "cursor_pos"))
                              (json-getf content "detail_level")))))
      (if (eval-error-p result)
        (with-slots (ename evalue) result
          (send-inspect-reply-error shell msg ename evalue))
        (send-inspect-reply-ok shell msg
          (let ((*package* package))
            (render result))))))
  t)

#|

### Message type: complete_request ###

|#

(defun handle-complete-request (kernel msg)
  (inform :info kernel "Handling complete_request message")
  (with-slots (shell package) kernel
    (let* ((content (message-content msg))
           (code (json-getf content "code"))
           (cursor-pos (json-getf content "cursor_pos"))
           (match-set (make-match-set :start cursor-pos :end cursor-pos :code code))
           (result (let ((*package* package))
                     (complete-code kernel match-set code cursor-pos))))
      (if (eval-error-p result)
        (with-slots (ename evalue) result
          (send-complete-reply-error shell msg ename evalue))
        (send-complete-reply-ok shell msg
                                (sort (mapcar #'match-text (match-set-matches match-set)) #'string-lessp)
                                (match-set-start match-set)
                                (match-set-end match-set)
                                (json-new-obj
                                  ("_jupyter_types_experimental" (mapcan (lambda (match)
                                                                           (when (match-type match)
                                                                             (list (json-new-obj
                                                                                     ("text" (match-text match))
                                                                                     ("type" (match-type match))))))
                                                                         (match-set-matches match-set))))))))
  t)


(defun handle-comm-info-request (kernel msg)
  (inform :info kernel "Handling comm_info_request message")
  (with-slots (shell comms) kernel
    (let* ((content (message-content msg))
           (target-name (json-getf content "target_name"))
           (comms-alist (hash-table-alist comms)))
      (send-comm-info-reply shell msg
                            (if target-name
                              (remove-if-not (lambda (p) (equal target-name (cdr p)))
                                comms-alist)
                              comms-alist))))
  t)

(defun handle-comm-open (kernel msg)
  (inform :info kernel "Handling comm_open message")
  (with-slots (iopub comms) kernel
    (let* ((content (message-content msg))
           (metadata (message-metadata msg))
           (buffers (message-buffers msg))
           (id (json-getf content "comm_id"))
           (target-name (json-getf content "target_name"))
           (data (json-getf content "data"))
           (inst (create-comm (intern target-name 'keyword) id data metadata buffers)))
      (if inst
        (progn
          (setf (gethash id comms) inst)
          (handling-comm-errors
            (on-comm-open inst data metadata buffers)))
        (send-comm-close-orphan iopub id))))
  t)

(defun handle-comm-message (kernel msg)
  (inform :info kernel "Handling comm_msg message")
  (with-slots (comms) kernel
    (let* ((content (message-content msg))
           (metadata (message-metadata msg))
           (buffers (message-buffers msg))
           (id (json-getf content "comm_id"))
           (data (json-getf content "data"))
           (inst (gethash id comms)))
      (when inst
        (handling-comm-errors
          (on-comm-message inst data metadata buffers)))))
  t)

(defun handle-comm-close (kernel msg)
  (inform :info kernel "Handling comm_close")
  (with-slots (comms) kernel
    (let* ((content (message-content msg))
           (metadata (message-metadata msg))
           (buffers (message-buffers msg))
           (id (json-getf content "comm_id"))
           (data (json-getf content "data"))
           (inst (gethash id comms)))
      (when inst
        (handling-comm-errors
          (on-comm-close inst data metadata buffers))
        (remhash id comms))))
  t)

(defun handle-history-request (kernel msg)
  (inform :info kernel "Handling history_request message")
  (with-slots (shell history) kernel
    (let* ((content (message-content msg))
           (output (json-getf content "output"))
           (history-type (json-getf content "hist_access_type"))
           (results (switch (history-type :test #'equal)
                      ("range" (history-range history
                                              (json-getf content "session")
                                              (json-getf content "start")
                                              (json-getf content "stop")))
                      ("search" (history-search history
                                                (json-getf content "n")
                                                (json-getf content "pattern")
                                                (json-getf content "unique")))
                      ("tail" (history-tail history
                                            (json-getf content "n"))))))
      (send-history-reply shell msg
        (if output
          (mapcar
               (lambda (item)
                 (list (first item)
                       (second item)
                       (list (third item) :null)))
            results)
          results))))
          t)

(defun send-result (result)
  "Send a result either as display data or an execute result."
  (with-slots (iopub package execution-count history) *kernel*
    (if (typep result 'error-result)
      (send-execute-error iopub *message*
                          (error-result-ename result)
                          (error-result-evalue result))
      (let ((data (let ((*package* package))
                    (render result))))
        (when data
          (if (result-display-data result)
            (send-display-data iopub *message* data)
            (send-execute-result iopub *message* execution-count data)))))))

(defun set-next-input (text &optional (replace nil))
  (declare (ignore replace))
  (vector-push-extend (json-new-obj
                        ("source" "set_next_input")
                        ("text" text))
                      *payload*))

(defun page (result &optional (start 0))
  (vector-push-extend (json-new-obj
                        ("source" "page")
                        ("data" (render result))
                        ("start" start))
                      *payload*))

(defun enqueue-input (kernel code)
  "Add code to input queue."
  (enqueue (kernel-input-queue kernel) code))

(defun clear (&optional (wait nil))
  "Send clear output message to frontend."
  (send-clear-output (kernel-iopub *kernel*) *message* wait))
