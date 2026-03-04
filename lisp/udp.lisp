(require 'usocket)
(require 'bordeaux-threads)
(require 'nibbles)


(defvar *connection-socket* 'nil)

(defvar *socket-recieved-msg* 'nil)

(defvar *recieve-message-lock* (bt:make-lock
				"message reciever lock"))

(defvar *message-recieve-thread* 'nil)
;; (setf *message-recieve-thread* 'nil)

(defun string->byte (string-value)
  (map '(vector (unsigned-byte 8)) #'char-code string-value))

(defun floats->bytes (float-list &key (type :single))
  (let* ((count (length float-list))
         (size (if (eq type :single) 4 8))
         (result (make-array (* count size)
			     :element-type '(unsigned-byte 8))))
   (loop for f in float-list
          for i from 0 by size
          do (if (eq type :single)
                 (setf (nibbles:ieee-single-ref/le result i)
		       (coerce f 'single-float))
                 (setf (nibbles:ieee-double-ref/le result i)
		       (coerce f 'double-float))))
    result))

(defun bytes->floats (bytes-vector len &key (type :single))
  (let ((float-size (if (eq type :single) 4 8)))
    (loop for i from 0 below len by float-size
	  collect
	  (if (eq type :single)
	      (progn
		(nibbles:ieee-single-ref/le bytes-vector i)
		)
	      (nibbles:ieee-double-ref/le bytes-vector i)))))

(defun socket-open-udp (host port)
  (if (not (eq *connection-socket* 'nil))
      (usocket:socket-close *connection-socket*))
  (setf *connection-socket*
	    (usocket:socket-connect host port
				    :protocol :datagram)))

(defun socket-clean ()
  (if (not (eq *connection-socket* 'nil))
      (progn 
	(usocket:socket-close *connection-socket*)
	(setf *connection-socket* 'nil))))

(defun send-socket-data (buffer)
  (usocket:socket-send *connection-socket* buffer nil))


(defun get-socket-data ()
  (multiple-value-bind (byte-buffer len)
      (usocket:socket-receive *connection-socket* nil 255)
    (progn

      (if (= len 0)
	  ;; if part
	  (progn
	    ;; (print "no data for now")
	    )
	  (bytes->floats byte-buffer len)))))


(defun socket-read-thered ()
  (let ((count 0))
    (loop
      ;; (print "running")
      (let ((message (get-socket-data)))
	(bt:with-lock-held (*recieve-message-lock*)
	  (setf *socket-recieved-msg* message)))
      (setf count (1+ count)))))

(defun read-udp-message ()
  (bt:with-lock-held (*recieve-message-lock*)
    *socket-recieved-msg*))

(defun kill-recieve-thread ()
  (if (not (eq *message-recieve-thread* 'nil))
      (progn 
	(bt:destroy-thread *message-recieve-thread*)
	(setf *message-recieve-thread* 'nil))))

(defun make-recieve-theread ()
  (kill-recieve-thread)
  (setf *message-recieve-thread*
	(bt:make-thread 'socket-read-thered)))


;; (socket-open-udp "localhost" 9999)

;; (send-socket-data (floats->bytes '(222.85)))

;; (make-recieve-theread)

;; (read-udp-message)

;; (kill-recieve-thread)

;; ;; ;;; 
;; ;; ;; (kill-recieve-thread)
;; (socket-clean)

