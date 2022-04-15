
(require 'asdf)
(require 'cl-store)

(load "network")

;; basic class for learning algorithms
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric run (learning-algorithm abort-crit &rest args)
  (:documentation "Start the learning process."))

(defgeneric incf-epoch (learning-algorithm)
  (:documentation "Increment the epoch counter."))

(defgeneric backprop-batch (learning-algorithm &key learning-rate temp)
  (:documentation "Backpropagation learning algorithm (batch mode)."))

(defgeneric backprop-incremental (learning-algorithm &key learning-rate temp)
  (:documentation "Backpropagation learning algorithm (incremental mode)."))

(defgeneric classify (learning-algorithm &key raw)
  (:documentation "Classify all patterns in data set."))

(defgeneric print-weights (learning-algorithm)
  (:documentation "Print weights of all neurons (for debugging purposes)."))

(defgeneric save-weights (learning-algorithm file)
  (:documentation "Save weights of a network to a file."))

(defgeneric restore-weights (learning-algorithm file)
  (:documentation "Restore weights from file."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *learning-rate* 0.02)
; how much the training error should affect weight updates
(defparameter *tempcontrol*   0.00)

(defclass learning-algorithm ()
  ((algorithm
    :initarg :algorithm
    :accessor algorithm
    :initform #'backprop-batch
    :documentation "Learning algorithm which is calles with data set and network.")
   (network
    :initarg :network
    :accessor network
    :initform (error "Network has to be specified.")
    :documentation "Artificial neural network which is used for learning.")
   (data
    :initarg :data
    :accessor data
    :initform (error "Dataset has to be specified.")
    :documentation "Dataset which should be learning by the network.")
   (epoch
    :accessor epoch
    :initform 0
    :documentation "Epoch of the learning algorithm.")))

(defmethod make-load-form ((self learning-algorithm) &optional environment)
  (make-load-form-saving-slots self :environment environment))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod incf-epoch ((algo learning-algorithm))
  (incf (epoch algo)))

(defmethod run ((algo learning-algorithm) abort-crit &rest args)
  (format t "~10a~18a~18a~%" "epoch" "error (training)" "error (validation)")
  (format t "----------------------------------------------~%")
  (format t "~10a~18a~18a~%" (epoch algo) (training-error (network algo) (data algo))
	  (validation-error (network algo) (data algo)))
  (loop until (funcall abort-crit algo) do
	(apply (algorithm algo) (append (cons algo nil) args))
	(format t "~10a~18a~18a~%" (epoch algo)
		(training-error (network algo) (data algo))
		(validation-error (network algo) (data algo)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; save changes to weights in a separate delta weight (for batch mode)
(defun update-delta-weights (neuron prev-layer learning-rate total-error temp)
  "Update all delta weights of a neuron."
  (let ((prev-layer-output (layer-output prev-layer))
	(part-error (part-error neuron)))
    (loop for i upto (1- (length prev-layer-output)) do
	  (setf (aref (delta-weights neuron) i)
		(+ (aref (delta-weights neuron) i)
		   (* learning-rate part-error (aref prev-layer-output i))
		   (* temp (float (/ (- (random 2000) 1000) 1000)) total-error))))))

; calculate how much a neuron contributes to the error
(defun update-part-error (neuron index next-layer)
  "Update partial error of a neuron (for all neurons in hidden layers)."
  (let ((sum (loop for i-neuron from 1 to (size next-layer)
		   for neuron = (get-neuron next-layer i-neuron)
		   for part-error = (part-error neuron)
		   for weight = (aref (weights neuron) index)
		   summing (* weight part-error)))
	(neuron-output (output neuron))
	(transf (transfer-function neuron)))
    (setf (part-error neuron) (* (call-deriv transf neuron-output) sum))))

(defun backprop-error (network target output learning-rate temp)
  "Propagate error back through the network."
  ;; calculate partial error on output layer
  (let ((output-layer (get-output-layer network))
	(total-error  (loop for i across output for j across target
			 summing (expt (- i j) 2))))
    (loop for i from 1 to (size output-layer)
	  for neuron     = (get-neuron output-layer i)
	  for derivation = (call-deriv (transfer-function neuron) (aref output (1- i)))
	  for deviation  = (- (aref target (1- i)) (aref output (1- i)))
	  do (setf (part-error neuron) (* deviation derivation)))
    ;; propagate error back through the network
    (loop for i-cur-layer from (1- (length (layers network))) downto 1
       for i-prev-layer = (1- i-cur-layer)
       for cur-layer    = (get-layer network i-cur-layer)
       for prev-layer   = (get-layer network i-prev-layer) do
       ;; update delta weights on current layer
	 (loop for i-neuron from 1 to (size cur-layer)
	    for neuron = (get-neuron cur-layer i-neuron)
	    do (update-delta-weights neuron prev-layer learning-rate total-error temp))
       ;; update partial errors on previous layer (when not input layer)
	 (when (> i-prev-layer 0)
	   (loop for i-neuron from 1 to (size prev-layer)
	      for neuron = (get-neuron prev-layer i-neuron)
	      do (update-part-error neuron i-neuron cur-layer))))))

(defun update-weights (network)
  "Copy all delta weights to real weights on all neurons in the network, after-
   wards reset all delta weights to zero."
  (loop for neuron across (neurons network) do
	(loop for i upto (1- (length (weights neuron))) do
	     (setf (aref (weights neuron) i) (+ (aref (weights neuron) i) (aref (delta-weights neuron) i)))
	     (setf (aref (delta-weights neuron) i) 0))))

(defmethod backprop-batch ((algo learning-algorithm) &key
			   (learning-rate *learning-rate*)
			   (temp          *tempcontrol*))
  (loop for pattern = (next-training-pattern (data algo)) while pattern do
       (let* ((input  (first  pattern))
	      (target (second pattern))
	      (output (propagate (network algo) input)))
	 (backprop-error (network algo) target output learning-rate temp)))
  ;; adjust weights here, that is, copy partial weight changes to real weights
  (update-weights (network algo))
  (reset-training-ptr (data algo))
  (incf-epoch algo))

(defmethod backprop-incremental ((algo learning-algorithm) &key
				 (learning-rate *learning-rate*)
				 (temp          *tempcontrol*))
  (loop for pattern = (next-training-pattern (data algo)) while pattern do
       (let* ((input  (first  pattern))
	      (target (second pattern))
	      (output (propagate (network algo) input)))
	 (backprop-error (network algo) target output learning-rate temp)
	 (update-weights (network algo))))
  (reset-training-ptr (data algo))
  (incf-epoch algo))

;; (defmethod classify ((algo learning-algorithm) &key (raw nil))
;;   (let ((dat (get-raw-data (data algo)))
;; 	(net (network algo)))
;;     (loop
;;        for p in dat
;;        for i from 1 to (length dat) do
;; 	 (format t "~10a~30a~10a~%" i
;; 		 (if raw
;; 		     (propagate net (car p) :classify nil)
;; 		     (propagate net (car p) :classify t))
;; 		 (cadr p)))))
(defmethod classify ((algo learning-algorithm) &key (raw nil))
  (let ((dat (get-raw-data (data algo)))
	(net (network algo)))
    (loop
       for p in dat
       for i from 1 to (length dat)
       for r = (if raw
		   (propagate net (car p) :classify nil)
		   (propagate net (car p) :classify t))
	 do (format t "~10a~30a~10a~%" i r (cadr p))
	 summing (if (equalp r (cadr p)) 1 0))))

(defmethod classify-valid ((algo learning-algorithm) &key (raw nil))
  (reset-validation-ptr (data algo))
  (loop for pattern = (next-validation-pattern (data algo)) while pattern
        for r = (if raw
		   (propagate (network algo) (car pattern) :classify nil)
		   (propagate (network algo) (car pattern) :classify t))
	 do (format t "~30a~10a~%" r (cadr pattern))
	 summing (if (equalp r (cadr pattern)) 1 0)))

(defmethod print-weights ((algo learning-algorithm))
  (loop for neuron across (neurons (network algo)) do
       (print (weights neuron))))

(defmethod save-weights ((algo learning-algorithm) file)
  (cl-store:store
   (loop for neuron across (neurons (network algo)) collecting
	(weights neuron)) file))

(defmethod restore-weights ((algo learning-algorithm) file)
  (let ((weights (cl-store:restore file)))
    (loop for neuron across (neurons (network algo))
	  for weight in weights do
	(setf (weights neuron) weight))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (defparameter features
;;   (make-instance 'learning-algorithm
;; 		 :network (make-instance 'ff-ann :topology (list :layers '(99 20 10) :transf *tanh*))
;; 		 :data    (make-instance 'data :file "datasets/features.ica.lisp" :percentage 20)
;; 		 :algorithm 'backprop-incremental))

(defparameter features
  (make-instance 'learning-algorithm
		 :network (make-instance 'ff-ann :topology (list :layers '(1023 15 10) :transf *tanh*))
		 :data    (make-instance 'data :file "datasets/features.10000.scaled.lisp" :percentage 10)
		 :algorithm 'backprop-incremental))

;; (defparameter raw
;;   (make-instance 'learning-algorithm
;; 		 :network (make-instance 'ff-ann :topology (list :layers '(4095 5 5 2) :transf *tanh*))
;; 		 :data    (make-instance 'data :file "datasets/raw.lisp" :percentage 50)
;; 		 :algorithm 'backprop-incremental))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Usage:
;;
;; (load "learning-algorithm.lisp")
;; (run features #'(lambda (a) (>= (epoch a) 200)) :learning-rate 0.001)
;; (run raw      #'(lambda (a) (>= (epoch a) 200)) :learning-rate 0.001)
;;
;; (save-weights    features "datasets/features.2.weights")
;; (restore-weights features "datasets/features.2.weights")
