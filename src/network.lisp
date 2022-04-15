
(load "perceptron")
(load "data")

;; simple feed-forward artificial neural network implementation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass layer ()
  ((neurons
    :initform (make-array 0 :fill-pointer t :adjustable t)
    :initarg :neurons
    :accessor neurons
    :documentation "Array of neurons in this layer.")
   (size
    :initform 0
    :initarg :size
    :accessor size
    :documentation "Number of neurons in this layer (excluding dummy node).")))

(defclass ff-ann ()
  ((topology
    :initarg :topology
    :accessor topology
    :initform (error "Network topology has to be provided.")
    :documentation "Description of the network topology from which the network is build.")
   (neurons
    :initform (make-array 0 :fill-pointer t :adjustable t)
    :accessor neurons
    :documentation "Array of neurons in this network.")
   (layers
    :initform (make-array 0 :fill-pointer t :adjustable t)
    :accessor layers
    :documentation "Array of layers in the ann.")))

(defmethod make-load-form ((self layer) &optional environment)
  (make-load-form-saving-slots self :environment environment))

(defmethod make-load-form ((self ff-ann) &optional environment)
  (make-load-form-saving-slots self :environment environment))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun myrand (&optional (from 0) (to 1) (precision 1000))
  "Generate a random number element in [from,to[ with specified precision."
  (let ((pf (* from precision))
	(pt (* to precision)))
    (float (/ (+ pf (random (- pt pf))) precision))))

(defun make-rand-array (size)
  "Generate an array with random values between -2 and 2."
  (let ((array (make-array 0 :fill-pointer t :adjustable t)))
    (loop repeat size do (vector-push-extend (myrand -0.001 0.001 10000000) array))
    array))

(defmethod initialize-instance :after ((net ff-ann) &key)
  (let ((layers (getf (topology net) :layers))
	(transf (getf (topology net) :transf))
	(dummy  (make-instance 'perceptron :id 'dummy :output 1)))
    (assert (>= (length layers) 2))
    (loop for cur in layers and prev = 0 then cur with n = 0 do
	  (let ((layer (make-instance 'layer :size cur)))
	    (vector-push-extend dummy (neurons layer))
	    (dotimes (i cur)
	      (let ((neuron (make-instance 'perceptron :id (incf n)
					   :transfer-function transf
					   :weights (make-rand-array (1+ prev)))))
		(vector-push-extend neuron (neurons net))
		(vector-push-extend neuron (neurons layer))))
	    (vector-push-extend layer (layers net))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric propagate (net pattern &key classify)
  (:documentation "Propagate pattern through the network and return the output."))

(defgeneric calc-error (network data-list &optional type)
  (:documentation "Calculate error of the network on the given list of data."))

(defgeneric training-error (network data &optional type)
  (:documentation "Calculate the error of the network on the training set."))

(defgeneric validation-error (network data &optional type)
  (:documentation "Calculate the error of the network on the validation set."))

(defgeneric get-input-layer (net)
  (:documentation "Returns the input layer of a network."))

(defgeneric get-output-layer (net)
  (:documentation "Returns the output layer of a network."))

(defgeneric get-layer (net index)
  (:documentation "Returns the nth layer of the network."))

(defgeneric get-neuron (container index)
  (:documentation "Returns the nth neuron in the container (e.g. neural net or layer)."))

(defgeneric net-output (net &key classify)
  (:documentation "Return a vector of output values from all neurons in the output layer."))

(defgeneric dummy-node (layer)
  (:documentation "Return dummy node which has always output 1."))

(defgeneric layer-output (layer)
  (:documentation "Returns a vector of outputs from all neurons (incl. dummy) in a layer."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod dummy-node ((layer layer))
  (aref (neurons layer) 0))

(defmethod get-neuron ((net ff-ann) index)
  (aref (neurons net) index))

(defmethod get-neuron ((layer layer) index)
  (aref (neurons layer) index))

(defmethod get-input-layer ((net ff-ann))
  (aref (layers net) 0))

(defmethod get-output-layer ((net ff-ann))
  (let ((layers (length (layers net))))
    (aref (layers net) (1- layers))))

(defmethod get-layer ((net ff-ann) index)
  (aref (layers net) index))

(defmethod net-output ((net ff-ann) &key (classify nil))
  (let ((result (make-array 0 :fill-pointer t :adjustable t))
	(output-layer (get-output-layer net)))
    (loop for i from 1 to (size output-layer) do
	 (vector-push-extend (noutput (aref (neurons output-layer) i) :classify classify) result))
    result))

(defmethod layer-output ((layer layer))
  (let ((result (make-array 0 :fill-pointer t :adjustable t)))
    (loop for i from 0 to (size layer) do
	  (vector-push-extend (output (aref (neurons layer) i)) result))
    result))

(defmethod calc-error ((network ff-ann) data-list &optional (type 'mse))
  (let ((squared-sum (loop for instance in data-list
			   for output = (propagate network (first instance))
			   for target = (second instance) summing
			   (loop for output_i across output
				 for target_i across target
				 summing (expt (- output_i target_i) 2)))))
    (if (equal type 'mse) (float (/ squared-sum (length data-list)))
      (float (/ squared-sum 2)))))

(defmethod training-error ((network ff-ann) data &optional (type 'mse))
  (calc-error network (training-data data) type))

(defmethod validation-error ((network ff-ann) data &optional (type 'mse))
  (calc-error network (validation-data data) type))

(defmethod propagate ((net ff-ann) pattern &key (classify nil))
  ;; length of input pattern has to match the number of neurons in the input layer
  (assert (= (length pattern) (size (get-input-layer net))))
  ;; initialize input layer
  (let ((n (length pattern))
	(input-layer (get-input-layer net)))
    (loop for i upto (1- n) do
	  (setf (output (get-neuron input-layer (1+ i))) (aref pattern i))))
  ;; propagate pattern through all layers
  (let ((layers (length (layers net))))
    (loop for prev from 0 to (- layers 2) and cur from 1 to (1- layers) do
	  (map nil #'(lambda (x) (calc-output x (neurons (get-layer net prev))))
	       (subseq (neurons (get-layer net cur)) 1))))
  ;; return the output of the network
  (net-output net :classify classify))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(make-instance 'ff-ann :topology (list :layers '(2 3 2) :transf *sigmoidal*))
