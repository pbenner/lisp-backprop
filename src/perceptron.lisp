
(load "transfer-function")

;; perceptron class definition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass perceptron ()
  ((id
    :initarg :id
    :initform nil
    :accessor id
    :documentation "Id of the neuron (usually a number).")
   (weights
    :initarg :weights
    :initform #()
    :accessor weights
    :documentation "Array of weights for input connections.")
   (delta-weights
    :accessor delta-weights
    :documentation "Array of summations of potential weight changes (\delta w).")
   (transfer-function
    :initarg :transfer-function
    :initform (make-instance 'transfer-function)
    :accessor transfer-function)
   (output
    :initarg :output
    :initform 0
    :accessor output)
   (part-error
    :initform 0
    :accessor part-error)))

(defmethod make-load-form ((self perceptron) &optional environment)
  (make-load-form-saving-slots self :environment environment))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric net-input (neuron input-neurons)
  (:documentation "Calculate the network input of a neuron."))

(defgeneric calc-output (neuron input-neurons)
  (:documentation "Calculate output neuron and update the output slot."))

(defgeneric noutput (neuron &key classify)
  (:documentation "Wrapper for the output function."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod net-input ((neuron perceptron) input-neurons)
  (loop for i upto (1- (length input-neurons)) summing
	(* (output (aref input-neurons i)) (aref (weights neuron) i))))

(defmethod calc-output ((neuron perceptron) input-neurons)
  (let ((sum (net-input neuron input-neurons))
	(fun (transfer-function neuron)))
    (setf (output neuron) (call fun sum))))

(defmethod noutput ((neuron perceptron) &key (classify nil))
  (if classify
      (call-tclassify (transfer-function neuron) (output neuron))
      (output neuron)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod initialize-instance :after ((neuron perceptron) &key)
  (setf (delta-weights neuron) (make-array (length (weights neuron)))))
