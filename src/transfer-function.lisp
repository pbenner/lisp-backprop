
;; class for transfer functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass transfer-function ()
  ((fun
    :initarg :fun
    :initform #'(lambda (x) x)
    :accessor fun
    :documentation "Transfer function.")
   (deriv
    :initarg :deriv
    :initform #'(lambda (x) (declare (ignore x)) 1)
    :accessor deriv
    :documentation "Derivative of the transfer function.")
   (tclassify
    :initarg :tclassify
    :initform #'(lambda (x) (declare (ignore x)) 1)
    :accessor tclassify
    :documentation "Derivative of the transfer function.")))

(defmethod make-load-form ((self transfer-function) &optional environment)
  (make-load-form-saving-slots self :environment environment))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric call (transfer-function value)
  (:documentation "Call transfer function."))

(defgeneric call-deriv (transfer-function value)
  (:documentation "Call derivative of transfer function."))

(defgeneric call-tclassify (transfer-function value)
  (:documentation "Call classification function."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod call ((transfer-function transfer-function) value)
  (funcall (fun transfer-function) value))

(defmethod call-deriv ((transfer-function transfer-function) value)
  (funcall (deriv transfer-function) value))

(defmethod call-tclassify ((transfer-function transfer-function) value)
  (funcall (tclassify transfer-function) value))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (defparameter *sigmoidal*
;;   (make-instance 'transfer-function
;; 		 :fun #'(lambda (x) (/ 1 (1+ (exp (- x)))))
;; 		 :deriv #'(lambda (x) (* x (- 1 x)))))

(defparameter *sigmoidal*
  (make-instance 'transfer-function
		 :fun #'(lambda (x) (if (> x 10) 1 (if (< x -10) 0 (/ 1 (1+ (exp (- x)))))))
		 :deriv #'(lambda (x) (* x (- 1 x)))
		 :tclassify #'(lambda (x) (if (>= x 0) 1 0))))

(defparameter *tanh*
  (make-instance 'transfer-function
		 :fun #'(lambda (x) (* 1 (tanh x)))
		 :deriv #'(lambda (x) (* 1 (- 1 (expt (tanh x) 2))))
		 :tclassify #'(lambda (x) (if (>= x 0) 1 -1))))
