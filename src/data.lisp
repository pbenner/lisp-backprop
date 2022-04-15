
;; class containing training/validation data
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass data ()
  ((file
    :initarg :file
    :initform (error "Data source has to be specified.")
    :accessor data-file
    :documentation "Path to file which contains training/validation data.")
   (percentage
    :initarg :percentage
    :initform 20
    :accessor percentage
    :documentation "Percentage of data which is used for the validation set.")
   (training
    :initform nil
    :accessor training-data
    :documentation "List of training samples.")
   (training-ptr
    :initform nil
    :accessor training-ptr
    :documentation "Pointer to the current training pattern.")
   (validation
    :initform nil
    :accessor validation-data
    :documentation "List of validation samples.")
   (validation-ptr
    :initform nil
    :accessor validation-ptr
    :documentation "Pointer to the current validation pattern.")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric inc-training-ptr (data)
  (:documentation "Set the training pointer to the next pattern."))

(defgeneric inc-validation-ptr (data)
  (:documentation "Set the validation pointer to the next pattern."))

(defgeneric next-training-pattern (data)
  (:documentation "Return the next training pattern and increment the pointer."))

(defgeneric next-validation-pattern (data)
  (:documentation "Return the next validation pattern and increment the pointer."))

(defgeneric reset-training-ptr (data)
  (:documentation "Set the training pointer to the beginning of the training set."))

(defgeneric reset-validation-ptr (data)
  (:documentation "Set the validation pointer to the beginning of the validaion set."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod inc-training-ptr ((data data))
  (setf (training-ptr data) (cdr (training-ptr data))))

(defmethod inc-validation-ptr ((data data))
  (setf (validation-ptr data) (cdr (validation-ptr data))))

(defmethod next-training-pattern ((data data))
  (prog1 (car (training-ptr data))
    (inc-training-ptr data)))

(defmethod next-validation-pattern ((data data))
  (prog1 (car (validation-ptr data))
    (inc-validation-ptr data)))

(defmethod reset-training-ptr ((data data))
  (setf (training-ptr data) (training-data data)))

(defmethod reset-validation-ptr ((data data))
  (setf (validation-ptr data) (validation-data data)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro pop-nth (index list)
  `(prog1 (nth ,index ,list)
     (if (= ,index 0) (setf ,list (cdr ,list))
       (setf (cdr (nthcdr (1- ,index) ,list)) (cdr (nthcdr ,index ,list))))))

(defmethod initialize-instance :after ((data data) &key)
  (let* ((raw-data (with-open-file (stream (data-file data))
		    (with-standard-io-syntax
		     (read stream nil))))
	 (n-valid (* (length raw-data) (/ (percentage data) 100))))
    (loop repeat n-valid for index = (random (length raw-data)) do
	  (push (pop-nth index raw-data) (validation-data data)))
    (loop until (null raw-data) for index = (random (length raw-data)) do
	  (push (pop-nth index raw-data) (training-data data))))
  (reset-training-ptr data)
  (reset-validation-ptr data))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; (make-instance 'data :file "datasets/iris.lisp")
