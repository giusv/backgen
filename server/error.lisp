(in-package :server)

(defparameter *errors* (make-hash-table))

;; (defmacro deferror (name message &key parent)
;;   `(progn (defparameter ,name (bl-error ',name ,message :parent ,parent)) 
;;           (setf (gethash ',name *errors*) ,name)))

(defprim bl-error (name parent)
  (:pretty () (list 'bl-error (list :name name :parent (synth :pretty parent))))
  (:java-implementation (package)
                   (java-unit name
                              (java-package (symb package '|.exception|))
                              (java-class name :public t
                                          :parent 'exception
                                          :fields (list (java-field-with-accessors nil 'message (java-object-type 'string))))))
  (:type () (java-object-type name)))

(defprim bl-error-instance (name parent message)
  (:pretty () (list 'bl-error-instance (list :name name :parent (synth :pretty parent) :message message)))
  (:call () (java-throw (java-new (synth :type this) message)))
  (:type () (java-object-type name)))

(defmacro deferror (name parent)
  `(progn (defun ,name (msg) (bl-error-instance ',name ,parent msg))
          (defparameter ,name 
            (bl-error ',name ,parent)) 
          (setf (gethash ',name *errors*) ,name)))

(defprim bl-bad-request-exception ()
  (:pretty () (list 'bl-bad-request-exception))
  (:call (message)
         (java-throw (java-new (synth :type this) message)))
  (:type () (java-object-type 'bad-request-exception)))
