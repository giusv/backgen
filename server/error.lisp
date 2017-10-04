(in-package :server)

(defparameter *errors* (make-hash-table))

;; (defmacro deferror (name message &key parent)
;;   `(progn (defparameter ,name (bl-exception ',name ,message :parent ,parent)) 
;;           (setf (gethash ',name *errors*) ,name)))

(defprim bl-exception (name parent)
  (:pretty () (list 'bl-exception (list :name name :parent (synth :pretty parent))))
  (:implementation (package)
                   (java-unit name
                              (java-package (symb package '|.exception|))
                              (java-class name :public t
                                          :parent 'exception
                                          :fields (list (java-field-with-accessors nil 'message (java-object-type 'string))))))
  (:type () (java-object-type name)))

(defprim bl-exception-instance (name parent message)
  (:pretty () (list 'bl-exception-instance (list :name name :parent (synth :pretty parent) :message message)))
  (:call () (java-throw (java-new (synth :type this) message)))
  (:type () (java-object-type name)))

(defmacro deferror (name parent)
  `(progn (defun ,name (msg) (bl-exception-instance ',name ,parent msg))
          (defparameter ,name 
            (bl-exception ',name ,parent)) 
          (setf (gethash ',name *errors*) ,name)))

(defprim bl-bad-request-exception ()
  (:pretty () (list 'bl-bad-request-exception))
  (:call (message)
         (java-throw (java-new (synth :type this) message)))
  (:type () (java-object-type 'bad-request-exception)))
