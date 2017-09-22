(in-package :server)

(defparameter *errors* (make-hash-table))

(defmacro deferror (name message &key parent)
  `(progn (defparameter ,name (bl-error ',name ,message :parent ,parent)) 
          (setf (gethash ',name *errors*) ,name)))

(defprim bl-error (name message &key parent)
  (:pretty () (list 'bl-error (list :name name :message message :parent (synth :pretty parent))))
  (:implementation (package)
                   (java-unit name
                              (java-package (symb package '|.exception|))
                              (java-class name :public t
                                          :parent 'exception
                                          :fields (list (java-field-with-accessors nil 'message (java-object-type 'string))))))
  (:type () (java-object-type name)))

