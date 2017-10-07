(in-package :documentation)

(defparameter *documentation* (make-hash-table))

(defmacro defdoc (name doc)
  `(progn (defparameter ,name ,doc) 
         (setf (gethash ',name *documentation*) ,name)))
