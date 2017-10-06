(in-package :gui)

(defparameter *elements* (make-hash-table))

(defmacro defelem (name &body elem)
  `(progn (defparameter ,name ,@elem) 
         (setf (gethash ',name *elements*) ,name)))
