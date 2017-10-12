(in-package :documentation)

(defparameter *documentation* nil)

(defmacro defdoc ((&rest names) doc)
  `(acons (list ,@names ,doc *documentation*)))

(defun get-documentation (namelist)
  (cdr (assoc namelist *documentation*)))
