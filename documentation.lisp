(in-package :documentation)

(defparameter *documentation* nil)

(defmacro quotify (&rest symbols)
  (if (null symbols)
      nil
      `(cons ',(car symbols) (quotify ,@(cdr symbols)))))
(quotify a b)
(defmacro defdoc ((&rest names) doc)
  `(setf *documentation* (acons (quotify ,@names) ,doc *documentation*)))

(defun get-documentation (namelist)
  (cdr (assoc namelist *documentation* :test #'equal)))
