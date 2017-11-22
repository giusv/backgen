(in-package :backgen)
(defparameter *soggetti-table* nil)
(defparameter *veicoli-table* nil)
(defparameter *sinistri-table* nil)

(defun cluster (series duration &optional (accessor :d-data-accad))
  (labels ((cluster-helper (series duration acc)
	     (if (null series)
		 (reverse (mapcar #'reverse acc))
		 (let ((head (car series)))
		   (cluster-helper (cdr series)
				   duration
				   (cons (list head)
					 (mapcar #'(lambda (cluster) 
						     (if (< (- (getf head accessor) 
							       (getf (car (last cluster)) accessor))
							    duration)
							 (cons head cluster)
							 cluster))
						 acc)))))))
    (aif (mapcar #'length (cluster-helper series duration nil))
         (apply #'max it)
         0)))
(defmacro defops (&body bindings)
  `(progn
     ,@(mapcar #'(lambda (binding)
                   (destructuring-bind (name op) binding
                     `(defun ,name (n1 n2)
                        (if (and (not (null n1)) (not (null n2)))
                            (,op n1 n2)
                            nil))))
	       bindings)))

(defops (ind-ge >=)
        (ind-gt >)
        (ind-le <=)
        (ind-lt <)
        (ind-eq =)
        (ind-plus +)
        (ind-minus -)
        (ind-times *)
        (ind-divide /))


(defun print-cluster (list)
  (format t "~{~{~a   ~}~%~}" list))

(defun restrict (table expression)
  (remove-if-not expression table))


(defun project (table &rest attributes)
  (mapcar #'(lambda (row) (reduce #'(lambda (acc att) 
				      (let ((val (getf row att)))
					(if val
					    (append acc (list att val))
					    acc)))
				  attributes
				  :initial-value nil))
	  table))


(defun product (table1 table2)
  (reduce #'(lambda (acc row1)
	      (append acc (mapcar #'(lambda (row2)
				      (append row1 row2))
				  table2)))
	  table1
	  :initial-value nil))

(defun natjoin (table1 table2)
  (reduce #'(lambda (acc row1)
	      (append acc (remove nil (mapcar #'(lambda (row2)
						  (if (joinable row1 row2)
						      (append row1 (car (apply #'project (list row2) 
									       (set-difference (attributes row2)
											       (common-attributes row1 row2)))))))
					      table2))))
	  table1
	  :initial-value nil))

(defun equijoin (table1 table2 &rest attrs)
  (reduce #'(lambda (acc row1)
	      (append acc 
		      (remove nil 
			      (mapcar #'(lambda (row2)
					  (if (apply #'joinable row1 row2 attrs)
					      (append row1 (car (apply #'project (list row2) 
								       (set-difference (attributes row2)
										       (common-attributes row1 row2)))))))
				      table2))))
	  table1
	  :initial-value nil))

(defun attributes (row)
  (mapcar #'car (group row 2)))
(defun common-attributes (row1 row2)
  (intersection (mapcar #'car (group row1 2)) (mapcar #'car (group row2 2))))

(defun joinable (row1 row2 &rest attrs)
  (if (null attrs)
      (apply #'joinable2 row1 row2 (common-attributes row1 row2))
      (apply #'joinable2 row1 row2 attrs)))

(defun joinable2 (row1 row2 &rest attrs)
  (reduce #'(lambda (con att)
	      (and con
		   (equal (getf row1 att) (getf row2 att))))
	  attrs
	  :initial-value t))


(defun field-values (table field)
  (mapcar #'(lambda (row) (getf row field))
	  table))

(defmacro sinistri (sogg &optional (condition t))
  `(restrict (equijoin *soggetti-table* *sinistri-table* :id-sini)
	    #'(lambda (row) 
		(and (equal (getf row :id-sogg) ,sogg)
		     ,condition))))

(defmacro indicatore (name args pars &body body)
  `(defun ,name (,@args ,@pars)
     ,@body))
(defmacro campo (name)
  `(getf row ,name))


(defun esiste (cluster occorrenze)
  (some #'(lambda (cl) (>= (length cl) occorrenze))
	cluster))

(defmacro numero-lesi (strict)
  (if strict
      `(getf row :m-num-lesi)
      `(getf row :m-num-lesi)))


(defmacro deflags (&body bindings)
  `(progn
     ,@(mapcar #'(lambda (binding)
                   (destructuring-bind (name flag) binding
                     `(defmacro ,name (strict)
                        (if strict 
                            `(eql (getf row ,,flag) t)
                            `(let ((flg (getf row ,,flag)))
                               (or (eql flg t) (null flg)))))))
	       bindings)))

(deflags 
  (interessato ':d-flg-interessato)
  (coinvolto ':d-flg-coinvolto)
  (richiedente ':d-flg-richiedente)
  (responsabile ':d-flg-respons) 
  (leso ':d-flg-leso)
  (richiedente ':d-flg-richiedente)
  (proprietario ':d-flg-proprietario)
  (contraente ':d-flg-contraente)
  (deceduto ':d-flg-deceduto)
  (testimone ':d-flg-testimone))

(defmacro defmeas (&body bindings)
  `(progn
     ,@(mapcar #'(lambda (binding)
                   (destructuring-bind (name flag) binding
                     `(defmacro ,name (strict)
                        (if strict
                            `(getf row ,,flag)
                            `(getf row ,,flag)))))
	       bindings)))
(defmeas 
    (numerolesi ':m-num-lesi)
    (dataaccadimento ':d-data-accad)
    (datadenuncia ':d-data-denun))
