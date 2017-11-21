(in-package :backgen)
(defparameter *soggetti-table* nil)
(defparameter *veicoli-table* nil)
(defparameter *sinistri-table* nil)

;; (defmacro defdb (name &rest args)
;;   (let ((glob (symb "*" name "-TABLE*"))
;; 	(add (symb "ADD-" name))
;; 	(make (symb "MAKE-" name))
;; 	(clean (symb "CLEAR-" name "-TABLE"))
;; 	(arg-names (mapcar #'car args)))
;;     `(progn (defparameter ,glob nil)
;; 	    (defun ,add (,name) 
;; 	      (push ,name ,glob))
;; 	    (defun ,clean () (defparameter ,glob nil))
;; 	    (defun ,make ,arg-names 
;; 	      (list ,@(apply #'append (mapcar #'(lambda (arg) (list (keyw arg) arg)) arg-names))))
;; 	    (defun ,(symb "RANDOM-" name "-TABLE") (n) 
;; 	      (,clean) 
;; 	      (dotimes (i n)
;; 		   (,add (,make ,@(mapcar #'(lambda (arg) (cadr arg)) args))))))))



;; (defun select (table selector-fn)
;;   (remove-if-not selector-fn table))

;; (defun make-comparison-expr (field value)
;;   `(equal (getf arg ,field) ,value))

;; (defun make-comparisons-list (fields)
;;   (loop while fields
;;      collecting (make-comparison-expr (pop fields) (pop fields))))

;; (defmacro where (&rest clauses)
;;   `#'(lambda (arg) (and ,@(make-comparisons-list clauses))))

;; (setq *accident-table* (sort *accident-table* #'< :key #'(lambda (acc) (getf acc :date))))

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
    (apply #'max (mapcar #'length (cluster-helper series duration nil)))))



(defun print-cluster (list)
  (format t "~{~{~a   ~}~%~}" list))
;; (defun sinistri (sogg)
;;   (select *accident-table* (where :id sogg)))


;; (print-cluster (cluster (sinistri 7) 5))

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

;; (defun joinable (row1 row2)
;;   (reduce #'(lambda (con att)
;; 	      (and con (or (not (getf row2 att)) 
;; 			   (equal (getf row1 att) (getf row2 att)))))
;; 	  row1
;; 	  :initial-value t))

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


(defparameter row1 '(:a 1 :b 2 :c 3))
(defparameter row2 '(:a 1 :b 2 :d 4))

;; (pprint (natjoin (list row1) (list row2)))
;; (pprint (natjoin *accident-table* *accident-table*))
;; (pprint (product *accident-table* *accident-table*))

(defun field-values (table field)
  (mapcar #'(lambda (row) (getf row field))
	  table))

;; (defdb soggetti 
;;     (id-sini (random-unique))
;;   (id-sogg (random-number 1 100))
;;   (date (random-date (encode-universal-time 0 0 0 10 2 2017) (encode-universal-time 0 0 0 12 2 2017)))
;;   (d-anno-accad (random-number 2010 2017))
;;   (d-mese-accad (random-number 1 12))
;;   (d-flg-coinvolto (random-boolean))
;;   (d-flg-richiedente (random-boolean))
;;   (d-flg-proprietario (random-boolean))
;;   (d-flg-contraente (random-boolean))
;;   (d-flg-coducente (random-boolean))
;;   (d-flg-trasporarto (random-boolean))
;;   (d-flg-pedone (random-boolean))
;;   (d-flg-leso (random-boolean))
;;   (d-flg-deceduto (random-boolean))
;;   (d-flg-testimone (random-boolean))
;;   (d-flg-medico (random-boolean))
;;   (d-flg-legale (random-boolean))
;;   (d-flg-carrozziere (random-boolean))
;;   (d-flg-respons (random-boolean))
;;   (d-flg-beneficiario (random-boolean))
;;   (d-flg-patente-invalida (random-boolean))
;;   (d-flg-grado-invalid (random-number 1 4))
;;   (m-num-senza-seg (random-number 1 10))
;;   (m-num-veic (random-number 1 10))
;;   (m-num-rich (random-number 1 10))
;;   (m-num-rich-fgvs (random-number 1 10))
;;   (m-num-tot-pagamenti (random-number 1 10))
;;   (m-num-tot-importo-les (random-number 1 10))
;;   (m-num-lesioni (random-number 1 10))
;;   (m-num-gg-da-decorrenza (random-number 1 10))
;;   (m-num-gg-a-decorrenza (random-number 1 10))
;;   ;; (m-ultimo-agg (random-date (encode-universal-time 0 0 0 10 2 2017) (encode-universal-time 0 0 0 12 2 2017)))
;;   )



;; (defdb sinistri
;;     (id-sini (random-from (field-values *soggetti-table* :id-sini)))
;;   (d-data-accad (random-date (encode-universal-time 0 0 0 10 2 2017) (encode-universal-time 0 0 0 12 2 2017)))
;;   (d-comune-accad (random-string 10))
;;   (d-flg-autorita (random-boolean))
;;   (d-flg-pedoni (random-boolean))
;;   (d-flg-danni-ogg (random-boolean))
;;   (d-data-denun (random-date (encode-universal-time 0 0 0 10 2 2017) (encode-universal-time 0 0 0 12 2 2017)))
;;   (m-num-imprese (random-number 1 10))
;;   (m-num-soggetti (random-number 1 10))
;;   (m-num-coinvolti (random-number 1 10))
;;   (m-num-interessati (random-number 1 10))
;;   (m-num-rich (random-number 1 10))
;;   (m-num-veic (random-number 1 10))
;;   (m-num-senza-seg (random-number 1 10))
;;   (m-num-danno-veic (random-number 1 10))
;;   (m-num-lesi (random-number 1 10))
;;   (m-num-trasp-les  (random-number 1 10))
;;   (m-num-decessi (random-number 1 10))
;;   (m-num-invalid (random-number 1 10))
;;   (m-tot-pagamenti (random-number 1 10))
;;   (m-tot-importo-veic (random-number 1 10))
;;   (m-tot-importo-les (random-number 1 10))
;;   (m-tot-importo-ogg (random-number 1 10))
;;   (m-num-incoerenze (random-number 1 10))
;;   (m-gg-da-decorrenza (random-number 1 10))
;;   (m-gg-a-scadenza (random-number 1 10))
;;   ;; (m-ultimo-agg (random-date (encode-universal-time 0 0 0 10 2 2017) (encode-universal-time 0 0 0 12 2 2017)))
;;   )

;; (defdb veicoli 
;;     (id-sini (random-from (field-values *soggetti-table* :id-sini)))
;;   (id-targa (random-unique))
;;   (d-num-targa (random-string 7))
;;   (d-tipo-veic (random-string 1))
;;   (d-anno-accad (random-number 2010 2017))
;;   (d-mese-accad (random-number 1 12))
;;   (d-comune-accad (random-string 10))
;;   (d-tipo-targa (random-string 1)) 
;;   (d-cod-impr-ass (random-string 5))
;;   (d-flg-black-box (random-boolean))
;;   (d-flg-senza-conducente (random-boolean))
;;   (d-flg-targa-incoerente (random-boolean))
;;   (d-flg-targa-inesistente (random-boolean))
;;   (d-flg-veic-cessato (random-boolean))
;;   (m-num-danni (random-number 1 5))
;;   (m-tot-importo-veic (random-number 1 5))
;;   (m-num-rich (random-number 1 5))
;;   (m-num-rich-fgvs (random-number 1 5))
;;   (m-num-prop (random-number 1 5))
;;   (m-anni-immatr (random-number 1 5))
;;   (m-num-incoerenze (random-number 1 5))
;;   ;; (m-ultimo-agg (random-date (encode-universal-time 0 0 0 10 2 2017) (encode-universal-time 0 0 0 12 2 2017)))
;;   )




;; (random-soggetti-table 100)
;; (random-veicoli-table 100)
;; (random-sinistri-table 100)

;; (pprint *soggetti-table*)
;; (defun where )

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

;; (defmacro deffield (name)
;;   `(campo ,(keyw "D-FLG-" name)))
;; (deffield interessato)
;; (defmacro deffields (&rest names)
;;   `(progn
;;      ,@(mapcar #'(lambda (name)
;; 		   `(deffield ,name))
;; 	       names)))

;; (deffields interessato coinvolto richiente)
(defmacro interessato ()
  `(getf row :d-flg-interessato))
(defmacro coinvolto ()
  `(getf row :d-flg-coinvolto))
(defmacro richiedente ()
  `(getf row :d-flg-richiedente))
(defmacro responsabile ()
  `(getf row :d-flg-respons))


(defun sco3 (soggetto durata lesi occorrenze)
  (some #'(lambda (cl) (> (length cl) occorrenze))
	(cluster (restrict (equijoin *soggetti-table* *sinistri-table* :id-sini)
			     #'(lambda (row)
				 (and (= (getf row :id-sogg) soggetto)
				      (or (getf row :d-flg-interessato)
					  (getf row :d-flg-coinvolto)
					  (getf row :d-flg-richiedente))
				      (> (getf row :m-num-lesi) lesi))))
		   durata)))

;; (pprint (restrict (natjoin *soggetti-table* *sinistri-table*)
;; 	   #'(lambda (row)
;; 	       (and (= (getf row :id-sogg) 16)
;; 		    (or (getf row :d-flg-interessato)
;; 			(getf row :d-flg-coinvolto)
;; 			(getf row :d-flg-richiedente))
;; 		    (> (getf row :m-num-lesi) 0)))))


;; (defun sco1 (soggetto durata)
;;   (cluster (restrict (sinistri soggetto)
;; 		     #'(lambda (row)
;; 			 (not (getf row :d-flg-interessato))))
;; 	   durata))

(defparameter sogg-row (restrict *soggetti-table* #'(lambda (row) (= (getf row :id-sini) 200))))
(defparameter sini-row (restrict *sinistri-table* #'(lambda (row) (= (getf row :id-sini) 200))))

;; (pprint (sco1 16 10000000000 6))
;; (pprint (sco3 16 100 6 2))

;; (defun test-sco1 (soggetto durata)
;;  (pprint (cluster (restrict (sinistri soggetto)
;; 			    #'(lambda (row)
;; 				(getf row :d-flg-respons)))
;; 		  durata)))
;; (test-sco1 16 1000)
;; (defun sco1 (soggetto)
;;   (restrict (sinistri soggetto)
;; 	    #'(lambda (row)
;; 		(not (getf row :d-flg-interessato)))))

;; (pprint (sco1 16))

