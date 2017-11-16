(in-package :test)

(defprim tl-variab (name type)
  (:pretty () (list 'tl-variab (list :name name)))
  (:java-implementation (cont &rest args) (apply cont (java-dynamic name) args)))

(defprim tl-lambda% (inputs expr)
  (:pretty () (list 'tl-lambda (list :inputs (synth-all :pretty inputs) :expr (synth :pretty expr))))
  (:java-implementation (cont &rest args)
                   (java-arrow (mapcar (lambda (arg)
                                         (java-pair (synth :name arg)
                                                    (synth :java-type (synth :type arg))))
                                       inputs)
                               (synth :java-implementation expr (lambda (x) (java-return x)))))
  (:type () (function-type (synth :type expr) (synth-all :type inputs))))

(defmacro tl-lambda (inputs expr)
  `(let* ,(mapcar #`(,(car a1) (tl-variab (gensym (mkstr ',(car a1))) ,(cadr a1))) inputs) 
     (tl-lambda% (list ,@(mapcar #'car inputs)) ,expr)))

(defun closure-equal (x y)
  (equal (synth :pretty x) (synth :pretty y)))

(defprim tl-call (function &rest inputs)
  (:pretty () (list 'tl-call (list :function (synth :pretty function) :inputs (synth-all :pretty inputs))))
  (:java-implementation (cont &rest args) 
                   (apply cont 
                          (java-chain (java-chain (synth :java-implementation function #'identity) :as (synth :java-type (synth :type function)))
                                      (mapcar (lambda (input)
                                                (java-call 'apply (synth :java-implementation input #'identity)))
                                              inputs)
                                      :as (synth :java-type (synth :type this)))
                          args))
  (:type () (if (every #'closure-equal (synth-all :type inputs) (synth :arg-types (synth :type function)))
                (synth :return-type (synth :type function))
                (error "type mismatch"))))

(defprim tl-binding (name expr)
  (:pretty () (list 'tl-binding (list :name name :expr (synth :pretty expr))))
  (:java-implementation (cont &rest args) 
                   (apply cont (synth :java-implementation expr (lambda (x) (java-statement (java-pair name (synth :java-type (synth :type expr)) :init x)))) args)))

(defprim tl-let% (bindings expr)
  (:pretty () (list 'tl-let (list :bindings (synth-all :pretty bindings) :expr (synth :pretty expr))))
  (:java-implementation (cont &rest args) 
                   (java-concat (synth-all :java-implementation bindings #'identity)
                                (apply cont (synth :java-implementation expr #'identity) args)
                                ;; (apply #'synth :java-implementation expr cont args)
                                ))
  (:type () (synth :type expr)))


(defmacro tl-let (bindings &body expr)
  `(let* ,(mapcar #`(,(car a1) (tl-variab ',(car a1) (synth :type ,(cadr a1)))) bindings)
     (tl-let% (list ,@(mapcar (lambda (binding) `(tl-binding ',(car binding) ,(cadr binding)))
                              bindings)) ,@expr)))

(defprim tl-test (name pre function post result)
  (:pretty () (list 'tl-test (list :name name 
                                   :pre (synth :pretty pre)
                                   :function (synth :pretty function)
                                   :post (synth :pretty post)
                                   :result (synth :pretty result))))
  (:java-implementation (cont &rest args) 
                        (apply cont (java-method name 
                                                 (mapcar (lambda (input)
                                                           (java-pair (synth :name input)
                                                                      (synth :java-type (synth :type input))))
                                                         (synth :inputs function))
                                                 (synth :java-type (synth :type (synth :expr function)))
                                                 (synth :java-implementation pre #'java-statement)
                                                 (synth :java-implementation (synth :expr function) #'(lambda (expr) (java-statement (java-pair (synth :name result) (synth :java-type (synth :type this)) :init expr)))) 
                                                 (synth :java-implementation post (lambda (cond) (java-statement (java-assert cond "error in postcondition"))))
                                                 (java-return (java-dynamic (synth :name result))))
                               args))
  (:type () (synth :type (synth :expr function))))

(defprim tl-test-instance (name &rest inputs)
  (:pretty () (list 'tl-test-instance (list :name name :inputs (synth-all :pretty inputs))))
  (:java-implementation (cont &rest args) 
                   (apply cont (apply #'java-call name (synth-all :java-implementation inputs #'identity)) args)))

(defprim tl-seq% (&rest test-bindings)
  (:pretty () (list 'tl-seq% (list :test-bindings (synth-all :pretty test-bindings))))
  (:java-implementation (cont &rest args) 
                   (apply cont (apply #'java-concat
                                      (synth-all :java-implementation test-bindings #'identity)) args)))

(defprim tl-test-binding (name test)
  (:pretty () (list 'tl-binding (list :name name :test (synth :pretty test))))
  (:java-implementation (cont &rest args) 
                   (apply cont (synth :call test 
                                      (lambda (x) 
                                        (java-pair name (synth :java-type (synth :type test)) :init x))) args)))

(defprim tl-ensure (assertion)
  (:pretty () (list 'tl-ensure (list :assertion (synth :pretty assertion))))
  (:java-implementation (cont &rest args) 
                   (apply cont (java-assert (synth :java-implementation assertion #'identity) "error in precondition") args)))

(defprim tl-require (assertion)
  (:pretty () (list 'tl-require (list :assertion (synth :pretty assertion))))
  (:java-implementation (cont &rest args) 
                   (apply #'synth :java-implementation assertion cont args)))

(defprim tl-equal (expr1 expr2)
  (:pretty () (list 'tl-equal (list :expr1 (synth :pretty expr1) :expr2 (synth :pretty expr2))))
  (:java-implementation (cont &rest args) 
                   (apply cont (java-equal (synth :java-implementation expr1 #'identity)
                                           (synth :java-implementation expr2 #'identity)) args))
  (:type () (java-primitive-type 'boolean))) 

(defprim tl-invoke-service (name)
  (:pretty () (list 'tl-invoke-service (list :name name)))
  (:java-implementation (cont &rest args) 
                   (apply cont (java-call name) args))
  (:type () (integer-type)))

(defprim tl-get (place object)
  (:pretty () (list 'tl-get (list :place place :object (synth :pretty object))))
  (:java-implementation (cont &rest args) 
                        (let ((logic (java-chain :as (synth :java-type (synth :type this))
                                                 (synth :java-implementation object #'identity) (java-call (symb 'get "-" place)))))
                          (apply cont logic args)))
  (:type () (synth :property-type (synth :type object) place)))

(defprim tl-http-get (url &key (mtype '|application/json|))
  (:pretty () (list 'tl-http-get (list :url (synth :pretty url) :mtypes mtypes)))
  (:java-implementation (cont &rest args) 
                        (java-concat 
                         (java-statement (java-pair 'client (java-object-type 'client) :init (java-chain (java-static 'client-builder) (java-call 'new-client))))
                         ;; (java-if (java-null (java-const 1)) (java-return) (java-return))
                         (apply cont (java-chain (java-dynamic 'client)
                                                 (java-call 'target (java-const (synth :string (synth :url url))))
                                                 (java-call 'request (java-const (mkstr mtype)))
                                                 (java-call 'get)) args)))
  (:type () (response-type)))

(defprim tl-http-status (response)
  (:pretty () (list 'tl-http-status (list :response (synth :pretty response))))
  (:java-implementation (cont &rest args) 
                        (apply cont (java-chain (java-dynamic (synth :name response)) (java-call 'get-status)) args))
  (:type () (integer-type)))

(defprim tl-http-body (response class)
  (:pretty () (list 'tl-http-body (list :response (synth :pretty response))))
  (:java-implementation (cont &rest args) 
                        (progn
                          (pprint (synth :pretty response))
                          (apply cont (java-chain (java-dynamic (synth :name response))
                                                  (java-call 'read-entity (java-chain (synth :java-type (synth :type class)) (java-dynamic 'class)))) args)))
  (:type () (synth :type class)))

 (defprim tl-suite (name cases)
   (:pretty () (list 'tl-suite (list :name name 
                                     :cases (synth-all :pretty cases))))
   (:java-implementation (package) 
                         (java-unit name
                                    (java-package package) 
                                    (java-class name
                                                :public t 
                                                :fields nil
                                                :methods nil ;; (list (java-method (mapcar (lambda (case)
                                                         ;;                   (java-with-annotations
                                                         ;;                    (list (java-annotation '|Test|))
                                                         ;;                    (synth :java-implementation case #'identity)))
                                                         ;;                 (append* cases))))
                                                ))))

 (defparameter *suites* (make-hash-table))
 (defmacro defsuite (name &rest cases)
   `(progn (defparameter ,name 
             (tl-suite ',name
                      ,@cases))
         (setf (gethash ',name *suites*) ,name)))


(defparameter *tests* (make-hash-table))
(defmacro deftest (name inputs pre expr post)
  `(progn (defun ,name ,(mapcar #'car inputs)
            (tl-test-instance ',name ,@(mapcar #'car inputs)))
          (defparameter ,name 
            (let* ,(mapcar #`(,(car a1) (tl-variab (gensym (mkstr ',(car a1))) ,(cadr a1))) 
                           inputs)
              (let ((this (tl-variab (gensym "RESPONSE") (synth :type ,expr))))
                (tl-test ',name
                         (tl-ensure ,pre)
                         (tl-lambda% (list ,@(mapcar #'car inputs)) ,expr)
                         (tl-require ,post)
                         this))))
          (setf (gethash ',name *tests*) ,name)))

;; (defmacro deftest (name inputs pre expr post)
;;   `(progn (defun ,name ,(mapcar #'car inputs)
;;             (tl-test-instance ',name ,@(mapcar #'car inputs)))
;;           (defparameter ,name 
;;             (let* ,(mapcar #`(,(car a1) (tl-variab (gensym (mkstr ',(car a1))) ,(cadr a1))) inputs) 
;;               (tl-test ',name
;;                        (tl-seq% (list (tl-ensure ,pre)
;;                                       (tl-let ((that ,expr)
;;                                                )
;;                                         that))))))
;;           (setf (gethash ',name *tests*) ,name)))



(defmacro tl-forall (i range &body formulas)
  `(apply #'append (loop for ,i in ,range collect (tl-and ,@formulas))))

(defmacro tl-and (&body formulas)
  `(apply #'append (list ,@formulas)))

(defmacro tl-exists ((name table) values &body formulas)
  `(let ((,name (list ',table (list ,@(apply #'append (mapcar (lambda (pair) (list (car pair) (cadr pair)))
                                                              (group values 2)))))))
     (cons ,name (append ,@formulas))))

(defmacro tl-retrieve (place object)
  `(getf (cadr ,object) ,place))


(defun tl-random-timestamp (t1 t2)
  (tl-timestamp (+ t1 (random (- t2 t1)))))

(defun tl-timestamp (time)
  (multiple-value-bind
	(second minute hour date month year day-of-week dst-p tz) (decode-universal-time time)
    (list 'to_timestamp (format nil "~2,'0d/~2,'0d/~4,'0d ~2,'0d\:~2,'0d\:~2,'0d" date month year hour minute second) "DD/MM/YYYY HH24:MI:SS")))



;; (defmacro tl-generate (n (name table) (&rest values) &body generators)
;;   `(apply #'append (loop for i from 1 to ,n collect
;;                         (let ((,name (list ',table (list ,@(apply #'append (mapcar (lambda (pair) (list (car pair) (cadr pair)))
;;                                                                                    (group values 2)))))))
;;                           (apply #'list ,name
;;                                  (append ,@generators))))))

;; (defprim tl-random-number (start end)
;;   (:pretty () (list 'tl-random-number (list :start start :end end)))
;;   (:java-implementation (cont &rest args) 
;;                    (apply cont (random-number start end) args)))

;; (defprim tl-random-string (length)
;;   (:pretty () (list 'tl-random-string (list :length length)))
;;   (:java-implementation (cont &rest args) 
;;                    (apply cont (random-string length) args)))

;; (defprim tl-record (table values)
;;   (:pretty () (list 'tl-record (list :table table :values values)))
;;   (:java-implementation (cont &rest args) 
;;                    (apply cont (java-const (synth :string (synth :sql (apply #'insert table values)))) args)))


;; stdlib.add(new StdLibEntry(
;; 				"coinvolto",
;; 				Type.BOOLEAN,
;; 				ListUtils.cons(new Identifier(new Identifier(new Word(
;; 						"soggetto", Tag.ID)), Type.SOGGETTO), ListUtils.cons(
;; 						new Identifier(new Identifier(new Word("sinistro",
;; 								Tag.ID)), Type.STRING),
;; 						new ArrayList<Identifier>())),
;; 				"function coinvolto(soggetto,sinistro) {return \"(D_FLG_COINVOLTO = 'S')\"}"));

(defun sqlify (value)
  (typecase value
    (number value)
    (string (format nil "'~a'" value))
    (null (format nil "NULL"))
    (list (format nil "~a(~{~a~^,~})" (car value) (mapcar #'sqlify (cdr value))))
    (t (error "value ~a not well typed" value))))
(defun tl-ddl (db)
  (with-output-to-string (*standard-output*) 
    (loop for record in db do 
       ;; (pprint record)
         (let ((name (car record))
               (values (group (cadr record) 2)))
           ;; (pprint values) 
           ;; (terpri)
         
           (format t "INSERT INTO ~a (~{~a~^,~}) VALUES (~{~a~^,~});~%" 
                   (upper name)
                   (mapcar #'upper (mapcar #'car values))
                   ;; (mapcar #'upper (mapcar #'cadr values))
                   (mapcar #'sqlify (mapcar #'cadr values)))))))


(defprim tl-db% (records)
  (:pretty () (list 'tl-db% (list :records records)))
  (:sql-implementation () (apply #'sql-concat records)))

(defmacro tl-db (&rest records)
  `(tl-db% (mapcar (lambda (record) (apply #'sql-insert (car record) (cadr record)))
                  (tl-and ,@records))))

(defun tl-range (start end)
  (loop for i from start to end collect i))


;; (defprim tl-http-get (url)
;;   (:pretty () (list 'tl-http-get (list :url (synth :pretty url))))
;;   (:java-implementation (cont &rest args) 
;;                    (apply cont (java-call name) args))
;;   (:type () (integer-type)))


;; (let ((gen (tl-generate 5 (ind indicators) (:id (random-number 10 20) :name (random-string 10))
;;              (tl-generate 2 (par parameters) (:id (tl-retrieve :id ind) :name (random-string 10))
;;                (tl-generate 2 (boh bohs) (:id (tl-retrieve :id par) :name (random-string 10)))))))
;;   (pprint gen)
;;   (pprint (listp gen))
;;   (pprint (synth :output (synth :doc (synth :java (synth :java-implementation (tl-db gen) #'identity))) 0)))

(defparameter *database* nil)
(defmacro defdb (&body records)
  `(defparameter *database* (tl-and ,@records)))

;; (defun defdb (records)
;;   (defparameter *database* records))
;; (defmacro defdb (&rest records)
;;   `(defparameter *database* (tl-db ,@records)))


