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
                                (apply #'synth :java-implementation expr cont args)))
  (:type () (synth :type expr)))


(defmacro tl-let (bindings &body expr)
  `(let* ,(mapcar #`(,(car a1) (tl-variab ',(car a1) (synth :type ,(cadr a1)))) bindings)
     (tl-let% (list ,@(mapcar (lambda (binding) `(tl-binding ',(car binding) ,(cadr binding)))
                              bindings)) ,@expr)))

(defprim tl-test (name pre function post)
  (:pretty () (list 'tl-test (list :name name 
                                   :pre (synth :pretty pre)
                                   :function (synth :pretty function)
                                   :post (synth :pretty post))))
  (:java-implementation (cont &rest args) 
                        (apply cont (java-method (doc:textify (lower-camel name)) 
                                                 (mapcar (lambda (input)
                                                           (java-pair (synth :name input)
                                                                      (synth :java-type (synth :type input))))
                                                         (synth :inputs function))
                                                 (synth :java-type (synth :type (synth :expr function)))
                                                 (synth :java-implementation pre #'java-statement)
                                                 (synth :java-implementation (synth :expr function) #'identity)
                                                 (synth :java-implementation post #'java-statement))
                               args))
  ;; (:type () (synth :type expr))
  )

(defprim tl-test-instance (name &rest inputs)
  (:pretty () (list 'tl-test-instance (list :name name :inputs (synth-all :pretty inputs))))
  (:java-implementation (cont &rest args) 
                   (apply cont (apply #'java-call name (synth-all :java-implementation inputs #'identity)) args))
  ;; (:type () (synth :type expr))
  )

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
                   (apply cont (java-call 'assert (synth :java-implementation assertion #'identity)) args))
  ;; (:type () (synth :type expr))
  )

(defprim tl-require (assertion)
  (:pretty () (list 'tl-require (list :assertion (synth :pretty assertion))))
  (:java-implementation (cont &rest args) 
                   (apply cont (java-call 'require (synth :java-implementation assertion #'identity)) args))
  ;; (:type () (synth :type expr))
  )

(defprim tl-equal (expr1 expr2)
  (:pretty () (list 'tl-equal (list :expr1 (synth :pretty expr1) :expr2 (synth :pretty expr2))))
  (:java-implementation (cont &rest args) 
                   (apply cont (java-equal (synth :java-implementation expr1 #'identity)
                                           (synth :java-implementation expr2 #'identity)) args))
  ;; (:type () (synth :type expr))
  )

(defprim tl-invoke-service (name)
  (:pretty () (list 'tl-invoke-service (list :name name)))
  (:java-implementation (cont &rest args) 
                   (apply cont (java-call name) args))
  (:type () (integer-type)))

(defprim tl-suite (name cases)
  (:pretty () (list 'tl-suite (list :name name 
                                    :cases (synth-all :pretty cases))))
  (:java-implementation (package) 
                        (java-unit name
                                   (java-package (symb package '|.test|)) 
                                   (java-class name
                                               :public t 
                                               :fields nil
                                               :methods (mapcar (lambda (case)
                                                                  (java-with-annotations
                                                                   (list (java-annotation '|Test|))
                                                                   (synth :java-implementation case #'identity)))
                                                                (append* cases))))))

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
            (let* ,(mapcar #`(,(car a1) (tl-variab (gensym (mkstr ',(car a1))) ,(cadr a1))) inputs) 
              (tl-test ',name
                       (tl-ensure ,pre)
                       (tl-lambda% (list ,@(mapcar #'car inputs)) ,expr)
                       (tl-let ((response ,expr))
                         (tl-require ,post)))))
          (setf (gethash ',name *tests*) ,name)))



(defmacro tl-forall (i range &body formulas)
  `(apply #'append (loop for ,i in ,range collect (tl-and ,@formulas))))

(defmacro tl-and (&body formulas)
  `(apply #'append (list ,@formulas)))

(defmacro tl-exists ((name table) (&rest values) &body formulas)
  `(let ((,name (list ',table (list ,@(apply #'append (mapcar (lambda (pair) (list (car pair) (cadr pair)))
                                                              (group values 2)))))))
     (cons ,name (append ,@formulas))))

(defmacro tl-get (place object)
  `(getf (cadr ,object) ,place))


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

(defprim tl-db% (records)
  (:pretty () (list 'tl-db% (list :records (synth-all :pretty records))))
  (:sql-implementation () (apply #'sql-concat records)))

(defmacro tl-db (&rest records)
  `(tl-db% (mapcar (lambda (record) (apply #'sql-insert (car record) (cadr record)))
                  (tl-and ,@records))))

;; (defprim tl-http-get (url)
;;   (:pretty () (list 'tl-http-get (list :url (synth :pretty url))))
;;   (:java-implementation (cont &rest args) 
;;                    (apply cont (java-call name) args))
;;   (:type () (integer-type)))

(deftest create-indicator ((id (integer-type))) 
  (tl-equal id (expr:const 1)) 
  (tl-invoke-service 'indicators)
  (tl-equal id (expr:const 1)))

;; (pprint (synth :string (synth :doc (synth :java (synth :java-implementation create-indicator #'identity)))))
;; (pprint (synth :string (synth :doc (synth :java (synth :java-implementation (create-indicator (expr:const 1)) #'identity)))))

;; (deftest indicator-sequence
;;     (tl-with-db (tl-forall ...)
;;                 (tl-orelse (verify-indicator 1)
;;                            (tl-let ((id (create-indicator 1)))
;;                              (verify-indicator 1)))))
(defsuite indicator-suite 
    (mapcar #'create-indicator (mapcar #'expr:const (list 1 2 3 4))))

;; (let ((gen (tl-generate 5 (ind indicators) (:id (random-number 10 20) :name (random-string 10))
;;              (tl-generate 2 (par parameters) (:id (tl-get :id ind) :name (random-string 10))
;;                (tl-generate 2 (boh bohs) (:id (tl-get :id par) :name (random-string 10)))))))
;;   (pprint gen)
;;   (pprint (listp gen))
;;   (pprint (synth :output (synth :doc (synth :java (synth :java-implementation (tl-db gen) #'identity))) 0)))

(defmacro defdb (&rest records)
  `(defparameter *database* (tl-db ,@records)))

(let* ((group-id (list "com" "extent"))
       (artifact-id "app")
       (basedir #p"D:/Dati/Profili/m026980/workspace/") 
       (package (append* group-id artifact-id))
       (package-symb (apply #'symb (interleave package ".")))
       (project-basedir (merge-pathnames (make-pathname :directory (list :relative artifact-id)) basedir))
       (main-basedir (merge-pathnames (make-pathname :directory (list :relative "src" "main")) project-basedir)) 
       (test-basedir (merge-pathnames (make-pathname :directory (apply #'list :relative "test" package)) main-basedir)) 
       (resources-basedir (merge-pathnames (make-pathname :directory (list :relative "resources")) main-basedir)) 
       (webapp-basedir (merge-pathnames (make-pathname :directory (list :relative "webapp")) main-basedir)) 
       (webinf-basedir (merge-pathnames (make-pathname :directory (list :relative "WEB-INF")) webapp-basedir)) 
       (metainf-basedir (merge-pathnames (make-pathname :directory (list :relative "META-INF")) resources-basedir)) 
       (app-tests (loop for value being the hash-values of *tests* collect value))
       (app-suites (loop for value being the hash-values of *suites* collect value)))
  (pprint test-basedir)
  (let ((filename (mkstr test-basedir "Common.java")))
    (pprint filename)
    (write-file filename
                (synth :string 
                       (synth :doc
                              (synth :java
                                     (java-unit 'common
                                                (java-package (symb package-symb '|.test|)) 
                                                (java-class 'common
                                                            :public t 
                                                            :fields nil
                                                            :methods (synth-all :java-implementation app-tests #'identity))))))))
  (mapcar (lambda (suite) 
            (let ((filename (mkstr test-basedir (upper-camel (synth :name suite)) ".java"))) 
              (pprint filename)
              (write-file filename
                          (synth :string (synth :doc (synth :java (synth :java-implementation suite package-symb)))))))
          app-suites))
