(in-package :test)

(defprim tl-variab (name type)
  (:pretty () (list 'tl-variab (list :name name)))
  (:implementation (cont &rest args) (apply cont (java-dynamic name) args)))

(defprim tl-lambda% (inputs expr)
  (:pretty () (list 'tl-lambda (list :inputs (synth-all :pretty inputs) :expr (synth :pretty expr))))
  (:implementation (cont &rest args)
                   (java-arrow (mapcar (lambda (arg)
                                         (java-pair (synth :name arg)
                                                    (synth :java-type (synth :type arg))))
                                       inputs)
                               (synth :implementation expr (lambda (x) (java-return x)))))
  (:type () (function-type (synth :type expr) (synth-all :type inputs))))

(defmacro tl-lambda (inputs expr)
  `(let* ,(mapcar #`(,(car a1) (tl-variab (gensym (mkstr ',(car a1))) ,(cadr a1))) inputs) 
     (tl-lambda% (list ,@(mapcar #'car inputs)) ,expr)))


(defun closure-equal (x y)
  (equal (synth :pretty x) (synth :pretty y)))

(defprim tl-call (function &rest inputs)
  (:pretty () (list 'tl-call (list :function (synth :pretty function) :inputs (synth-all :pretty inputs))))
  (:implementation (cont &rest args) 
                   (apply cont 
                          (java-chain (java-chain (synth :implementation function #'identity) :as (synth :java-type (synth :type function)))
                                      (mapcar (lambda (input)
                                                (java-call 'apply (synth :implementation input #'identity)))
                                              inputs)
                                      :as (synth :java-type (synth :type this)))
                          args))
  (:type () (if (every #'closure-equal (synth-all :type inputs) (synth :arg-types (synth :type function)))
                (synth :return-type (synth :type function))
                (error "type mismatch"))))

(defprim tl-binding (name expr)
  (:pretty () (list 'tl-binding (list :name name :expr (synth :pretty expr))))
  (:implementation (cont &rest args) 
                   (apply cont (synth :implementation expr (lambda (x) (java-statement (java-pair name (synth :java-type (synth :type expr)) :init x)))) args)))

(defprim tl-let% (bindings expr)
  (:pretty () (list 'tl-let (list :bindings (synth-all :pretty bindings) :expr (synth :pretty expr))))
  (:implementation (cont &rest args) 
                   (java-concat (synth-all :implementation bindings #'identity)
                                (apply #'synth :implementation expr cont args)))
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
  (:implementation (cont &rest args) 
                   (apply cont ;; (synth :implementation function #'identity)
                          (java-method (doc:textify (lower-camel name)) 
                                       (mapcar (lambda (input)
                                                 (java-pair (synth :name input)
                                                            (synth :java-type (synth :type input))))
                                               (synth :inputs function))
                                       (synth :java-type (synth :type (synth :expr function)))
                                       (synth :implementation pre #'java-statement)
                                       (synth :implementation (synth :expr function) #'identity)
                                       (synth :implementation post #'java-statement)
                                       )
                          args))
  ;; (:type () (synth :type expr))
  )

(defprim tl-test-instance (name &rest inputs)
  (:pretty () (list 'tl-test-instance (list :name name :inputs (synth-all :pretty inputs))))
  (:implementation (cont &rest args) 
                   (apply cont (apply #'java-call name (synth-all :implementation inputs #'identity)) args))
  ;; (:type () (synth :type expr))
  )

(defprim tl-seq% (&rest test-bindings)
  (:pretty () (list 'tl-seq% (list :test-bindings (synth-all :pretty test-bindings))))
  (:implementation (cont &rest args) 
                   (apply cont (apply #'java-concat
                                      (synth-all :implementation test-bindings #'identity)) args)))

(defprim tl-test-binding (name test)
  (:pretty () (list 'tl-binding (list :name name :test (synth :pretty test))))
  (:implementation (cont &rest args) 
                   (apply cont (synth :call test 
                                      (lambda (x) 
                                        (java-pair name (synth :java-type (synth :type test)) :init x))) args)))

(defprim tl-ensure (assertion)
  (:pretty () (list 'tl-ensure (list :assertion (synth :pretty assertion))))
  (:implementation (cont &rest args) 
                   (apply cont (java-call 'assert (synth :implementation assertion #'identity)) args))
  ;; (:type () (synth :type expr))
  )

(defprim tl-require (assertion)
  (:pretty () (list 'tl-require (list :assertion (synth :pretty assertion))))
  (:implementation (cont &rest args) 
                   (apply cont (java-call 'require (synth :implementation assertion #'identity)) args))
  ;; (:type () (synth :type expr))
  )

(defprim tl-equal (expr1 expr2)
  (:pretty () (list 'tl-equal (list :expr1 (synth :pretty expr1) :expr2 (synth :pretty expr2))))
  (:implementation (cont &rest args) 
                   (apply cont (java-equal (synth :implementation expr1 #'identity)
                                           (synth :implementation expr2 #'identity)) args))
  ;; (:type () (synth :type expr))
  )

(defprim tl-invoke-service (name)
  (:pretty () (list 'tl-invoke-service (list :name name)))
  (:implementation (cont &rest args) 
                   (apply cont (java-call name) args))
  (:type () (integer-type)))


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



(deftest create-indicator ((id (integer-type))) 
  (tl-equal id (expr:const 1)) 
  (tl-invoke-service 'indicators)
  (tl-equal id (expr:const 1)))


(defprim tl-generate (n name table values &rest generators)
  (:pretty () (list 'tl-generate (list :n n :name name :table (synth :pretty table) :values (synth-all :pretty values)
                                       :generators (synth-all :pretty generators))))
  (:implementation (cont &rest args) 
                   (apply cont (reduce #'java-+ (list 
                                                 (java-const "insert into ") 
                                                 (java-const (mkstr name)))
                                       :initial-value nil) args))
  ;; (:type () (synth :type expr))
  )

(pprint (synth :string (synth :doc (synth :java (synth :implementation create-indicator #'identity)))))
(pprint (synth :string (synth :doc (synth :java (synth :implementation (create-indicator (expr:const 1)) #'identity)))))
(terpri)
(synth :output (synth :doc (synth :java (synth :implementation (tl-generate nil 'indicators nil nil) #'identity))) 0)
