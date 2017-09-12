(in-package :server)


(defprim bl-binding (name expr)
  (:pretty () (list 'bl-binding (list :name name :expr (synth :pretty expr))))
  (:logic (cont &rest args) 
          (apply cont (synth :logic expr (lambda (x) (java-statement (java-pair name (synth :java-type (synth :type expr)) :init x)))) args)))


(defprim bl-let% (bindings expr)
  (:pretty () (list 'bl-let (list :bindings (synth-all :pretty bindings) :expr (synth :pretty expr))))
  (:logic (cont &rest args) 
          (java-concat (synth-all :logic bindings #'identity)
                       (apply #'synth :logic expr cont args))
          ;; (let* ((lhs-names (mapcar #'car bindings))
          ;;        (lhs-types (synth-all :type (mapcar #'cadr bindings)))
          ;;        (logics (mapcar (lambda (lhs type rhs) 
          ;;                          (synth :logic rhs (lambda (x) (java-statement (java-pair lhs type :init x)))))
          ;;                        lhs-names
          ;;                        lhs-types
          ;;                        (mapcar #'cadr bindings))))
          ;;   (java-concat (synth-all :logic bindings #'identity) (synth :logic expr cont args)))
          )
  (:type () (synth :type expr)))

(defmacro bl-let (bindings &body expr)
  `(let* ,(mapcar #`(,(car a1) (bl-variab ',(car a1) (synth :type ,(cadr a1)))) bindings)
     (bl-let% (list ,@(mapcar (lambda (binding) `(bl-binding ',(car binding) ,(cadr binding)))
                                      bindings)) ,@expr)))


(defprim bl-create-entity% (entity bindings)
  (:pretty () (list 'create-entity (list :entity entity :bindings (synth-plist :pretty bindings)))) 
  (:logic (cont &rest args) 
          (let* ((new-entity-name (gensym (symbol-name (synth :name entity)))) 
                 (new-entity (java-dynamic new-entity-name)))
            (java-concat
             (java-statement (java-pair new-entity-name #1=(java-object-type (synth :name entity)) 
                                        :init (java-new #1#)))
             (synth-plist-merge
              (lambda (binding)
                (java-statement (java-chain new-entity
                                            (java-call (symb "SET-" (car binding)) (synth :logic (cadr binding) #'identity)))))
              bindings)
             (java-statement (java-chain (java-dynamic 'entity-manager)
                                         (java-call 'persist new-entity)))
             (apply cont new-entity args))))
  (:type () (entity-type entity)))

(defmacro bl-create-entity (entity &rest bindings)
  `(bl-create-entity% ,entity (list ,@bindings)))

(defprim bl-get (place object)
  (:pretty () (list 'bl-get (list :place place :object (synth :pretty object))))
  (:logic (cont &rest args) 
          (let ((logic (java-chain :as (synth :java-type (synth :type this))
                                   (java-dynamic (synth :name object)) (java-call (symb 'get "-" place)))))
            (apply cont logic args)))
  (:type () (synth :property-type (synth :type object) place)))

(defprim bl-arg (name type)
  (:pretty () (list 'bl-arg (list :name name :type (synth :pretty type)))))

(defprim bl-lambda% (inputs expr)
  (:pretty () (list 'bl-lambda (list :inputs (synth-all :pretty inputs) :expr (synth :pretty expr))))
  (:logic (cont &rest args) (java-arrow (mapcar (lambda (arg)
                                                  (java-pair (synth :name arg)
                                                             (synth :java-type (synth :type arg))))
                                                inputs)
                                        (synth :logic expr (lambda (x) (java-return x)))))
  (:type () (function-type (synth :type expr) (synth-all :type inputs))))

(defmacro bl-lambda (inputs expr)
  `(let* ,(mapcar #`(,(car a1) (bl-variab (gensym (mkstr ',(car a1))) ,(cadr a1))) inputs) 
     (bl-lambda% (list ,@(mapcar #'car inputs)) ,expr)))

(defprim bl-map% (function collection)
  (:pretty () (list 'bl-map (list :function (synth :pretty function) :collection (synth :pretty collection))))
  
  (:logic (cont &rest args) (apply cont (java-chain (java-call 'map (synth :logic function)) 
                                                    :as (synth :type this))
                                   args))
  (:type () (collection-type (synth :type function))))

(defprim bl-variab (name type)
  (:pretty () (list 'bl-variab (list :name name)))
  (:logic (cont &rest args) (apply cont (java-dynamic name) args)))

(defun closure-equal (x y)
  (equal (synth :pretty x) (synth :pretty y)))

(defprim bl-call (function &rest inputs)
  (:pretty () (list 'bl-call (list :function (synth :pretty function) :inputs (synth-all :pretty inputs))))
  (:logic (cont &rest args) 
          (apply cont 
                 (java-chain (java-chain (synth :logic function #'identity) :as (synth :java-type (synth :type function)))
                             (mapcar (lambda (input)
                                       (java-call 'apply (synth :logic input #'identity)))
                                     inputs)
                             :as (synth :java-type (synth :type this)))
                 args))
  (:type () (if (every #'closure-equal (synth-all :type inputs) (synth :arg-types (synth :type function)))
                (synth :return-type (synth :type function))
                (error "type mismatch"))))

(defprim bl-cat (&rest exps)
  (:pretty () (list 'bl-cat (:exps (synth-all :pretty exps)))) 
  (:logic (cont &rest args) (apply cont (reduce #'java-+ (synth-all :logic exps #'identity)) args))
  (:type () (string-type 20)))
;; (defmacro mapcomm (command collection)
;;   `(let ((result (gensym (symbol-name (symb (synth :name ,collection))))))
;;      (values (mapcomm% ,command result ,collection) (bl-variab result))))
