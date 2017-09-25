(in-package :server)

(defprim bl-binding (name expr)
  (:pretty () (list 'bl-binding (list :name name :expr (synth :pretty expr))))
  (:implementation (cont &rest args) 
          (apply cont (synth :implementation expr (lambda (x) (java-statement (java-pair name (synth :java-type (synth :type expr)) :init x)))) args))
  (:errors () (synth :errors expr))
  (:latex (cont &rest args) (apply cont (paragraph (line (normal "Si assegna a ~a il risultato della seguente espressione:" name))
                                                   (synth :latex expr #'identity)))))

(defprim bl-let% (bindings expr)
  (:pretty () (list 'bl-let (list :bindings (synth-all :pretty bindings) :expr (synth :pretty expr))))
  (:implementation (cont &rest args) 
          (java-concat (synth-all :implementation bindings #'identity)
                       (apply #'synth :implementation expr cont args)))
  (:errors () (apply #'append (synth :errors expr) (synth-all :errors bindings)))
  (:type () (synth :type expr))
  (:latex (cont &rest args) (apply cont (append (synth-all :latex bindings #'identity) args))))
(defmacro bl-let (bindings &body expr)
  `(let* ,(mapcar #`(,(car a1) (bl-variab ',(car a1) (synth :type ,(cadr a1)))) bindings)
     (bl-let% (list ,@(mapcar (lambda (binding) `(bl-binding ',(car binding) ,(cadr binding)))
                                      bindings)) ,@expr)))

(defprim bl-create-entity% (entity bindings)
  (:pretty () (list 'create-entity (list :entity entity :bindings (synth-plist :pretty bindings)))) 
  (:implementation (cont &rest args) 
          (let* ((new-entity-name (gensym (symbol-name (synth :name entity)))) 
                 (new-entity (java-dynamic new-entity-name)))
            (java-concat
             (java-statement (java-pair new-entity-name #1=(java-object-type (synth :name entity)) 
                                        :init (java-new #1#)))
             (synth-plist-merge
              (lambda (binding)
                (java-statement (java-chain new-entity
                                            (java-call (symb "SET-" (car binding)) (synth :implementation (cadr binding) #'identity)))))
              bindings)
             (java-statement (java-chain (java-dynamic 'entity-manager)
                                         (java-call 'persist new-entity)))
             (apply cont new-entity args))))
  (:errors () nil)
  (:type () (entity-type entity))
  (:latex (cont &rest args) (apply cont (normal "Creazione dell'entita ~a" (synth :name entity)) args)))

(defmacro bl-create-entity (entity &rest bindings)
  `(bl-create-entity% ,entity (list ,@bindings)))

(defprim bl-get (place object)
  (:pretty () (list 'bl-get (list :place place :object (synth :pretty object))))
  (:implementation (cont &rest args) 
          (let ((logic (java-chain :as (synth :java-type (synth :type this))
                                   (java-dynamic (synth :name object)) (java-call (symb 'get "-" place)))))
            (apply cont logic args)))
  (:errors () nil)
  (:type () (synth :property-type (synth :type object) place))
  (:latex (cont &rest args) (apply cont (normal "l'estrazione del campo ~a dall'oggetto ~a" place (synth :name object)))))

(defprim bl-arg (name type)
  (:pretty () (list 'bl-arg (list :name name :type (synth :pretty type)))))

(defprim bl-lambda% (inputs expr)
  (:pretty () (list 'bl-lambda (list :inputs (synth-all :pretty inputs) :expr (synth :pretty expr))))
  (:implementation (cont &rest args) (java-arrow (mapcar (lambda (arg)
                                                  (java-pair (synth :name arg)
                                                             (synth :java-type (synth :type arg))))
                                                inputs)
                                        (synth :implementation expr (lambda (x) (java-return x)))))
  (:errors () (synth :errors expr))
  (:type () (function-type (synth :type expr) (synth-all :type inputs)))
  (:latex (cont &rest args) (apply cont (synth :latex expr #'identity) args)))

(defmacro bl-lambda (inputs expr)
  `(let* ,(mapcar #`(,(car a1) (bl-variab (gensym (mkstr ',(car a1))) ,(cadr a1))) inputs) 
     (bl-lambda% (list ,@(mapcar #'car inputs)) ,expr)))

(defprim bl-map% (function collection)
  (:pretty () (list 'bl-map (list :function (synth :pretty function) :collection (synth :pretty collection))))
  
  (:implementation (cont &rest args) (apply cont (java-chain (java-call 'map (synth :implementation function)) 
                                                    :as (synth :type this))
                                   args))
  (:type () (collection-type (synth :type function)))
  (:errors () (synth :errors function))
  (:latex (cont &rest args) (apply cont 
                                   (sequence 
                                    (normal "per ogni elemento della collezione ~a viene effettuata la seguente operazione:" name)
                                    (synth :latex function #'identity)))))
(defprim bl-variab (name type)
  (:pretty () (list 'bl-variab (list :name name)))
  (:implementation (cont &rest args) (apply cont (java-dynamic name) args))
  (:errors () nil))

(defun closure-equal (x y)
  (equal (synth :pretty x) (synth :pretty y)))

(defprim bl-call (function &rest inputs)
  (:pretty () (list 'bl-call (list :function (synth :pretty function) :inputs (synth-all :pretty inputs))))
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
                (error "type mismatch")))
  (:errors () (synth :errors function))
  (:latex (cont &rest args) (apply cont 
                                   (seq 
                                    (normal "per ogni elemento della collezione ~a viene effettuata la seguente operazione:" name)
                                    (synth :latex function #'identity)))))
(defprim bl-cat (&rest exps)
  (:pretty () (list 'bl-cat (:exps (synth-all :pretty exps)))) 
  (:implementation (cont &rest args) (apply cont (reduce #'java-+ (synth-all :implementation exps #'identity)) args))
  (:errors () nil)
  (:type () (string-type 20)))

;; (defmacro mapcomm (command collection)
;;   `(let ((result (gensym (symbol-name (symb (synth :name ,collection))))))
;;      (values (mapcomm% ,command result ,collection) (bl-variab result))))

(defprim bl-condition (test expr)
  (:pretty () (list 'bl-condition (list :test (synth :pretty test) :expr (synth :pretty expr))))
  ;; (:implementation (cont &rest args) 
  ;;                  (apply cont (java-if (synth :implementation test #'identity)
  ;;                                         (java-throw (synth :implementation expr (lambda (e)
  ;;                                                                                   (java-new (synth :type e)
  ;;                                                                                             (synth :implementation e #'identity))))))
  ;;                         args))
  (:errors () (list expr))
  (:type () (synth :type expr)))

(defprim bl-unless% (conditions expr)
  (:pretty () (list 'bl-unless (list :conditions (synth-all :pretty conditions) :expr (synth :pretty expr))))
  (:implementation (cont &rest args)
                   (reduce (lambda (condition acc) 
                             (java-if (synth :implementation (synth :test condition) #'identity)
                                      (java-throw (java-new (synth :type (synth :expr condition))
                                                            (synth :implementation (synth :message (synth :expr condition)) #'identity)))
                                      acc))
                           conditions
                           :from-end t
                           :initial-value (apply #'synth :implementation expr cont args)))
  (:errors () (apply #'append (synth :errors expr) (synth-all :errors conditions)))
  (:type () (synth :type expr)))
