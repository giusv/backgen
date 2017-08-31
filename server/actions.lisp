(in-package :server)

(defprim empty ()
  (:pretty () (list 'empty))
  (:logic () ()))


;; (symbol-macrolet ((x '(value x))
;;                   (y '(value y)))
;;   (pprint (list x y)))

(defmacro with-fields ((&rest names) format &body actions)
  `(symbol-macrolet ,(mapcar #`(,(car a1) (expr:attr ,format ',(cadr a1))) names)
     ,@actions))

(defprim create-entity% (entity result bindings)
  (:pretty () (list 'create-entity (list :entity entity :result result :bindings (synth-plist :pretty bindings)))) 
  (:logic () (let* ((new-entity-name (gensym (symbol-name (synth :name entity)))) 
                    (new-entity (java-dynamic new-entity-name)))
               (java-concat
                (java-statement (java-pair new-entity-name (java-type (synth :name entity)) 
                                       :init (java-new (synth :name entity))))
                (synth-plist-merge
                 (lambda (binding)
                   (java-statement (java-chain new-entity
                                           (java-call (symb "SET-" (car binding)) (synth :blub (cadr binding))))))
                 bindings)
                (java-statement (java-chain (java-dynamic 'entity-manager)
                                        (java-call 'persist new-entity)))
                (java-return (java-chain new-entity (java-call (symb "GET-" (synth :name (synth :primary entity)))) (java-call 'to-string))))))
  (:type () (java-object-type (synth :name entity)))
  (:blub () (let* ((new-entity-name (gensym (symbol-name (synth :name entity)))) 
                    (new-entity (java-dynamic new-entity-name)))
               (java-concat
                (java-statement (java-pair new-entity-name (java-type (synth :name entity)) 
                                       :init (java-new (synth :name entity))))
                (synth-plist-merge
                 (lambda (binding)
                   (java-statement (java-chain new-entity
                                           (java-call (symb "SET-" (car binding)) (synth :blub (cadr binding)))))) 
                 bindings)
                (java-return new-entity)))))


(defmacro create-entity (entity &rest bindings)
  `(let ((result (gensym (symbol-name (symb (synth :name ,entity))))))
     (values (create-entity% ,entity result (list ,@bindings)) (expr:variab result))))

(defprim update-entity% (entity id result bindings)
  (:pretty () (list 'update-entity (list :entity entity :id :id :result result :bindings (synth-plist :pretty bindings))))
  
  (:logic () (let* ((new-entity-name (gensym (symbol-name (synth :name entity)))) 
                    (new-entity (java-dynamic new-entity-name)))
               (java-concat
                (java-statement (java-pair new-entity-name (java-type (synth :name entity)) 
                                       :init (java-chain (java-dynamic 'entity-manager)
                                                       (java-call 'find (java-chain (java-static (synth :name entity)) (java-dynamic 'class))
                                                                (synth :call id)))))
                (synth-plist-merge
                 (lambda (binding)
                   (java-statement (java-chain new-entity
                                           (java-call (symb "SET-" (car binding)) (synth :blub (cadr binding))))))
                 bindings))))
  (:type () (java-object-type (synth :name entity)))
  (:blub () (let* ((new-entity-name (gensym (symbol-name (synth :name entity)))) 
                   (new-entity (java-dynamic new-entity-name)))
              (java-concat
               (java-statement (java-pair new-entity-name (java-type (synth :name entity)) 
                                       :init (java-chain (java-dynamic 'entity-manager)
                                                       (java-call 'find (java-dynamic id)))))
               (synth-plist-merge
                (lambda (binding)
                  (java-statement (java-chain new-entity
                                          (java-call (symb "SET-" (car binding)) (synth :blub (cadr binding)))))) 
                bindings)
               (java-return new-entity)))))


(defmacro update-entity (entity id &rest bindings)
  `(let ((result (gensym (symbol-name (symb (synth :name ,entity))))))
     (values (update-entity% ,entity ,id result (list ,@bindings)) (expr:variab result))))


(defprim find-entity% (entity result id)
  (:pretty () (list 'find-entity (list :entity entity :result result :id id)))
  (:logic () (java-statement (java-pair result (java-type (synth :name entity)) 
                                    :init (java-chain (java-dynamic 'entity-manager)
                                                    (java-call 'find (java-chain (java-static (synth :name entity)) (java-dynamic 'class)) 
                                                             (synth :call id))))))
  (:type () (java-object-type (synth :name entity))))

(defmacro find-entity (entity id)
  `(let ((result (gensym (symbol-name (synth :name ,entity)))))
     (values (find-entity% ,entity result ,id) (expr:variab result))))

(defprim exec-query% (query result)
  (:pretty () (list 'exec-query (list :query (synth :pretty query) :result :result)))
  (:logic () (java-with-annotations 
              (list (java-annotation2 '|SuppressWarnings| (java-const "unchecked")))
              (java-statement (java-pair result (synth :type query)
                                     :init (java-chain (java-dynamic 'entity-manager)
                                                     (synth :call query)
                                                     :as (synth :type query))))))
  (:type () (synth :type query)))
(defmacro exec-query (query)
  `(let ((result (gensym (symbol-name (synth :name ,query)))))
     (values (exec-query% ,query result) (expr:variab result))))

(defprim concat% (&rest actions)
  (:pretty () (list 'concat (synth-all :pretty actions)))
  (:logic () (java-concat (synth-all :logic actions)))
  (:type () (synth :type (car (last actions)))))

(defmacro concat (&rest bindings)
  (let ((new-bindings (mapcar #'(lambda (binding)
			  (cons (gensym) binding)) 
			      bindings)))
    `(bindall ,new-bindings
      (concat% ,@(mapcar #'car new-bindings)))))

(defprim mu% (input command)
  (:pretty () (list 'mu (list :input input :command command)))
  (:logic () (java-arrow (list (java-dynamic input)) (synth :logic command)))
  (:blub () (java-arrow (list (java-dynamic input)) (synth :blub command)))
  (:type () (synth :type command)))

(defmacro mu (input command)
  `(let* ((,input ',input)) 
     (mu% ,input ,command)))


(defprim mapcomm% (command result collection)
  (:pretty () (list 'mapcomm (list :command command :collection collection)))
  (:logic () (java-statement
              (java-pair result (synth :type this)
                       :init (java-chain (java-static 'arrays)
                                       (java-call 'stream (synth :blub collection))
                                       (java-call 'map (synth :blub command))
                                       (java-call 'to-array)
                                       :as  (synth :type this)))))
  (:blub () (java-chain (java-static 'arrays)
                      (java-call 'stream (synth :blub collection))
                      (java-call 'map (synth :blub command))
                      (java-call 'to-array)
                      :as (synth :type this)))
  (:type () (java-array-type (synth :type command))))


(defmacro mapcomm (command collection)
  `(let ((result (gensym (symbol-name (symb (synth :name ,collection))))))
     (values (mapcomm% ,command result ,collection) (expr:variab result))))

(defprim fork (condition success failure)
  (:pretty () (list 'fork (list :condition condition :success success :failure failure)))
  (:logic () (java-if (synth :blub condition) 
                    (synth :logic success) 
                    (synth :logic failure))))

(defprim create-transfer% (target result bindings)
  (:pretty () (list 'create-transfer (list :target target :result result :bindings (synth-plist :pretty bindings))))
  (:logic () (let* ((new-class (symb (synth :name target) "-J-T-O")))
               (java-concat
                (java-statement (java-pair result (java-type new-class)
                                       :init (java-new new-class)))
                (synth-plist-merge
                 (lambda (binding)
                   (java-statement (java-chain (java-dynamic result)
                                           (java-call (symb "SET-" (car binding)) (synth :blub (cadr binding))))))
                 bindings)
                )))
  (:type () (java-object-type (symb (synth :name target) "-J-T-O")))
  (:blub () (java-concat 
             (synth :logic this)
             (java-return (java-dynamic result)))))

(defmacro create-transfer (target &rest bindings)
  `(let ((result (gensym (symbol-name (symb (synth :name ,target) "-J-T-O")))))
     (values (create-transfer% ,target result (list ,@bindings)) (expr:variab result))))

(defprim respond (code &optional item)
  (:pretty () (list 'respond (list :code code :item item))) 
  (:logic () (case code
               ((:ok) (java-return (java-dynamic (synth :name item))))
               ((:no-content) (java-empty))
               ((:not-found) (java-throw (java-new 'exception))))))

