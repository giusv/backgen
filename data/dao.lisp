(in-package :data)

(defparameter *daos* (make-hash-table))
(defmacro defdao (name entity &rest methods)
  `(progn (defparameter ,name (dao ,entity ,@methods)) 
         (setf (gethash ',name *daos*) ,name)))

(defprim dao (entity &rest methods)
  (:pretty () (list 'dao (list :methods (synth-all :pretty methods))))
  (:req () (synth-all :req methods))
  (:name () (symb (synth :name entity) "-D-A-O"))
  (:dao (package) (java-unit (synth :name entity)
                             (java-package (symb package '|.dao|))
                             
                             ;; (java-import '|java.util| '|List|)
                             (java-class (synth :name this)
                                         :public t 
                                         :fields (list (java-with-annotations (list (java-annotation '|PersistenceContext|))
                                                                              (java-statement (java-pair 'em (java-object-type 'entity-manager) :private t))))
                                         :methods (synth-all :implementation methods)
                                         ))))

(defprim dao-query (query)
  (:pretty () (list 'dao-query (list :query (synth :pretty query))))
  (:implementation () (let ((et (java-object-type (synth :name (synth :entity query))))
                            (attributes (synth :schema query))
                            (args (synth :args query)))
                        (java-method (doc:textify (lower-camel (synth :name query)))
                                     (mapcar (lambda (arg) (java-pair arg (java-object-type 'string)))
                                             args)
                                     (java-template-type 'list et)
                                     (java-concat (java-statement (java-pair 'query (java-object-type 'query) 
                                                                             :init (java-chain (java-dynamic 'this)
                                                                                               (java-dynamic 'em)
                                                                                               (java-call 'create-named-query (java-const (mkstr (upper-camel (synth :name query))))))))
                                                  (mapcar (lambda (arg) 
                                                            (java-statement (java-chain (java-dynamic 'query)
                                                                                        (java-call 'set-parameter (java-const (mkstr (lower-camel arg))) (java-dynamic arg)))))
                                                          args)
                                                  (java-with-annotations (list (java-annotation '|SuppressWarnings| (java-const "rawtypes")))
                                                                         (java-statement 
                                                                          (java-pair 'rows (java-object-type 'list) 
                                                                                     :init (java-chain (java-dynamic 'query)
                                                                                                       (java-call 'get-result-list)))))
                                                  (java-statement 
                                                   (java-pair 'results (java-template-type 'list et) 
                                                              :init (java-new (java-template-type 'array-list et))))
                                                  (java-foreach (java-dynamic 'row) (java-array-type (java-object-type 'object))
                                                                (java-dynamic 'rows)
                                                                (java-concat (java-statement (java-pair 'temp et :init (java-new et))) 
                                                                             (mapcar (lambda (att index) 
                                                                                       (java-statement (java-chain (java-dynamic 'temp)
                                                                                                                   (java-call (symb 'set "-" (car att)) 
                                                                                                                              (java-element 'row index :as (synth :java-type (synth :type (cdr att))))))))
                                                                                     attributes
                                                                                     (loop for i from 0 to (length attributes) collect (java-const i)))
                                                                             (java-statement (java-chain (java-dynamic 'results)
                                                                                                         (java-call 'add (java-dynamic 'temp)))))
                                                                )
                                                  (java-return (java-dynamic 'results)))
                                     )
                        )))


(defprim dao-insert ()
  (:pretty () (list 'dao-insert)))

(defprim dao-delete ()
  (:pretty () (list 'dao-delete)))

(defprim dao-find ()
  (:pretty () (list 'dao-find)))



