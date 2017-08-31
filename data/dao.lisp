(in-package :data)

(defparameter *daos* (make-hash-table))
(defmacro defdao (name entity &rest methods)
  `(progn (defparameter ,name (dao ,entity ,@methods)) 
         (setf (gethash ',name *daos*) ,name)))

(defprim dao (entity &rest methods)
  (:pretty () (list 'dao (list :methods (synth-all :pretty methods))))
  (:req () (synth-all :req methods))
  (:dao (package) (java-unit name
                             (java-package (symb package '|.dao|))
                             
                             ;; (java-import '|java.util| '|List|)
                             (java-class (symb (synth :name entity) "-D-A-O")
                                         :public t 
                                         :fields (list (java-with-annotations (java-annotation 'persistence-context)
                                                                              (java-pair 'em (java-object-type 'entity-manager) :private t)))
                                         :methods (synth-all :implementation methods)))))

(defprim dao-query (query input output)
  (:pretty () (list 'dao-query (list :input (synth :pretty input) :output (synth :pretty output))))
  (:implementation (let ((et (java-object-type (synth :name entity))))
                     (java-method (synth :name query)
                                  (mapcar (lambda (arg) (java-pair arg (java-objet-type 'string)))
                                          (synth :args query))
                                  (java-template-type 'list et))
                                  (java-concat (java-statement (java-pair 'query (java-object-type 'query) 
                                                                          :init (java-chain (java-dynamic 'this)
                                                                                            (java-dynamic 'em)
                                                                                            (java-call 'create-named-query (synth :name query)))))
                                               (mapcar (lambda (arg) 
                                                         (java-statement (java-chain (java-dynamic 'query)
                                                                                     (java-call 'set-parameter (java-string arg) arg))))
                                                       args)
                                               (java-with-annotations (java-annotation 'suppress-warnings (java-string "rawtypes"))
                                                                      (java-statement 
                                                                       (java-pair 'rows (java-object-type 'list) 
                                                                                  :init (java-chain (java-dynamic 'query)
                                                                                                    (java-call 'get-result-list)))))
                                               (java-statement 
                                                (java-pair 'results (java-template-type 'list et) 
                                                           :init (java-new (java-template-type 'array-list et (java-chain (java-dynamic 'rows (java-call size)))))
                                                           ))
                                               )))))

(defprim dao-find ()
  (:pretty (list 'dao-find)))

(defprim dao-insert ()
  (:pretty (list 'dao-insert)))

(defprim dao-delete ()
  (:pretty (list 'dao-delete)))

(defprim dao-find ()
  (:pretty (list 'dao-find)))


(defdao indicator-dao )
