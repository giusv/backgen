(in-package :data)

(defparameter *daos* (make-hash-table))
(defun defdao (name entity &rest methods)
  (progn (defparameter name (apply #'dao entity methods)) 
         (setf (gethash name *daos*) name)
         name))

(defprim dao (entity &rest methods)
  (:pretty () (list 'dao (list :entity (synth :pretty entity) :methods (synth-all :pretty methods))))
  (:req () (synth-all :req methods))
  (:name () (symb (synth :name entity) "-D-A-O"))
  (:dao (package) (java-unit (synth :name entity)
                             (java-package (symb package '|.dao|)) 
                             (if (get-queries entity) 
                                 (list (java-import '|java.util| '|List| '|ArrayList| '|Date|)
                                       (java-import '|java.util.stream| '|Stream| '|Collectors|)
                                       (java-import '|javax.persistence| '|Query|)))
                             (java-import package '|model.*|)
                             (java-import package '|dto.*|)
                             (java-import '|javax.persistence| '|EntityManager| '|PersistenceContext|)

                             (java-class (synth :name this)
                                         :public t 
                                         :fields (list (java-with-annotations (list (java-annotation '|PersistenceContext|))
                                                                              (java-statement (java-pair 'entity-manager (java-object-type 'entity-manager) :private t))))
                                         :methods (synth-all :java-implementation methods entity)
                                         ))))

(defprim dao-query (query)
  (:pretty () (list 'dao-query (list :query (synth :pretty query))))
  (:java-implementation (entity) (let* ((attributes (synth :schema query))
                                   (args (synth :args query))
                                   (entity-name (synth :name entity))
                                   (entity-var (java-dynamic entity-name))
                                   (dto-name (symb entity-name "-D-T-O"))
                                   (dto-type (java-object-type dto-name))
                                   (dto (java-dynamic dto-name))
                                   (dto-list-type (java-template-type 'list dto-type))) 
                              (java-method (synth :name query)
                                           (mapcar (lambda (arg) (java-pair (synth :name arg) (synth :java-type (synth :type arg))))
                                                   args)
                                           (java-template-type 'list dto-type)
                                           (java-concat (java-statement (java-pair 'query (java-object-type 'query) 
                                                                                   :init (java-chain (java-dynamic 'entity-manager)
                                                                                                     (java-call 'create-named-query (java-const (string-downcase (mkstr (synth :name query))))))))
                                                        (mapcar (lambda (arg) 
                                                                  (java-statement (java-chain (java-dynamic 'query)
                                                                                              (java-call 'set-parameter (java-const (mkstr (lower-camel (synth :name arg)))) (synth :java-implementation arg #'identity)))))
                                                                args)
                                                        (java-with-annotations (list (java-annotation '|SuppressWarnings| (java-const "rawtypes")))
                                                                               (java-statement 
                                                                                (java-pair 'rows (java-object-type 'list) 
                                                                                           :init (java-chain (java-dynamic 'query)
                                                                                                             (java-call 'get-result-list)))))
                                                        (java-with-annotations (list (java-annotation '|SuppressWarnings| (java-const "unchecked")))
                                                                               (java-statement 
                                                          (java-pair 'results dto-list-type
                                                                     :init 
                                                                     (java-chain :as dto-list-type
                                                                                 (java-chain :as (java-template-type 'stream (java-array-type (java-object-type 'object)))
                                                                                             (java-dynamic 'rows)
                                                                                             (java-call 'stream))
                                                                                 (java-call 'map
                                                                                            (java-arrow (list (java-pair 'row (java-array-type (java-object-type 'object))))
                                                                                                        (java-concat 
                                                                                                         (java-statement (java-pair dto-name dto-type :init (java-new dto-type)))
                                                                                                         (mapcar (lambda (att index) 
                                                                                                                   (java-statement (java-chain dto
                                                                                                                                               (java-call (symb 'set "-" (car att)) 
                                                                                                                                                          (java-element 'row index :as (synth :java-type (synth :type (cdr att))))))))
                                                                                                                 attributes
                                                                                                                 (loop for i from 0 to (length attributes) collect (java-const i)))
                                                                                                         (java-return dto))))
                                                                                 (java-call 'collect (java-chain (java-static 'collectors) (java-call 'to-list)))))))
                                                        ;; (java-foreach (java-dynamic 'row) (java-array-type (java-object-type 'object))
                                                        ;;               (java-dynamic 'rows)
                                                        ;;               (java-concat (java-statement (java-pair 'temp et :init (java-new et))) 
                                                        ;;                            (mapcar (lambda (att index) 
                                                        ;;                                      (java-statement (java-chain (java-dynamic 'temp)
                                                        ;;                                                                  (java-call (symb 'set "-" (car att)) 
                                                        ;;                                                                             (java-element 'row index :as (synth :java-type (synth :type (cdr att))))))))
                                                        ;;                                    attributes
                                                        ;;                                    (loop for i from 0 to (length attributes) collect (java-const i)))
                                                        ;;                            (java-statement (java-chain (java-dynamic 'results)
                                                        ;;                                                        (java-call 'add (java-dynamic 'temp))))))
                                                        (java-return (java-dynamic 'results)))))))
 
(defprim dao-create ()
  (:pretty () (list 'dao-create)) 
  (:java-implementation (entity) 
                   (let* ((new-entity-name (synth :name entity)) 
                          (dto-name (symb (synth :name entity) "-D-T-O")) 
                          (new-entity (java-dynamic new-entity-name))
                          (dto (java-dynamic dto-name)))
                     (java-method 'create 
                                  (list (java-pair dto-name (java-object-type dto-name)))
                                  (java-object-type dto-name)
                                  (java-concat
                                   (java-statement (java-pair new-entity-name #1=(java-object-type (synth :name entity)) 
                                                              :init (java-new #1#)))
                                   (mapcar
                                    (lambda (field)
                                      (java-statement (java-chain new-entity
                                                                  (java-call (symb "SET-" (synth :name field))
                                                                             (java-chain dto 
                                                                                         (java-call (symb "GET-" (synth :name field))))))))
                                    (synth :fields entity))
                                   (java-statement (java-chain (java-dynamic 'entity-manager)
                                                               (java-call 'persist new-entity)))
                                   (java-return (java-new (java-object-type dto-name) new-entity))))))
  (:errors () nil))

(defprim dao-find ()
  (:pretty () (list 'dao-find)) 
  (:java-implementation (entity) 
                   (let* ((found-entity-name (synth :name entity)) 
                          (dto-name (symb (synth :name entity) "-D-T-O")) 
                          (found-entity (java-dynamic found-entity-name))
                          (dto (java-dynamic dto-name)))
                     (java-method 'find 
                                  (list (java-pair 'id (synth :java-type (integer-type))))
                                  (java-object-type dto-name)
                                  (java-concat
                                   ;; (java-statement (java-pair dto-name #1=(java-object-type dto-name) 
                                   ;;                            :init (java-new #1#)))
                                   (java-statement (java-pair found-entity-name (java-object-type (synth :name entity)) 
                                                              :init (java-chain (java-dynamic 'entity-manager)
                                                                                (java-call 'find (java-chain (java-static found-entity-name) (java-dynamic 'class)) (java-dynamic 'id)))))
                                   ;; (mapcar
                                   ;;  (lambda (field)
                                   ;;    (java-statement (java-chain dto
                                   ;;                                (java-call (symb "SET-" (synth :name field))
                                   ;;                                           (java-chain found-entity 
                                   ;;                                                       (java-call (symb "GET-" (synth :name field))))))))
                                   ;;  (synth :fields entity))
                                   
                                   (java-return (java-new (java-object-type dto-name) found-entity))
                                   ))))
  (:errors () nil))

(defprim dao-delete ()
  (:pretty () (list 'dao-delete)) 
  (:java-implementation (entity) 
                   (let* ((deleted-entity-name (synth :name entity)) 
                          (deleted-entity (java-dynamic deleted-entity-name)))
                     (java-method 'delete 
                                  (list (java-pair 'id (synth :java-type (integer-type))))
                                  (java-primitive-type 'void)
                                  (java-concat
                                   (java-statement (java-chain (java-dynamic 'entity-manager)
                                                               (java-call 'remove (java-call 'find (java-dynamic 'id)))))))))
  (:errors () nil))

(defun generate-dao (entity)
  ;; (pprint (synth :name entity)) (pprint (synth-all :pretty (get-queries entity)))
  (apply #'dao entity (append* (dao-create)
                               (dao-find)
                               (dao-delete)
                               (mapcar #'dao-query (get-queries entity)))))
