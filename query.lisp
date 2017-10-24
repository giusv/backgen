(in-package :data)

(defmacro with-queries (bindings query)
  `(let ,(mapcar #`(,(car a1) (query ',(car a1) ,(cadr a1)))
                bindings)
     ,query))

(defprim query (name value)
  (:pretty () (list 'query (list :name name :value value)))
  (:schema () (synth :schema value))
  (:sql () (doc:hcat+ (synth :sql value) (doc:text "~a" name))))

(defprim relation (entity)
  (:pretty () (list 'relation (list :entity (synth :pretty entity))))
  (:schema () (synth :schema entity))
  (:sql () (doc:textify (upper (synth :name entity)))))

(defprim product (&rest queries) 
  (:pretty () (list 'product (list :queries (synth-all :pretty queries))))
  (:schema () (apply #'append (synth-all :schema queries)))
  (:sql ()  (apply #'doc:punctuate (doc:comma) nil (synth-all :sql queries))))

(defprim project (query &rest attributes) 
  (:pretty () (list 'project (list :attributes attributes :query (synth :pretty query))))
  (:schema () (let ((schema (synth :schema query))) 
                ;; (pprint (reduce (lambda (acc att) (cons (assoc att schema) acc)) 
                ;;          attributes
                ;;          :initial-value nil))
                (reduce (lambda (att acc) (cons (assoc att schema) acc)) 
                        attributes
                        :initial-value nil
                        :from-end t)))
  (:sql () (doc:parens (doc:hcat+
                        (doc:text "SELECT")
                        (if attributes 
                            (apply #'doc:punctuate (doc:comma) nil (mapcar #'doc:textify (mapcar #'car (synth :schema this))))
                            (doc:text "*"))
                        (doc:text "FROM")
                        (synth :sql query)))))

(defprim restrict (query expression) 
  (:pretty () (list 'restrict (list :expression (synth :pretty expression) :query (synth :pretty query))))
  (:schema () (synth :schema query))
  (:sql () (doc:hcat+
            (synth :sql query)
            (doc:text "WHERE")
            (synth :sql expression))))

(defprim equijoin (query1 query2 &rest attributes) 
  (:pretty () (list 'equijoin (list :query1 (synth :pretty query1) :query2 (synth :pretty query2) :attributes (synth-all :pretty attributes))))
  (:schema () (let ((union (append (synth :schema query1)
                                   (synth :schema query2))))
                (reduce #'(lambda (acc attr) (remove attr acc :count 1)) 
                        attributes
                        :initial-value union)))
  (:sql () (doc:hcat+
            (synth :sql query1)
            (doc:text "INNER JOIN")
            (synth :sql query2)
            (doc:text "ON ~a.~a = ~a.~a" (upper (synth :name query1)) (upper (car attributes)) (upper (synth :name query2)) (upper (car attributes))))))

;; (pprint (synth :pretty 
;;                (let ((trips (relation 'trips))
;;                      (cities (relation 'cities)))
;;                  (project (restrict (product trips cities)
;;                                     (expr:+true+))
;;                           'id 'name))))

(defprim ql-variab (name type)
  (:pretty () (list 'ql-variab (list :name name)))
  (:java-implementation (cont &rest args) (apply cont (java-dynamic name) args))
  (:sql () (doc:text ":~a" (lower-camel name)))
  (:errors () nil)
  (:entities () nil))

(defprim named-query (name args entity template)
  (:pretty () (list 'named-query (list :name name 
                                       :entity (synth :pretty entity)
                                       :args args
                                       :template (synth :pretty template))))
  (:annotation () (java-annotation '|NamedNativeQuery|
                                   (java-object :|name| (java-const (string-downcase name))
                                                :|query| (java-const (synth :string (synth :sql template))))))
  (:schema () (synth :schema template)))

(defprim named-query-instance (name entity &rest args)
  (:pretty () (list 'named-query (list :name name :entity (synth :pretty entity) :args (synth-plist :pretty args))))
  (:type () (java-array-type (java-object-type (synth :name entity))))
  (:call () (java-chain (java-call 'create-named-query (java-const (mkstr name)))
                        (synth-plist-merge 
                         (lambda (arg)
                           (java-call 'set-parameter (java-const (mkstr (car arg))) (synth :call (cadr arg)))) 
                         args)
                        (java-call 'get-result-list)
                        (java-call 'to-array))))

(defparameter *queries* (make-hash-table))
(defmacro defquery (name args entity body)
  `(progn (defun ,name ,(mapcar #'car args) 
            (named-query-instance ',name ,entity ,@(apply #'append (mapcar #`(,(keyw (car a1)) ,(car a1)) args))))
          ;; (defparameter ,(symb name "-TEMPLATE")
          ;;   (let ,(mapcar #`(,a1 (expr:param ',a1)) args)
          ;;     (named-query ',name ,body)))
          (defparameter ,name 
            (let ,(mapcar #`(,(car a1) (ql-variab ',(car a1) ,(cadr a1))) args)
              ;; ,(mapcar #`(,a1 (expr:param ',a1)) args)
              (named-query ',name (list ,@(mapcar #`,(car a1) args)) ,entity
                           ,body)))
          
          (setf (gethash ',name *queries*) ,name)))





;; (defparameter *query* (restrict (equijoin (relation 'news-entity)
;;                                           (relation 'subscription-entity) 
;;                                           'news-id) 
;;                                 (+equal+ (+true+) (+true+))))

;; (pprint (synth output (synth to-doc (synth to-html *query*)) 0))


;;(synth attributes *people*)

