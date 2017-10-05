(in-package :type)

(defprim string-type (size)
  (:pretty () (list 'string-type (list :size size)))
  (:java-type () (java-object-type 'string))
  (:sql-type () (doc:text "VARCHAR2(~a)" size)))


(defprim integer-type ()
  (:pretty () (list 'integer-type))
  (:java-type () (java-primitive-type 'long))
  (:sql-type () (doc:text "NUMBER")))

(defprim date-type ()
  (:pretty () (list 'date-type))
  (:java-type () (java-object-type 'date))
   (:sql-type () (doc:text "TIMESTAMP")))

(defprim boolean-type ()
  (:pretty () (list 'boolean-type))
  (:java-type () (java-primitive-type 'boolean))
  (:sql-type () (doc:text "VARCHAR(1)")))

(defprim entity-type (entity)
  (:pretty () (list 'entity-type (list :entity (synth :pretty entity))))
  (:property-type (prop) (let ((primary (synth :primary entity))
                               (fields (synth :fields entity))
                               (name (synth :name entity)))
                           ;; (pprint (mapcar (lambda (att) (synth :name att))
                           ;;                 (cons primary fields)))
                           
                           (aif (car (remove-if-not (lambda (att) (eq prop (synth :name att)))
                                                    (cons primary fields)))
                                (synth :type it)
                                (error "property ~a does not exist in ~a" prop name))))
  (:java-type () (java-object-type (synth :name entity))))

(defprim transfer-type (entity)
  (:pretty () (list 'transfer-type (list :entity (synth :pretty entity))))
  (:property-type (prop) (let ((primary (synth :primary entity))
                               (fields (synth :fields entity))
                               (name (synth :name entity)))
                           ;; (pprint (mapcar (lambda (att) (synth :name att))
                           ;;                 (cons primary fields)))
                           
                           (aif (car (remove-if-not (lambda (att) (eq prop (synth :name att)))
                                                    (cons primary fields)))
                                (synth :type it)
                                (error "property ~a does not exist in ~a" prop name))))
  (:java-type () (java-object-type (symb (synth :name entity) "-D-T-O"))))

(defprim format-type (format)
  (:pretty () (list 'format-type (list :format (synth :pretty format))))
  (:property-type (name) (aif (car (remove-if-not (lambda (prop) (eq name (synth :name prop)))
                                                  (synth :props format)))
                              (synth :type it)
                              (error "property ~a does not exist in ~a" name (synth :name format))))
  (:java-type () (java-object-type (symb (synth :name format) "-V-O"))))

(defprim collection-type (type)
  (:pretty () (list 'array-type (list :type (synth :pretty type))))
  (:java-type () (java-template-type 'list (synth :java-type type))))

(defprim function-type (return-type arg-types)
  (:pretty () (list 'function-type (list :return-type (synth :pretty return-type) :arg-types (synth-all :pretty arg-types))))
  (:java-type () (labels ((ftype (rtype args)
                            (cond ((null args) (java-template-type 'function 
                                                                   (java-object-type 'void)
                                                                   (synth :java-type rtype)))
                                  ((= 1 (length args)) (java-template-type 'function 
                                                                           (synth :java-type (car args))
                                                                           (synth :java-type rtype)))
                                  (t (java-template-type 'function 
                                                         (synth :java-type (car args)) 
                                                         (ftype rtype (cdr args)))))))
                   (ftype return-type arg-types))))

