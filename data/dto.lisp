(in-package :data)

;; (defparameter *dtos* (make-hash-table))
;; (defun defdto (name entity)
;;   (progn (defparameter name (apply #'dto entity methods)) 
;;          (setf (gethash name *dtos*) name)
;;          name))

(defprim dto (entity)
  (:pretty () (list 'dto (list :entity (synth :pretty entity))))
  (:name () (symb (synth :name entity) "-D-T-O"))
  (:dto (package) (java-unit (synth :name entity)
                             (java-package (symb package '|.dto|))
                             (java-import package '|model.*|)
                             (java-class (synth :name this)
                                         :public t
                                         :constructors (let* ((entity-name (synth :name entity))
                                                              (entity-var (java-dynamic entity-name))
                                                              (dto-name (symb entity-name "-D-T-O"))
                                                              (dto-type (java-object-type dto-name))
                                                              (dto (java-dynamic dto-name))) 
                                                         (list (java-constructor dto-name 
                                                                                 (list (java-pair entity-name (synth :java-type (synth :type entity))))
                                                                                 (java-concat
                                                                                  ;; (java-statement (java-pair dto-name dto-type  :init (java-new dto-type)))
                                                                                  (mapcar
                                                                                   (lambda (field)
                                                                                     (java-statement (java-chain (java-dynamic 'this) 
                                                                                                                 (java-call (symb "SET-" (synth :name field))
                                                                                                                            (java-chain entity-var
                                                                                                                                        (java-call (symb "GET-" (synth :name field))))))))
                                                                                   (synth :fields entity))
                                                                                  ;; (java-return dto)
                                                                                  ))
                                                               (java-constructor dto-name nil))) 
                                         :fields (mapcar (lambda (field)
                                                           (java-field-with-accessors nil (synth :name field) (synth :java-type (synth :type field))))
                                                         (cons (synth :primary entity) (synth :fields entity)))))))

