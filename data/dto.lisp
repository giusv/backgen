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
                             (java-class (synth :name this)
                                         :public t 
                                         :fields (mapcar (lambda (field)
                                                           (java-field-with-accessors nil (synth :name field) (synth :java-type (synth :type field))))
                                                         (cons (synth :primary entity) (synth :fields entity)))))))

