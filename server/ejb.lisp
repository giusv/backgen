(in-package :server)

(defparameter *ejbs* (make-hash-table))
(defmacro defejb (name &rest methods)
  `(progn (defparameter ,name (ejb ,@methods)) 
         (setf (gethash ',name *ejbs*) ,name)))

(defprim stateless-ejb (name &key daos methods)
  (:pretty () (list 'stateless-ejb (list :name name :daos (synth-all :pretty daos) :methods (synth-all :pretty methods))))
  (:req () (synth-all :req methods))
  (:name () (symb (synth :name entity) "-E-J-B"))
  (:ejb (package) (let ((bean-name (synth :name this)))
                    (java-unit bean-name
                               (java-package (symb package '|.ejb|))
                               (java-import '|javax.ejb| '|EJB| '|Stateless|)
                               (java-import '|javax.persistence| '|EntityManager| '|PersistenceContext|)
                               (java-import (symb package '|.jto|) '|*|)
                               (java-import (symb package '|.model|) '|*|)
                               (java-import '|java.util| '|List|)
                               (java-import '|java.util| '|Arrays|)
                               (java-with-annotations 
                                (list (java-annotation '|Stateless|))
                                (java-class bean-name 
                                            :public t
                                            ;; :interfaces (list (symb name "-BEAN"))
                                            ;; :constructor (java-constructor name nil)
                                            :fields (list (java-with-annotations (list (java-annotation '|PersistenceContext|))
                                                                                 (java-statement (java-pair 'entity-manager (java-type 'entity-manager) :private t)))
                                                          (mapcar (lambda (dao)
                                                                    (java-with-annotations (list (java-annotation '|Inject|))
                                                                                           (java-statement (java-pair (synth :name dao) (java-object-type (synth :name dao)) :private t))))
                                                                  daos))
                                            :methods (synth-all :implementation methods)))))))

(defprim ejb-method (name)
  (:pretty () (list 'ejb-method (list :name name)))
  )
