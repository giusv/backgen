(in-package :server)

(defparameter *ejbs* (make-hash-table))
(defmacro defejb (name &rest methods)
  `(progn (defparameter ,name (ejb ,@methods)) 
         (setf (gethash ',name *ejbs*) ,name)))

(defprim stateless-ejb (name &key daos methods)
  (:pretty () (list 'stateless-ejb (list :name name :methods (synth-all :pretty methods))))
  (:req () (synth-all :req methods))
  (:name () (symb (synth :name entity) "-E-J-B"))
  (:ejb (package) (java-unit (synth :name this)
                             (java-package (symb package '|.ejb|)) 
                             (java-with-annotations 
                              (list (java-annotation '|Stateless|))
                              (java-class (synth :name this)
                                          :public t 
                                          :fields 
                                          :methods (synth-all :implementation methods)
                                          )))))
