(in-package :validator)

(defprim required () 
  (:pretty () (list 'required))
  (:annotation () (java-annotation '|NotNull|)))
