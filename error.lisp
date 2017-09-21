(in-package :server)

(defprim bl-error (name &key message parent)
  (:pretty () (list 'bl-error (list :name name :message message :parent (synth :pretty parent))))
  (:implementation (java-)))
