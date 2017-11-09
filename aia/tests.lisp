(in-package :backgen)

(deftest create-indicator ((id (integer-type))) 
  (tl-equal id (expr:const 1)) 
  (tl-let ((a (tl-http-get (url `(www % test % it / b / a ? q = a )))))
    a)
  (tl-let ((b (tl-http-body this indicator-format)))
    (tl-equal (tl-get :indicator-id b) (expr:const "200"))))



;; (pprint (synth :string (synth :doc (synth :java (synth :java-implementation create-indicator #'identity)))))
;; (pprint (synth :string (synth :doc (synth :java (synth :java-implementation (create-indicator (expr:const 1)) #'identity)))))

;; (deftest indicator-sequence
;;     (tl-with-db (tl-forall ...)
;;                 (tl-orelse (verify-indicator 1)
;;                            (tl-let ((id (create-indicator 1)))
;;                              (verify-indicator 1)))))
(defsuite indicator-suite 
    (mapcar #'create-indicator (mapcar #'expr:const (list 1 2 3 4))))
