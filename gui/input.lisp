(in-package :gui)

(defprim input (name label &key init)
  (:pretty () (list 'input (list :name name 
                                 :label (synth :pretty label)
                                 :init (synth :pretty init))))
  (:req (path namelist) (paragraph (normal "Campo di input identificato come ~a"
                                           (string-downcase name))
                                   (normal " etichettato con ")
                                   (synth :req label)))
  (:brief (path namelist) (synth :req this path namelist))
  (:reqlist (path namelist) nil) 
  (:form-template (loopvar indexes) (html:div :|class| "form-group" 
                                              (html:label :|class| "center-block"
                                                          (synth :template label)
                                                          (html:input ;; :|type| "text"
                                                           :|class| "form-control" 
                                                           :|formControlName| (lower-camel name)))))
  (:template () (html:div :|class| "form-group" 
                          (html:input 
                           :|type| "text"
                           :|id| (string-downcase name) 
                           :|placeholder| (if init (synth :doc init) ""))))

  (:controller () (ts-empty))
  (:form-controller (path) (ts-empty))
  (:components (*) nil)
  (:routes (*) nil)
  (:form () (ts-new 'form-control (ts-const "")))
  (:ts-imports () nil)
  (:dependencies () nil))

