(in-package :gui)
(defprim link (name expr target)
  (:pretty () (list 'link (list :name name 
                                :expr (synth :pretty expr) 
                                :target (synth :pretty target))))
  (:req (*) (paragraph 
             (normal "Link identificato come ~a"
                     (lower-camel name))
             (normal " e etichettato con la seguente espressione:") 
             (synth :req expr)))
  (:brief (path) (synth :req this path)) 
  (:reqlist (*) nil)
  (:template () (html:a 
                 :|routerLink| (synth :url target)
                 :|routerLinkActive| "active"
                 (synth :template expr)))

  (:controller () (ts-empty))
  (:components (*) nil)
  (:routes (*) nil)
  (:ts-imports () nil)
  (:dependencies () nil))


