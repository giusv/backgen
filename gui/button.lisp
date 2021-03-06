(in-package :gui)

(defmacro evnames (&rest actions)
  `(append ,@(mapcar #'(lambda (action) 
                         `(if ,action (list ,(keyw "(" action ")") (concatenate 'string (lower-camel ',action) (upper-camel name) "()"))))
                     actions)))


(defprim button (name expr &key click)
  (:pretty () (list 'button (list :name name 
                                  :expr (synth :pretty expr)
                                  :click (synth :pretty click))))
  (:req (path namelist) (seq 
                         (get-documentation (append* namelist name))
                         (normal "Pulsante identificato come ~a" (string-downcase name))
                         (normal " e etichettato con la seguente espressione:") 
                         (doc expr)))
  (:brief (path namelist) (synth :req this path namelist))
  (:reqlist (path namelist) nil) 
  (:template () (html:button :|(click)| (doc:text "~aClick()" (lower-camel name))
                             expr))

  (:controller () (typescript:ts-method (doc:text "~aClick" (lower-camel name)) 
                                        nil
                                        (ts-primitive-type 'void)))
  (:components (*) nil)
  (:routes (father) nil)
  (:ts-imports () nil)
  (:dependencies () nil))
 

