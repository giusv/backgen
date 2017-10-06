(in-package :gui)
(defprim navbar (name &rest links)
  (:pretty () (list 'navbar (list :name name :links (synth-all :pretty links))))
  (:req (path) (seq (normal "Barra di navigazione ")
                          (if (not  links) (normal "vuota") (normal "composta dai seguenti link:"))
                          (apply #'itemize (synth-all :req links path))))
  (:brief (path) (synth :req this path))

  (:reqlist (*) nil)
  (:template () (html:nav 
                 :|class| "navbar navbar-default"
                 (html:div
                  :|class| "container-fluid"
                  (html:ul
                   :|class| "nav navbar-nav"
                   (mapcar (lambda (link)
                             (html:li (synth :template link)))
                           links)))))

  (:controller () (ts-empty))
  (:components (*) nil)
  (:routes (father) nil)
  (:ts-imports () nil)
  (:dependencies () nil))

