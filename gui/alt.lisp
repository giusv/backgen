(in-package :gui)
(defprim alt (default &rest elements)
  (:pretty () (list 'alt (:default (synth :pretty default) :elements (synth-all :pretty elements))))
  (:req (path namelist)
        (seq
         (normal "Tale elemento mostra, a seconda dell'URL, una alternativa della lista")
         (if (not elements) (normal " vuota") (normal " seguente:"))
         (itemize (if default (synth :req default path namelist)) 
                  (synth-all :brief elements path namelist))))
  (:brief (path namelist) (synth :req this path namelist))
  (:reqlist (path namelist) (apply #'append (synth-all :req (cons default elements) path namelist)))
  (:template () (html:tag 'router-outlet
                          (doc:empty))) 
  (:controller () (ts-empty))
  (:components (father) (apply #'append
                               (let ((unit-name (if father (symb father "-DEFAULT")
                                                    (symb "DEFAULT"))))
                                 ;; (pprint (synth :pretty default))
                                 (list (ts-unit unit-name
                                                (ts-import "@angular/core" 'component)
                                                ;; (ts-import "@angular/http" 'http 'response)
                                                ;; (ts-import "rxjs/Rx" 'observable)
                                                ;; (ts-import "rxjs/add/operator/map")
                                                ;; (ts-import "rxjs/add/operator/catch")
                                                (synth :ts-imports default)
                                                (ts-annotation 'component
                                                               :selector (ts-const (string-downcase unit-name))
                                                               :template (ts-template (synth :template default)))
                                                (ts-class (symb unit-name "-COMPONENT")
                                                          :constructor (ts-constructor (synth :dependencies this))
                                                          :fields (list (synth :controller default))))))
                               (synth-all :components elements father)))
  (:routes (father) (cons (ts-object :path (ts-const "")
                                     :component (ts-static (mkstr father "-default-component")))
                          (apply #'append (synth-all :routes elements father))))
  (:ts-imports () (apply #'append (synth :ts-imports default) (synth-all :ts-imports elements)))
  (:dependencies () (apply #'append (synth :dependencies default) (synth-all :dependencies elements))))
