(in-package :gui)

(defprim static% (name queries element)
  (:pretty () (list 'static (list :name name :queries queries :element (synth :pretty element))))
  (:brief (path namelist) (let ((newpath (url:backward-chain (url:chunk name) path)))
                            (bold 
                             (seq (normal "~a (URL: " (upper-camel name #\Space)) 
                                  (verbatim (doc (synth :url newpath)))
                                  (doc:text ")")))))

  (:req (path namelist) (let ((newpath (url:backward-chain (url:chunk name) path)))
                          (section 
                           (seq (normal "~a (URL: " (upper-camel name #\Space)) 
                                (html:code :id (synth :string (synth :url newpath) 0) 
                                           (synth :url newpath))
                                (doc:text ")"))
                           (synth :req element newpath (append* namelist name)))))
  (:reqlist (path namelist) 
            (let ((newpath (url:backward-chain (url:chunk name) path))) 
              (cons (synth :req this path namelist)
                    (synth :reqlist element newpath (append* namelist name)))))
  (:template () (html:tag name (doc:empty)))
  (:controller () (ts-empty)) 
  (:components (father) 
               (let ((unit-name (if father 
                                    (symb father "-" name)
                                    name))) 
                 ;; (pprint (synth :pretty (synth :template element)))
                 (cons (ts-unit unit-name
                                (ts-import "@angular/core" 'component)
                                (synth :ts-imports this)
                                (ts-annotation 'component
                                               :selector (ts-const (string-downcase name))
                                               :template (ts-template (synth :template element)))
                                (ts-class (mkstr unit-name "-component")
                                          :constructor (ts-constructor (synth :dependencies this))
                                          :fields (list (synth :controller element))))
                       (synth :components element name))))
  (:routes (father) 
           (list (ts-object :path (ts-const (string-downcase name))
                            :component (ts-static (mkstr father "-" name "-component"))
                            (aif (synth :routes element name)
                                 (list :children (ts-array it))))))
  (:ts-imports () (synth :ts-imports element))
  (:dependencies () (synth :dependencies element)))

(defmacro static (name (&rest queries) element)
  `(let ,(mapcar #'(lambda (query) 
                     `(,query (url:query-parameter ',query)))
                 queries)
     (static% ,name (list ,queries) ,element)))
