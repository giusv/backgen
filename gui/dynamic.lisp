(in-package :gui)

(defprim dynamic% (name param element)
  (:pretty () (list 'dynamic (list :name name :param param :element (synth :pretty element))))
  ;; (:brief (path) (let ((newpath (backward-chain (dynamic-chunk name) path)))
  ;;                  (html:strong 
  ;;                   (doc:text "~a (URL: " (upper-camel name #\Space)) 
  ;;                   (html:a :href (doc:text "#~a" (synth :string (synth :url newpath) 0))
  ;;                           (html:code (synth :url newpath)))
  ;;                   (doc:text ")"))))

  ;; (:req (path) (let ((newpath (backward-chain (dynamic-chunk name) path)))
  ;;                (html:taglist 
  ;;                 (html:section 
  ;;                  (html:h4 nil (doc:text "~a (URL: " (upper-camel name #\Space)) 
  ;;                           (html:code :id (synth :string (synth :url newpath) 0) 
  ;;                                      (synth :url newpath))
  ;;                           (doc:text ")"))
  ;;                  (synth :req element newpath)))))
  ;; (:reqlist (path) 
  ;;           (let ((newpath (backward-chain (dynamic-chunk name) path))) 
  ;;             (cons (synth :req this path)
  ;;                   (synth :reqlist element newpath))))
  (:template () (html:tag name (doc:empty)))
  (:controller () (ts-empty)) 
  (:components (father) 
               (let ((unit-name (if father 
                                    (symb father "-" name)
                                    name))) 
                 ;; (pprint (synth :pretty (synth :template element)))
                 (cons (ts-unit unit-name
                                (ts-import "@angular/core" 'component 'on-init)
                                (ts-import "@angular/router" 'router 'activated-route 'params)
                                (synth :ts-imports this)
                                (ts-annotation 'component
                                              :selector (ts-const (string-downcase name))
                                              :template (ts-template (synth :template element)))
                                (ts-class (mkstr unit-name "-component")
                                          :interfaces (list 'on-init)
                                          :constructor (ts-constructor 
                                                        (apply #'list 
                                                               (ts-pair 'route (ts-object-type 'activated-route) :private t)
                                                               (ts-pair 'router (ts-object-type 'router) :private t) 
                                                               (synth :dependencies this)))
                                          :fields (list 
                                                   (ts-pair param (ts-primitive-type 'any))
                                                   (synth :controller element))
                                          :methods (list (ts-method (doc:text "~a" (lower-camel 'ng-on-init))
                                                                    nil (ts-primitive-type 'void)
                                                                    (ts-chain (ts-dynamic 'this)
                                                                              (ts-dynamic 'route)
                                                                              (ts-dynamic 'params)
                                                                              (ts-call 'subscribe 
                                                                                       (ts-arrow (list (ts-pair 'params (ts-primitive-type 'any)))
                                                                                                 (ts-assign (ts-chain (ts-dynamic 'this) 
                                                                                                                      (ts-dynamic param))
                                                                                                            (ts-element 'params (ts-const "id"))))))))))
                       (synth :components element name))))
  (:routes (father) 
           (list (ts-object :path (ts-const (string-downcase (mkstr ":" param)))
                            :component (ts-static (mkstr father "-" name "-component"))
                            (aif (synth :routes element name)
                                 (list :children (ts-array it))))))
  (:ts-imports () (synth :ts-imports element))
  (:dependencies () (synth :dependencies element)))

(defmacro dynamic (name (param type) element)
  `(let ((,param (url:path-parameter ',param ,type)))
     (dynamic% ,name ',param ,element)))
