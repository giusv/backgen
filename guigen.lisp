(in-package :backgen)

(deformat place "Formato JSON di un luogo"
    (jsobject 'place
              (jsprop :name  "Identificativo univoco del luogo" (jsstring))))

(deformat city "Formato JSON di una citta'"
    (jsobject 'city
              (jsprop :name  "Identificativo univoco della citta'" (jsstring))
              (jsprop :places  "luoghi" (jsarray place))))

(deformat trip "Formato JSON di un viaggio"
    (jsobject 'trip
              (jsprop :name  "Identificativo univoco del viaggio" (jsstring))
              (jsprop :cities  "citta'" (jsarray city))))


(defelem gui
  (gui:vert
   (gui:navbar 'nav 
               (gui:link 'home (expr:const "home") (url:void))
               (gui:link 'nested (expr:const "nested") (url:url `(nested)))
               (gui:link 'nested2 (expr:const "form") (url:url `(my-form)))
               (gui:link 'dynamic (expr:const "dynamic") (url:url `(nested / param)))) 
   (gui:alt 
    (gui:vert 
     (gui:panel 'panel-test 
                (gui:label (expr:const "header2"))
                (gui:label (expr:const "body2")))
     (gui:button 'test (doc:text "level 0 1")) 
     (with-data ((places (remote 'places place 
                                 (url:url `(home)))))
       (gui:table 'table places (row)
         :|Name| (gui:label (expr:attr row 'name))
         :|Value| (gui:label (expr:attr row 'value)))))
    (gui:static 'nested nil 
                (gui:alt (gui:label (expr:const "nested"))
                         (gui:dynamic 'dyn (id (integer-type)) 
                                      (gui:label (expr:value id)))))
    (gui:static 'nested2 nil 
                (gui:vert (gui:label (expr:const "nested 2"))
                          (with-data ((places 
                                       (rand 'places (jsarray place))))
                            (gui:table 'table places (row)
                              :|Name| (gui:label (expr:attr row 'name))
                              :|Value| (gui:label (expr:attr row 'value))
                              :|Description| (gui:description 'description row 
                                               :|Name| (expr:attr row 'name)
                                               :|Value| (expr:attr row 'value))
                              :|Details| (gui:button 'details (doc:text "Details"))
                              :|Panel| (gui:panel 'panel (gui:label (expr:attr row 'name)) 
                                                  (gui:label (expr:attr row 'value)))))))    
    (gui:static 'my-form nil
                (gui:form 'trip-form trip
                          ((name name (gui:input 'name (expr:const "Trip name")))
                           (cities cities (gui:arr 'cities city 
                                                   ((city-name city-name (gui:input 'city-name (expr:const "City name"))) 
                                                    (places places (gui:arr 'places place
                                                                            ((place-name place-name (gui:input 'place-name (expr:const "Place name"))))
                                                                            place-name)))
                                                   (gui:vert city-name places))))
                          (gui:vert name cities))))))

(defun to-string (x)
  (synth :string (synth :doc (synth :typescript x))))

(defun process (name code) 
  ;; (format t "~%~%~a~%--------------------------------------------------~%~%~a~%--------------------------------------------------~%" name (to-string code))
  (write-file name (to-string code)))

(let* ((basedir "d:/giusv/angular/template/src/app/")
       (app-formats (loop for value being the hash-values of *formats* collect value))
       (app-models (synth-all :ts-implementation app-formats))
       (app-components (synth :components gui nil))
       (app-component-names (cons (ts-static 'app-component)
                                  (mapcar (lambda (component)
                                            (ts-static (symb (synth :name component) "-COMPONENT")))
                                          app-components)))
       (app (ts-unit 'app
                     (ts-import "@angular/core" 'component)
                     (ts-import "@angular/forms" 'form-array 'form-builder 'form-group 'form-control)
                     (ts-annotation 'component
                                   :selector (ts-const (string-downcase 'app))
                                   :template (ts-template (synth :template gui)))
                     (ts-class 'app-component
                               :fields (list (synth :controller gui))))) 
       (app-module (ts-unit 'app
                            (ts-import "@angular/core" 'ng-module)
                            (ts-import "@angular/platform-browser" 'browser-module)
                            (ts-import "@angular/http" 'http-module)
                            (ts-import "@angular/forms" 'reactive-forms-module)
                            (ts-import "@angular/router" 'router-module 'routes)
                            (ts-import "./app.component" 'app-component) ;; FIXME
                            (mapcar (lambda (component)
                                      (ts-import (mkstr "./" (string-downcase (synth :name component)) ".component") 
                                                 (symb (synth :name component) "-COMPONENT")))
                                    app-components)
                            (ts-pair 'app-routes (ts-object-type 'routes) :const t 
                                     :init (ts-array (synth :routes gui nil)))
                            (ts-annotation 'ng-module
                                          :imports (ts-array (ts-static 'browser-module)
                                                             (ts-static 'http-module)
                                                             (ts-static 'reactive-forms-module)
                                                             (ts-chain (ts-static 'router-module) 
                                                                       (ts-call 'for-root (ts-dynamic 'app-routes))))
                                          :declarations (ts-array app-component-names) 
                                          :bootstrap (ts-array (ts-static 'app-component)))
                            (ts-class 'app-module)))
       (app-components (synth :components gui nil))) 
  (process (mkstr basedir (string-downcase (synth :name app-module)) ".module.ts") app-module)
  (process (mkstr basedir (string-downcase (synth :name app)) ".component.ts") app )
  (mapcar (lambda (component) 
            (process (mkstr basedir (string-downcase (synth :name component)) ".component.ts") component))
          app-components)
  (mapcar (lambda (model) 
            (process (mkstr basedir (string-downcase (synth :name model)) ".ts") model))
          app-models)
  )
