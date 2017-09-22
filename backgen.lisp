(in-package :backgen)

(deformat parameter-format  "Formato JSON del valore di un parametro relativo a un indicatore"
    (jsobject 'parameter
              (jsprop :parameter-id "Identificativo univoco del parametro" (jsstring))
              (jsprop :name "Nome" (jsstring))
              (jsprop :value "Valore" (jsstring))))

(deformat indicator-format "Formato JSON di un indicatore dinamico"
    (jsobject 'parameter
              (jsprop :indicator-id "Identificativo univoco dell'indicatore" (jsstring))
              (jsprop :source "Codice sorgente" (jsstring))
              (jsprop :start-date "Data inizio" (jsstring))
              (jsprop :parameters "parametri " (jsarray parameter-format))))

(defent indicator-entity
    (entity 'indicator 
            :primary (attribute :indicator-id (string-type 20))
            :fields (list (attribute :name (string-type 20) :desc "Nome dell'indicatore")
                          (attribute :source-code (string-type 200) :desc "Codice sorgente scritto dall'utente")
                          (attribute :object-code (string-type 200) :desc "Codice oggetto prodotto dal compilatore")
                          (attribute :start-date (string-type 8) :desc "Data inizio validita"))))

(defent parameter-entity
    (entity 'parameter
            :primary (attribute :parameter-id (integer-type))
            :fields (list (attribute :name (string-type 20) :desc "Name del parametro")
                          (attribute :value (string-type 20) :desc "Valore del parametro"))))

(defrel indicator-parameters
    (relationship 'parameters-indicator indicator-entity parameter-entity :one-to-many))

(server:deferror bad-request "error")
(server:deferror parse-indicator-error "error" :parent bad-request)

(server:defresource indicator-item
    (server:rest-item 'indicator ((indicator (url:path-parameter 'indicator (integer-type)))) 
                      (list 
                       (server:rest-get ()
                                        (server:bl-let ((entity1 (server:bl-create-entity indicator-entity 
                                                                                          :indicator-id (expr:const 1)))
                                                        (entity2 (server:bl-let ((entity3 (server:bl-create-entity indicator-entity 
                                                                                                                   :indicator-id (expr:const 2)))
                                                                                 (entity4 entity3)) 
                                                                   entity4))
                                                        (name (server:bl-get :indicator-id entity2))
                                                        (name-test (server:bl-call (server:bl-lambda ((x (string-type 20))
                                                                                                      (y (string-type 20)))
                                                                                                     (server:bl-cat x (server:bl-call (server:bl-lambda ((x (string-type 20))
                                                                                                                                                         (y (string-type 20)))
                                                                                                                                                        (server:bl-cat x y))
                                                                                                                                      name name)))
                                                                                   name name))
                                                        (cond-test (server:bl-unless% (list (server:bl-condition entity1 parse-indicator-error)
                                                                                            (server:bl-condition entity2 parse-indicator-error))
                                                                                      name-test))) 
                                          (server:bl-unless% (list (server:bl-condition entity1 parse-indicator-error)
                                                                   (server:bl-condition entity2 parse-indicator-error))
                                                            cond-test))))))

(server:defservice server (server:rest-service 'indicator-service (url:void) indicator-item))

(defquery indicator-by-name (name) indicator-entity
          (with-queries ((inds (relation indicator-entity))
                         (pars (relation parameter-entity)))
            (project (restrict (product inds pars)
                               (expr:+and+ 
                                (expr:+equal+ (expr:attr inds 'id)
                                              (expr:attr pars 'id))
                                (expr:+equal+ (expr:attr inds 'name)
                                              name)))
                     'indicator-id 'name)))
(defquery all-indicators () indicator-entity
          (with-queries ((inds (relation indicator-entity)))
            inds))

;; (server:defresource indicators-collection
;;     (server:rest-collection 
;;      'indicators
;;      (list 
;;       (server:rest-get 
;;        () 
;;        (server:concat
;;         (indicator-list (server:exec-query (all-indicators)))
;;         (ret (server:mapcomm 
;;               (server:mu indicator
;;                          (server:with-fields ((name name)
;;                                               (code source-code)
;;                                               (start-date start-date)
;;                                               (parameters parameter-list)) indicator
;;                            (server:create-transfer indicator-format 
;;                                                    :name name
;;                                                    :source-code code
;;                                                    :start-date start-date
;;                                                    :parameters (server:mapcomm 
;;                                                                 (server:mu parameter
;;                                                                            (server:with-fields ((parameter-name name) (parameter-value value)) parameter
;;                                                                              (server:create-transfer parameter-format 
;;                                                                                                      :name parameter-name
;;                                                                                                      :value parameter-value)))
;;                                                                 parameters))))
;;               indicator-list))
;;         ((server:respond :ok ret))))
;;       (server:rest-post% indicator-format 
;;                          (server:concat
;;                           (ret (server:with-fields ((name name)
;;                                                     (code source-code)
;;                                                     (start-date start-date)) indicator-format
;;                                  (server:create-entity indicator-entity
;;                                                        :name name
;;                                                        :source-code code
;;                                                        :start-date start-date))) 
;;                           ((server:respond :created)))))
;;      indicator-item))

;; (defparameter parameters-collection
;;   (server:rest-collection 'parameters
;;                          (list 
;;                           (server:rest-put (jsarray 'parameter-array "aaa" parameter-format)
;;                                            (server:concat
;;                                             (server:mapcomm 
;;                                              (server:mu param
;;                                                         (server:with-fields ((name name)
;;                                                                              (value value)) param
;;                                                           (server:create-entity 
;;                                                                 parameter-entity
;;                                                                 :name name
;;                                                                 :value value)))
;;                                              trip-list)
;;                                             (ret (server:with-fields ((name name)
;;                                                                       (code source-code)
;;                                                                       (start-date start-date)) inst
;;                                                    (server:update-entity indicator-format 
;;                                                                          :name name
;;                                                                          :code code
;;                                                                          :start-date start-date))) 
;;                                             ((server:respond :no-content)))))
;;                          parameter-item))


;; (server:defservice server (server:rest-service 'indicator-service (url:void) indicators-collection))





;; (pprint (synth :pretty (generate-dao indicator-entity)))

(let* ((package '|it.bancaditalia.backgen|)
       ;; (basedir "D:/Dati/Profili/m026980/workspace/backgen/src/main/java/it/bancaditalia/backgen/")
       (basedir "D:/giusv/temp/backgen/")
       (app-entities (loop for value being the hash-values of *entities* collect value))
       (app-daos (mapcar #'generate-dao app-entities)) 
       (app-formats (loop for value being the hash-values of *formats* collect value))
       (app-services (loop for value being the hash-values of server:*services* collect value))
       (app-errors (loop for value being the hash-values of server:*errors* collect value))
       (app-ejbs (mapcar #'server:generate-ejb app-services))
       )
 
  ;; (process (mkstr basedir (string-downcase (synth :name app-module)) ".module.ts") app-module)
  ;; (process (mkstr basedir (string-downcase (synth :name app)) ".component.ts") app )
  ;; (mapcar (lambda (component) 
  ;;           (process (mkstr basedir (string-downcase (synth :name component)) ".component.ts") component))
  ;;         app-components)
  (mapcar (lambda (entity) 
            (let ((filename (mkstr basedir "model/" (upper-camel (synth :name entity)) ".java"))) 
              (pprint filename)
              (write-file filename
                          (synth :string (synth :doc (synth :java (synth :entity entity package)))))))
          app-entities)
  (mapcar (lambda (dao) 
            (let ((filename (mkstr basedir "dao/" (upper-camel (synth :name dao)) ".java"))) 
              (pprint filename)
              (write-file filename
                          (synth :string (synth :doc (synth :java (synth :dao dao package)))))))
          app-daos)
  (mapcar (lambda (ejb) 
            (let ((filename (mkstr basedir "ejb/" (upper-camel (synth :name ejb)) ".java"))) 
              (pprint filename)
              (write-file filename
                          (synth :string (synth :doc (synth :java (synth :ejb ejb package)))))))
          app-ejbs)
  (mapcar (lambda (error) 
            (let ((filename (mkstr basedir "error/" (upper-camel (synth :name error)) ".java"))) 
              (pprint filename)
              (write-file filename
                          (synth :string (synth :doc (synth :java (synth :implementation error package)))))))
          app-errors) 
  (mapcar (lambda (format) 
            (let ((filename (mkstr basedir "vo/" (upper-camel (symb (synth :name format) '|-V-O|)) ".java"))) 
              (pprint filename)
              (write-file filename
                          (synth :string (synth :doc (synth :req format)))
                          ;; (synth :string (synth :doc (synth :java (synth :req format))))
                          )))
          app-formats)
  
  (mapcar (lambda (service) 
            (let ((filename (mkstr basedir "service/" (upper-camel (synth :name service)) ".java"))) 
              (pprint filename)
              (write-file filename
                          (synth :string (synth :doc (synth :java (synth :implementation service package)))))))
          app-services)
  ;; (mapcar (lambda (service) 
  ;;           (let ((filename (mkstr basedir "ejb/" (upper-camel (symb (synth :name service) '|-Bean|)) ".java"))) 
  ;;             (pprint filename)
  ;;             (write-file filename
  ;;                         (synth :string (synth :doc (synth :java (synth :bean-class service package)))))))
  ;;         app-services)
  )


;; (let ((test (server:bl-let ((entity1 (server:bl-create-entity indicator-entity 
;;                                                               :indicator-id (expr:const 1)))
;;                             (entity2 (server:bl-let ((entity3 (server:bl-create-entity indicator-entity 
;;                                                                                        :indicator-id (expr:const 2)))
;;                                                      (entity4 entity3)) 
;;                                        entity4))
;;                             (name (server:bl-get :indicator-id entity2))
;;                             ;; (all-inds (server:bl-call (all-indicators)))
;;                             ;; (all-ind-ids (server:bl-map (mu (ind) (bl-get :id ind))
;;                             ;;                             all-inds))
;;                             (name-test (server:bl-call (server:bl-lambda ((x (string-type 20))
;;                                                                           (y (string-type 20)))
;;                                                                          (server:bl-cat x (server:bl-call (server:bl-lambda ((x (string-type 20))
;;                                                                                                                              (y (string-type 20)))
;;                                                                                                                             (server:bl-cat x y))
;;                                                                                                           name name)))
;;                                                        name name))) 
;;               name-test))
;;       ;; (test (server:bl-create-entity indicator-entity (list :indicator-id (expr:const 1))))
;;       )
;;   (pprint (synth :string (synth :doc (synth :java (synth :logic test (lambda (x) (java-return x))))))))


;; (defun to-string (x)
;;   (synth :string (synth :doc (synth :typescript x))))

;; (defun process (name code) 
;;   ;; (format t "~%~%~a~%--------------------------------------------------~%~%~a~%--------------------------------------------------~%" name (to-string code))
;;   (write-file name (to-string code)))


;; (pprint (synth-all :pretty (synth :source (car (:get-sources trip-entity)))))
;; (pprint (synth-all :pretty (:get-sources trip-entity)))

;; (pprint (synth :pretty trip-entity))
;; (pprint trip-entity)

;; (synth-all :output (synth-all :java (synth-all :entity 
;;                                                (loop for value being the hash-values of *entities* collect value))) 0)

;; (synth-all :output (synth-all :java (synth-all :eao-interface 
;;                                                (loop for value being the hash-values of *entities* collect value))) 0)
;; (synth :output (synth :java (synth :jax-class server)) 0)

;; (synth :output (synth :java (synth :bean-class server)) 0)
;; (synth :output  (apply #'doc:postpend (doc:semi) t
                           ;; (remove nil (synth-all :ddl (list trip-entity city-entity place-entity trip-city city-place)))) 0)

;; (synth-all :output (synth-all :java (synth-all :model (list trip-format city-format place-format) :server '|com.example.json|)) 0)

;; (defparameter place-format
;;   (jsobject 'place "aaa"
;;                  (jsprop 'name t (jsstring 'name "aaa"))))

;; (defparameter city-format
;;   (jsobject 'city "aaa"
;;                  (jsprop 'name t (jsstring 'name "aaa"))
;;                  (jsprop 'places t (jsarray 'places "aaa" place-format))))

;; (defparameter trip-format
;;   (jsobject 'trip "aaa"
;;                  (jsprop 'name t (jsstring 'name "aaa"))
;;                  (jsprop 'cities t (jsarray 'cities "aaa" city-format))))

;; (defparameter model-list (list place-format city-format trip-format))


;; (defparameter gui
;;   (gui:vert
;;    (gui:navbar 'nav 
;;                (gui:link 'home (expr:const "home") (url:void))
;;                (gui:link 'nested (expr:const "nested") (url:url `(nested)))
;;                (gui:link 'nested2 (expr:const "form") (url:url `(my-form)))
;;                (gui:link 'dynamic (expr:const "dynamic") (url:url `(nested / param)))) 
;;    (gui:alt 
;;     (gui:vert 
;;      (gui:panel 'panel-test 
;;                 (gui:label (expr:const "header2"))
;;                 (gui:label (expr:const "body2")))
;;      (gui:button 'test (doc:text "level 0 1")) 
;;      (with-data ((places (remote 'places place-format 
;;                                            (url:url `(home)))))
;;        (gui:table 'table places (row)
;;          :|Name| (gui:label (expr:attr row 'name))
;;          :|Value| (gui:label (expr:attr row 'value)))))
;;     (gui:static 'nested nil 
;;                 (gui:alt (gui:label (expr:const "nested"))
;;                          (gui:dynamic 'dyn (id) 
;;                                       (gui:label (expr:value id)))))
;;     (gui:static 'nested2 nil 
;;                 (gui:vert (gui:label (expr:const "nested 2"))
;;                           (with-data ((places 
;;                                             (rand 'places (jsarray 'places "aaa" place-format))))
;;                             (gui:table 'table places (row)
;;                               :|Name| (gui:label (expr:attr row 'name))
;;                               :|Value| (gui:label (expr:attr row 'value))
;;                               :|Description| (gui:description 'description row 
;;                                                :|Name| (expr:attr row 'name)
;;                                                :|Value| (expr:attr row 'value))
;;                               :|Details| (gui:button 'details (doc:text "Details"))
;;                               :|Panel| (gui:panel 'panel (gui:label (expr:attr row 'name)) 
;;                                                   (gui:label (expr:attr row 'value)))))))    
;;     (gui:static 'my-form nil
;;                 (gui:form 'trip-form trip-format
;;                           ((name name (gui:input 'name (expr:const "Trip name")))
;;                            (cities cities (gui:arr 'cities city-format 
;;                                                    ((city-name city-name (gui:input 'city-name (expr:const "City name"))) 
;;                                                     (places places (gui:arr 'places place-format
;;                                                                             ((place-name place-name (gui:input 'place-name (expr:const "Place name"))))
;;                                                                             place-name)))
;;                                                    (gui:vert city-name places))))
;;                           (gui:vert name cities))))))



;; (let* ((basedir "d:/giusv/angular/template/src/app/")
;;        (app-models (mapcar (lambda (format) (synth :model format)) 
;;                            model-list))
;;        (app-components (synth :components gui nil))
;;        (app-component-names (cons (java-static 'app-component)
;;                                   (mapcar (lambda (component)
;;                                             (java-static (symb (synth :name component) "-COMPONENT")))
;;                                           app-components)))
;;        (app (java-unit 'app
;;                      (java-import "@angular/core" 'component)
;;                      (java-import "@angular/forms" 'form-array 'form-builder 'form-group 'form-control)
;;                      (java-annotation 'component
;;                                    :selector (java-const (string-downcase 'app))
;;                                    :template (java-template (synth :template gui)))
;;                      (java-class 'app-component
;;                                :fields (list (synth :controller gui))))) 
;;        (app-module (java-unit 'app
;;                             (java-import "@angular/core" 'java-module)
;;                             (java-import "@angular/platform-browser" 'browser-module)
;;                             (java-import "@angular/http" 'http-module)
;;                             (java-import "@angular/forms" 'reactive-forms-module)
;;                             (java-import "@angular/router" 'router-module 'routes)
;;                             (java-import "./app.component" 'app-component) ;; FIXME
;;                             (mapcar (lambda (component)
;;                                       (java-import (mkstr "./" (string-downcase (synth :name component)) ".component") 
;;                                                  (symb (synth :name component) "-COMPONENT")))
;;                                     app-components)
;;                             (java-pair 'app-routes (java-type 'routes) :const t 
;;                                      :init (java-array (synth :routes gui nil)))
;;                             (java-annotation 'java-module
;;                                           :imports (java-array (java-static 'browser-module)
;;                                                              (java-static 'http-module)
;;                                                              (java-static 'reactive-forms-module)
;;                                                              (java-chain (java-static 'router-module) 
;;                                                                        (java-call 'for-root (java-dynamic 'app-routes))))
;;                                           :declarations (java-array app-component-names) 
;;                                           :bootstrap (java-array (java-static 'app-component)))
;;                             (java-class 'app-module)))
;;        (app-components (synth :components gui nil))) 
;;   (process (mkstr basedir (string-downcase (synth :name app-module)) ".module.ts") app-module)
;;   (process (mkstr basedir (string-downcase (synth :name app)) ".component.ts") app )
;;   (mapcar (lambda (component) 
;;             (process (mkstr basedir (string-downcase (synth :name component)) ".component.ts") component))
;;           app-components)
;;   (mapcar (lambda (model) 
;;             (process (mkstr basedir (string-downcase (synth :name model)) ".ts") model))
;;           app-models))



;; ;; ;; (pprint (synth :pretty (synth :random (jsarray 'places "aaa" place-format))))
;; ;; ;; (pprint (synth :pretty (synth :model (synth :random (jsarray 'places "aaa" place-format)))))
;; ;; ;; (defparameter gui 
;; ;; ;;   (gui:form 'hero-form nil 
;; ;; ;;             (gui:arr 'secrets nil 
;; ;; ;;                      (gui:obj 'secret nil 
;; ;; ;;                               ((secret 
;; ;; ;;                                 secret 
;; ;; ;;                                 (gui:input 'secret (expr:const "Secret Lair")))
;; ;; ;;                                ;; (accomplice 
;; ;; ;;                                ;;  accomplice
;; ;; ;;                                ;;  (gui:obj 'accomplice nil 
;; ;; ;;                                ;;           ((name name (gui:input 'name (expr:const "name"))))
;; ;; ;;                                ;;           name))
;; ;; ;;                                ;; (accomplices 
;; ;; ;;                                ;;  accomplices 
;; ;; ;;                                ;;  (gui:arr 'accomplices nil 
;; ;; ;;                                ;;           (gui:obj 'accomplice nil 
;; ;; ;;                                ;;                    ((name name (gui:input 'name (expr:const "name"))))
;; ;; ;;                                ;;                    name)))
;; ;; ;;                                )
;; ;; ;;                               (gui:vert secret ;; accomplices
;; ;; ;;                                         )))
;; ;; ;;             ;; (gui:obj 'comp-data nil 
;; ;; ;;             ;;                      ((name name (gui:input 'name (expr:const "Name")))
;; ;; ;;             ;;                       (address address (gui:input 'address (expr:const "Address")))
;; ;; ;;             ;;                       (secrets secrets 
;; ;; ;;             ;;                                (gui:arr 'secrets nil 
;; ;; ;;             ;;                                         (gui:obj 'secret nil 
;; ;; ;;             ;;                                                  ((secret 
;; ;; ;;             ;;                                                    secret 
;; ;; ;;             ;;                                                    (gui:input 'secret (expr:const "Secret Lair")))
;; ;; ;;             ;;                                                   ;; (accomplice 
;; ;; ;;             ;;                                                   ;;  accomplice
;; ;; ;;             ;;                                                   ;;  (gui:obj 'accomplice nil 
;; ;; ;;             ;;                                                   ;;           ((name name (gui:input 'name (expr:const "name"))))
;; ;; ;;             ;;                                                   ;;           name))
;; ;; ;;             ;;                                                   ;; (accomplices 
;; ;; ;;             ;;                                                   ;;  accomplices 
;; ;; ;;             ;;                                                   ;;  (gui:arr 'accomplices nil 
;; ;; ;;             ;;                                                   ;;           (gui:obj 'accomplice nil 
;; ;; ;;             ;;                                                   ;;                    ((name name (gui:input 'name (expr:const "name"))))
;; ;; ;;             ;;                                                   ;;                    name)))
;; ;; ;;             ;;                                                   )
;; ;; ;;             ;;                                                  (gui:vert secret ;; accomplices
;; ;; ;;             ;;                                                            )))))
;; ;; ;;             ;;                      (gui:vert name address secrets))
;; ;; ;;             ))
;; ;; (defparameter secret-format
;; ;;   (jsobject 'secret "aa"
;; ;;                  (jsprop 'secret t (jsstring 'secret "aaa"))))
;; ;; (defparameter hero-format 
;; ;;   (jsobject 'hero "aaa"
;; ;;                  (jsprop 'name t (jsstring 'name "aaa"))
;; ;;                  (jsprop 'address t (jsstring 'address "aaa"))
;; ;;                  (jsprop 'addresses t (jsarray 'addresses "aaa" secret-format))))
;; ;; (defparameter role-format
;; ;;   (jsobject 'ruolo "ddd"
;; ;;                  (jsprop 'nome t (jsstring 'ruolo "Ruolo assunto nel sinistro"))))

;; ;; (defparameter person-format 
;; ;;   (jsobject 'persona "Formato JSON dei dati relativi a una persona"
;; ;;                  (jsprop 'id-persona nil (jsstring 'id-persona "Identificativo univoco della persona")) 
;; ;;                  (jsprop 'nome t (jsstring 'nome "Nome")) 
;; ;;                  (jsprop 'cognome t (jsstring 'cognome "Cognome")) 
;; ;;                  (jsprop 'codice-fiscale nil (jsstring 'codice-fiscale "Codice fiscale")) 
;; ;;                  (jsprop 'partita-iva nil (jsstring 'partita-iva "Partita IVA")) 
;; ;;                  (jsprop 'luogo-nascita t (jsstring 'luogo-nascita "Luogo di nascita"))
;; ;;                  (jsprop 'data-nascita t (jsstring 'data-nascita "Data di nascita"))  
;; ;;                  (jsprop 'ruoli nil (jsarray 'ruoli "Lista di ruoli assunti nel sinistro" role-format))))

;; ;; (let ((json ;; (synth :random (jsarray 'test "aaa")))
;; ;;        (synth :random schema)))
;; ;;   (pprint (synth :pretty json))
;; ;;   (format t "~%~a" (synth :string (synth :string json))))







;; ;; (synth :output (synth :typescript (java-unit (java-import (java-const "@angular/core") 'component 'onInit)
;; ;;                                            (java-annotation 'component 
;; ;;                                                          :selector (java-const "my-heroes") 
;; ;;                                                          :template-url  (java-const "test") 
;; ;;                                                          :style-urls (java-array (java-const "test")))
;; ;;                                            (java-class 'hero-search 
;; ;;                                                      :fields (list (java-pair 'heroes0 'string :init (java-new 'heroes))
;; ;;                                                                    (java-pair 'heroes 'string :init (java-array (java-const "aaa")))
;; ;;                                                                    (java-pair 'heroes2 'string :init (java-call 'get (java-const "aaa")))
;; ;;                                                                    (java-pair 'heroes3 'string :init (java-chain (java-call 'get (java-const "aaa"))
;; ;;                                                                                                              (java-call 'set (java-const "aaa"))
;; ;;                                                                                                              (java-call 'set (java-const "bbb"))))
;; ;;                                                                    (java-pair 'heroes4 'string :init (java-call 'catch (java-arrow (list (java-pair 'e 'error)) (java-call 'test (java-const 'e)))) :const t))
;; ;;                                                      :constructor (java-constructor (list (java-pair 'heroes 'string)))
;; ;;                                                      :methods (list (java-method (text "on-init") 
;; ;;                                                                                (list (java-pair 'heroes 'string))
;; ;;                                                                                'void))))) 0)

;;  ;; (synth :output (nest 10 (java-const "~a" 24)) 0)
;; ;; (pprint (synth :output (synth :doc (html:div :class "a" (doc:java-const "SS"))) 0))
;; ;; (pprint (synth :pretty (html:div :class "a" (java-const "ss"))))
;; ;; (synth :output (synth :doc (synth :template (gui:input 'name (java-const "Name") :init (java-const "hello")))) 0)

;; ;; (pprint (synth :pretty (java-unit
;; ;;                         (java-import (java-const "@angular/core") 'component 'onInit)
;; ;;                         (java-annotation 'component 
;; ;;                                       :selector (java-const "my-heroes") 
;; ;;                                       :template-url  (java-const "test") 
;; ;;                                       :style-urls (java-array (java-const "test")))
;; ;;                         (java-class 'hero-search 
;; ;;                                   :fields (list (java-pair 'heroes0 'string :init (java-new 'heroes))
;; ;;                                                 (java-pair 'heroes 'string :init (java-array (java-const "aaa")))
;; ;;                                                 (java-pair 'heroes2 'string :init (java-call 'get (java-const "aaa")))
;; ;;                                                 (java-pair 'heroes3 'string :init (java-chain (java-call 'get (java-const "aaa"))
;; ;;                                                                                           (java-call 'set (java-const "aaa"))
;; ;;                                                                                           (java-call 'set (java-const "bbb"))))
;; ;;                                                 (java-pair 'heroes4 'string :init (java-call 'catch (java-arrow (list (java-pair 'e 'error)) (java-call 'test (java-const 'e)))) :const t))
;; ;;                                   :constructor (java-constructor (list (java-pair 'heroes 'string)))
;; ;;                                   :methods (list (java-method 'on-init 
;; ;;                                                             (list (java-pair 'heroes 'string))
;; ;;                                                             'void))))))

;; ;; (synth :typescript (java-pair 'e 'error))
;; ;; (synth :typescript (java-unit
;; ;;                     (java-import (text "@angular/core") 'component 'onInit)
;; ;;                     ;; (java-annotation 'component 
;; ;;                     ;;               :selector (text "my-heroes") 
;; ;;                     ;;               :template-url  (text "test") 
;; ;;                     ;;               :style-urls (java-array (text "test")))
;; ;;                     ;; (java-class 'hero-search 
;; ;;                     ;;           :fields (list (java-pair 'heroes0 'string :init (java-new 'heroes))
;; ;;                     ;;                         (java-pair 'heroes 'string :init (java-array (text "aaa")))
;; ;;                     ;;                         (java-pair 'heroes2 'string :init (java-call 'get (text "aaa")))
;; ;;                     ;;                         (java-pair 'heroes3 'string :init (java-chain (java-call 'get (text "aaa"))
;; ;;                     ;;                                                                   (java-call 'set (text "aaa"))
;; ;;                     ;;                                                                   (java-call 'set (text "bbb"))))
;; ;;                     ;;                         (java-pair 'heroes4 'string :init (java-call 'catch (java-arrow (list (java-pair 'e 'error)) (java-call 'test (text 'e)))) :const t))
;; ;;                     ;;           :constructor (java-constructor (list (java-pair 'heroes 'string)))
;; ;;                     ;;           :methods (list (java-method 'on-init 
;; ;;                     ;;                                     (list (java-pair 'heroes 'string))
;; ;;                     ;;                                     'void)))
;; ;;                     ))

;; ;; (pprint (synth :pretty (div)))

;; ;; (synth :output (html:div :class "a" (text "aaa")))
;; ;; (pprint (parse (many (atomic)) '(a b c &optional d)))


;; ;; (let ((l '(a b c &optional d1 (d2 0 d2-supplied-p) &rest e &key f (g 99 g-supplied-p))))
;; ;;   ;; (pprint (parse (lambda-list) l))
;; ;;   (pprint (arg-names l))
;; ;;   ;; (let ((args (parse (lambda-list) l)))
;; ;;   ;;   (pprint (apply #'append (mapcar (lambda (x) (getf args x))  (list :req :opt :rest :key))))
;; ;;   ;;   )
;; ;;   )
;; ;; (pprint (parse (ttt) (list 'a 'b)))
;; ;; (pprint (parse (ttt) '(a b)))
;; ;; (pprint (parse (var-init) '((a b))))


;; ;; (pprint (parse (var-init) '((name init))))
;; ;;; "backgen" goes here. Hacks and glory await!

