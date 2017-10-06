(in-package :backgen)

(deformat parameter-format  "Formato JSON del valore di un parametro relativo a un indicatore"
    (jsobject 'parameter
              (jsprop :parameter-id "Identificativo univoco del parametro" (jsstring))
              (jsprop :name "Nome" (jsstring))
              (jsprop :value "Valore" (jsstring))))

(deformat indicator-format "Formato JSON di un indicatore dinamico"
    (jsobject 'indicator
              (jsprop :indicator-id "Identificativo univoco dell'indicatore" (jsstring))
              (jsprop :source "Codice sorgente" (jsstring))
              (jsprop :start-date "Data inizio" (jsstring))
              (jsprop :parameters "parametri " (jsarray parameter-format))))

(defent dwh-indicatori
    (entity 'dwh-indicatori 
            :primary (attribute :id (string-type 20))
            :fields (list (attribute :name (string-type 20) :desc "Nome dell'indicatore")
                          (attribute :source-code (string-type 200) :desc "Codice sorgente scritto dall'utente")
                          (attribute :object-code (string-type 200) :desc "Codice oggetto prodotto dal compilatore")
                          (attribute :start-date (string-type 8) :desc "Data inizio validita"))))

(defent parameter-entity
    (entity 'parameter
            :primary (attribute :parameter-id (integer-type))
            :fields (list (attribute :name (string-type 20) :desc "Name del parametro")
                          (attribute :value (string-type 20) :desc "Valore del parametro"))))

;; (defent ana-soggetto
;;     (entity 'ana-soggetto
;;             :primary (attribute :uid (integer-type))
;;             :fields nil))

;; (defent ana-veicolo
;;     (entity 'ana-veicolo
;;             :primary (attribute :uid (integer-type))
;;             :fields nil))

;; (defent sin-sinistro
;;     (entity 'sin-sinistro
;;             :primary (attribute :id-sini (integer-type))
;;             :fields nil))

;; (defent dwh-score-sogg
;;     (entity 'dwh-score-sogg
;;             :primary (attribute :score-id (integer-type))
;;             :fields (list (attribute :score (integer-type) :nullable nil) 
;;                           (attribute :qscore (integer-type) :nullable nil) 
;;                           (attribute :last-update (date-type) :nullable nil))))
;; (defent dwh-score-trg-veic
;;     (entity 'dwh-score-trg-veic
;;             :primary (attribute :score-id (integer-type))
;;             :fields (list (attribute :score (integer-type) :nullable nil)
;;                           (attribute :qscore (integer-type) :nullable nil)
;;                           (attribute :last-update (date-type) :nullable nil))))

;; (defent dwh-score-sini
;;     (entity 'dwh-score-sini
;;             :primary (attribute :score-id (integer-type))
;;             :fields (list (attribute :score (integer-type) :nullable nil)
;;                           (attribute :qscore (integer-type) :nullable nil)
;;                           (attribute :last-update (date-type) :nullable nil))))

;; (defent dwh-ind-risc-sogg
;;     (entity 'dwh-ind-risc-sogg
;;             :primary (attribute :ind-value-id (integer-type))
;;             :fields (list ;; (attribute :ind-id (integer-type) :nullable nil)
;;                           (attribute :value(string-type 1) :nullable nil)
;;                           ;; (attribute :uid (integer-type) :nullable nil)
;;                           ;; (attribute :id-sini (integer-type) :nullable nil)
;;                           (attribute :last-update (date-type) :nullable nil))))
;; (defent dwh-ind-risc-trg-veic
;;     (entity 'dwh-ind-risc-trg-veic
;;             :primary (attribute :ind-value-id (integer-type))
;;             :fields (list (attribute :value (string-type 1) :nullable nil) 
;;                           (attribute :last-update (date-type) :nullable nil))))

;; (defent dwh-ind-risc-sini
;;     (entity 'dwh-ind-risc-sini
;;             :primary (attribute :ind-value-id (integer-type))
;;             :fields (list (attribute :value (string-type 1) :nullable nil) 
;;                           (attribute :last-update (date-type) :nullable nil))))

;; (defrel ind-risc-trg-veic-indicators
;;     (relationship 'dwh-ind-risc-trg-veic-indicators dwh-ind-risc-trg-veic dwh-indicatori :many-to-one))

;; (defrel dwh-ind-risc-trg-veic-ana-veicolo
;;     (relationship 'dwh-ind-risc-trg-veic-ana-veicolo dwh-ind-risc-trg-veic ana-veicolo :many-to-one))

;; (defrel dwh-ind-risc-trg-veic-sin-sinistro
;;     (relationship 'dwh-ind-risc-sogg-sin-sinistro dwh-ind-risc-trg-veic sin-sinistro :many-to-one))

;; (defrel dwh-ind-risc-sogg-indicators
;;     (relationship 'dwh-ind-risc-sogg-indicators dwh-ind-risc-sogg dwh-indicatori :many-to-one))

;; (defrel dwh-ind-risc-sogg-ana-soggetto
;;     (relationship 'dwh-ind-risc-sogg-ana-soggetto dwh-ind-risc-sogg ana-soggetto :many-to-one))

;; (defrel dwh-ind-risc-sogg-sin-sinistro
;;     (relationship 'dwh-ind-risc-sogg-sin-sinistro dwh-ind-risc-sogg sin-sinistro :many-to-one))

(defrel indicator-parameters
    (relationship 'parameters-indicator dwh-indicatori parameter-entity :one-to-many))

;; (defrel dwh-ind-risc-sini-sin-sinistro
;;     (relationship 'dwh-ind-risc-sini-sin-sinistro dwh-ind-risc-sini sin-sinistro :many-to-one))

;; (defrel dwh-score-trg-veic-ana-veicolo
;;     (relationship 'dwh-score-trg-veic-ana-veicolo dwh-score-trg-veic ana-veicolo :many-to-one))

;; (defrel dwh-score-trg-veic-sin-sinistro
;;     (relationship 'dwh-score-sogg-sin-sinistro dwh-score-trg-veic sin-sinistro :many-to-one))

;; (defrel dwh-score-sogg-ana-soggetto
;;     (relationship 'dwh-score-sogg-ana-soggetto dwh-score-sogg ana-soggetto :many-to-one))

;; (defrel dwh-score-sogg-sin-sinistro
;;     (relationship 'dwh-score-sogg-sin-sinistro dwh-score-sogg sin-sinistro :many-to-one))

;; (defrel dwh-score-sini-sin-sinistro
;;     (relationship 'dwh-score-sini-sin-sinistro dwh-score-sini sin-sinistro :many-to-one))

(server:deferror parse-indicator-exception (server:bl-bad-request-exception))
(server:deferror indicator-not-found-exception (server:bl-bad-request-exception))

(server:defresource indicator-item
    (server:rest-item 'indicator ((indicator (url:path-parameter 'indicator (integer-type)))) 
                      (list 
                       (server:rest-get ()
                                        (server:bl-let ((ent (server:bl-find-entity dwh-indicatori indicator)))
                                          ent)))))


(defquery indicator-by-name ((name (string-type 20))) dwh-indicatori
          (with-queries ((inds (relation dwh-indicatori))
                         (pars (relation parameter-entity)))
            (project (restrict (product inds pars)
                               (expr:+and+ 
                                (expr:+equal+ (expr:attr inds 'id)
                                              (expr:attr pars 'id))
                                (expr:+equal+ (expr:attr inds 'name)
                                              name)))
                     :id :name)))

(defquery all-indicators () dwh-indicatori
          (with-queries ((inds (relation dwh-indicatori)))
            inds))
(server:defresource indicators-collection
    (server:rest-collection 
     'indicators
     (list 
      (server:rest-post% indicator-format 
                         (server:bl-let ((source (server:bl-get :source indicator-format))
                                         (entity (server:bl-create-entity dwh-indicatori 
                                                                          :source-code source))) 
                           ;; (server:bl-value-object entity)
                           entity))
      (server:rest-get ((name (url:query-parameter 'name (string-type 20))))
                       (server:bl-let ((ent (server:bl-exec-query ;; (all-indicators)
                                                                  (indicator-by-name name)
                                                                  )))
                         (server:bl-unless (((server:bl-null ent) (indicator-not-found-exception nil)))
                           ent))))
     indicator-item))
(server:defservice server (server:rest-service 'indicator-service (url:void) indicators-collection))


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






;; (pprint (synth :pretty (generate-dao dwh-indicatori)))

(let* ((package (list "com" "extent" "backgen"))
       (package-symb (apply #'symb (interleave package ".")))
       ;; (basedir "D:/Dati/Profili/m026980/workspace/backgen/src/main/java/it/bancaditalia/backgen/")
       ;; (basedir "D:/giusv/temp/backgen/")
       (basedir (merge-pathnames (make-pathname :directory (cons :relative package))  #p"D:/Dati/Profili/m026980/workspace/backgen/src/main/java/")) 
       (app-entities (loop for value being the hash-values of *entities* collect value))
       (app-daos (mapcar #'generate-dao app-entities)) 
       (app-dtos (mapcar #'dto app-entities)) 
       (app-formats (loop for value being the hash-values of *formats* collect value))
       (app-services (loop for value being the hash-values of server:*services* collect value))
       (app-exceptions (loop for value being the hash-values of server:*errors* collect value))
       (app-ejbs (mapcar #'server:generate-ejb app-services)))
 
  ;; (process (mkstr basedir (string-downcase (synth :name app-module)) ".module.ts") app-module)
  ;; (process (mkstr basedir (string-downcase (synth :name app)) ".component.ts") app )
  ;; (mapcar (lambda (component) 
  ;;           (process (mkstr basedir (string-downcase (synth :name component)) ".component.ts") component))
  ;;         app-components)
  ;; (pprint (synth-all :pretty app-ejbs))
  (pprint basedir)
  (synth :output  (apply #'doc:postpend (doc:semi) t
                         (remove nil (synth-all :ddl app-entities))) 0)
  (let ((filename "D:/giusv/temp/doc/test.tex")) 
              (pprint filename)
              (write-file filename
                          (synth :string (document 'title 'author
                         (section 'section
                                  (paragraph (normal "hello ")
                                             (normal "world!"))
                                  (tabular (row (normal "name") (normal "surname") (normal "address"))
                                         (row (normal "a") (normal "b") (normal "b"))
                                         (row (normal "c") (normal "d")))
                                  (itemize (normal "a")
                                           (normal "b"))
                                  (outline (a . (normal "a"))
                                               (b . (normal "b"))))))))
  (mapcar (lambda (entity) 
            (let ((filename (mkstr basedir "model/" (upper-camel (synth :name entity)) ".java"))) 
              (pprint filename)
              (write-file filename
                          (synth :string (synth :doc (synth :java (synth :entity entity package-symb)))))))
          app-entities)
  (mapcar (lambda (dao) 
            (let ((filename (mkstr basedir "dao/" (upper-camel (synth :name dao)) ".java"))) 
              (pprint filename)
              (write-file filename
                          (synth :string (synth :doc (synth :java (synth :dao dao package-symb)))))))
          app-daos)
  (mapcar (lambda (dto) 
            (let ((filename (mkstr basedir "dto/" (upper-camel (synth :name dto)) ".java"))) 
              (pprint filename)
              (write-file filename
                          (synth :string (synth :doc (synth :java (synth :dto dto package-symb)))))))
          app-dtos)
  (mapcar (lambda (ejb) 
            (let ((filename (mkstr basedir "ejb/" (upper-camel (synth :name ejb)) ".java"))) 
              (pprint filename)
              (write-file filename
                          (synth :string (synth :doc (synth :java (synth :ejb ejb package-symb)))))))
          app-ejbs)
  (mapcar (lambda (error) 
            (let ((filename (mkstr basedir "exception/" (upper-camel (synth :name error)) ".java"))) 
              (pprint filename)
              (write-file filename
                          (synth :string (synth :doc (synth :java (synth :java-implementation error package-symb)))))))
          app-exceptions) 
  (mapcar (lambda (format) 
            (let ((filename (mkstr basedir "vo/" (upper-camel (symb (synth :name (synth :format format)) '|-V-O|)) ".java"))) 
              (pprint filename)
              (write-file filename
                          (synth :string (synth :doc (synth :java (synth :java-implementation format package-symb))))
                          ;; (synth :string (synth :doc (synth :java (synth :req format))))
                          )))
          app-formats)
  (mapcar (lambda (service) 
            (let ((filename (mkstr basedir "service/" (upper-camel (synth :name service)) ".java"))) 
              (pprint filename)
              (write-file filename
                          (synth :string (synth :doc (synth :java (synth :java-implementation service package-symb)))))))
          app-services)
  )


;; (let ((test (server:bl-let ((entity1 (server:bl-create-entity dwh-indicatori 
;;                                                               :indicator-id (expr:const 1)))
;;                             (entity2 (server:bl-let ((entity3 (server:bl-create-entity dwh-indicatori 
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
;;       ;; (test (server:bl-create-entity dwh-indicatori (list :indicator-id (expr:const 1))))
;;       )
;;   (pprint (synth :string (synth :doc (synth :java (synth :logic test (lambda (x) (java-return x))))))))




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

