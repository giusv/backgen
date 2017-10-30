(in-package :backgen)

;; (load #p"D:/giusv/lisp/backgen/aia/aia.lisp")
;; (load #p"D:/giusv/lisp/backgen/app/app.lisp")

;; (pprint (synth :pretty (generate-dao dwh-indicatori)))

(let* ((group-id (list "com" "extent"))
       (artifact-id "app")
       (basedir #p"D:/Dati/Profili/m026980/workspace/") 

       (package (append* group-id artifact-id))
       (package-symb (apply #'symb (interleave package ".")))
       (project-basedir (merge-pathnames (make-pathname :directory (list :relative artifact-id)) basedir))
       (main-basedir (merge-pathnames (make-pathname :directory (list :relative "src" "main")) project-basedir)) 
       (java-basedir (merge-pathnames (make-pathname :directory (apply #'list :relative "java" package)) main-basedir)) 
       (resources-basedir (merge-pathnames (make-pathname :directory (list :relative "resources")) main-basedir)) 
       (webapp-basedir (merge-pathnames (make-pathname :directory (list :relative "webapp")) main-basedir)) 
       (webinf-basedir (merge-pathnames (make-pathname :directory (list :relative "WEB-INF")) webapp-basedir)) 
       (metainf-basedir (merge-pathnames (make-pathname :directory (list :relative "META-INF")) resources-basedir)) 
       (app-entities (loop for value being the hash-values of *entities* collect value))
       (app-daos (mapcar #'generate-dao app-entities)) 
       (app-dtos (mapcar #'dto app-entities)) 
       (app-formats (loop for value being the hash-values of *formats* collect value))
       (app-services (loop for value being the hash-values of server:*services* collect value))
       (app-exceptions (loop for value being the hash-values of server:*errors* collect value))
       (app-ejbs (mapcar #'server:generate-ejb app-services))
       (app-ddls (apply #'doc:postpend (doc:semi) t (remove nil (synth-all :ddl app-entities))))
       (app-db *database*))
  (pprint java-basedir)
  (let ((filename (mkstr metainf-basedir "persistence.xml")))
    (pprint filename)
    (write-file filename
                (synth :string (synth :doc (persistence nil)))))
  (let ((filename (mkstr project-basedir "pom.xml"))) 
    (pprint filename)
    (write-file filename
                (synth :string (synth :doc (pom group-id artifact-id)))))
  (let ((filename (mkstr webinf-basedir "web.xml"))) 
    (pprint filename)
    (write-file filename
                (synth :string (synth :doc (web group-id artifact-id app-services)))))
  (let ((filename (mkstr webinf-basedir "beans.xml"))) 
    (pprint filename)
    (write-file filename
                (synth :string (synth :doc (beans)))))
  (mapcar (lambda (entity) 
            (let ((filename (mkstr java-basedir "model/" (upper-camel (synth :name entity)) ".java"))) 
              (pprint filename)
              (write-file filename
                          (synth :string (synth :doc (synth :java (synth :entity entity package-symb)))))))
          app-entities)
  (mapcar (lambda (dao) 
            (let ((filename (mkstr java-basedir "dao/" (upper-camel (synth :name dao)) ".java"))) 
              (pprint filename)
              (write-file filename
                          (synth :string (synth :doc (synth :java (synth :dao dao package-symb)))))))
          app-daos)
  (mapcar (lambda (dto) 
            (let ((filename (mkstr java-basedir "dto/" (upper-camel (synth :name dto)) ".java"))) 
              (pprint filename)
              (write-file filename
                          (synth :string (synth :doc (synth :java (synth :dto dto package-symb)))))))
          app-dtos)
  (mapcar (lambda (ejb) 
            (let ((filename (mkstr java-basedir "ejb/" (upper-camel (synth :name ejb)) ".java"))) 
              (pprint filename)
              (write-file filename
                          (synth :string (synth :doc (synth :java (synth :ejb ejb package-symb)))))))
          app-ejbs)
  (mapcar (lambda (error) 
            (let ((filename (mkstr java-basedir "exception/" (upper-camel (synth :name error)) ".java"))) 
              (pprint filename)
              (write-file filename
                          (synth :string (synth :doc (synth :java (synth :java-implementation error package-symb)))))))
          app-exceptions) 
  (mapcar (lambda (format) 
            (let ((filename (mkstr java-basedir "vo/" (upper-camel (symb (synth :name (synth :format format)) '|-V-O|)) ".java"))) 
              (pprint filename)
              (write-file filename
                          (synth :string (synth :doc (synth :java (synth :java-implementation format package-symb))))
                          ;; (synth :string (synth :doc (synth :java (synth :req format))))
                          )))
          app-formats)
  ;; (let ((filename (mkstr metainf-basedir "application.java")))
  ;;   (pprint filename)
  ;;   (write-file filename
  ;;               (synth :string (synth :doc (rest-application app-services)))))
  (mapcar (lambda (service) 
            (let ((filename (mkstr java-basedir "service/" (upper-camel (synth :name service)) ".java"))) 
              (pprint filename)
              (write-file filename
                          (synth :string (synth :doc (synth :java (synth :java-implementation service package-symb)))))))
          app-services)
  (let ((filename (mkstr resources-basedir "create.sql"))) 
    (pprint filename)
    (write-file filename
                (synth :string 
                       (vcat (synth :sql (sql-create-sequence 'hibernate-sequence 101 1)) 
                             app-ddls)))
    (let ((filename (mkstr resources-basedir "data.sql"))) 
      (pprint filename)
      (write-file filename
                  (synth :string (synth :sql (synth :sql-implementation app-db)))))))


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


