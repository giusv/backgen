(in-package :backgen)

;; (deformat parameter-format  "Formato JSON del valore di un parametro relativo a un indicatore"
;;     (jsobject 'parameter
;;               (jsprop :parameter-id "Identificativo univoco del parametro" (jsstring))
;;               (jsprop :name "Nome" (jsstring))
;;               (jsprop :value "Valore" (jsstring))))

(deformat indicator-format "Formato JSON di un indicatore dinamico"
    (jsobject 'indicator
              (jsprop :indicator-id "Identificativo univoco dell'indicatore" (jsstring))
              (jsprop :source "Codice sorgente" (jsstring))
              (jsprop :start-date "Data inizio" (jsstring))
              ;; (jsprop :parameters "parametri " (jsarray parameter-format))
              ))

(defent dwh-indicatori
    (entity 'dwh-indicatori 
            :primary (attribute :id (string-type 20))
            :fields (list (attribute :name (string-type 20) :desc "Nome dell'indicatore")
                          (attribute :source-code (string-type 200) :desc "Codice sorgente scritto dall'utente")
                          (attribute :object-code (string-type 200) :desc "Codice oggetto prodotto dal compilatore")
                          (attribute :start-date (string-type 8) :desc "Data inizio validita"))))

;; (defent parameter-entity
;;     (entity 'parameter
;;             :primary (attribute :parameter-id (integer-type))
;;             :fields (list (attribute :name (string-type 20) :desc "Name del parametro")
;;                           (attribute :value (string-type 20) :desc "Valore del parametro"))))

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

;; (defent ind-stat-sogg-sini
;;     (entity 'ind-stat-sogg-sini
;;             :primary (attribute :ind-stat-sogg-sini-id (integer-type))
;;             :fields (list (attribute :id-sini (integer-type) :desc "identificativo sinistro")
;;                           (attribute :id-sogg (integer-type) :desc "identificativo del soggetto")
;;                           (attribute :d-tipo-sogg (integer-type) :desc "tipo soggetto (persona - organizzazione)")
;;                           (attribute :d-anno-accad (string-type 1) :desc "anno di accadimento sinistro")
;;                           (attribute :d-mese-accad (integer-type) :desc "mese di accadimento sinistro")
;;                           (attribute :d-comune-accad (integer-type) :desc "comune di accadimento sinistro")
;;                           (attribute :d-flg-coinvolto (string-type 6) :desc "se ha partecipato come 'coinvolto diretto'")
;;                           (attribute :d-flg-interessato (string-type 1) :desc "se ha partecipato come 'interessato'")
;;                           (attribute :d-flg-richiedente (string-type 1) :desc "se ha pertecipato come richiedente")
;;                           (attribute :d-flg-proprietario (string-type 1) :desc "se e' proprietario di veicolo coinvolto")
;;                           (attribute :d-flg-contraente (string-type 1) :desc "se e' contraente polizza di veicolo coinvolto")
;;                           (attribute :d-flg-conducente (string-type 1) :desc "se e' coinvolto con ruolo conducente")
;;                           (attribute :d-flg-trasportato (string-type 1) :desc "se e' coinvolto con ruolo trasportato")
;;                           (attribute :d-flg-pedone (string-type 1) :desc "se e' coinvolto con ruolo pedone")
;;                           (attribute :d-flg-leso (string-type 1) :desc "se ha partecipato come 'leso' ")
;;                           (attribute :d-flg-deceduto (string-type 1) :desc "se e' stato dichiarato deceduto")
;;                           (attribute :d-flg-testimone (string-type 1) :desc "se interessato con ruolo testimone")
;;                           (attribute :d-flg-medico (string-type 1) :desc "se interessato con ruolo medico")
;;                           (attribute :d-flg-legale (string-type 1) :desc "se interessato con ruolo legale/studio infortuni ")
;;                           (attribute :d-flg-carrozziere (string-type 1) :desc "se interessato con ruolo carroziere/officina")
;;                           (attribute :d-flg-perito (string-type 1) :desc "se interessato con ruolo perito")
;;                           (attribute :d-flg-respons (string-type 1) :desc "se coinvolto come responsabile ")
;;                           (attribute :d-flg-beneficiario (string-type 1) :desc "se e' beneficiario di un pagamento")
;;                           (attribute :d-flg-patente-invalida (string-type 1) :desc "se e' stato verificato come conducente con patente invalida (o senza patente)")
;;                           (attribute :d-grado-invalid (string-type 1) :desc "classifica sulla percentuale invalidita' (non nota, assente, bassa, media, alta)")
;;                           (attribute :m-num-imprese (string-type 10) :desc "numero di imprese a cui il soggetto e' collegato")
;;                           (attribute :m-senza-seg (integer-type) :desc "numero di richieste senza seguito (nel caso ci siano piu' richieste dallo stesso richiedente)")
;;                           (attribute :m-num-veic (integer-type) :desc "numero di distinti veicoli coinvolto")
;;                           (attribute :m-num-rich (integer-type) :desc "numero di richieste a cui il soggetto e' collegato")
;;                           (attribute :m-num-rich-fgvs (integer-type) :desc "numero di richieste gestite da fgvs a cui il soggetto e' collegato")
;;                           (attribute :m-tot-pagamenti (integer-type) :desc "totale dei pagamenti incassati")
;;                           (attribute :m-tot-importo-les (integer-type) :desc "totale importo per lesioni alla persona")
;;                           (attribute :m-num-lesioni (integer-type) :desc "numero di lesioni alla persona")
;;                           (attribute :m-gg-da-decorrenza (integer-type) :desc "numero di giorni trascorsi dalla decorrenza polizza per il contraente")
;;                           (attribute :m-gg-a-decorrenza (integer-type) :desc "numero di giorni che mancano alla scadenza polizza per il contraente")
;;                           (attribute :ultimo-agg (date-type) :desc "data ultimo aggiornamento"))))

;; (defent ind-stat-trg-veic-sini
;;     (entity 'ind-stat-trg-veic-sini
;;             :primary (attribute :ind-stat-trg-veic-sini-id (integer-type))
;;             :fields (list (attribute :id-sini (integer-type) :desc "identificativo sinistro")
;;                           (attribute :id-targa (integer-type) :desc "id targa")
;;                           (attribute :d-num-targa (string-type 25) :desc "numero della targa dichiarata alla bds")
;;                           (attribute :d-tipo-veic (string-type 1) :desc "identificativo del tipo di veicolo")
;;                           (attribute :d-anno-accad (integer-type) :desc "anno di accadimento sinistro")
;;                           (attribute :d-mese-accad (integer-type) :desc "mese di accadimento sinistro")
;;                           (attribute :d-comune-accad (string-type 6) :desc "comune di accadimento sinistro")
;;                           (attribute :d-tipo-targa (string-type 1) :desc "identificativo del tipo di targa")
;;                           (attribute :d-cod-impr-ass (string-type 5) :desc "codice ivass della impresa assicuratrice (che copre i danni al momento del sinistro")
;;                           (attribute :d-flg-black-box (string-type 1) :desc "presenza di blackbox")
;;                           (attribute :d-flg-senza-conducente (string-type 1) :desc "coinvolto in sinistro con ruolo senza conducente)")
;;                           (attribute :d-flg-targa-incoerente (string-type 1) :desc "indica se la targa e' stata identificata come incoerente (non (non corrispondente a num. telaio")
                          
;;                           (attribute :d-flg-targa-inesistente (string-type 1) :desc "indica se la targa e' stata identificata come inesistente")
;;                           (attribute :d-flg-veic-cessato (string-type 1) :desc "indica se la targa/veicolo e' stata identificata come cessato")
;;                           (attribute :m-num-danni (integer-type) :desc "numero totale danni al veicolo")
;;                           (attribute :m-tot-importo-veic (integer-type) :desc "totale importo per danni al veicolo")
;;                           (attribute :m-num-rich (integer-type) :desc "numero di richieste associate al veicolo")
;;                           (attribute :m-num-rich-fgvs (integer-type) :desc "numero di richieste gestite da fgvs a cui la targa e' collegata")
;;                           (attribute :m-num-prop (integer-type) :desc "numero di passaggi di proprieta' antecedenti alla data del sinistro")
;;                           (attribute :m-anni-immatr (integer-type) :desc "anni trascorsi dalla immatricolazione alla data del sinistro")
;;                           (attribute :m-num-incoerenze (integer-type) :desc "numero di richieste con incoerenze danni associate al veicolo")
;;                           (attribute :ultimo-agg (date-type) :desc "data ultimo aggiornamento"))))


;; (defent ind-stat-sini
;;     (entity 'ind-stat-sini
;;             :primary (attribute :ind-stat-sini-id (integer-type))
;;             :fields (list (attribute :id-sini (integer-type) :desc "id sinistro")
;;                           (attribute :d-data-accad (date-type) :desc "data accadimento")
;;                           (attribute :d-comune-accad (string-type 6) :desc "comune di accadimento")
;;                           (attribute :d-flg-autorita (string-type 1) :desc "se sono intervenute le autorita'")
;;                           (attribute :d-flg-pedoni (string-type 1) :desc "se ci sono pedoni coinvolti")
;;                           (attribute :d-flg-danni-ogg (string-type 1) :desc "se ci sono danni a cose e animali")
;;                           (attribute :d-data-denun (date-type) :desc "data denuncia (data prima richiesta)")
;;                           (attribute :m-num-imprese (integer-type) :desc "numero di imprese distinte di assicurazione coinvolte nel sinistro")
;;                           (attribute :m-num-soggetti (integer-type) :desc "numero di soggetti distinti coinvolti e/o interessati")
;;                           (attribute :m-num-coinvolti (integer-type) :desc "numero di soggetti distinti coinvolti")
;;                           (attribute :m-num-interessati (integer-type) :desc "numero di soggetti distinti interessati ")
;;                           (attribute :m-num-rich (integer-type) :desc "numero di richieste avanzate")
;;                           (attribute :m-num-veic (integer-type) :desc "numero di veicoli distinti coinvolti ")
;;                           (attribute :m-num-senza-seg (integer-type) :desc "numero di richieste senza seguito")
;;                           (attribute :m-num-danno-veic (integer-type) :desc "numero totale di veicoli distinti danneggiati")
;;                           (attribute :m-num-lesi (integer-type) :desc "numero totale di persone distinte lese ")
;;                           (attribute :m-num-trasp-les (integer-type) :desc "numero totale di persone distinte lese")
;;                           (attribute :m-num-decessi (integer-type) :desc "numero totale di persone distinte decedute")
;;                           (attribute :m-num-invalid (integer-type) :desc "numero totale di persone distinte con invalidita' permanente")
;;                           (attribute :m-tot-pagamenti (integer-type) :desc "totale pagamenti effettuati")
;;                           (attribute :m-tot-importo-veic (integer-type) :desc "totale importo per danni a veicoli")
;;                           (attribute :m-tot-importo-les (integer-type) :desc "totale importo per lesioni a persone")
;;                           (attribute :m-tot-importo-ogg (integer-type) :desc "totale importo per danni a cose/animali")
;;                           (attribute :m-num-incoerenze (integer-type) :desc "numero di danni non coerenti con la dinamica del sinitro")
;;                           (attribute :m-gg-da-decorrenza (integer-type) :desc "minimo di giorni trascorsi da decorrenza polizza per veicoli coinvolti")
;;                           (attribute :m-gg-a-scadenza (integer-type) :desc "minimo di giorni che mancano per la scadenza polizza per veicoli coinvolti")
;;                           (attribute :ultimo-agg (date-type) :desc "data ultimo aggiornamento"))))


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


;; (defrel indicator-parameters
;;     (relationship 'parameters-indicator dwh-indicatori parameter-entity :one-to-many))

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
                         ;; (pars (relation parameter-entity))
                         )
            (project (restrict (product inds ;; pars
                                        )
                               (expr:+and+ 
                                ;; (expr:+equal+ (expr:attr inds 'id)
                                ;;               (expr:attr pars 'id))
                                (expr:+equal+ (expr:attr inds 'name)
                                              name)))
                     :id :name)))

(defquery all-indicators () dwh-indicatori
          (with-queries ((inds (relation dwh-indicatori)))
            (project inds)))

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
  ;; (synth :output (apply #'doc:postpend (doc:semi) t
  ;;                        (remove nil (synth-all :ddl app-entities))) 0)
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

(let ((maven (xml:node '|project| 
     :|xmlns| "http://maven.apache.org/POM/4.0.0" :|xmlns:xsi| "http://www.w3.org/2001/XMLSchema-instance" 
     :|xsi:schemaLocation| "http://maven.apache.org/POM/4.0.0 http://maven.apache.org/maven-v4_0_0.xsd"
     (xml:node '|modelVersion| (xml:simple "4.0.0")) 
     (xml:node '|groupId| (xml:simple "com.extent")) 
     (xml:node '|artifactId| (xml:simple "backgen"))
     (xml:node '|packaging| (xml:simple "war")) 
     (xml:node '|version| (xml:simple "0.0.1-SNAPSHOT")) 
     (xml:node '|name| (xml:simple "backgen Maven Webapp")) 
     (xml:node '|url| (xml:simple "http://maven.apache.org")) 
     (xml:node '|dependencies|
          (xml:node '|dependency|
               (xml:node '|groupId| (xml:simple "junit")) 
               (xml:node '|artifactId| (xml:simple "junit")) 
               (xml:node '|version| (xml:simple "3.8.1")) 
               (xml:node '|scope| (xml:simple "test")))
          (xml:node '|dependency|
               (xml:node '|groupId| (xml:simple "javax.servlet")) 
               (xml:node '|artifactId| (xml:simple "javax.servlet-api")) 
               (xml:node '|version| (xml:simple "3.1.0")) 
               (xml:node '|scope| (xml:simple "provided")))
          (xml:node '|dependency|
               (xml:node '|groupId| (xml:simple "javax")) 
               (xml:node '|artifactId| (xml:simple "javaee-api")) 
               (xml:node '|version| (xml:simple "7.0")))
          (xml:node '|dependency|
               (xml:node '|groupId| (xml:simple "org.apache.derby")) 
               (xml:node '|artifactId| (xml:simple "derby")) 
               (xml:node '|version| (xml:simple "10.8.3.0"))))
     (xml:node '|build|
          (xml:node '|finalName| (xml:simple "backgen")) 
		
          (xml:node '|plugins|
               (xml:node '|plugin|
                    (xml:node '|artifactId| (xml:simple "maven-compiler-plugin")) 
                    (xml:node '|version| (xml:simple "3.0")) 
                    (xml:node '|configuration|
                         (xml:node '|source| (xml:simple "1.8")) 
                         (xml:node '|target| (xml:simple "1.8")))))))))
  (synth :output (synth :doc maven) 0))
