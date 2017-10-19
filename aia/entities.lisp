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
;;                           (attribute :value(character-type) :nullable nil)
;;                           ;; (attribute :uid (integer-type) :nullable nil)
;;                           ;; (attribute :id-sini (integer-type) :nullable nil)
;;                           (attribute :last-update (date-type) :nullable nil))))
;; (defent dwh-ind-risc-trg-veic
;;     (entity 'dwh-ind-risc-trg-veic
;;             :primary (attribute :ind-value-id (integer-type))
;;             :fields (list (attribute :value (character-type) :nullable nil) 
;;                           (attribute :last-update (date-type) :nullable nil))))

;; (defent dwh-ind-risc-sini
;;     (entity 'dwh-ind-risc-sini
;;             :primary (attribute :ind-value-id (integer-type))
;;             :fields (list (attribute :value (character-type) :nullable nil) 
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


(defent ind-stat-sogg-sini
    (entity 'ind-stat-sogg-sini
            :primary (attribute :ind-stat-sogg-sini-id (integer-type))
            :fields (list (attribute :id-sini (integer-type) :desc "identificativo sinistro")
                          (attribute :id-sogg (integer-type) :desc "identificativo del soggetto")
                          (attribute :d-tipo-sogg (integer-type) :desc "tipo soggetto (persona - organizzazione)")
                          (attribute :d-anno-accad (character-type) :desc "anno di accadimento sinistro")
                          (attribute :d-mese-accad (integer-type) :desc "mese di accadimento sinistro")
                          (attribute :d-comune-accad (integer-type) :desc "comune di accadimento sinistro")
                          (attribute :d-flg-coinvolto (string-type 6) :desc "se ha partecipato come 'coinvolto diretto'")
                          (attribute :d-flg-interessato (character-type) :desc "se ha partecipato come 'interessato'")
                          (attribute :d-flg-richiedente (character-type) :desc "se ha pertecipato come richiedente")
                          (attribute :d-flg-proprietario (character-type) :desc "se e' proprietario di veicolo coinvolto")
                          (attribute :d-flg-contraente (character-type) :desc "se e' contraente polizza di veicolo coinvolto")
                          (attribute :d-flg-conducente (character-type) :desc "se e' coinvolto con ruolo conducente")
                          (attribute :d-flg-trasportato (character-type) :desc "se e' coinvolto con ruolo trasportato")
                          (attribute :d-flg-pedone (character-type) :desc "se e' coinvolto con ruolo pedone")
                          (attribute :d-flg-leso (character-type) :desc "se ha partecipato come 'leso' ")
                          (attribute :d-flg-deceduto (character-type) :desc "se e' stato dichiarato deceduto")
                          (attribute :d-flg-testimone (character-type) :desc "se interessato con ruolo testimone")
                          (attribute :d-flg-medico (character-type) :desc "se interessato con ruolo medico")
                          (attribute :d-flg-legale (character-type) :desc "se interessato con ruolo legale/studio infortuni ")
                          (attribute :d-flg-carrozziere (character-type) :desc "se interessato con ruolo carroziere/officina")
                          (attribute :d-flg-perito (character-type) :desc "se interessato con ruolo perito")
                          (attribute :d-flg-respons (character-type) :desc "se coinvolto come responsabile ")
                          (attribute :d-flg-beneficiario (character-type) :desc "se e' beneficiario di un pagamento")
                          (attribute :d-flg-patente-invalida (character-type) :desc "se e' stato verificato come conducente con patente invalida (o senza patente)")
                          (attribute :d-grado-invalid (character-type) :desc "classifica sulla percentuale invalidita' (non nota, assente, bassa, media, alta)")
                          (attribute :m-num-imprese (string-type 10) :desc "numero di imprese a cui il soggetto e' collegato")
                          (attribute :m-senza-seg (integer-type) :desc "numero di richieste senza seguito (nel caso ci siano piu' richieste dallo stesso richiedente)")
                          (attribute :m-num-veic (integer-type) :desc "numero di distinti veicoli coinvolto")
                          (attribute :m-num-rich (integer-type) :desc "numero di richieste a cui il soggetto e' collegato")
                          (attribute :m-num-rich-fgvs (integer-type) :desc "numero di richieste gestite da fgvs a cui il soggetto e' collegato")
                          (attribute :m-tot-pagamenti (integer-type) :desc "totale dei pagamenti incassati")
                          (attribute :m-tot-importo-les (integer-type) :desc "totale importo per lesioni alla persona")
                          (attribute :m-num-lesioni (integer-type) :desc "numero di lesioni alla persona")
                          (attribute :m-gg-da-decorrenza (integer-type) :desc "numero di giorni trascorsi dalla decorrenza polizza per il contraente")
                          (attribute :m-gg-a-decorrenza (integer-type) :desc "numero di giorni che mancano alla scadenza polizza per il contraente")
                          (attribute :ultimo-agg (date-type) :desc "data ultimo aggiornamento"))))

(defent ind-stat-trg-veic-sini
    (entity 'ind-stat-trg-veic-sini
            :primary (attribute :ind-stat-trg-veic-sini-id (integer-type))
            :fields (list (attribute :id-sini (integer-type) :desc "identificativo sinistro")
                          (attribute :id-targa (integer-type) :desc "id targa")
                          (attribute :d-num-targa (string-type 25) :desc "numero della targa dichiarata alla bds")
                          (attribute :d-tipo-veic (character-type) :desc "identificativo del tipo di veicolo")
                          (attribute :d-anno-accad (integer-type) :desc "anno di accadimento sinistro")
                          (attribute :d-mese-accad (integer-type) :desc "mese di accadimento sinistro")
                          (attribute :d-comune-accad (string-type 6) :desc "comune di accadimento sinistro")
                          (attribute :d-tipo-targa (character-type) :desc "identificativo del tipo di targa")
                          (attribute :d-cod-impr-ass (string-type 5) :desc "codice ivass della impresa assicuratrice (che copre i danni al momento del sinistro")
                          (attribute :d-flg-black-box (character-type) :desc "presenza di blackbox")
                          (attribute :d-flg-senza-conducente (character-type) :desc "coinvolto in sinistro con ruolo senza conducente)")
                          (attribute :d-flg-targa-incoerente (character-type) :desc "indica se la targa e' stata identificata come incoerente (non (non corrispondente a num. telaio")
                          
                          (attribute :d-flg-targa-inesistente (character-type) :desc "indica se la targa e' stata identificata come inesistente")
                          (attribute :d-flg-veic-cessato (character-type) :desc "indica se la targa/veicolo e' stata identificata come cessato")
                          (attribute :m-num-danni (integer-type) :desc "numero totale danni al veicolo")
                          (attribute :m-tot-importo-veic (integer-type) :desc "totale importo per danni al veicolo")
                          (attribute :m-num-rich (integer-type) :desc "numero di richieste associate al veicolo")
                          (attribute :m-num-rich-fgvs (integer-type) :desc "numero di richieste gestite da fgvs a cui la targa e' collegata")
                          (attribute :m-num-prop (integer-type) :desc "numero di passaggi di proprieta' antecedenti alla data del sinistro")
                          (attribute :m-anni-immatr (integer-type) :desc "anni trascorsi dalla immatricolazione alla data del sinistro")
                          (attribute :m-num-incoerenze (integer-type) :desc "numero di richieste con incoerenze danni associate al veicolo")
                          (attribute :ultimo-agg (date-type) :desc "data ultimo aggiornamento"))))


(defent ind-stat-sini
    (entity 'ind-stat-sini
            :primary (attribute :ind-stat-sini-id (integer-type))
            :fields (list (attribute :id-sini (integer-type) :desc "id sinistro")
                          (attribute :d-data-accad (date-type) :desc "data accadimento")
                          (attribute :d-comune-accad (string-type 6) :desc "comune di accadimento")
                          (attribute :d-flg-autorita (character-type) :desc "se sono intervenute le autorita'")
                          (attribute :d-flg-pedoni (character-type) :desc "se ci sono pedoni coinvolti")
                          (attribute :d-flg-danni-ogg (character-type) :desc "se ci sono danni a cose e animali")
                          (attribute :d-data-denun (date-type) :desc "data denuncia (data prima richiesta)")
                          (attribute :m-num-imprese (integer-type) :desc "numero di imprese distinte di assicurazione coinvolte nel sinistro")
                          (attribute :m-num-soggetti (integer-type) :desc "numero di soggetti distinti coinvolti e/o interessati")
                          (attribute :m-num-coinvolti (integer-type) :desc "numero di soggetti distinti coinvolti")
                          (attribute :m-num-interessati (integer-type) :desc "numero di soggetti distinti interessati ")
                          (attribute :m-num-rich (integer-type) :desc "numero di richieste avanzate")
                          (attribute :m-num-veic (integer-type) :desc "numero di veicoli distinti coinvolti ")
                          (attribute :m-num-senza-seg (integer-type) :desc "numero di richieste senza seguito")
                          (attribute :m-num-danno-veic (integer-type) :desc "numero totale di veicoli distinti danneggiati")
                          (attribute :m-num-lesi (integer-type) :desc "numero totale di persone distinte lese ")
                          (attribute :m-num-trasp-les (integer-type) :desc "numero totale di persone distinte lese")
                          (attribute :m-num-decessi (integer-type) :desc "numero totale di persone distinte decedute")
                          (attribute :m-num-invalid (integer-type) :desc "numero totale di persone distinte con invalidita' permanente")
                          (attribute :m-tot-pagamenti (integer-type) :desc "totale pagamenti effettuati")
                          (attribute :m-tot-importo-veic (integer-type) :desc "totale importo per danni a veicoli")
                          (attribute :m-tot-importo-les (integer-type) :desc "totale importo per lesioni a persone")
                          (attribute :m-tot-importo-ogg (integer-type) :desc "totale importo per danni a cose/animali")
                          (attribute :m-num-incoerenze (integer-type) :desc "numero di danni non coerenti con la dinamica del sinitro")
                          (attribute :m-gg-da-decorrenza (integer-type) :desc "minimo di giorni trascorsi da decorrenza polizza per veicoli coinvolti")
                          (attribute :m-gg-a-scadenza (integer-type) :desc "minimo di giorni che mancano per la scadenza polizza per veicoli coinvolti")
                          (attribute :ultimo-agg (date-type) :desc "data ultimo aggiornamento"))))
