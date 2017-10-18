
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
