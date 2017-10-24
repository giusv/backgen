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
