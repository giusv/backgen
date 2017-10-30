(in-package :backgen)

;; (deformat place-format ""
;;   (jsobject 'place
;;             (jsprop :place-id "" (jsnumber))
;;             (jsprop :name "" (jsstring))
;;             (jsprop :temperature "" (jsnumber))))

;; (deformat city-format ""
;;   (jsobject 'city
;;             (jsprop :city-id "" (jsnumber))
;;             (jsprop :name "" (jsstring))
;;             (jsprop :places "" (jsarray place-format))))

(deformat trip-format ""
  (jsobject 'trip
            (jsprop :trip-id "" (jsnumber))
            (jsprop :name "" (jsstring)) 
            (jsprop :start-date "" (jsnumber))
            (jsprop :end-date "" (jsnumber))
            ;; (jsprop :cities "" (jsarray city-format))
            ))
