(in-package :backgen)
(defquery trip-by-name ((name (string-type 20))) trip-entity
          (with-queries ((trips (relation trip-entity)))
            (project (restrict trips
                      (expr:+equal+ (expr:attr trips 'name)
                                    name))
                     :name :start-date :end-date)))

(defquery all-trips () trip-entity
          (with-queries ((trips (relation trip-entity)))
            (project trips)))
