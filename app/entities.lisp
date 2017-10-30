(in-package :backgen)

(defent trip-entity
    (entity 'trips
            :primary (attribute :id (integer-type))
            :fields (list (attribute :name (string-type 20))
                          (attribute :start-date (string-type 8))
                          (attribute :end-date (string-type 8)))))

;; (defent city-entity
;;     (entity 'cities
;;             :primary (attribute :id (integer-type))
;;             :fields (list (attribute :name (string-type 20)))))

;; (defent place-entity
;;     (entity 'places
;;             :primary (attribute :id (integer-type))
;;             :fields (list (attribute :name (string-type 20))
;;                           (attribute :temperature (integer-type)))))

;; (defrel trip-city
;;     (relationship 'trip-city trip-entity city-entity :one-to-many))

;; (defrel city-place
;;     (relationship 'city-place city-entity place-entity :one-to-many))
