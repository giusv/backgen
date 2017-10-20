(in-package :backgen)
(defun uniq (&rest inds)
  (read-from-string (apply #'mkstr inds)))

(defdb
    (tl-forall i (list 1 2) 
      (tl-exists (trip trips) 
          (:id i :name (random-string 8) :start-date (random-string 8) :end-date (random-string 8))
        (tl-forall j (list 1 2 3) 
          (tl-exists (city cities)
              (:id (uniq i j) :trips-id i :name (random-string 8))
            (tl-forall k (list 1 2 3 4) 
              (tl-exists (place places)
                  (:id (uniq i j k) :cities-id (uniq i j) :name (random-string 8) :temperature (random-number 30 40)))))))))


