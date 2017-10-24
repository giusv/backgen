(in-package :backgen)
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

(defquery all-stdlib-entries () stdlib-entries
          (with-queries ((ents (relation stdlib-entries))
                         (pars (relation stdlib-entry-parameters)))
            (project (equijoin ents pars :stdlib-entries-id))))

