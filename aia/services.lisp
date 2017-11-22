(in-package :backgen)

;; (server:defresource indicator-item
;;     (server:rest-item 'indicator ((indicator (url:path-parameter 'indicator (integer-type)))) 
;;                       (list 
;;                        (server:rest-get ()
;;                                         (server:bl-let ((ent (server:bl-find-entity dwh-indicatori indicator)))
;;                                           ent)))))

;; (server:defresource indicators-collection
;;     (server:rest-collection 
;;      'indicators
;;      (list 
;;       (server:rest-post% indicator-format 
;;                          (server:bl-let ((source (server:bl-get :source indicator-format))
;;                                          (entity (server:bl-create-entity dwh-indicatori 
;;                                                                           :source-code source))) 
;;                            ;; (server:bl-value-object entity)
;;                            entity))
;;       (server:rest-get ((name (url:query-parameter 'name (string-type 20))))
;;                        (server:bl-let ((ent (server:bl-exec-query ;; (all-indicators)
;;                                                                   (indicator-by-name name)
;;                                                                   )))
;;                          (server:bl-unless (((server:bl-null ent) (indicator-not-found-exception nil)))
;;                            ent))))
;;      indicator-item))
;; (server:defservice server (server:rest-service 'indicator-service (url:void) indicators-collection))
