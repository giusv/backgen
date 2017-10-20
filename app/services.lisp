(in-package :backgen)

(defresource trip-item
    (rest-item 'trip ((trip (url:path-parameter 'trip (integer-type)))) 
               (list 
                (rest-get ()
                          (bl-let ((ent (bl-find-entity trip-entity trip)))
                            ent)))))

(defresource trips-collection
    (rest-collection 
     'trips
     (list 
      (rest-post% trip-format 
                  (bl-let ((name (bl-get :name trip-format))
                           (entity (bl-create-entity trip-entity 
                                                    :name name ))) 
                    ;; (bl-value-object entity)
                    entity))
      (rest-get ((name (url:query-parameter 'name (string-type 20))))
                (bl-let ((ent (bl-exec-query (trip-by-name name))))
                  (bl-unless (((bl-null ent) (trip-not-found-exception nil)))
                    ent))))
     trip-item))


(defservice server (rest-service 'trip-service (url:void) trips-collection))
