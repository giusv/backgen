(in-package :sql)

(defprim select (from &key fields where)
  (:pretty () (list 'select (list :from from :fields (synth-all :pretty fields) :where (synth :pretty where))))
  (:sql () (vcat (apply #'doc:hcat+
                         (append* (doc:text "SELECT")
                                      (if fields (apply #'punctuate (comma) nil (synth-all :sql fields))
                                          (doc:text "*")))))))

(defprim insert (into &rest pairs)
  (:pretty () (list 'insert (list :into into :pairs (synth-plist :pretty pairs))))
  (:sql () (apply #'doc:hcat+
                   (append* (doc:text "INSERT INTO ~a" into)
                            (apply #'doc:punctuate (doc:comma) nil (mapcar (lambda (pair)
                                                                             (textify (car pair)))
                                                                           pairs))
                            (doc:text "VALUES")
                            (apply #'doc:punctuate (doc:comma) nil (mapcar (lambda (pair)
                                                                             (synth :sql (cadr pair)))
                                                                           pairs))))))
