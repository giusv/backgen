(in-package :sql)

(defprim sql-const (lit)
  (:pretty () (list 'sql-const (list :lit lit))) 
  (:sql () (cond ((stringp lit) (doc:single-quotes (text "~a" lit)))
                 ((numberp lit) (doc:text "~a" lit))
                 ((symbolp lit) (doc:text "~a" (lower-camel lit)))
                 (t (empty)))))

(defprim sql-select (from &key fields where)
  (:pretty () (list 'sql-select (list :from from :fields (synth-all :pretty fields) :where (synth :pretty where))))
  (:sql () (vcat (apply #'doc:hcat+
                         (append* (doc:text "SELECT")
                                      (if fields (apply #'punctuate (comma) nil (synth-all :sql fields))
                                          (doc:text "*")))))))

(defprim sql-create-sequence (name start increment)
  (:pretty () (list 'sql-create-sequence (list :name name :start start :increment increment)))
  (:sql () (doc:hcat+ (doc:text "CREATE SEQUENCE ~a START WITH ~a INCREMENT BY ~a;" (upper name) start increment))))

(defprim sql-insert (into &rest pairs)
  (:pretty () (list 'sql-insert (list :into into :pairs (synth-plist :pretty pairs))))
  (:sql () (doc:hcat (apply #'doc:hcat+
                            (append* (doc:text "INSERT INTO ~a" (upper into))
                                     (parens (apply #'doc:punctuate (doc:comma) nil (mapcar (lambda (pair)
                                                                                              (textify (upper (car pair))))
                                                                                            (group pairs 2))))
                                     (doc:text "VALUES")
                                     (parens (apply #'doc:punctuate (doc:comma) nil (synth-plist-merge (lambda (pair)
                                                                                                         (synth :sql (sql-const (cadr pair))))
                                                                                                       pairs)))))
                     (doc:semi))))

(defprim sql-concat (&rest statements)
  (:pretty () (list 'sql-concat (list :statements (synth-all :pretty statements))))
  (:sql () (apply #'doc:vcat (synth-all :sql statements))))
