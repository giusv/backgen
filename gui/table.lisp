(in-package :gui)

(defprim table% (name source rowname bindings)
  (:pretty () (list 'table (list :name name 
                                 :source (synth :pretty source) 
                                 :rowname rowname
                                 :bindings (synth-plist :pretty bindings))))
  (:req (path)
        (paragraph
         (normal "Tabella denominata ~a"
                 (string-downcase name))
         (normal " associata a ~a"
                 (string-downcase (synth :name source)))
         (normal "(istanza del formato dati ")
         (verbatim (synth :req (synth :schema source))) 
         ;; (normal "con la seguente espressione:") 
         ;; (synth :req bindings path)
         ))
  (:brief (path) (synth :req this path))
  (:reqlist (*) nil)

 
  (:template () (html:table 
                    :|class| "table table-striped"
                  (html:thead 
                   (html:tr 
                    (mapcar (lambda (name) (html:th (doc:text "~a" (symbol-name name)))) 
                            (plist-keys bindings))))
                  (html:tbody
                   :|*ngFor| (doc:hcat (doc:text "let ~a of ~a" (lower-camel rowname) (lower-camel (synth :name source))))
                   (html:tr
                    (mapcar (lambda (element) (html:td (synth :template element))) 
                            (plist-values bindings))))))

  (:controller () (ts-empty))
  (:components (*) nil)
  (:routes (father) nil)
  (:ts-imports () (apply #'append 
                      (synth-plist-merge (lambda (pair)
                                           (synth :ts-imports (cadr pair)))
                                         bindings)))
  (:dependencies () (apply #'append 
                           (synth-plist-merge (lambda (pair)
                                                (synth :dependencies (cadr pair)))
                                              bindings))))

;; (defun csplice (cond &rest exps)
;;   (if cond
;;       `(,@exps)))


(defmacro table (name source (rowname) &body bindings)
  `(table% ,name
           ,source
           ',rowname
           (let ((,rowname ',rowname))
             (list ,@bindings))))


;; (defprim (table% (name source bindings))
;;   (:list () `(table (:name ,name 
;;                                :source ,(synth :list source) 
;;                                :bindings ,(synth-list-merge 2
;;                                                             (lambda (pair) 
;;                                                               (list (first pair) 
;;                                                                     (synth :list (funcall (second pair) (synth schema source)))))
;;                                                             bindings))))
;;   (:html (path)
;; 	   (div nil 
;;                 (text "Tabella denominata ")
;;                 (span-color (lower-camel name))
;;                 (text " associata a ")
;;                 (span-color (lower-camel (synth name source)))
;;                 (text "(istanza del formato dati ")
;;                 (a (list :href (concatenate 'string "#" (synth :string (synth :brief (synth schema source)) 0)))
;;                    (code nil (synth :brief (synth schema source ))))
;;                 (text "con le seguenti colonne:" ;; (lower-camel (synth name (synth schema source)))
;;                       ) 
;;                 (apply #'dl (list :class "row")
;;                        (synth-list-merge 2
;;                                          (lambda (pair) 
;;                                            (let ((header (textify (first pair))) 
;;                                                  (element (synth :html 
;;                                                                  (funcall (second pair) (synth schema source))
;;                                                                  path))) 
;;                                              (multitags (dt (list :class "col-sm-2") header) 
;;                                                         (dl (list :class "col-sm-10") element))))
;;                                          bindings))))
;;   (:brief (path) (synth :html (apply #'table name source bindings)))
;;   (toplevel () nil)
;;   (req (path) nil))


;; (defmacro table (name source (rowname &optional index) &body bindings)
;;   `(table% ,name
;;              ,source
;;              ,@(apply #'append (mapcar (lambda (binding)
;;                                          (list (first binding) `(lambda (,rowname ,@(csplice index index)) (declare (ignorable ,rowname ,@(csplice index index))) ,(second binding))))
;;                                        bindings))))

(defmacro table* (source (rowname &optional index) &body bindings)
  `(table% (gensym "TABLE") 
          ,source
          ,@(apply #'append (mapcar (lambda (binding)
                                      (list (first binding) `(lambda (,rowname ,@(csplice index index)) (declare (ignorable ,rowname ,@(csplice index index))) ,(second binding))))
                                   bindings))))

