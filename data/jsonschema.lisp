;;https://hackage.haskell.org/package/json-schema-0.7.4.1/docs/Data-JSON-Schema-Types.html
;;http://cswr.github.io/JsonSchema/spec/grammar/
;; (defprod jsdoc (jsdoc ((id jid) (defs jdefs) (sch jsch)))
;;   (to-json () (apply #'jobject 
;; 		     :id id
;; 		     :definitions (apply #'jobject defs)
;; 		     (synth-plist to-json defs))))

(in-package :data)

(defparameter *formats* (make-hash-table))
(defmacro deformat (name desc format)
  `(progn (defparameter ,name (jsformat ',name ,desc ,format)) 
         (setf (gethash ',name *formats*) ,name)))

(defprim jsformat (name desc format)
  (:pretty () (list 'jsformat (list :name name :desc desc  :format (synth :pretty format))))
  (:java-implementation (package) (synth :java-implementation format package))
  (:ts-implementation () (synth :ts-implementation format))
  (:ts-imports () (synth :ts-imports format))
  (:java-implementation () (synth :java-implementation format))
  (:java-imports () (synth :java-imports format))
  (:type () (synth :type format))
  (:random () (synth :random format))
  (:req () (synth :req format))
  (:ref () (normal "~a" name)))

(defprim jsbool ()
  (:pretty () (list 'jsbool))
  (:req () (doc:text "boolean"))
  (:ref () (synth :req this))
  (:java-implementation () (java-primitive-type 'boolean))
  (:random () (jbool (random-boolean)))
  (:java-imports () nil)
  (:ts-imports () nil)
  (:type () (boolean-type))
  (:init () (java-const 'true)))

(defprim jsstring ()
  (:pretty () (list 'jsstring))
  (:req () (doc:text "string"))
  (:ref () (synth :req this))
  (:java-implementation () (java-object-type 'string))
  (:random () (jstring (random-string 10)))
  (:java-imports () nil)
  (:ts-imports () nil)
  (:type () (string-type 20))
  (:init () (java-const "")))

(defprim jsnumber ()
  (:pretty () (list 'jsnumber))
  (:req () (doc:text "number"))
  (:ref () (synth :req this))
  (:java-implementation () (java-primitive-type 'long))
  (:random () (jnumber (random-number 0 100)))
  (:java-imports () nil)
  (:ts-imports () nil)
  (:type () (integer-type)) 
  (:init () (java-const 0)))

;; handle choice in instantiation
;; (defprim (jschoice (&rest (schemas (list jsschema))))
;;   (:pretty () `(jschoice :schemas (synth-all :pretty schemas)))
;;   (:req () (vcat (doc:text "scelta tra i seguenti schemi:")
;; 		   (nest 4 (apply #'vcat (synth-all :req schemas))))))

(defprim jsobject (name &rest props)
  (:pretty () (list 'jsobject (list :name name :props (synth-all :pretty props))))
  (:req () (apply #'tabular
            (row (normal "Nome")
                 (normal "Vincoli")
                 (normal "Descrizione")
                 (normal "Contenuto"))
            (synth-all :req props)))
  (:ref () (normal "~a" name))
  (:random () (apply #'jobject (apply #'append (synth-all :random props))))
  (:java-implementation (package)
                        (let ((vo-name (symb name "-V-O")))
                          (java-unit vo-name
                                     (java-package (symb package '|.vo|))
                                     (apply #'append (synth-all :java-imports props))
                                     (java-class vo-name 
                                                 :public t
                                                 :fields (synth-all :java-implementation props)))))
  ;; (:definition () (java-pair vo-name (synth :type this)))
  (:type () (format-type this))
  (:ts-implementation () (ts-unit name 
                                  (apply #'append (synth-all :ts-imports props)) 
                                  (ts-class name
                                            :fields (synth-all :ts-implementation props))))
  (:vo (package)
       (java-unit name 
                  (java-package (symb package '|.vo|)) 
                  (java-class (symb name "-V-O") :public t
                              :fields (mapcar #'java-statement (synth-all :type props '|-V-O|))
                              :methods (apply #'append (synth-all :accessors props '|-V-O|)))))
  (:java-imports () nil) 
  (:ts-imports () (list (ts-import (mkstr "./" (string-downcase name)) name))) 
  (:init () nil))

(defprim jsprop (name description content &rest validators)
  (:pretty () (list 'jsprop (list :name name :description description :validators (synth-all :pretty validators) :content (synth :pretty content))))
  (:req () (row
            (normal "~a" name)
            ;; (td (ul (synth-all :req validators)))
            (normal "~a" description)
            (synth :ref content)))
  (:java-implementation () (java-field-with-accessors nil name (synth :java-type (synth :type content))))
  (:ts-implementation () (ts-field-with-accessors nil name (synth :ts-type (synth :type content))))
  (:type () (synth :type content))
  (:random () (list (keyw name) (synth :random content)))
  (:java-imports () (synth :java-imports content))
  (:ts-imports () (synth :ts-imports content))
  (:init () (error "should not be reachable"))
  (:accessors (&optional suffix) 
              (list (java-method (doc:text "get~a" (upper-camel name)) nil (synth :type content suffix)
                                 (java-return (java-dynamic name)))
                    (java-method (doc:text "set~a" (upper-camel name)) (list (java-pair name (synth :type content suffix))) 
                                 (java-primitive-type 'void)
                                 (java-statement (java-assign (java-chain (java-dynamic 'this) 
                                                                          (java-dynamic name))
                                                              (java-dynamic name)))))))

(defprim jsarray (element)
  (:pretty () (list 'jsarray (list :element (synth :pretty element)))) 
  (:req () (doc:text "array di ~a" (synth :ref element)))
  (:ref () (synth :req this))
  (:random () (let* ((length (random-number 2 5))
                     (values (loop for i from 0 to length collect (synth :random element)))) 
                (apply #'jarray values))) 
  (:java-imports () (cons (java-import '|java.util| '|List|) 
                          (synth :java-imports element)))
  (:ts-imports () (synth :ts-imports element))
  (:type () (collection-type (synth :type element)))
  (:init () nil))


(defun get-ident ()
  #'(lambda (jsschema)
      (list jsschema)))

(defun get-elem ()
  #'(lambda (jsschema)
      (list (synth :elem jsschema))))

(defun get-prop (name)
  #'(lambda (jsschema)
      (synth-all :content 
		 (remove-if-not #'(lambda (prop) 
				    (eql name (synth :name prop)))
				(synth :props jsschema)))))

(defun compose-filter (f g)
  #'(lambda (jsschema)
      (let ((temps (funcall f jsschema))) ;;temps is a list of jsschema
	(apply #'append (mapcar #'(lambda (temp) 
				    (funcall g temp))
				temps)))))

(defun compose-filters (&rest filters)
  #'(lambda (jsschema)
      (funcall (reduce #'compose-filter filters) jsschema)))



(defprim ident ()
  (:pretty () (list 'ident))
  (:func () (get-ident))
  ;; (:req () (doc:text "ident"))
  (:req () (doc:text "ident")))

(defprim prop (name)
  (:pretty () (list 'prop (list :name name)))
  (:func () (get-prop name))
  ;; (:req () (doc:text "~a" name))
  (:req () (span nil (doc:text "~a" name))))

(defprim elem ()
  (:pretty () (list 'elem))
  (:func () (get-elem))
  ;; (:req () (doc:text "#"))
  (:req () (span nil (doc:text "#"))))

(defprim comp (&rest filters)
  (:pretty () (list 'comp (list :filters (synth-all :pretty filters))))
  (:func () (apply #'compose-filters (synth-all :func filters)))
  ;; (:req () (apply #'punctuate (forward-slash) nil (synth-all :req filters)))
  (:req () (span nil (apply #'punctuate (forward-slash) nil (synth-all :req filters)))))

(defun parse-filter ()
  (do-with ((filters (sepby (item) (sym '>>>))))
    (result (apply #'comp (mapcar #'eval filters)))))

(defun filter (filt obj) 
  (car (funcall (synth :func filt) obj)))

;; (pprint (synth :pretty (car (funcall (get-prop 'addresses) *user*))))
;; (pprint (synth :pretty (car (funcall (get-elem) *addresses*))))

;; (pprint (synth :pretty (car (funcall (comp (comp (get-prop 'addresses) (get-elem)) (get-prop 'city)) *user*))))
;; (defparameter *address*
;;   (jsobject (jsprop 'city t (jsstring 'city-string))
;;             (jsprop 'state t (jsstring 'state-string))))
;; (defparameter *addresses* (jsarray 'address-array *address*))
;; (defparameter *user* 
;;   (jsobject 'user-object 
;;             (jsprop 'name t (jsstring 'name-string))
;; 	    (jsprop 'addresses t *addresses*)
;; 	    (jsprop 'numbers t (jsarray 'numbers-array (jsnumber 'number-number)))))



;; (pprint (synth :pretty (car (funcall (compose-filters (get-prop 'addresses) (get-elem) (get-prop 'city)) *user*))))
;; (pprint (synth :pretty (car (funcall (synth :func (comp (prop 'addresses) (elem) (prop 'city))) *user*))))
;; (pprint (synth :pretty (car (funcall 
;; 			     (synth :func (parse (parse-filter) 
;; 						   '((prop 'addresses) >>> (elem)))) *user*))))
