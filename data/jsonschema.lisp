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
  (:req () (synth :req format))
  (:ref () name))
(defprim jsbool ()
  (:pretty () (list 'jsbool))
  (:req () (doc:text "boolean"))
  (:ref () (synth :req this))
  (:random () (jbool (random-boolean)))
  (:imports () nil)
  (:type (&optional suffix) (java-primitive-type 'bool))
  (:init () (java-const 'true)))

(defprim jsstring ()
  (:pretty () (list 'jsstring))
  (:req () (doc:text "string"))
  (:ref () (synth :req this))
  (:random () (jstring (random-string 10)))
  (:imports () nil)
  (:type (&optional suffix) (java-primitive-type 'string))
  (:init () (java-const "")))

(defprim jsnumber ()
  (:pretty () (list 'jsnumber))
  (:req () (doc:text "number"))
  (:ref () (synth :req this))
  (:random () (jnumber (random-number 0 100)))
  (:imports () nil)
  (:type (&optional suffix) (java-primitive-type 'number))
  (:init () (java-const 0)))

;; handle choice in instantiation
;; (defprim (jschoice (&rest (schemas (list jsschema))))
;;   (:pretty () `(jschoice :schemas (synth-all :pretty schemas)))
;;   (:req () (vcat (doc:text "scelta tra i seguenti schemi:")
;; 		   (nest 4 (apply #'vcat (synth-all :req schemas))))))

(defprim jsobject (name &rest props)
  (:pretty () (list 'jsobject (list :name name :props (synth-all :pretty props))))
  (:req () (table
            (tr (th (doc:text "Nome"))
                (th (doc:text "Vincoli"))
                (th (doc:text "Descrizione"))
                (th (doc:text "Contenuto")))
            (synth-all :req props)))
  (:ref () name)
  (:random () (apply #'jobject (apply #'append (synth-all :random props))))
  (:model ()
          (java-unit name 
                     (synth-all :imports props) 
                     (java-class name
                                 :fields (synth-all :type props))))
  (:vo (package)
       (java-unit name 
                  (java-package (symb package '|.vo|)) 
                  (java-class (symb name "-V-O") :public t
                              :fields (mapcar #'java-statement (synth-all :type props '|-V-O|))
                              :methods (apply #'append (synth-all :accessors props '|-V-O|)))))
  (:imports ()  (java-import (mkstr "./" (string-downcase name)) name)) 
  (:type (&optional suffix) (java-object-type name))
  (:init () nil))

(defprim jsprop (name description content &rest validators)
  (:pretty () (list 'jsprop (list :name name :description description :validators (synth-all :pretty validators) :content (synth :pretty content))))
  (:req () (tr
            (td (doc:textify name))
            (td (ul (synth-all :req validators)))
            (td (doc:text "~a" description))
            (td (synth :ref content))))
  (:random () (list (keyw name) (synth :random content)))
  (:imports () (synth :imports content))
  (:type (&optional suffix) (java-pair name (synth :type content suffix) :init (synth :init content)))
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
  (:imports () (synth :imports element))
  (:type (&optional suffix) (java-array-type (java-object-type (symb (synth :name element) suffix))))
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
