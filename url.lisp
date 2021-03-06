(in-package :url)
(defprim void ()
  (:url () (empty))
  (:pretty () (list 'void))
  ;; (:last () "default";; (error "no last elements in (void)")
  ;;        )
  (:path-parameters () nil))

(defprim protocolled (name pose)
  (:pretty () (list 'protocolled (list :name name :url (synth :pretty pose))))
  (:url () (doc:hcat (text "~a://" (string-downcase name)) (synth :url pose)))
  (:path-parameters () nil))

(defprim host (&rest segments)
  (:pretty () (list 'host (list :segments segments)))
  (:url () (text "~{~a~^.~}" (mapcar #'string-downcase segments)))
  (:path-parameters () nil))


(defprim chunk (name)
  (:pretty () (list 'chunk (list :name name)))
  (:url () (doc:text "~a" (string-downcase name)))
  ;; (:last () name)
  (:path-parameters () nil))

;; (defprim path-parameter (name type &key validators)
;;   (:pretty () (list 'path-parameter (list :name name :type type :validators (synth-all :pretty validators))))
;;   )

(defprim path-parameter (name type &key validators)
  (:pretty () (list 'path-parameter (list :name name :type (synth :pretty type) :validators (synth-all :pretty validators))))
  ;; (:type () (doc:text "path"))
  (:declaration (&optional full) 
                (let ((pair (java-pair (symb (lower-camel name) "-ID") (synth :java-type type)))) 
                  (if full
                      (java-with-annotations (cons (java-annotation '|PathParam| (java-object :|value| (java-const (lower-camel name))))
                                                   (synth-all :annotation validators))
                                             pair
                                             :newline nil)
                                           pair)))
  (:java-implementation (cont &rest args) (apply cont 
                                                 (java:java-dynamic (symb (lower-camel name) "-ID"))
                                                 args))
  ;; (:req () (html:taglist 
  ;;           (html:span-color (string-downcase name))
  ;;           (doc:text "(parametro path)")))
  ;; (:url () (path-parameter name type :validators validators))
  (:url () (doc:braces (doc:text "~a" (string-downcase name))))
  ;; (:last () name)
  (:path-parameters () (list this)))

(defprim query-parameter (name type &key validators value)
  (:pretty () (list 'query-parameter (list :name name :type (synth :pretty type) :value value :validators (synth-all :pretty validators))))
  (:url () (doc:hcat (doc:text "~a" (string-downcase name)) 
                     (if value 
                         (doc:hcat (doc:equals) (synth :url value))
                         (doc:empty))))
  ;; (:req () (html:taglist 
  ;;           (html:span-color (string-downcase name))
  ;;           (doc:text "(parametro query)")))
  (:declaration (&optional full) 
                (let ((pair (java-pair (lower-camel name) (synth :java-type type)))) 
                  (if full
                      (java-with-annotations (cons (java-annotation '|QueryParam| (java-object :|value| (java-const (lower-camel name))))
                                                   (synth-all :annotation validators))
                                             pair
                                             :newline nil)
                      pair)))
  (:java-implementation (cont &rest args) (apply cont (java:java-dynamic name) args))
  ;; (:type () (doc:text "query"))
  )

;; (defprim login-parameter (name)
;;   (:req () (html:taglist 
;;                (html:span-color (string-downcase name))
;;                (doc:text "(parametro login)")))
;;   (:type () (doc:text "login"))
;;   (:pretty () (list 'login-parameter (list :name name))))

;; backward-chain holds reversed path
(defprim backward-chain (segment pose)
  (:url () (if pose 
		 (doc:hcat (synth :url pose) (doc:text "/") (synth :url segment))))
  (:pretty () (list 'backward-chain (list :segment (synth :pretty segment) :pose (synth :pretty pose))))
  ;; (:last () (synth :last segment))
  (:path-parameters () (append (synth :path-parameters segment) (synth :path-parameters pose))))

(defprim multi (&rest poses)
  (:url () (doc:parens (apply #'punctuate (doc:text ",") nil (synth-all :url poses))))
  (:pretty () (list 'multi (list :poses (synth-all :pretty poses))))
  ;; (:last () (error "no last elements in multi"))
  (:path-parameters () (apply #'append (synth-all :path-parameters poses))))

(defprim forward-chain (segment pose)
  (:url () (doc:hcat (synth :url segment) (doc:text "/") (synth :url pose)))
  (:pretty () (list 'forward-chain (list :segment (synth :pretty segment) :pose (synth :pretty pose))))
  ;; (:last () (synth :last pose))
  (:path-parameters () (append (synth :path-parameters segment) (synth :path-parameters pose))))

(defprim queried (segment &rest parameters)
  (:url () (doc:hcat (synth :url segment) (doc:text "?") (apply #'punctuate (doc:text "&") nil (synth-all :url parameters))))
  (:pretty () (list 'queried (list :segment (synth :pretty segment) :parameters (synth-all :pretty parameters))))
  (:last () (synth :last segment)))

(defun parse-query-parameter ()
  (do-with ((name (item))
	    ((sym '=))
	    (value (choose (do-with (((sym '{))
				     (value (item))
				     ((sym '}))) 
			     (result value))
			   (do-with ((value (item))) 
                             (result (expr:const value))))))
    (result (query-parameter name nil :value value))))
(defun parse-chunk ()
  (choose (do-with (((sym '{))
		    (seg (item))
		    ((sym '}))) 
	    (result (chunk seg)))
	  (choose (do-with ((seg (item))
			    ((sym '?))
			    (pars (sepby1 (parse-query-parameter) (sym '&))))
		    (result (apply #'queried (chunk seg) pars)))
		  (do-with ((seg (item)))
		    (result (chunk seg))))))

(defun parse-relative-url ()
  (do-with ((segs (sepby (choose (do-with (((sym '<))
					   (poses (sepby (parse-url) (sym '&)))
					   ((sym '>)))
				   (result (apply #'multi poses)))
				 (parse-chunk))
			 (sym '/)))) 
    ;; (result (reduce #'forward-chain segs :from-end t))
    (result (reduce #'forward-chain segs :from-end t))))
(defun parse-host ()
  (do-with ((segs (sepby (item)
                          (sym '%)))) 
    (result (apply #'host segs))))

(defun parse-absolute-url ()
  (do-with ((host (parse-host))
            ((sym '/))
            (rel (parse-relative-url))) 
    (result (protocolled 'http (forward-chain host rel)))))

;; (defun parse-url ()
;;  (parse-absolute-url))

(defun parse-url ()
 (choose (parse-absolute-url)
         (parse-relative-url)))

(defmacro merge-urls (head tail)
  ;; (reduce #'forward-chain head :from-end t :initial-value (url tail))
  (labels ((listify (x)
             (if (consp x)
                 x
                 (list x))))
    `(parse (parse-url) `(,@',(listify head)  / ,@',(listify tail)))))

;; (merge-urls (seg1 / seg2) (seg3 / seg4))

;; (merge-urls :seg1 (seg3 / seg4))
(defmacro url (u)
  `(parse (parse-url) ,u))

;;(synth output (synth :url (chain 'a (multi (chain 'b) (chain 'c)))) 0)
;; (synth output (synth :url (multi (chain 'b) (chain 'c))) 0)
;;(parse (parse-url) '(a </> b))

;; (synth output (synth :url (parse (parse-url) '({ a }))) 0)
;; (synth output (synth :url (parse (parse-url) `(b ? q = a & r = { ,(value (button 'ok nil)) }))) 0)
(synth :output (synth :url (parse (parse-url) `(www % test % it))) 0)
(terpri)
(let* ((id (expr:const 1)) 
       (u (url `(www % test % it / b ? q = a & r = { ,id })))) 
  (synth :output (synth :url u) 0))

 

;; (synth output (synth :url (reduce #'forward-chain (parse (parse-url) '({ a } / b)) :from-end t :initial-value (void))) 0)

;; (synth output (synth :url (reduce #'forward-chain (parse (parse-url) '({ a } / < b & c >)) :from-end t :initial-value (void))) 0)

;; (synth output (synth :url (reduce #'forward-chain (parse (parse-url) '({ a } / < b & c >)) :from-end t :initial-value (void))) 0)

;; (reduce #'forward-chain (parse (parse-url) '({ a } / < b & c >)) :from-end t :initial-value (void))



;; (pprint (synth :pretty (parse (parse-url) '(a / < b & c >))))
;;(synth output (synth :url (parse (parse-url) '(a / < b & c >))) 0)

