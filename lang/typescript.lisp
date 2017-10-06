(in-package :typescript)

;;  (defmacro within-braces (before inside)
;;      `(vcat (hcat ,before (text "{"))
;;             (nest 2 ,inside)
;;             (text "}")))
;; (defmacro within-parens (before inside)
;;   `(vcat (hcat ,before (text "("))
;;          (nest 2 ,inside)
;;          (text ")")))
;; (within-braces (text "@Component") (vcat (text "selector: ~a" selector)))

(defprim ts-empty ()
  (:pretty () (list 'ts-empty))
  (:typescript () (empty)))

(defprim ts-comment (text)
  (:pretty () (list 'ts-comment (list :text text)))
  (:typescript () (text "//~a" (synth :string text))))

(defprim ts-pair (name type &key init const private)
  (:pretty () (list 'ts-pair (list :name name :type (synth :pretty type) :init (synth :pretty init) :const const)))
  (:typescript () (hcat (if private (text "private ") (empty))
                        (if const (text "const ") (empty))
                        (text "~a: " (lower-camel name)) (synth :typescript type)
                        (if init 
                            (hcat (text " = ") (synth :typescript init))
                            (empty)))))




(defprim ts-const (lit)
  (:pretty () (list 'ts-const (list :lit lit)))
  (:typescript () (cond ((stringp lit) (single-quotes (text "~a" lit)))
                        ((numberp lit) (text "~a" lit))
                        ((symbolp lit) (text "~a" (lower-camel lit)))
                        (t (empty)))))




(defprim ts-try (body catches &optional finally)
  (:pretty () (list 'ts-try (list :body (synth :pretty body) :catches (synth-plist :pretty catches))))
  (:typescript () (error "not implemented yet")))

(defprim ts-catch% (exceptions name body)
  (:pretty () (list 'ts-catch (list :exceptions exceptions :name name :body (synth :pretty body))))
  (:typescript () (error "not implemented yet")))

(defmacro ts-catch (exceptions body)
  `(let* ((,(car exceptions) ',(car exceptions))) 
     (ts-catch% (list ,@(cdr exceptions)) ,(car exceptions) ,body)))

(defprim ts-primitive-type (name)
  (:pretty () (list 'ts-primitive-type (list :name name)))
  (:typescript () (hcat (text "~a" (lower-camel name)))))

(defprim ts-array-type (type)
  (:pretty () (list 'ts-array-type (list :type type)))
  (:typescript () (hcat (synth :typescript type) 
                        (text "[]"))))

(defprim ts-object-type (name)
  (:pretty () (list 'ts-object-type (list :name name)))
  (:typescript () (textify (upper-camel name))))

(defprim ts-wildcard-type ()
  (:pretty () (list 'ts-wildcard-type))
  (:typescript () (error "not implemented yet")))

(defprim ts-template-type (name &rest types)
  (:pretty () (list 'ts-template-type (list :name name :types (synth-all :pretty types))))
  (:typescript () (hcat (textify (upper-camel name))
                        (angular (apply #'punctuate (comma) nil (mapcar (lambda (type)
                                                                          (synth :typescript type))
                                                                        types))))))

(defprim ts-type (name &key primitive array template)
  (:pretty () (list 'ts-type (list :name name :primitive primitive :array array :template template)))
  (:typescript () (hcat (text "~a" (if (or (eq name :string)
                                           (eq name :number)
                                           (eq name :any)
                                           (eq name :void)) 
                                       (lower-camel name)
                                       (upper-camel name)))
                        (if array (brackets (empty)) (empty))
                        (if template (angular (synth :typescript template))))))


;; (defprim ts-bool (value)
;;   (:pretty () (list 'ts-bool (list :value (synth :pretty value))))
;;   (:string () (synth :string value)))

;; (defprim ts-number (value)
;;   (:pretty () (list 'ts-number (list :value (synth :pretty value))))
;;   (:string () (synth :string value)))

;; (defprim ts-string (value)
;;   (:pretty () (list 'ts-string (list :value (synth :pretty value))))
;;   (:string () (synth :string value)))

(defprim ts-array (&rest elems)
  (:pretty () (list 'ts-array (list :elems (synth-all :pretty elems))))
  (:typescript () (brackets (apply #'punctuate (comma) t (synth-all :typescript (apply #'append* elems))) :padding 1 :newline nil)))

(defprim ts-object (&rest elems)
  (:pretty () (list 'ts-object (list :elems (synth-plist :pretty (apply #'append* elems)))))
  (:typescript () (braces 
                   (nest 4 (apply #'punctuate (comma) t 
                                  (synth-plist-merge 
                                   #'(lambda (pair) (hcat (text "~a: " (lower-camel (first pair)))
                                                          (synth :typescript (second pair)))) 
                                   (apply #'append* elems))))
                   :newline t)))

(defprim ts-template (element)
  (:pretty () (list 'ts-template (list :element (synth :pretty element))))
  (:typescript () (back-quotes (synth :doc element) :newline t)))

(defprim ts-with-annotations (annotations expr &key (newline t))
  (:pretty () (list 'ts-with-annotation (list :annotations (synth-all :pretty annotations) :expr (synth :pretty expr))))
  (:typescript () (apply (if newline #'vcat #'hcat) (append* (synth-all :typescript annotations)
                                                                 (synth :typescript expr)))))

(defprim ts-annotation (name &rest props)
  (:pretty () (list 'ts-annotation (list name name :props (synth-plist :pretty props))))
  (:typescript () (vcat (text "@~a" (upper-camel name)) 
                        (parens
                         (braces 
                          (nest 4 (apply #'punctuate (comma) nil
                                         (synth-plist-merge 
                                          #'(lambda (pair) (hcat (text "~a: " (string-downcase (first pair)))
                                                                 (synth :typescript (second pair)))) 
                                          props))))))))

(defprim ts-annotation2 (name &optional content)
  (:pretty () (list 'ts-annotation2 (list name name :content (synth :pretty content))))
  (:typescript () (error "not implemented yet")))

(defprim ts-class (name &key public interfaces parent fields constructor methods)
  (:pretty () (list 'ts-class (list :name name 
                                    :public public
                                    :interfaces interfaces 
                                    :parent parent 
                                    :fields (synth-all :pretty fields) 
                                    :constructor (synth :pretty constructor)
                                    :methods (synth-all :pretty methods))))
  (:typescript () (vcat (hcat (text "export class ~a" (upper-camel name))
                              (if interfaces (hcat (text " implements ") 
                                                   (punctuate (comma) nil (mapcar (lambda (int) (text "~a" (upper-camel int)))
                                                                                  interfaces))))) 
                        (braces 
                         (nest 4 (apply #'vcat (apply #'postpend (semi) t 
                                                      (synth-all :typescript fields))
                                        (synth :typescript constructor)
                                        (synth-all :typescript methods)))
                         :newline t))))

(defprim ts-interface (name &key public interfaces methods)
  (:pretty () (list 'ts-interface (list :name name 
                                        :public public
                                        :interfaces interfaces 
                                        :methods (synth-all :pretty methods))))
  (:typescript () (error "not implemented yet")))

;; (defprim taglist (&rest tags)
;;   (:pretty () (list 'taglist (list :tags (synth-all :pretty tags))))
;;   (:doc () (apply #'doc:vcat (synth-all :doc (apply #'append* tags)))))



(defprim ts-statement (expression)
  (:pretty () (list 'ts-statement (list :expression expression)))
  (:typescript () (hcat (synth :typescript expression) (semi))))


(defprim ts-concat (&rest statements)
  (:pretty () (list 'ts-concat (list :statements (synth-all :pretty (apply #'append* statements))))) 
  (:typescript () (apply #'vcat (synth-all :typescript (apply #'append* statements)))))


(defprim ts-method (name parameters rtype &rest args)
  (:pretty () (list 'ts-method (list :name name 
                                     :parameters (synth-all :pretty parameters) 
                                     :rtype rtype
                                     :throws (synth-all :pretty (getf (rest-key args) :throws))
                                     :statements (synth-all :pretty (rest-plain args)))))
  (:typescript () (vcat (hcat name
                              (parens (apply #'punctuate (comma) t (synth-all :typescript parameters)))
                              (text ": ") 
                              (synth :typescript rtype)) 
                        (braces 
                         (nest 4 (apply #'postpend (semi) t 
                                        (synth-all :typescript (rest-plain args))))
                         :newline t))))

(defprim ts-signature (name parameters rtype)
  (:pretty () (list 'ts-signature (list :name name 
                                     :parameters (synth-all :pretty parameters) 
                                     :rtype rtype)))
  (:typescript () (error "not implemented")))

(defprim ts-import (name &rest elements)
  (:pretty () (list 'ts-import (list :name (synth :pretty name) 
                                     :elements elements)))
  (:typescript () (hcat (text "import ")
                        (if elements 
                            (hcat (braces (apply #'punctuate (text ", ") nil (mapcar #'text (mapcar #'upper-camel elements))) :padding 1)
                                  (text " from "))
                            (empty)) 
                        (text "'~a'" name)
                        (semi))))

(defprim ts-package (name)
  (:pretty () (list 'ts-package (list :name name)))
  (:typescript () (error "not foreseen")))

(defprim ts-assign (lhs rhs &key as)
  (:pretty () (list 'ts-assign (list :lhs lhs :rhs rhs)))
  (:typescript () (hcat (synth :typescript lhs)
                        (text " = ")
                        (synth :typescript rhs)
                        (if as (text " as ~a" (upper-camel as))))))

(defprim ts-new (type &rest parameters)
  (:pretty () (list 'ts-new (list :type (synth :pretty type) 
                                  :parameters (synth-all :pretty parameters))))
  (:typescript () (hcat (text "new ~a" (upper-camel type)) 
                        (parens (apply #'punctuate (comma) nil (synth-all :typescript parameters))))))

(defprim ts-call (name &rest args)
  (:pretty () (list 'ts-call (list :name name 
                                   :parameters (synth-all :pretty (rest-plain args))
                                   :as (getf (rest-key args) :as))))
  (:typescript () (hcat (text "~a" (lower-camel name))
                        (parens (apply #'punctuate (comma) nil (synth-all :typescript (rest-plain args))))
                        (aif (getf (rest-key args) :as)
                             (text " as ~a" (upper-camel it))
                             (empty)))))
(defprim ts-static (name)
  (:pretty () (list 'ts-static (list :name name)))
  (:typescript () (text "~a" (upper-camel name))))


(defprim ts-dynamic (name &key as)
  (:pretty () (list 'ts-dynamic (list :name name :as as)))
  (:typescript () (text "~a" (lower-camel name))))

(defprim ts-enum (name)
  (:pretty () (list 'ts-enum (list :name name)))
  (:typescript () (text "~a" (string-upcase name))))

(defprim ts-element (array index)
  (:pretty () (list 'ts-element (list :array array :index (synth :pretty index))))
  (:typescript () (hcat (text "~a" (lower-camel array))
                        (brackets (synth :typescript index)))))

(defprim ts-chain (&rest args)
  (:pretty () (list 'ts-chain (list :calls (synth-all :pretty (apply #'append* (rest-plain args)))
                                    :as (getf (rest-key args) :as))))
  (:typescript () (let* ((calls (synth-all :typescript (apply #'append* (rest-plain args))))
                         (as (getf (rest-key args) :as))
                         (chain (hcat (car calls) (apply #'prepend (dot) t (cdr calls)))))
                    (if as 
                        (parens (hcat (text "<~a>" (upper-camel as)) 
                                      chain))
                        chain))))

(defprim ts-constructor (parameters &rest statements)
  (:pretty () (list 'ts-constructor (list :parameters (synth-all :pretty parameters) 
                                          :statements (synth-all :pretty statements))))
  (:typescript () (vcat (hcat (text "constructor") 
                              (parens (apply #'punctuate (comma) nil (synth-all :typescript parameters)))) 
                        (braces 
                         (nest 4 (apply #'postpend (semi) t 
                                        (synth-all :typescript statements)))
                         :newline t))))

;; (defprim ts-arrow (parameters &rest statements)
;;   (:pretty () (list 'ts-arrow (list :parameters (synth-all :pretty parameters) 
;;                                     :statements (synth-all :pretty statements))))
;;   (:typescript () (hcat (parens (apply #'punctuate (comma) nil (synth-all :typescript parameters)))
;;                         (text " => ") 
;;                         (braces 
;;                          (nest 4 (apply #'postpend (semi) t 
;;                                         (synth-all :typescript statements)))
;;                          :newline t))))
(defprim ts-arrow (parameters expression)
  (:pretty () (list 'ts-arrow (list :parameters (synth-all :pretty parameters) 
                                    :expression (synth :pretty expression))))
  (:typescript () (hcat (parens (apply #'punctuate (comma) nil (synth-all :typescript parameters)))
                        (text " => ") 
                        (synth :typescript expression))))


(defprim ts-unit (name &rest elements)
  (:pretty () (list 'ts-unit (list :name name :elements (synth-all :pretty (apply #'append* elements)))))
  (:typescript () (apply #'vcat (synth-all :typescript (apply #'append* elements)))))

(defprim ts-return (&optional expression)
  (:pretty () (list 'ts-return (list :expression expression)))
  (:typescript () (hcat (text "return ")
                        (synth :typescript expression))))

(defprim ts-if (expression success &optional failure)
  (:pretty () (list 'ts-if (list :expression expression :success success :failure failure)))
  (:typescript () (error "not implemented yet")))

(defprim ts-switch (expression &rest cases)
  (:pretty () (list 'ts-switch (list :expression expression 
                                     :cases (synth-all :pretty (rest-plain cases)) 
                                     :default (getf (rest-key cases) :default))))
  (:typescript () (error "not implemented yet")))

(defprim ts-break ()
  (:pretty () (list 'ts-break))
  (:typescript () (error "not implemented yet")))

(defprim ts-case (expression statement)
  (:pretty () (list 'ts-case (list :expression expression :statement statement)))
  (:typescript () (error "not implemented yet"))) 

(defmacro defop (name)
  `(defprim ,(symb "TS-" name) (op1 op2)
     (:pretty () (list ',(symb "TS-" name) (list :op1 op1 :op2 op2)))
     (:typescript () (error "not implemented yet"))))


(defop +)
(defop -)
(defop *)
(defop /)

;; (defprim ts-equal (op1 op2)
;;      (:pretty () (list 'ts-equal (list :op1 op1 :op2 op2)))
;;      (:typescript () (error "not implemented yet"))
;;      )
;;                      (text " == ")
;;                      )
(defun ts-null (item)
  (ts-equal item (ts-nil)))

(defprim ts-nil ()
     (:pretty () (list 'ts-null))
     (:typescript () (error "not implemented yet")))

(defprim ts-throw (exception)
     (:pretty () (list 'ts-throw (list :exception exception)))
     (:typescript () (error "not implemented yet")))


(defun ts-field-with-accessors (annotations name type)
  (ts-concat (ts-with-annotations annotations
                                      (ts-statement (ts-pair name type :private t)))
               (ts-method (doc:text "get~a" (upper-camel name)) nil type
                            (ts-return (ts-chain (ts-dynamic 'this) (ts-dynamic name))))
               (ts-method (doc:text "set~a" (upper-camel name))
                            (list (ts-pair name type)) (ts-primitive-type 'void)
                            (ts-statement (ts-assign (ts-chain (ts-dynamic 'this) 
                                                                     (ts-dynamic name))
                                                         (ts-dynamic name))))))


(defmacro defbexp (operator &optional representation (arity 0))
  (let ((name (symb "TS-" operator)))
    `(defprim ,name 
         ,(if (eq arity 'unbounded)
              `(&rest exps)
              (loop for i from 1 to arity collect (symb "EXP" i)))
       (:pretty () (list ',name 
			 ,(if (eq arity 'unbounded)
			      `(list :exps (synth-all :pretty exps))
			      `(list ,@(apply #'append (loop for i from 1 to arity collect (list (keyw "EXP" i) `(synth :pretty ,(symb "EXP" i)))))))))
       (:typescript () (error "not implemented yet")))))


(defmacro defbexps (&rest bexps)
  `(progn
     ,@(mapcar #'(lambda (bexp)
		   `(defbexp ,(car bexp) ,@(cdr bexp)))
	       bexps)))

;;(def-bexp true)
;; (def-bexp equal 2)

(defbexps (true '|true|) (false '|false|) (and '&& 2) (or '\|\| 2) (not '! 1) (equal '== 2) (less-than '< 2) (greater-than '> 2))
