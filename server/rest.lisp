(in-package :server)

(defparameter *resources* (make-hash-table))
(defmacro defresource (name resource)
  `(progn (defparameter ,name ,resource) 
         (setf (gethash ',name *resources*) ,name)))

(defparameter *services* (make-hash-table))
(defmacro defservice (name service)
  `(progn (defparameter ,name ,service) 
         (setf (gethash ',name *services*) ,name)))

(defprim rest-service (name url &rest resources)
  (:pretty () (list 'rest-service (list :name name :url (synth :pretty url) :resources (synth-all :pretty resources))))
  (:java-implementation (package) 
                   (java-unit name
                              (java-package (symb package '|.service|))
                              (java-import '|javax.ws.rs| '|*|)
                              (java-import '|javax.naming| '|Context| '|InitialContext| '|NamingException|)
                              (java-import (symb package '|.exception|) '|*|)
                              (java-import '|javax.ejb| '|EJB| '|LocalBean| '|Stateless|)
                              (java-import '|javax.ws.rs.core| '|Response|)
                              (java-import '|javax.ws.rs.core.Response| '|ResponseBuilder|)
                              (java-import '|java.net| '|URI| '|URISyntaxException|)
                              (java-import (symb package '|.ejb|) '|*|)
                              (java-import (symb package '|.vo|) '|*|)
                              (java-with-annotations 
                               (list (java-annotation '|Path| (java-const (synth :string (synth :url url))))
                                     (java-annotation '|LocalBean|)
                                     (java-annotation '|Stateless|))
                               (java-class name :public t
                                           :fields (list (java-with-annotations 
                                                          (list (java-annotation '|EJB|))
                                                          (let ((bean-name  (symb name "-E-J-B")))
                                                            (java-statement (java-pair bean-name 
                                                                                       (java-object-type bean-name) :private t )))))
                                           :methods (apply #'append (synth-all :java-implementation resources name url)))))))

(defprim rest-singleton (name actions)
  (:pretty () (list 'rest-singleton (list :name name :actions (synth-all :pretty actions))))
  (:java-implementation (bean path)  (let* ((chunk (url:chunk name))
                                       (newpath (url:backward-chain chunk path)))
                                  (synth-all :verb actions bean newpath chunk)))
  (:ejb-methods (path) (let* ((chunk (url:chunk name))
                               (newpath (url:backward-chain chunk path)))
                          (synth-all :ejb-method actions newpath chunk))))

(defprim rest-collection (name actions &rest resources)
  (:pretty () (list 'rest-collection (list :name name :resources (synth-all :pretty resources) 
                                           :actions (synth-all :pretty actions))))
  (:java-implementation (bean path)  (let* ((chunk (url:chunk name))
                                       (newpath (url:backward-chain chunk path)))
                                  (apply #'append (synth-all :verb actions bean newpath chunk)
                                         (synth-all :java-implementation resources bean newpath))))
  (:ejb-methods (path) (let* ((chunk (url:chunk name))
                               (newpath (url:backward-chain chunk path)))
                          (append (synth-all :ejb-method actions newpath chunk)
                                  (apply #'append (synth-all :ejb-methods resources newpath))))))

;; (defmacro rest-collection (name actions &rest resources)
;;   `(rest-collection% ,name ,actions ,@resources))

(defprim rest-item% (name param actions &rest resources)
  (:pretty () (list 'rest-item (list :name name :param (synth :pretty param) :resources (synth-all :pretty resources) 
                                     :actions (synth-all :pretty actions))))
  (:java-implementation (bean path) (let* ((chunk param)
                                      (newpath (url:backward-chain chunk path)))
                                 (apply #'append (synth-all :verb actions bean newpath chunk)
                                        (synth-all :java-implementation resources bean newpath))))
  (:ejb-methods (path) (let* ((chunk param)
                               (newpath (url:backward-chain chunk path)))
                          (append (synth-all :ejb-method actions newpath chunk)
                                  (apply #'append (synth-all :ejb-methods resources newpath))))))

(defmacro rest-item (name (param) actions &rest resources)
  `(let ((,(car param) ,(cadr param)))
     (rest-item% ,name ,(car param) ,actions ,@resources)))



(defun parlist (type pars)
  (mapcar (lambda (par)
            (java-with-annotations (list (java-annotation type (java-const (synth :string (doc:text "~a" (lower-camel par))))))
                                 (java-pair (lower-camel par) (java-type 'String)) :newline nil)) 
          pars))

(defmacro with-lookup (bean-name body)
  `(java-try (java-concat
              (java-statement (java-pair 'context (java-type 'context) :init (java-new (java-object-type 'initial-context))))
              (java-statement (java-pair ,bean-name (java-type ,bean-name) :init (java-chain (java-dynamic 'context) 
                                                                                             (java-call 'lookup (java-const (mkstr "java:module/" (upper-camel ,bean-name)) )) :as (java-object-type ,bean-name))))
              ,body)
             (list (java-catch (e :naming-exception) 
                               (java-concat (java-statement (java-chain (java-dynamic e)
                                                                        (java-call 'print-stack-trace)))
                                            (java-return (java-chain (java-static 'response)
                                                                     (java-call 'status (java-const 500))
                                                                     (java-call 'build))))))))

(defprim rest-get% (queries action &key (mtypes (list '|application/json|)))
  (:pretty () (list 'rest-get (list :queries (synth-all :pretty queries) :action (synth :pretty action) :mtypes mtypes)))
  (:verb (bean path chunk) 
         (java-with-annotations
          (list (java-annotation '|GET|)
                (java-annotation '|Path| (java-const (synth :string (synth :url path))))
                (if mtypes 
                    (java-annotation '|Produces| 
                                     (apply #'java-array (mapcar 
                                                          (lambda (type) (java-const (mkstr type))) 
                                                          mtypes)))))
          (java-method (symb "GET-" (synth :name chunk))
                       (synth-all :declaration (append queries (synth :path-parameters path)) t)
                       (java-object-type 'response)
                       (let* ((bean-name (symb bean "-E-J-B"))
                              (ret (java-return (java-chain (java-static 'response)
                                                            (java-call 'ok  
                                                                       (java-chain (java-dynamic bean-name)  
                                                                                   (apply #'java-call (symb 'retrieve "-" (synth :name chunk))
                                                                                          ;; (mapcar #'java-dynamic (append queries (synth :path-parameters path)))
                                                                                          (synth-all :java-implementation (append queries (synth :path-parameters path)) #'identity))))
                                                            (java-call 'build)))))
                         (with-lookup bean-name
                           (aif (synth :errors action)
                                (java-try ret
                                          (mapcar (lambda (err)
                                                    (java-catch% (list (synth :name err)) 
                                                                 (synth :name err)
                                                                 (synth :call (synth :parent err) (java-chain (java-dynamic (synth :name err))
                                                                                                              (java-call 'get-message)))))
                                                  it))
                                ret))))))
  (:ejb-method (path chunk)
               (ejb-method (symb 'retrieve "-" (synth :name chunk))
                           (synth-all :declaration (append queries (synth :path-parameters path)))
                           action)
               ;; (java-method (doc:text "retrieve~a" (upper-camel (synth :name chunk)))
               ;;              (synth-all :declaration (append queries (synth :path-parameters path)))
               ;;              (cond ((eq type 'single) 
               ;;                     (java-object-type (symb (synth :name chunk) "-J-T-O")))
               ;;                    ((eq type 'collection) 
               ;;                     (java-array-type (java-object-type (symb (singular (synth :name chunk)) "-J-T-O")))))
               ;;              (synth :logic action))
               ))

;; (defmacro rest-get ((&rest queries) action &key mtypes)
;;   `(let ,(mapcar #'(lambda (query) 
;;                      `(,query ',query))
;;                  queries)
;;      (rest-get% (list ,@queries) ,action ,@(if mtypes `(:mtypes ,mtypes)))))

(defmacro rest-get ((&rest queries) action &key mtypes)
  `(let ,(mapcar #`(,(car a1) ,(cadr a1)) queries)
     (rest-get% (list ,@(mapcar #'car queries)) ,action ,@(if mtypes `(:mtypes ,mtypes)))))


(defprim rest-post% (format action &key (mtypes (list '|application/json|)))
  (:pretty () (list 'rest-post (list :format format :action (synth :pretty action) :mtypes mtypes)))
  (:verb (bean path chunk) 
         (java-with-annotations 
          (list (java-annotation '|POST|)
                (java-annotation '|Path| (java-const (synth :string (synth :url path))))
                (if mtypes 
                    (java-annotation '|Consumes|  
                                     (apply #'java-array (mapcar 
                                                          (lambda (type) (java-const (mkstr type))) 
                                                          mtypes)))))
          (java-method (symb "POST-" (singular (synth :name chunk))) 
                       (append* (synth-all :declaration (synth :path-parameters path) t)
                                (java-pair (synth :name format) (synth :java-type (synth :type format))))
                       (java-type 'response)
                       (let* ((bean-name (symb bean "-E-J-B")))
                         (java-try (java-return 
                                    (java-chain (java-static 'response)
                                                (java-call 'created
                                                           (java-new (java-object-type 'u-r-i) 
                                                                     (reduce #'java-+ 
                                                                             (list (java-const (synth :string (synth :url path)))
                                                                                   (java-const "/")
                                                                                   (java-chain (java-dynamic bean-name) 
                                                                                               (apply #'java-call (symb 'add "-" (singular (synth :name chunk)))
                                                                                                      (append*  (synth-all :java-implementation (synth :path-parameters path) #'identity)
                                                                                                                (java-dynamic (synth :name format))))
                                                                                               (java-call 'get-id)))
                                                                             )))
                                                (java-call 'build)))
                                   
                                   (list (java-catch (e :u-r-i-syntax-exception)  
                                                     (java-concat (java-statement (java-chain (java-dynamic e)
                                                                                              (java-call 'print-stack-trace)))
                                                                  (java-return (java-chain (java-static 'response)
                                                                                           (java-call 'status (java-const 500))
                                                                                           (java-call 'build)))))))))))
  (:ejb-method (path chunk) 
               (ejb-method (symb 'add "-" (singular (synth :name chunk)))
                           (append* (synth-all :declaration (synth :path-parameters path))
                                    (java-pair (synth :name format) (synth :java-type (synth :type format))))
                           action)))
  ;; (:bean-method (path chunk type)
  ;;               (let ((name (symb (synth :name format) "-J-T-O"))) 
  ;;                 (java-method (doc:text "add~a" (upper-camel (singular (synth :name chunk))))
  ;;                            (append* (synth-all :declaration (synth :path-parameters path))
  ;;                                     (java-pair (synth :name format) (java-type name)))
  ;;                            (java-type 'string)
  ;;                            (synth :logic action))))

(defmacro rest-post (format (&rest fields) action &key mtypes)
  `(with-fields ,fields ,format 
     (rest-post% ,format ,action ,@(if mtypes `(:mtypes ,mtypes)))))

(defprim rest-put (format action &key (mtypes (list '|application/json|)))
  (:pretty () (list 'rest-put (list :format format :action (synth :pretty action) :mtypes mtypes)))
  (:verb (bean path chunk)
         (java-with-annotations (list (java-annotation '|PUT|)
                                      (java-annotation '|Path| (java-const (synth :string (synth :url path))))
                                      (if mtypes 
                                          (java-annotation '|Consumes|  
                                                           (apply #'java-array (mapcar 
                                                                                (lambda (type) (java-const (mkstr type))) 
                                                                                mtypes)))))
                                (let ((name (symb (synth :name format) "-J-T-O"))) 
                                  (java-method (symb "PUT-" (synth :name chunk)) 
                                               (append* (synth-all :declaration (synth :path-parameters path) t)
                                                        (java-pair (synth :name format) (java-type name)))
                                               (java-type 'response)
                                               (let* ((bean-name (symb bean "-E-J-B")))
                                                 (java-statement (java-chain (java-dynamic bean-name) 
                                                                             (apply #'java-call (symb 'update "-" (synth :name chunk))
                                                                                    (append*  (synth-all :java-implementation (synth :path-parameters path) #'identity)
                                                                                              (java-dynamic (synth :name format)))))))))))
  (:ejb-method (path chunk) 
               (ejb-method (symb "PUT-"(synth :name chunk))
                           (append* (synth-all :declaration (synth :path-parameters path))
                                    (java-pair (synth :name format) (synth :type format)))
                           action))
  ;; (:bean-method (path chunk type) (let ((name (symb (synth :name format) "-J-T-O"))) 
  ;;                                   (java-method (doc:text "update~a" (upper-camel (synth :name chunk)))
  ;;                                                (append* (synth-all :declaration (synth :path-parameters path))
  ;;                                                         (java-pair (synth :name format) (java-type name)))
  ;;                                                (java-type :void)
  ;;                                                (synth :logic action))))
  )
