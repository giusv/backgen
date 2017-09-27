;;;; package.lisp 
;; (push #p"d:/giusv/lisp/nextent/" asdf:*central-registry*)

(defpackage :lol
  (:use :cl)
  (:export :this :it
           :aif 
           :pandoriclet :get-pandoric  :dlambda :flatten :group :mkstr :symb :keyw))
(defpackage :utils
  (:use :cl :lol)
  (:export :random-number :random-string :random-boolean
           :rest-key :rest-plain
           :plist-keys :plist-values
           :plist-p
           :singular
           :bindall
           :glue
           :lower
           :lower-camel :upper-camel
           :split-str :interleave
           :append* 
           :write-file
           :my-debug
           :hash-table-keys
           :hash-table-values
           :overlaps
           :with-multiple-value-bindings))

(defpackage :parser
  (:use :cl :lol :utils)
  (:export :tuple :result :apply-parser :parse :bind :fail :item :do-with :sat :sym :choose :choose-among :zero :plus :choice 
           :many :many1 :sepby :sepby1 :sublist :pair :optional :atomic :var-init :req-var :opt-var :lambda-list :arg-names)) 


(defpackage :grammar
  (:use :cl :lol :utils)
  (:export :defprim :defprod
           :synth :synth-all :synth-plist :synth-plist-merge))

(defpackage :doc
  (:use :cl :lol :utils :grammar)
  (:export :empty :text :nest :vcat :hcat :hcat+
           :parens :brackets :braces :single-quotes :double-quotes :back-quotes :angular
           :comma :dot :semi :colon :forward-slash :equals :blank
           :punctuate :prepend :postpend
           :textify))

(defpackage :html
  (:use :cl :lol :utils :grammar)
  (:export :tag 
           :taglist
           :span-color
           :html :head :title :meta :link :body :h1 :h2 :h3 :h4 :h5 :div :span :li :dl :dt :dd :ul :ol :pre :i 
           :strong :code :script
           :table :thead :tbody :tr :th :td
           :section :article :aside :p :a
           :button :input :textarea
           :label
           :form
           :nav))

(defpackage :expr
  (:use :cl :lol :utils :grammar)
  (:export :const :attr :variab :value :param
           :+true+ :+false+ :+and+ :+or+ :+not+ :+equal+ :+less-than+ :+greater-than+ :+null+
           :cat
           ))


(defpackage :java
  (:use :cl :lol :utils :grammar :doc)
  (:export :java-empty :java-comment :java-const 
           :java-type :java-primitive-type :java-array-type :java-object-type :java-template-type :java-wildcard-type
           :java-pair :java-array :java-element
           :java-object
           :java-annotation :java-with-annotations :java-class :java-interface :java-method :java-signature 
           :java-import :java-package :java-new :java-call :java-static :java-dynamic :java-enum :java-chain :java-constructor :java-arrow 
           :java-concat :java-unit :java-template :java-assign :java-return :java-throw :java-statement
           :java-null :java-nil 
           :java-try :java-catch% :java-catch
           :java-switch :java-case :java-break
           :java-if :java-for :java-foreach
           :java-+ :java-- :java-* :java-/
           :java-or :java-and :java-not
           :java-true :java-false
           :java-equal :java-greater-than :java-less-than
           :java-field-with-accessors))

(defpackage :url
  (:use :cl :lol :utils :parser :grammar :doc :java)
  (:export :void :static-chunk :dynamic-chunk :expression-chunk :path-parameter :query-parameter :login-parameter 
           :backward-chain :multi :forward-chain :queried
           :url))

(defpackage :type
  (:use :cl :lol :utils :parser :grammar :java) 
  (:export :integer-type
           :boolean-type
           :string-type
           :entity-type
           :transfer-type
           :format-type
           :collection-type
           :parametric-type
           :function-type))


(defpackage :data
  (:use :cl :lol :utils :parser :grammar :java :html :type) 
  (:export :with-data :with-data%
           :remote
           :rand
           :jnull :jbool :jnumber :jstring :jobject :jarray
           :jsbool :jsnumber :jsstring :jsprop :jsobject :jsarray
           :filter :ident :prop :elem :comp
           :atype :attribute :primary-key :entity :relationship
           :defent :defrel :deformat :defquery
           :query :relation :product :project :restrict :equijoin :with-queries 
           :named-query
           :defdao :dao-query
           :generate-dao
           :dto
           :*entities* :*relationships* :*formats* :*queries* :*daos*))




(defpackage :latex
  (:use :cl :lol :utils :doc :grammar)
  (:export :line :normal :section
           :item :itemize :enumerate :description% :description
           :document
           :paragraph
           :seq
           :table :row))

(defpackage :server
  (:use :cl :lol :utils :grammar :type :java :latex)
  (:export :defresource
           :*resources*
           :defservice
           :*services*
           :deferror
           :*errors*
           :rest-service 
           :rest-collection
           :rest-singleton
           :rest-item% :rest-item
           :rest-get% :rest-get
           :rest-post% :rest-post
           :rest-put :rest-delete
           :with-fields
           :concat% :concat
           :empty :create-entity% :create-entity 
           :update-entity% :update-entity
           :exec-query% :exec-query
           :find-entity% :find-entity
           :create-transfer% :create-transfer
           :mu% :mu :mapcomm% :mapcomm :fork
           :respond
           :bl-let% :bl-let
           :bl-create-entity% :bl-create-entity :bl-find-entity :bl-delete-entity
           :bl-lambda% :bl-lambda
           :bl-arg :bl-call
           :bl-exec-query
           :bl-null
           :bl-get
           :bl-unless% :bl-unless
           :bl-condition
           :bl-variab
           :bl-cat
           :bl-error :bl-error-instance :bl-bad-request-error
           :stateless-ejb
           :ejb-method
           ;; :ejb-parameter
           :generate-ejb))

;; (defpackage :validator
;;   (:use :cl :lol :utils :grammar :java)
;;   (:export :required))



(defpackage :backgen
  (:use :cl :lol :utils :doc :grammar :java :type :data))


