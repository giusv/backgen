;;;; package.lisp 
;; (push #p"d:/giusv/lisp/nextent/" asdf:*central-registry*)

(defpackage :lol
  (:use :cl)
  (:export :this :it
           :aif 
           :pandoriclet :get-pandoric  :dlambda :flatten :group :mkstr :symb :keyw))
(defpackage :utils
  (:use :cl :lol)
  (:export :random-number :random-string :random-boolean :random-unique :random-date :random-from
           :rest-key :rest-plain
           :plist-keys :plist-values
           :plist-p
           :singular
           :bindall
           :glue
           :lower :upper
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
           :parens :brackets :braces :single-quotes :double-quotes :back-quotes :angular :vbars
           :comma :dot :semi :colon :forward-slash :equals :blank
           :punctuate :prepend :postpend
           :textify))

(defpackage :documentation
  (:use :cl :lol :utils :grammar)
  (:export :defdoc :get-documentation))

(defpackage :html
  (:use :cl :lol :utils :grammar)
  (:export :tag 
           :taglist
           :span-color
           :html :head :title :meta :link :body :h1 :h2 :h3 :h4 :h5 :div :span :li :dl :dt :dd :ul :ol :pre :i 
           :strong :code :script
           :table :thead :tbody :tr :th :td
           ;; :section
           :article :aside :p :a
           :button :input :textarea
           :label
           :form
           :nav))

(defpackage :xml
  (:use :cl :lol :utils :grammar)
  (:export :node :simple))


(defpackage :latex
  (:use :cl :lol :utils :doc :grammar)
  (:export :line :normal :section
           :emph :bold :verbatim
           :doc
           :litem :itemize :enumerate :outline% :outline
           :document
           :paragraph
           :seq
           :tabular :row))

(defpackage :expr
  (:use :cl :lol :utils :grammar :latex)
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
           :java-concat :java-unit :java-template :java-assign :java-return :java-assert :java-throw :java-statement
           :java-null :java-nil 
           :java-try :java-catch% :java-catch
           :java-switch :java-case :java-break
           :java-if :java-for :java-foreach
           :java-+ :java-- :java-* :java-/
           :java-or :java-and :java-not
           :java-true :java-false
           :java-equal :java-greater-than :java-less-than
           :java-field-with-accessors))

(defpackage :typescript
  (:use :cl :lol :utils :grammar :doc)
  (:export :ts-empty :ts-comment :ts-const 
           :ts-type :ts-primitive-type :ts-array-type :ts-object-type :ts-template-type :ts-wildcard-type
           :ts-pair :ts-array :ts-element
           :ts-object :ts-annotation :ts-annotation2 :ts-with-annotations :ts-class :ts-interface :ts-method :ts-signature 
           :ts-import :ts-package :ts-new :ts-call :ts-static :ts-dynamic :ts-enum :ts-chain :ts-constructor :ts-arrow 
           :ts-concat :ts-unit :ts-template :ts-assign :ts-return :ts-throw :ts-statement
           :ts-null :ts-nil 
           :ts-try :ts-catch% :ts-catch
           :ts-switch :ts-case :ts-break
           :ts-if
           :ts-+ :ts-- :ts-* :ts-/
           :ts-or :ts-and :ts-not
           :ts-true :ts-false
           :ts-equal :ts-greater-than :ts-less-than
           :ts-field-with-accessors))

(defpackage :sql
  (:use :cl :lol :utils :grammar :doc)
  (:export :sql-const :sql-insert :sql-select :sql-concat :sql-create-sequence))

(defpackage :url
  (:use :cl :lol :utils :parser :grammar :doc :java)
  (:export :void :chunk :path-parameter :query-parameter :login-parameter 
           :backward-chain :multi :forward-chain :queried :protocolled
           :url :parse-url))

(defpackage :type
  (:use :cl :lol :utils :parser :grammar :java :typescript :latex) 
  (:export :integer-type
           :boolean-type
           :character-type
           :string-type
           :date-type
           :entity-type
           :transfer-type
           :format-type 
           :collection-type
           :parametric-type
           :function-type
           :response-type))


(defpackage :data
  (:use :cl :lol :utils :parser :grammar :java :typescript :latex :html :type) 
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
           ;; :with-fields
           ;; :concat% :concat
           ;; :create-entity% :create-entity 
           ;; :update-entity% :update-entity
           ;; :exec-query% :exec-query
           ;; :find-entity% :find-entity
           ;; :create-transfer% :create-transfer
           ;; :mu% :mu :mapcomm% :mapcomm :fork
           ;; :respond
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
           :bl-exception :bl-exception-instance :bl-bad-request-exception
           :stateless-ejb
           :ejb-method
           ;; :ejb-parameter
           :generate-ejb))

;; (defpackage :validator
;;   (:use :cl :lol :utils :grammar :java)
;;   (:export :required))


(defpackage :gui
  (:use :cl :lol :utils :grammar :typescript :latex :documentation)
  (:export :defelem 
           :*elements*
           :input :button
           :vert :vert*
           :horz :horz*
           :abst
           :static :static%
           :dynamic :dynamic%
           :label
           :listing :listing%
           :alt
           :form% :form :bnd :obj% :obj :arr% :arr
           :table :table%
           :description :description%
           :panel
           :link :navbar))
(defpackage :pgen
  (:use :cl :lol :utils :doc :grammar :java))

(defpackage :test
  (:use :cl :lol :utils :doc :grammar :parser :java :type :data :sql :url)
  (:export :defdb :*database* :deftest :defsuite :*tests* :*suites*
           :tl-forall :tl-exists :l-and :tl-retrieve :tl-get :tl-ddl :tl-range :tl-random-timestamp :tl-timestamp :tl-and
           :tl-variab :tl-lambda% :tl-call :tl-binding :tl-let% :tl-let
           :tl-test :tl-test-instance :tl-seq% :tl-test-binding :tl-ensure
           :tl-require :tl-equal :tl-invoke-service :tl-suite :tl-db% 
           :tl-http-get :tl-http-status :tl-http-body))

(defpackage :documentation
     (:use :cl :lol :utils :doc :grammar :latex))

(defpackage :conf
  (:use :cl :lol :utils :doc :server :grammar :java :type :typescript :data :latex :gui :test :documentation :xml)
  (:export :pom :web :beans :persistence))

(defpackage :backgen
  (:use :cl :lol :utils :doc :server :grammar :java :type :typescript :data :latex :gui :test :sql :documentation :conf :url))


