;;;; backgen.asd

(asdf:defsystem :backgen
  :serial t
  :description "Backgen is a JEE code generator"
  :author "Giuseppe Viola gius.viola@gmail.com"
  :license "Specify license here"
  :components ((:file "package")
               (:file "lol")
               (:file "utils")
               (:file "parser")
               (:file "grammar")
               (:file "doc")
               (:file "html")
               (:module "lang"
                        :serial t
                        :components ((:file "java")
                                     ;; (:file "sql")
                                     ))
               (:file "expression")
               (:file "type")
               ;; (:file "validator")
               
               (:file "url")
               (:file "query") 
               (:module "data"
                        :serial t
                        :components ((:file "json")
                                     (:file "jsonschema")
                                     (:file "entity")
                                     (:file "dao")
                                     ))
               
               
               
               (:module "server"
                        :serial t
                        :components ((:file "rest")
                                     (:file "ejb")
                                     (:file "logic")))
               (:file "latex")
               (:file "backgen")
               ))
