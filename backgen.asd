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
               (:file "type")
               (:file "expression")
               ;; (:file "validator")
               
               (:file "url")
               (:file "query") 
               (:module "data"
                        :serial t
                        :components ((:file "json")
                                     (:file "jsonschema")
                                     (:file "entity")
                                     (:file "dao")
                                     (:file "dto")
                                     ))
               
               
               
               (:module "server"
                        :serial t
                        :components ((:file "error")
                                     (:file "rest")
                                     (:file "ejb")
                                     (:file "logic")))
               (:file "latex")
               (:file "test")
               (:file "backgen")
               ))
