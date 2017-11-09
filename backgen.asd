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
               (:module "lang"
                        :serial t
                        :components ((:file "html")
                                     (:file "xml")
                                     (:file "java")
                                     (:file "sql")
                                     (:file "typescript")
                                     (:file "latex")))
               (:file "expression")
               (:file "type")
               ;; (:file "validator") 
               (:file "url")
               (:file "query")
               (:file "documentation")
               (:module "data"
                        :serial t
                        :components ((:file "json")
                                     (:file "jsonschema")
                                     (:file "entity")
                                     (:file "dao")
                                     (:file "dto")
                                     (:file "data")
                                     ))
               (:module "server"
                        :serial t
                        :components ((:file "error")
                                     (:file "rest")
                                     (:file "ejb")
                                     (:file "logic"))) 
               ;; (:module "gui"
               ;;          :serial t
               ;;          :components ((:file "common")
               ;;                       (:file "input")
               ;;                       (:file "button")
               ;;                       (:file "link")
               ;;                       (:file "navbar")
               ;;                       (:file "vert")
               ;;                       (:file "horz")
               ;;                       (:file "label")
               ;;                       (:file "panel")
               ;;                       (:file "listing")
               ;;                       (:file "table")
               ;;                       (:file "description")
               ;;                       (:file "abst")
               ;;                       (:file "static")
               ;;                       (:file "dynamic")
               ;;                       (:file "alt")
               ;;                       (:file "form")))

               (:file "conf")
               (:file "test")
               (:module "aia"
                        :serial t
                        :components ((:file "entities")
                                     (:file "queries")
                                     (:file "errors")
                                     (:file "formats")
                                     (:file "services")
                                     (:file "database")
                                     (:file "tests")))
               
               ;; (:module "app"
               ;;          :serial t
               ;;          :components ((:file "entities")
               ;;                       (:file "queries")
               ;;                       (:file "errors")
               ;;                       (:file "formats")
               ;;                       (:file "services")
               ;;                       (:file "database")))
              
               (:file "backgen")
               ;; (:file "guigen")
               ))
