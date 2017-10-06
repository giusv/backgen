(in-package :latex)

(defun with-env (name param &rest contents)
  (vcat (text "\\begin{~a}~@[{~a}~]" (string-downcase name) param)
        (apply #'vcat (append* contents))
        (text "\\end{~a}" (string-downcase name))))


(defprim doc (doc)
  (:pretty () (list 'doc (list :doc (synth :pretty doc)))))

(defprim normal (template &rest args)
  (:pretty () (list 'normal (list :template template :args (synth :pretty args))))
  (:doc () (apply #'text template args)))

(defprim paragraph (&rest contents)
  (:pretty () (list 'paragraph (list :contents (synth-all :pretty contents))))
  (:doc () (hcat (apply #'vcat (synth-all :doc contents)) 
                 (text "\\\\"))))

(defprim emph (content)
  (:pretty () (list 'emph (list :content (synth :pretty content))))
  (:doc () (hcat (text "\\emph") (brackets (synth :doc content)))))

(defprim bold (content)
  (:pretty () (list 'bold (list :content (synth :pretty content))))
  (:doc () (hcat (text "\\bfseries") (brackets (synth :doc content)))))

(defprim verbatim (content)
  (:pretty () (list 'verbatim (list :content (synth :pretty content))))
  (:doc () (hcat (text "\\texttt") (brackets (synth :doc content)))
        ;; (hcat (text "\\verb") (vbars (synth :doc content)))
        ))

(defprim seq (&rest elements)
  (:pretty () (list 'seq (list :elements (synth-all :pretty (apply #'append* elements)))))
  (:doc () (apply #'vcat (synth-all :doc (apply #'append* elements)))))

(defprim section (title &rest contents)
  (:pretty () (list 'section (list :title title :contents (synth-all :pretty (apply #'append* contents)))))
  (:doc () (apply #'vcat 
                  (hcat (text "\\section")
                        (brackets (synth :doc title)))
                  (synth-all :doc (apply #'append* contents)))))

(defprim tabular (&rest rows)
  (:pretty () (list 'tabular (list :rows (synth-all :pretty rows))))
  (:doc () (with-env 'tabular (apply #'symb (interleave (make-list (apply #'max (synth-all :size rows))
                                                                   :initial-element '|c|) '|\||))
                     (apply #'punctuate (text "~%\\hline") t (synth-all :doc rows)))))

(defprim litem (name &rest contents)
  (:pretty () (list 'litem (list :name name :contents (synth :pretty (append* contents)))))
  (:doc () (hcat+ (text "\\item~@[[~a]~]" name)
                  (apply #'vcat (synth-all :doc (apply #'append* contents))))))
(defprim itemize (&rest items)
  (:pretty () (list 'itemize (list :items (synth-all :pretty (apply #'append* items)))))
  (:doc () (with-env 'itemize nil (synth-all :doc (mapcar (lambda (i) (litem nil i))
                                                          (apply #'append* items))))))

(defprim enumerate (&rest items)
  (:pretty () (list 'enumerate (list :items (synth-all :pretty (apply #'append* items)))))
  (:doc () (with-env 'enumerate nil (synth-all :doc (mapcar (lambda (i) (item nil i))
                                                          (apply #'append* items))))))

(defprim outline% (&rest items)
  (:pretty () (list 'outline (list :items (synth-all :pretty (apply #'append* items)))))
  (:doc () (with-env 'outline nil (synth-all :doc (apply #'append* items)))))

(defmacro outline (&rest items)
  `(outline% ,@(mapcar (lambda (i) `(item ',(car i) ,(cdr i)))
                          items)))

(defprim row (&rest cols)
  (:pretty () (list 'row (list :cols (synth-all :pretty cols))))
  (:size () (length cols))
  (:doc () (hcat+ (apply #'punctuate (text " & ") nil (synth-all :doc cols)) (text "\\\\"))))

(defun document (title author &rest contents)
  (vcat (text "\\documentclass[12pt]{article}") 
        (text "\\title{~a}" title)
        (text "\\author{~a}" author)
        (with-env 'document nil
          (text "\\maketitle")
          (synth-all :doc contents))))
  
;; (synth :output (document 'title 'author
;;                          (section 'section
;;                                   (paragraph (normal "hello ")
;;                                              (normal "world!"))
;;                                   (tabular (row (normal "name") (normal "surname") (normal "address"))
;;                                          (row (normal "a") (normal "b") (normal "b"))
;;                                          (row (normal "c") (normal "d")))
;;                                   (itemize (normal "a")
;;                                            (normal "b"))
;;                                   (outline (a . (normal "a"))
;;                                                (b . (normal "b"))))) 0)
