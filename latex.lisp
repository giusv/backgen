(in-package :latex)

(defun with-env (name param &rest contents)
  (vcat (text "\\begin{~a}~@[{~a}~]" (string-downcase name) param)
        (apply #'vcat (append* contents))
        (text "\\end{~a}" (string-downcase name))))

(defprim normal (template &rest args)
  (:pretty () (list 'line (list :template template :args (synth :pretty args))))
  (:doc () (apply #'text template args)))

(defprim paragraph (&rest contents)
  (:pretty () (list 'paragraph (list :contents (synth-all :pretty contents))))
  (:doc () (hcat (apply #'vcat (synth-all :doc contents)) 
                 (text "\\\\"))))

(defprim seq (&rest elements)
  (:pretty () (list 'seq (list :elements (synth-all :pretty elements))))
  (:doc () (apply #'vcat (synth-all :doc elements))))

(defprim section (title &rest contents)
  (:pretty () (list 'section (list :title title :contents (synth-all :pretty (apply #'append* contents)))))
  (:doc () (apply #'vcat (text "\\section{~a}" (string-downcase title))
                  (synth-all :doc (apply #'append* contents)))))

(defprim table (&rest rows)
  (:pretty () (list 'table (list :rows (synth-all :pretty rows))))
  (:doc () (with-env 'tabular (apply #'symb (interleave (make-list (apply #'max (synth-all :size rows))
                                                                   :initial-element '|c|) '|\||))
                     (apply #'punctuate (text "~%\\hline") t (synth-all :doc rows)))))

(defprim item (name &rest contents)
  (:pretty () (list 'item (list :name name :contents (synth :pretty (append* contents)))))
  (:doc () (hcat+ (text "\\item~@[[~a]~]" name)
                  (apply #'vcat (synth-all :doc (apply #'append* contents))))))
(defprim itemize (&rest items)
  (:pretty () (list 'itemize (list :items (synth-all :pretty items))))
  (:doc () (with-env 'itemize nil (synth-all :doc (mapcar (lambda (i) (item nil i))
                                                          items)))))

(defprim enumerate (&rest items)
  (:pretty () (list 'enumerate (list :items (synth-all :pretty items))))
  (:doc () (with-env 'enumerate nil (synth-all :doc (mapcar (lambda (i) (item nil i))
                                                          items)))))

(defprim description% (&rest items)
  (:pretty () (list 'description (list :items (synth-all :pretty items))))
  (:doc () (with-env 'description nil (synth-all :doc items))))

(defmacro description (&rest items)
  `(description% ,@(mapcar (lambda (i) `(item ',(car i) ,(cdr i)))
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
  
(synth :output (document 'title 'author
                         (section 'section
                                  (paragraph (normal "hello ")
                                             (normal "world!"))
                                  (table (row (normal "name") (normal "surname") (normal "address"))
                                         (row (normal "a") (normal "b") (normal "b"))
                                         (row (normal "c") (normal "d")))
                                  (itemize (normal "a")
                                           (normal "b"))
                                  (description (a . (normal "a"))
                                               (b . (normal "b"))))) 0)
