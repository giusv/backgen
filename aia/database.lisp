(in-package :backgen)
(defun uniq (&rest inds)
  (read-from-string (apply #'mkstr inds)))




(defmacro bool-entry (n fname name val)
  `(tl-forall i (tl-range ,n ,n)
     (tl-exists (entry stdlib-entries)
         (:stdlib-entry-id ,n :name ,(format nil "~a" (lower fname "")) :type "boolean" :body ,(format nil "function ~a(soggetto,sinistro,strict) {if (strict) { return \"(~a = ''~a'')\"; } else { return \"(~a = ''~a'' OR ~a IS NULL)\"; }}" (lower fname "") (upper name) (lower val) (upper name) (lower val) (upper name)))
       (tl-exists (par stdlib-entry-parameters) (:stdlib-entry-parameter-id (uniq i 1) :stdlib-entry-id ,n :name "soggetto" :type "soggetto"))
       (tl-exists (par stdlib-entry-parameters) (:stdlib-entry-parameter-id (uniq i 2) :stdlib-entry-id ,n :name "sinistro" :type "string")))))

(defmacro op-entry (n fname op)
  `(tl-forall i (tl-range ,n ,n)
     (tl-exists (entry stdlib-entries)
         (:stdlib-entry-id ,n :name  ,(format nil "~a" (lower fname "")) :type "string" :body,(format nil "function ~a(op1,op2,strict)
{if (strict) { return \"(\" + op1 + \" ~a \" + op2 + \"); } else { return \"((\" + op1 + \" ~a \" + op2 + \") OR (\" + op1 + \"IS NULL) OR (\" + op2 + \"IS NULL))\"; }}" (lower fname "") op op))

       (tl-exists (par stdlib-entry-parameters)
           (:stdlib-entry-parameter-id (uniq i 1) :stdlib-entry-id ,n :name "op1" :type "null"))
       (tl-exists (par stdlib-entry-parameters) (:stdlib-entry-parameter-id (uniq i 2) :stdlib-entry-id ,n :name "op2" :type "null")))))

;; (tl-forall i (tl-range 1 n) 
;;         (tl-exists (sini gv-ind-stat-sini) 
;;             (:ind-stat-sini-id i :id-sini i))
;;         (tl-exists (sogg gv-ind-stat-sogg-sini) 
;;             (:ind-stat-sogg-sini-id i :id-sini i :id-sogg (mod i (/ n m)) :d-flg-leso "s"))
;;         (tl-exists (veic gv-ind-stat-trg-veic-sini) 
;;             (:ind-stat-trg-veic-sini-id i :id-sini i :id-targa (mod i (/ n m)) :d-flg-targa-incoerente "s")))
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun tl-oneof (values)
    (nth (random (length values)) values)))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun tl-someof (flags n val)
    (if (eql n 0)
        nil 
        (let ((flag (tl-oneof flags)))
          (apply #'list flag val (tl-someof (remove flag flags) (- n 1) val))))))

;; (defmacro tl-someof (flags n val)
;;   (if (eql n 0)
;;       nil 
;;       (let ((flag (tl-oneof flags)))
;;         `(apply #'list ,flag ,val (tl-someof ,(remove flag flags) ,(- n 1) ,val)))))

;; (defmacro tl-someof (flags val)
;;   (let ((len (length flags)))
;;     `(apply #'append (remove-duplicates (loop for i from 1 to ,len collect (list (nth (random ,len) ,flags) ,val)) :test (lambda (f1 f2) (eql (car f1) (car f2)))))))

;; (tl-someof (:d-flg-coinvolto :d-flg-leso :d-flg-richiedente :d-flg-proprietario :d-flg-contraente :d-flg-deceduto :d-flg-testimone :d-flg-responsabile) 2 "s")


;; (tl-someof (list :d-flg-coinvolto :d-flg-leso :d-flg-richiedente :d-flg-proprietario :d-flg-contraente :d-flg-deceduto :d-flg-testimone :d-flg-responsabile) 2 "s")

(defmacro sogg-record (id-sini id-sogg date days occurrences flags)
  `(tl-exists (sini gv-ind-stat-sini) 
       (:ind-stat-sini-id ,id-sini :id-sini ,id-sini :d-data-accad (tl-timestamp ,date))
     (tl-exists (sogg gv-ind-stat-sogg-sini) 
         (:ind-stat-sogg-sini-id ,id-sogg :id-sini ,id-sini :id-sogg ,id-sogg))
     (tl-forall i (tl-range 1 ,occurrences)
       (tl-exists (sini gv-ind-stat-sini) 
           (:ind-stat-sini-id (uniq ,id-sini i) :id-sini (uniq ,id-sini i)  
                              :d-data-accad (tl-random-timestamp (- ,date (* ,days 86400)) ,date))
         (tl-exists (sogg gv-ind-stat-sogg-sini) 
             (:ind-stat-sogg-sini-id (uniq (tl-retrieve :id-sini sini) i) :id-sini (tl-retrieve :id-sini sini) :id-sogg ,id-sogg
                                     ,@flags))))))

(defmacro sco1-true (id-sini id-sogg date days occurrences)
  `(sogg-record ,id-sini ,id-sogg ,date ,days ,occurrences ,(tl-someof #1=(list :d-flg-coinvolto :d-flg-leso :d-flg-richiedente :d-flg-proprietario :d-flg-contraente :d-flg-deceduto :d-flg-testimone :d-flg-responsabile) (random (length #1#)) "s")))

;; (defmacro sco1-false (id-sini id-sogg date days occurrences)
;;   `(sogg-records ,id-sini ,id-sogg ,date ,days ,(- occurrences 1) :d-flg-coinvolto :d-flg-leso :d-flg-richiedente :d-flg-proprietario :d-flg-contraente :d-flg-deceduto :d-flg-testimone :d-flg-responsabile))

(let* ((sco1-days 180)
       (sco1-occurrences 3)
       (end-date (get-universal-time)))
  (defdb 
    (sco1-true 1 1000 end-date sco1-days sco1-occurrences)
    ;; (sco1-false 1 1000 end-date sco1-days sco1-occurrences)
    ;; (tl-forall i (tl-range 1 1) 
    ;;   (tl-exists (sini gv-ind-stat-sini) 
    ;;       (:ind-stat-sini-id i :id-sini i :d-data-accad (tl-timestamp end-date)))
    ;;   (tl-exists (sogg gv-ind-stat-sogg-sini) 
    ;;       (:ind-stat-sogg-sini-id i :id-sini i :id-sogg i :d-flg-leso "s"))
    ;;   (tl-forall j (tl-range 1 sco1-occurrences)
    ;;     (tl-exists (sini gv-ind-stat-sini) 
    ;;         (:ind-stat-sini-id (uniq i j) :id-sini (uniq i j) :d-flg-leso "s" :d-data-accad (tl-random-timestamp start-date end-date)))))
    ))

(pprint *database*)



;; (defdb
;;     (tl-forall i (tl-range 1 3) 
;;       (tl-exists (sinistro ind-stat-sini) 
;;           (:id-sini i))
;;       (tl-forall j (list 1 2) 
;;         (tl-exists (soggetto ind-stat-sogg) 
;;             (:id-sini i :id-sogg j :flg-leso "s")))
;;       (tl-exists (veicolo ind-stat-veic) 
;;           (:id-sini i :id-targa (random-string 7) :flg-targa-incoerente "s"))))
;; (defdb 
;;     (tl-forall i (tl-range 3 3)
;;       (tl-exists (entry stdlib-entries)
;;           (:stdlib-entry-id 3 :name "dataaccadimento" :type "date" :body "function dataaccadimento(sinistro) {return \"D_DATA_ACCAD\"}")
;;         (tl-exists (par stdlib-entry-parameters)
;;             (:stdlib-entry-parameter-id (uniq i 1) :stdlib-entry-id 3 :name "sinistro" :type "string"))))
    
;;     (tl-forall i (tl-range 4 4)
;;       (tl-exists (entry stdlib-entries)
;;           (:stdlib-entry-id 4 :name "datadenuncia" :type "date" :body "function datadenuncia(sinistro) {return \"D_DATA_DENUN\"}")
;;         (tl-exists (par stdlib-entry-parameters)
;;             (:stdlib-entry-parameter-id (uniq i 1) :stdlib-entry-id 4 :name "sinistro" :type "string"))))
;;   (tl-forall i (tl-range 5 5)
;;     (tl-exists (entry stdlib-entries)
;;         (:stdlib-entry-id 5 :name "intervallo" :type "number" :body "function intervallo(fine,inizio) {return fine + \"-\" + inizio}")
;;       (tl-exists (par stdlib-entry-parameters)
;;           (:stdlib-entry-parameter-id (uniq i 1) :stdlib-entry-id 5 :name "fine" :type "date"))
;;       (tl-exists (par stdlib-entry-parameters) (:stdlib-entry-parameter-id (uniq i 2) :stdlib-entry-id 5 :name "inizio" :type "date"))))
  
;;   (tl-forall i (tl-range 6 6)
;;     (tl-exists (entry stdlib-entries)
;;         (:stdlib-entry-id 6 :name "numerolesi" :type "number" :body "function numerolesi(sinistro) {return \"M_NUM_LESI\"}")
;;       (tl-exists (par stdlib-entry-parameters)
;;           (:stdlib-entry-parameter-id (uniq i 1) :stdlib-entry-id 6 :name "sinistro" :type "string"))))
  
;;   (tl-forall i (tl-range 7 7)
;;     (tl-exists (entry stdlib-entries)
;;         (:stdlib-entry-id 7 :name "numeroveicoli" :type "number" :body "function numeroveicoli(sinistro) {return \"M_NUM_VEIC\"}")
;;       (tl-exists (par stdlib-entry-parameters)
;;           (:stdlib-entry-parameter-id (uniq i 1) :stdlib-entry-id 7 :name "sinistro" :type "string"))))
  
;;   (tl-forall i (tl-range 8 8)
;;     (tl-exists (entry stdlib-entries)
;;         (:stdlib-entry-id 8 :name "numerofgvs" :type "number" :body "function numerofgvs(sinistro) {return \"M_NUM_RICH_FGVS\"}")
;;       (tl-exists (par stdlib-entry-parameters)
;;           (:stdlib-entry-parameter-id (uniq i 1) :stdlib-entry-id 8 :name "sinistro" :type "string"))))
  
;;   (bool-entry 9 leso d-flg-leso s)

;;   (tl-forall i (tl-range 10 10)
;;     (tl-exists (entry stdlib-entries)
;;         (:stdlib-entry-id 10 :name "score" :type "number" :body "function score(sinistro) {print(\"SELECT SCORE FROM SCORE_SINI WHERE ID_SINI = \" + sinistro); return 1;}")
;;       (tl-exists (par stdlib-entry-parameters)
;;           (:stdlib-entry-parameter-id (uniq i 1) :stdlib-entry-id 10 :name "sinistro" :type "string"))))
  
;;   (bool-entry 11 beneficiario d-flg-beneficiario s)
;;   (bool-entry 12 persona d-tipo-sogg p)
;;   (bool-entry 13 conducente d-flg-conducente s)
;;   (bool-entry 14 patenteinvalida d-flg-patente-invalida s)
;;   (bool-entry 15 coinvolto d-flg-coinvolto s)
;;   (bool-entry 16 richiedente d-flg-richiedente s)
;;   (bool-entry 17 proprietario d-flg-proprietario s)
;;   (bool-entry 18 contraente d-flg-contraente s)
;;   (bool-entry 19 deceduto d-flg-deceduto s)
;;   (bool-entry 20 testimone d-flg-testimone s)
;;   (bool-entry 21 responsabile d-flg-responsabile s)
;;   (bool-entry 22 danneggiato d-flg-danneggiato s)
  
;;   (tl-forall i (tl-range 24 24)
;;     (tl-exists (entry stdlib-entries)
;;         (:stdlib-entry-id 24 :name "cluster" :type "number" :body "function cluster(sinistri,mesi) {print(\"sinistri = \" + sinistri.size());return sinistri.size();}")
;;       (tl-exists (par stdlib-entry-parameters) (:stdlib-entry-parameter-id (uniq i 1) :stdlib-entry-id 24 :name "sinistri" :type "query"))
;;       (tl-exists (par stdlib-entry-parameters)
;;           (:stdlib-entry-parameter-id (uniq i 2) :stdlib-entry-id 24 :name "mesi" :type "number"))))
  
;;   (tl-forall i (tl-range 26 26)
;;     (tl-exists (entry stdlib-entries)
;;         (:stdlib-entry-id 26 :name "conta" :type "number" :body "function conta(query) {return query.size()}")
;;       (tl-exists (par stdlib-entry-parameters)
;;           (:stdlib-entry-parameter-id (uniq i 1) :stdlib-entry-id 26 :name "query" :type "query"))))
  
;;   (tl-forall i (tl-range 27 27)
;;     (tl-exists (entry stdlib-entries)
;;         (:stdlib-entry-id 27 :name "querysoggetti" :type "number" :body "function querysoggetti(input,query) {var q = \"SELECT B.ID_SINI, B.D_DATA_ACCAD FROM IND_STAT_SOGG_SINI A INNER JOIN IND_STAT_SINI B ON A.ID_SINI = B.ID_SINI WHERE A.ID_SOGG = \" + input + (query!=null? (\" AND \" + query) : \"\");print(q);result = em.createNativeQuery(q).getResultList();print(\"result = \" + result);return result;}")
;;       (tl-exists (par stdlib-entry-parameters)
;;           (:stdlib-entry-parameter-id (uniq i 1) :stdlib-entry-id 27 :name "table" :type "null"))
;;       (tl-exists (par stdlib-entry-parameters) (:stdlib-entry-parameter-id (uniq i 2) :stdlib-entry-id 27 :name "query" :type "string"))))
  
;;   (tl-forall i (tl-range 28 28)
;;     (tl-exists (entry stdlib-entries)
;;         (:stdlib-entry-id 28 :name "queryveicoli" :type "number" :body "function queryveicoli(input,query) {var q = \"SELECT * FROM IND_STAT_TRG_VEIC_SINI A INNER JOIN IND_STAT_SINI B ON A.ID_SINI = B.ID_SINI WHERE ID_TRG_VEIC = \" + input + (query!=null? (\" AND \" + query) : \"\");print(q);result = em.createNativeQuery(q).getResultList();print(\"result = \" + result);return result;}")
;;       (tl-exists (par stdlib-entry-parameters)
;;           (:stdlib-entry-parameter-id (uniq i 1) :stdlib-entry-id 28 :name "table" :type "null"))
;;       (tl-exists (par stdlib-entry-parameters) (:stdlib-entry-parameter-id (uniq i 2) :stdlib-entry-id 28 :name "query" :type "string"))))
  
;;   (op-entry 29 equal =)
;;   (op-entry 30 lt <)
;;   (op-entry 31 te <=)
;;   (op-entry 32 gt >)
;;   (op-entry 33 ge >=)
  
;;   (tl-forall i (tl-range 34 34)
;;     (tl-exists (entry stdlib-entries)
;;         (:stdlib-entry-id 34 :name "and" :type "string" :body "function and(op1,op2) {return \"(\" + op1 + \" AND \" + op2 + \")\";}")
;;       (tl-exists (par stdlib-entry-parameters)
;;           (:stdlib-entry-parameter-id (uniq i 1) :stdlib-entry-id 34 :name "op1" :type "null"))
;;       (tl-exists (par stdlib-entry-parameters) (:stdlib-entry-parameter-id (uniq i 2) :stdlib-entry-id 34 :name "op2" :type "null"))))
  
;;   (tl-forall i (tl-range 35 35)
;;     (tl-exists (entry stdlib-entries)
;;         (:stdlib-entry-id 35 :name "or" :type "string" :body "function or(op1,op2) {return \"(\" + op1 + \" OR \" + op2 + \")\";}")
;;       (tl-exists (par stdlib-entry-parameters)
;;           (:stdlib-entry-parameter-id (uniq i 1) :stdlib-entry-id 35 :name "op1" :type "null"))
;;       (tl-exists (par stdlib-entry-parameters) (:stdlib-entry-parameter-id (uniq i 2) :stdlib-entry-id 35 :name "op2" :type "null"))))
  
;;   (tl-forall i (tl-range 36 36)
;;     (tl-exists (entry stdlib-entries)
;;         (:stdlib-entry-id 36 :name "not" :type "string" :body "function not(op1) {return \"(NOT \" + op1 +\")\";}")
;;       (tl-exists (par stdlib-entry-parameters)
;;           (:stdlib-entry-parameter-id (uniq i 1) :stdlib-entry-id 36 :name "op1" :type "null")))))



