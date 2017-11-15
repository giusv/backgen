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
;;             (:ind-stat-sogg-sini-id i :id-sini i :id-sogg (mod i (/ n m)) :d-flg-leso "1"))
;;         (tl-exists (veic gv-ind-stat-trg-veic-sini) 
;;             (:ind-stat-trg-veic-sini-id i :id-sini i :id-targa (mod i (/ n m)) :d-flg-targa-incoerente "1")))
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

;; (tl-someof (:d-flg-coinvolto :d-flg-leso :d-flg-richiedente :d-flg-proprietario :d-flg-contraente :d-flg-deceduto :d-flg-testimone :d-flg-respons) 2 "1")


;; (tl-someof (list :d-flg-coinvolto :d-flg-leso :d-flg-richiedente :d-flg-proprietario :d-flg-contraente :d-flg-deceduto :d-flg-testimone :d-flg-respons) 2 "1")


(defmacro sini-records2 (id-sini id-sogg date sini-flags sogg-flags)
  `(tl-exists (sini gv-ind-stat-sini) 
       (:id-sini ,id-sini :d-data-accad (tl-timestamp ,date) ,@sini-flags)
     (tl-exists (sogg gv-ind-stat-sogg-sini) 
         (:id-sini ,id-sini :id-sogg ,id-sogg ,@sogg-flags))))

(defmacro sogg-records2 (id-sini id-sogg date days occurrences sini-flags sogg-flags)
  `(tl-forall i ,occurrences
     (tl-exists (sini gv-ind-stat-sini) 
         (:id-sini (uniq ,id-sini i) ,@sini-flags)
       (tl-exists (sogg gv-ind-stat-sogg-sini) 
           (:id-sini (tl-retrieve :id-sini sini) :id-sogg ,id-sogg
                     ,@sogg-flags)))))


;; (defmacro sini-record (id-sini id-sogg date &rest flags)
;;   `(tl-exists (sini gv-ind-stat-sini) 
;;        (:id-sini ,id-sini :d-data-accad (tl-timestamp ,date) ,@flags)))

;; (defmacro sogg-record (id-sini id-sogg date days &rest flags)
;;   `(tl-exists (sogg gv-ind-stat-sogg-sini) 
;;        (:id-sini ,id-sini :id-sogg ,id-sogg ,@flags)))

;; (defmacro sogg-records (id-sini id-sogg date days occurrences &rest flags)
;;   `(tl-forall i (tl-range 1 ,occurrences)
;;      (tl-exists (sini gv-ind-stat-sini) 
;;          (:id-sini (uniq ,id-sini i)  
;;                    :d-data-accad (tl-random-timestamp (- ,date (* ,days 86400)) ,date))
;;        (tl-exists (sogg gv-ind-stat-sogg-sini) 
;;            (:id-sini (tl-retrieve :id-sini sini) :id-sogg ,id-sogg
;;                      ,@flags)))))

;; (list :d-flg-coinvolto :d-flg-leso :d-flg-richiedente :d-flg-proprietario :d-flg-contraente :d-flg-deceduto :d-flg-testimone :d-flg-respons)
(defmacro sco (id-ind date days occurrences (sogg-flags flag) sini-flags)
  (let* ((other-flags (apply #'append (mapcar (lambda (flag) (list flag "0"))
                                              (remove flag sogg-flags))))
         (id-true (1+ (* id-ind 100)))
         (id-false (+ id-true 1))
         (id-null (+ id-true 2))
         (occurrences-true `(tl-range 1 ,occurrences))
         (occurrences-false `(tl-range 1 (- ,occurrences 1)))
         (occurrences-null `(tl-range ,occurrences ,occurrences)))
    `(tl-and (sini-records2 ,id-true ,id-true ,date ,sini-flags (,flag "1" ,@other-flags))
             (sogg-records2 ,id-true ,id-true ,date ,days ,occurrences-true ,sini-flags (,flag "1" ,@other-flags))

             (sini-records2 ,id-false ,id-false ,date ,sini-flags (,flag "1" ,@other-flags))
             (sogg-records2 ,id-false ,id-false ,date ,days ,occurrences-false ,sini-flags (,flag "1" ,@other-flags))
             
             (sini-records2 ,id-null ,id-null ,date ,sini-flags (,flag "1" ,@other-flags))
             (sogg-records2 ,id-null ,id-null ,date ,days ,occurrences-false ,sini-flags (,flag "1" ,@other-flags))
             (sogg-records2 ,id-null ,id-null ,date ,days ,occurrences-null ,sini-flags (,flag nil ,@other-flags)))))

(defmacro sco1 (flag)
  (let ((days 180)
        (occurrences 3)
        (date (get-universal-time)))
    `(sco 1 ,date ,days (- ,occurrences 1) ((:d-flg-coinvolto :d-flg-leso :d-flg-richiedente :d-flg-proprietario :d-flg-contraente :d-flg-deceduto :d-flg-testimone :d-flg-respons) ,flag) (:d-data-accad (tl-random-timestamp (- ,date (* ,days 86400)) ,date)))))

(defmacro sco2 (flag)
  (let ((days 180)
        (occurrences 3)
        (date (get-universal-time)))
    `(sco 2 ,date ,days (- ,occurrences 1) ((:d-flg-coinvolto :d-flg-leso :d-flg-richiedente :d-flg-proprietario :d-flg-contraente :d-flg-deceduto :d-flg-testimone :d-flg-respons) ,flag) (:d-data-accad (tl-random-timestamp (- ,date (* ,days 86400)) ,date)))))

(defmacro sco3 (flag)
  (let ((days 180)
        (occurrences 3)
        (threshold 3)
        (date (get-universal-time)))
    `(sco 3 ,date ,days (- ,occurrences 1) ((:d-flg-coinvolto :d-flg-leso :d-flg-richiedente :d-flg-proprietario :d-flg-contraente :d-flg-deceduto :d-flg-testimone :d-flg-respons) ,flag) (:d-data-accad (tl-random-timestamp (- ,date (* ,days 86400)) ,date) :m-num-lesi ,threshold))))

;; (defmacro sco4 (flag)
;;   (let ((days 180)
;;         (occurrences 3)
;;         (threshold 3)
;;         (date (get-universal-time)))
;;     `(sco 3 ,date ,days (- ,occurrences 1) ((:d-flg-coinvolto :d-flg-leso :d-flg-richiedente :d-flg-proprietario :d-flg-contraente :d-flg-deceduto :d-flg-testimone :d-flg-respons) ,flag) (:d-data-accad (tl-random-timestamp (- ,date (* ,days 86400)) ,date) :m-num-lesi ,threshold))))


;; (defmacro sco1 (id-sini id-sogg date days occurrences flags flag)
;;   (let ((other-flags (apply #'append (mapcar (lambda (flag) (list flag "0"))
;;                                              (remove flag flags))))
;;         (id-sini-true id-sini)
;;         (id-sini-false (+ id-sini 1))
;;         (id-sini-null-1 (+ id-sini 2))
;;         (id-sini-null-2 (+ id-sini 3))
;;         (id-sogg-true id-sogg)
;;         (id-sogg-false (+ id-sogg 1))
;;         (id-sogg-null (+ id-sogg 2)))
;;     `(tl-and (sini-record ,id-sini-true ,id-sogg-true ,date)
;;              (sogg-record ,id-sini-true ,id-sogg-true ,date ,days ,flag "1" ,@other-flags)
;;              (sogg-records ,id-sini-true ,id-sogg-true ,date ,days ,occurrences ,flag "1" ,@other-flags)

;;              (sini-record ,id-sini-false ,id-sogg-false ,date)
;;              (sogg-record ,id-sini-false ,id-sogg-false ,date ,days ,flag "1" ,@other-flags)
;;              (sogg-records ,id-sini-false ,id-sogg-false ,date ,days (- ,occurrences 1) ,flag "1" ,@other-flags)

;;              (sini-record ,id-sini-null-1 ,id-sogg-null ,date)
;;              (sogg-record ,id-sini-null-1 ,id-sogg-null ,date ,days ,flag "1" ,@other-flags)
;;              (sogg-records ,id-sini-null-1 ,id-sogg-null ,date ,days (- ,occurrences 1) ,flag "1" ,@other-flags)
;;              (sogg-records ,id-sini-null-2 ,id-sogg-null ,date ,days 1 ,flag nil ,@other-flags))))

;; (defmacro sco2 (id-sini id-sogg date days occurrences flags flag)
;;   `(sco1 ,id-sini ,id-sogg ,date ,days ,occurrences ,flags ,flag))

;; (defmacro sco3 (id-sini id-sogg date days occurrences threshold flags flag)
;;   (let ((other-flags (apply #'append (mapcar (lambda (flag) (list flag "0"))
;;                                              (remove flag flags))))
;;         (id-sini-true id-sini)
;;         (id-sini-false (+ id-sini 1))
;;         (id-sini-null-1 (+ id-sini 2))
;;         (id-sini-null-2 (+ id-sini 3))
;;         (id-sogg-true id-sogg)
;;         (id-sogg-false (+ id-sogg 1))
;;         (id-sogg-null (+ id-sogg 2)))
;;     `(tl-and (sini-record ,id-sini-true ,id-sogg-true ,date  :m-num-lesi ,threshold)
;;              (sogg-record ,id-sini-true ,id-sogg-true ,date ,days ,flag "1" ,@other-flags)
;;              (sogg-records ,id-sini-true ,id-sogg-true ,date ,days ,occurrences ,flag "1" ,@other-flags)

;;              (sini-record ,id-sini-false ,id-sogg-false ,date  :m-num-lesi ,threshold)
;;              (sogg-record ,id-sini-false ,id-sogg-false ,date ,days ,flag "1" ,@other-flags)
;;              (sogg-records ,id-sini-false ,id-sogg-false ,date ,days (- ,occurrences 1) ,flag "1" ,@other-flags)

;;              (sini-record ,id-sini-null-1 ,id-sogg-null ,date  :m-num-lesi ,threshold)
;;              (sogg-record ,id-sini-null-1 ,id-sogg-null ,date ,days ,flag "1" ,@other-flags)
;;              (sogg-records ,id-sini-null-1 ,id-sogg-null ,date ,days (- ,occurrences 1) ,flag "1" ,@other-flags)
;;              (sogg-records ,id-sini-null-2 ,id-sogg-null ,date ,days 1 ,flag nil ,@other-flags)))

;;   ;; (let ((other-flags (apply #'append (mapcar (lambda (flag) (list flag "0"))
;;   ;;                                            (remove flag flags)))))
;;   ;;   `(tl-and (sini-record ,id-sini ,id-sogg ,date :m-num-lesi ,threshold ,flag "1" ,@other-flags)
;;   ;;            (sogg-record ,id-sini ,id-sogg ,date ,days ,flag "1" ,@other-flags)

;;   ;;            (sogg-records ,id-sini ,id-sogg ,date ,days ,occurrences ,flag "1" ,@other-flags)

;;   ;;            (sini-record (+ ,id-sini 1) (+ ,id-sogg 1) ,date :m-num-lesi ,threshold ,flag "1" ,@other-flags)
;;   ;;            (sogg-record (+ ,id-sini 1) (+ ,id-sogg 1) ,date ,days ,flag "1" ,@other-flags)
;;   ;;            (sogg-records (+ ,id-sini 1) (+ ,id-sogg 1) ,date ,days (- ,occurrences 1) ,flag "1" ,@other-flags)

;;   ;;            (sini-record (+ ,id-sini 2) (+ ,id-sogg 2) ,date :m-num-lesi ,threshold ,flag "1" ,@other-flags)
;;   ;;            (sogg-record (+ ,id-sini 2) (+ ,id-sogg 2) ,date ,days ,flag "1" ,@other-flags)

;;   ;;            (sogg-records (+ ,id-sini 2) (+ ,id-sogg 2) ,date ,days (- ,occurrences 1) ,flag "1" ,@other-flags)
;;   ;;            (sogg-records (+ ,id-sini 3) (+ ,id-sogg 2) ,date ,days 1 ,flag nil ,@other-flags)
;;   ;;            ))
;;   )

(defdb 
  (sco1 :d-flg-respons)
  (sco2 :d-flg-respons)
  (sco3 :d-flg-respons)
  
  ;; (sco 2 date sco2-days (- sco2-occurrences 1) ((:d-flg-coinvolto :d-flg-leso :d-flg-richiedente :d-flg-proprietario :d-flg-contraente :d-flg-deceduto :d-flg-testimone :d-flg-respons) :d-flg-respons) nil)
  ;; (sco 3 date sco3-days (- sco3-occurrences 1) sco3-threshold (:d-flg-coinvolto :d-flg-leso :d-flg-richiedente :d-flg-proprietario :d-flg-contraente :d-flg-deceduto :d-flg-testimone :d-flg-respons) :d-flg-respons)
  )

;; (pprint *database*)


;; (symbol-macrolet ((lst '(1 2 3)))
;;   (pprint lst))

;; (defdb
;;     (tl-forall i (tl-range 1 3) 
;;       (tl-exists (sinistro ind-stat-sini) 
;;           (:id-sini i))
;;       (tl-forall j (list 1 2) 
;;         (tl-exists (soggetto ind-stat-sogg) 
;;             (:id-sini i :id-sogg j :flg-leso "1")))
;;       (tl-exists (veicolo ind-stat-veic) 
;;           (:id-sini i :id-targa (random-string 7) :flg-targa-incoerente "1"))))
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
;;   (bool-entry 21 responsabile d-flg-respons s)
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



