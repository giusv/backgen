(in-package :backgen)
(defparameter *soggetti-table* nil)
(defparameter *veicoli-table* nil)
(defparameter *sinistri-table* nil)

(defun cluster (series duration &optional (accessor :d-data-accad))
  (labels ((cluster-helper (series duration acc)
	     (if (null series)
		 (reverse (mapcar #'reverse acc))
		 (let ((head (car series)))
		   (cluster-helper (cdr series)
				   duration
				   (cons (list head)
					 (mapcar #'(lambda (cluster) 
						     (if (< (- (floor (getf head accessor) 86400) 
							       (floor (getf (car (last cluster)) accessor) 86400))
							    duration)
							 (cons head cluster)
							 cluster))
						 acc)))))))
    (aif (mapcar #'length (cluster-helper series duration nil))
         (apply #'max it)
         0)))

(defmacro defops (&body bindings)
  `(progn
     ,@(mapcar #'(lambda (binding)
                   (destructuring-bind (name op) binding
                     `(defun ,name (n1 n2)
                        (if (and (not (null n1)) (not (null n2)))
                            (,op n1 n2)
                            nil))))
	       bindings)))

(defops (ind-ge >=)
        (ind-gt >)
        (ind-le <=)
        (ind-lt <)
        (ind-eq =)
        (ind-match equalp)
        (ind-plus +)
        (ind-minus -)
        (ind-times *)
        (ind-divide /))


(defun print-cluster (list)
  (format t "~{~{~a   ~}~%~}" list))

(defun restrict (table expression)
  (remove-if-not expression table))


(defun project (table &rest attributes)
  (mapcar #'(lambda (row) (reduce #'(lambda (acc att) 
				      (let ((val (getf row att)))
					(if val
					    (append acc (list att val))
					    acc)))
				  attributes
				  :initial-value nil))
	  table))


(defun product (table1 table2)
  (reduce #'(lambda (acc row1)
	      (append acc (mapcar #'(lambda (row2)
				      (append row1 row2))
				  table2)))
	  table1
	  :initial-value nil))

(defun natjoin (table1 table2)
  (reduce #'(lambda (acc row1)
	      (append acc (remove nil (mapcar #'(lambda (row2)
						  (if (joinable row1 row2)
						      (append row1 (car (apply #'project (list row2) 
									       (set-difference (attributes row2)
											       (common-attributes row1 row2)))))))
					      table2))))
	  table1
	  :initial-value nil))

(defun equijoin (table1 table2 &rest attrs)
  (reduce #'(lambda (acc row1)
	      (append acc 
		      (remove nil 
			      (mapcar #'(lambda (row2)
					  (if (apply #'joinable row1 row2 attrs)
					      (append row1 (car (apply #'project (list row2) 
								       (set-difference (attributes row2)
										       (common-attributes row1 row2)))))))
				      table2))))
	  table1
	  :initial-value nil))

(defun attributes (row)
  (mapcar #'car (group row 2)))
(defun common-attributes (row1 row2)
  (intersection (mapcar #'car (group row1 2)) (mapcar #'car (group row2 2))))

(defun joinable (row1 row2 &rest attrs)
  (if (null attrs)
      (apply #'joinable2 row1 row2 (common-attributes row1 row2))
      (apply #'joinable2 row1 row2 attrs)))

(defun joinable2 (row1 row2 &rest attrs)
  (reduce #'(lambda (con att)
	      (and con
		   (equal (getf row1 att) (getf row2 att))))
	  attrs
	  :initial-value t))


(defun field-values (table field)
  (mapcar #'(lambda (row) (getf row field))
	  table))

(defmacro sinistri-soggetto (sini sogg &optional (condition t))
  `(restrict (equijoin *soggetti-table* *sinistri-table* :id-sini)
             #'(lambda (row) 
                 (and (ind-eq (getf row :id-sogg) ,sogg)
                      (ind-lt (getf row :d-data-accad) (car (field-values (restrict *sinistri-table* (lambda (s) 
                                                                                                       (ind-eq (getf s :id-sini) ,sini))) :d-data-accad)) )
                      ,condition))))

(defmacro soggetti (sini sogg &optional (condition t))
  `(restrict *soggetti-table*
             #'(lambda (row) 
                 (and (ind-eq (getf row :id-sogg) ,sogg)
                      (ind-eq (getf row :id-sini) ,sini) 
                      ,condition))))

(defmacro veicoli (sini veic &optional (condition t))
  `(restrict *veicoli-table*
             #'(lambda (row) 
                 (and (ind-eq (getf row :id-targa) ,veic)
                      (ind-eq (getf row :id-sini) ,sini)
                      ,condition))))

(defmacro sinistri-veicolo (sini veic &optional (condition t))
  `(restrict (equijoin *veicoli-table* *sinistri-table* :id-sini)
             #'(lambda (row) 
                 (and (ind-eq (getf row :id-targa) ,veic)
                      (ind-lt (getf row :d-data-accad) (car (field-values (restrict *sinistri-table* (lambda (s) 
                                                                                                       (ind-eq (getf s :id-sini) ,sini))) :d-data-accad)) )
                      ,condition))))

(defmacro indicatore (name args pars &body body)
  `(defun ,name (,@args ,@pars)
     ,@body))
(defmacro campo (name)
  `(getf row ,name))


(defun esiste (cluster occorrenze)
  (some #'(lambda (cl) (>= (length cl) occorrenze))
	cluster))

(defmacro deflags (&body bindings)
  `(progn
     ,@(mapcar #'(lambda (binding)
                   (destructuring-bind (name flag) binding
                     `(defmacro ,name (strict)
                        (if strict 
                            `(equalp (getf row ,,flag) "1")
                            `(let ((flg (getf row ,,flag)))
                               (or (equalp flg "1") (null flg)))))))
	       bindings)))

(deflags 
  (interessato ':d-flg-interessato)
  (coinvolto ':d-flg-coinvolto)
  (richiedente ':d-flg-richiedente)
  (responsabile ':d-flg-respons) 
  (leso ':d-flg-leso)
  (richiedente ':d-flg-richiedente)
  (proprietario ':d-flg-proprietario)
  (contraente ':d-flg-contraente)
  (deceduto ':d-flg-deceduto)
  (testimone ':d-flg-testimone)
  (targainesistente ':d-flg-targa-inesistente)
  (targaincoerente ':d-flg-targa-incoerente))

(defmacro defmeas (&body bindings)
  `(progn
     ,@(mapcar #'(lambda (binding)
                   (destructuring-bind (name flag) binding
                     `(defmacro ,name (strict &optional (divisor 1))
                        (if strict
                            `(aif (getf row ,,flag) (floor it ,divisor))
                            `(aif (getf row ,,flag) (floor it ,divisor))))))
	       bindings)))
(defmeas 
    (numerolesi ':m-num-lesi)
    (dataaccadimento ':d-data-accad)
    (datadenuncia ':d-data-denun)
    (numerofgvs ':m-num-rich-fgvs)
    (numeroincoerenzeveicolo ':m-num-incoerenze)
    (giornidecorr ':m-gg-da-decorrenza)
    (giorniscad ':m-gg-a-scadenza))

(defun my-random-state ()
  #S(RANDOM-STATE :STATE #.(MAKE-ARRAY 627 :ELEMENT-TYPE '(UNSIGNED-BYTE 32)
                                       :INITIAL-CONTENTS
                                       '(0 2567483615 317 1820319860
                                         885514969 3247580297 16076187
                                         934297011 3170244972 381156652
                                         163434080 3199630205 1508201563
                                         1098691546 4252380852 3874520176
                                         2279636013 3718068715 3143787276
                                         1263843371 2533322690 2249610856
                                         2672538054 3508385277 484231079
                                         1611526233 2227759245 4205977809
                                         1643934947 1071469732 3058992527
                                         3225411219 1142733678 343871023
                                         602037743 3135541128 74597249
                                         3112875923 729267687 1456355070
                                         638544632 1738640522 4178875915
                                         168255370 2689439723 364435308
                                         2340752696 452110881 540361760
                                         3832954587 839440634 3745061710
                                         3876207872 834173600 3673442636
                                         2870378472 1287439487 2182660169
                                         5482536 1200562620 881159240
                                         58345931 255938649 1618014243
                                         2627634370 4155313004 3265691716
                                         2907638835 250317269 1647280624
                                         109919369 432656611 3877367735
                                         3194392467 571745825 2860733311
                                         4209781220 1563682625 2427597398
                                         790470351 1786917894 3753885613
                                         2480894734 3276591591 3135936946
                                         250889350 718119854 3374827008
                                         2929759152 1150028158 2887198646
                                         770552615 4074336047 1260907541
                                         540606007 1369257170 4268619081
                                         2396163686 1349053267 167700137
                                         2603215576 1806931429 1015938151
                                         1054776943 1346619078 3954990340
                                         4205961661 1932344027 2206016540
                                         3570047192 2373359193 3791931554
                                         2989527396 2279194930 2486038473
                                         3311940951 60029980 980676173
                                         1668191335 1435359823 2254499851
                                         3963422927 681828007 690306121
                                         3624052373 939367466 4058327130
                                         3713828680 4157329481 868232219
                                         1893332598 4109627520 2545970771
                                         1478135882 1464455294 604279744
                                         1455129760 940477451 3551374056
                                         609734190 3734504538 3436125112
                                         144289939 170449127 289480620
                                         2255272673 199553249 853367408
                                         3299580607 1726969507 4038712188
                                         4052182642 573038739 1004595977
                                         1390613368 3715960 4131579563
                                         1506117752 3372903029 2020461321
                                         504636236 879272871 1258073781
                                         3246011557 1485326544 772071004
                                         1440285479 2564796464 1387303551
                                         3877083030 3710434505 3989842203
                                         2262697111 1639841234 2784581589
                                         2559160183 1770499908 2037736205
                                         3780191730 1258641287 322251671
                                         1500540973 2411685955 1697026669
                                         3955318704 981730584 1607673180
                                         472819315 1275950433 1723586584
                                         204598568 1148111743 3840457017
                                         399515804 996767972 3071022763
                                         2380503271 2286525220 4144838124
                                         3755193662 3414316355 18680816
                                         3331805556 51117237 493561787
                                         1555695706 4131064563 4198159511
                                         2491901441 3992517311 2200346102
                                         1297922632 721298910 283742979
                                         4128020237 3393211054 1379536149
                                         2409542923 3246303275 3893794122
                                         224502014 3319811565 2434263207
                                         1496969898 1682492922 1008085752
                                         2639548813 2748701184 2354697150
                                         692809300 2408339496 1416713292
                                         1138643390 3656431209 2986597722
                                         3505543628 2754284245 1223298420
                                         3659786667 4098483888 542962713
                                         2026160160 3797921308 2364192724
                                         856043661 451314181 2423738463
                                         3407554053 3103862012 2869874055
                                         952418005 3960822575 2507061762
                                         1021363462 2170081153 309129730
                                         2987419038 4172261337 2263587505
                                         2588167832 601398626 599189468
                                         2283983195 1339764707 1618567695
                                         1983659935 3545641848 2349673562
                                         2092900402 3300342849 2173485759
                                         3642933851 1283353225 3253585568
                                         2575636088 632695915 2324781364
                                         4120004284 3617649023 228012415
                                         2147588717 3881864709 3802054608
                                         2695194478 586195090 1848798132
                                         1885069922 1287957827 3424160523
                                         822901543 405374431 2123746540
                                         641006165 1942829467 1392300335
                                         2572793353 305534917 404939253
                                         1908294144 2293277375 3605077391
                                         3757695104 2858701597 128108014
                                         2321122305 2282085143 2082822836
                                         3395527026 34166675 4028677267
                                         4131550299 2096087500 3931236581
                                         3286028965 187431889 3186147730
                                         3194607907 787172114 1233260151
                                         3436768937 3723550204 2236502775
                                         1743899572 3066436425 1192384610
                                         369635095 2037561714 1334771520
                                         43310174 1693013574 3608005091
                                         3475834223 2013572575 4032170381
                                         3191022200 2232067585 1205301627
                                         1849881836 2424708207 2543456759
                                         2209737697 60539592 2610141258
                                         1580969837 3862179969 2316800995
                                         697428682 1357980625 521449585
                                         3173900287 536999323 795293338
                                         2525992033 4093869261 2195710800
                                         3349135921 4076013236 1888030577
                                         3016543406 3917079386 984760754
                                         157869420 2851700502 2629993674
                                         3224387412 570692238 4057600757
                                         4164000636 3498256747 959138988
                                         3414909094 732133580 3115430836
                                         732626214 952036789 2814117857
                                         764074187 724801823 255901275
                                         4186360525 2813646242 1681277552
                                         3791744081 3071800075 2711708105
                                         3637762527 1074704817 2399623394
                                         4162796136 819474067 686464094
                                         2640790226 2285126314 1979539214
                                         152808868 785983987 1378472761
                                         3230518797 3853563096 1169537368
                                         1670781315 396347889 1072705096
                                         4018021611 2908670500 766374669
                                         1611231164 4214746494 1844282421
                                         1270414210 1414732558 2741844660
                                         2960944848 3170092980 4038656276
                                         1523825281 4079081012 1839571709
                                         487403254 3496097556 1450939201
                                         3541862019 1614437177 593448120
                                         261639940 3309549761 137787834
                                         2930423798 843123570 358399975
                                         1927775326 1573460129 1897059884
                                         1589931782 2247939401 3216304531
                                         371730174 2334363116 3616866975
                                         2559705930 472903886 1538500542
                                         504573000 534008109 1890039594
                                         2478859080 1953811809 613577292
                                         2126030972 987943666 460077214
                                         1037451119 447912725 1015649175
                                         4243942039 4235587444 1409170253
                                         745513600 4169762325 917378530
                                         2653533784 340584813 1117620507
                                         952557237 1198054456 3818594603
                                         3684180742 3054968922 3430709493
                                         3672692520 457256598 2587149437
                                         2626862524 149609763 2835047689
                                         2550439178 4192932874 3079876397
                                         1428578761 3597506037 1487434
                                         2324796665 1849406044 4109162866
                                         1994928385 588768803 585499764
                                         1186627305 2163729745 1670665430
                                         2496582891 1718916642 1963039351
                                         1074566681 3336833785 2544338739
                                         2312085596 1568102523 433759339
                                         2192773253 2999625214 1148890757
                                         3845311307 428047474 2588020500
                                         3854850553 2042708761 3179690258
                                         454754527 795117174 1113541049
                                         557058324 4265557963 1969727217
                                         2226155177 3250161965 2760961916
                                         266955680 2536952694 4113390015
                                         379748935 4193442950 2722719178
                                         3821581350 3117272106 1351437332
                                         903164258 284095961 3984845394
                                         3582474802 1238845515 1519495757
                                         1283151287 2950237155 1536171362
                                         1520366526 396137071 507051197
                                         1930012300 276308635 1209558695
                                         1311533335 751724861 2685844713
                                         103912912 1055994205 4227954751
                                         1463079386 81801955 4153657737
                                         1711719702 3667024700 2031244583
                                         1429767260 2209091159 2776193381
                                         4096905828 1867597625 2073441085
                                         1978357750 1209998003 1666323017
                                         3180987706 1875852244 4123817564
                                         4159055839 2128270906 303913889
                                         2814660862 3391074722 344923513
                                         212036952 3740870957 4288498109
                                         1556982826 1621182250 2919377853
                                         1918628123 3794124919 2987169776
                                         2858239788 1064412388 240989047
                                         3559370757 638342408 2417691835
                                         3113795375 3556953818 490707907
                                         2461891612 1607045814 3099100340
                                         3444782931 3745233231 2088141062
                                         814314795 812904219 4228763479
                                         3495442219 1211285732 727615170
                                         3949522705 3061230910 4171892695
                                         4138451723 22722569 1781993558
                                         2172428167 2988817710 3642129041
                                         1487839041 3054520428 3382941593
                                         2836638971 1538807892 1628368424
                                         3113198785 2201847798 1434213772
                                         45723901 2977763368 3399477974
                                         3954464819 3967406935 2336462793
                                         425355023 864700770))))
