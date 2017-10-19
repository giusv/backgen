(defdb
    (tl-forall i (list 1 2 3 4 5) 
      (tl-exists (sini ind-stat-sini) 
          (:ind-stat-sini-id i :id-sini i))
      (tl-exists (sogg ind-stat-sogg-sini) 
          (:ind-stat-sogg-sini-id i :id-sini i :id-sogg 5 :d-flg-leso "s"))))
