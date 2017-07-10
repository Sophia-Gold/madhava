(ns madhava.arithmetic)

(defn add-dim [poly dim]
  (mapv #(vec (concat (take dim %) [0] (drop dim %))) poly))

(defn remove-terms [term poly]
  (filterv #(not= (next term) (next %)) poly))

(defn denull [poly]
  (filterv #(not= 0 (first %)) poly))

(defn negate [poly]
  (mapv #(update % 0 -) poly))

(defn sort-terms [poly]
;; graded lexicographic order
  (vec
   (sort-by #(- (reduce + (next %)))
            (sort-by (comp - fnext) poly))))

(defn union [intersection & sets]
  (loop [i 0
         test intersection
         difference sets]
    (if (> i (count intersection))
      (into intersection (mapcat identity difference))
      (recur (inc i) (next test) (mapv (partial remove-terms (first test)) difference)))))

(defn intersection [poly1 poly2]
  (vec
   (for [term1 poly1
         term2 poly2
         :when (= (next term1) (next term2))]
     (update term1 0 + (first term2)))))

(defn simplify [poly]
  (loop [idx 0
         test poly
         result []]
    (if (> idx (dec (count poly)))
      result
      (if (= (nfirst test) (nfirst (next test)))
        (recur (+ 2 idx) (nnext test)
               (conj result (vec (cons (+ (ffirst test) (ffirst (next test))) (nfirst test)))))
        (recur (inc idx) (next test) (conj result (first test)))))))

(defn add
  ([] [])
  ([poly] poly)
  ([poly1 poly2]
   (sort-terms
    (denull
     (union (intersection poly1 poly2) poly1 poly2))))
  ([poly1 poly2 & more]
   (reduce add (add poly1 poly2) more)))

(defn sub
  ([] [])
  ([poly] (negate poly))
  ([poly1 poly2]
   (add poly1 (negate poly2)))
  ([poly1 poly2 & more]
   (reduce sub (add poly1 (negate poly2)) more)))

(defn scale [poly scalar]
  (mapv #(update % 0 * scalar) poly))

(defn mul
  ([] [])
  ([poly] poly)
  ([poly1 poly2]
   (simplify
    (sort-terms
     (for [term1 poly1
           term2 poly2
           :let [coeff (* (first term1) (first term2))]]
       (vec
        (cons coeff
              (for [idx (range 1 (count (first poly1)))]
                (+ (get term1 idx) (get term2 idx)))))))))
  ([poly1 poly2 & more]
   (reduce mul (mul poly1 poly2) more)))

(defn sqrt [f]
  (mapv (fn [v]
          (conj (vector (Math/sqrt (first v)))
                (mapv #(/ % 2) v)))
        f))
