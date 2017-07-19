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

(defn compl [term1 term2] 
  (map (fn [x y]
         (let [lcm (max x y)]
           (if (> lcm x)
             (- lcm x)
             0)))
       term1
       term2))

(defn s-poly [f g]
  (let [f-vars (nfirst f)
        g-vars (nfirst g)]
    (sub
     (mul [(vec (cons 1 (compl f-vars g-vars)))]
          f)
     (mul [(vec (cons (/ (ffirst f) (ffirst g))
                      (compl g-vars f-vars)))]
          g))))

(defn normal-form [f g]
  (loop [f f]
    (let [s 
             (for [f f
                   g g
                   :let [s g]
                   :when (= [true]
                            (distinct
                             (map #(or (zero? %2)
                                       (and (not= 0 %2) (<= %2 %1)))
                                  (next f)
                                  (next g))))]
               [s])]
      (cond
        (empty? f) []
        (empty? s) f
        :else (recur (s-poly f (first s)))))))

;; (defn product? [f g]
;;   (= [true]
;;      (distinct
;;       (map #(or (and (zero? %1) (not= 0 %2))
;;                 (and (not= 0 %1) (zero? %2)))
;;            (next f) (next g)))))

;; (defn groebner [f g]
;;   (let [s (for [f-term g
;;                 g-term g
;;                 :when (and (not= f-term g-term)
;;                            (not (product? f-term g-term)))]
;;             [f-term g-term])]
;;     (letfn [(groebner-loop [f s]
;;               (let [nf (normal-form (s-poly [(ffirst s)]
;;                                            (vec (nfirst s)))
;;                                     f)]
;;                 (cond
;;                   (empty s) f
;;                   (empty? (next s)) (do (println 1)
;;                                         (groebner-loop f (first s)))
;;                   (empty? nf) (do (println 1)
;;                                   (groebner-loop f (next s)))
;;                   :else (do (println 2)
;;                             (groebner-loop (vec (concat nf f))
;;                                            (concat (next s) (for [new-s f
;;                                                                   :when (not (product? nf new-s))]
;;                                                               [nf new-s])))))))]
;;       (groebner-loop f s))))
