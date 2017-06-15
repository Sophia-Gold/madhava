(ns Madhava.core
  (:require [clojure.pprint :refer [pprint]]
            [clojure.data.int-map :as i]
            [com.rpl.specter :refer :all]
            [MonteCarlo.core :refer :all]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; DIFFERENTIATION & INTEGRATION

(defn partial-diff [poly store idx]
  (let [i (peek idx)
        key (Long/parseLong (apply str idx))
        partial (vec
                 (for [expr poly
                       :let [v (get expr i)]
                       :when (not (zero? v))] 
                   (-> expr
                       (update 0 * v)
                       (update i dec))))]
    (swap! store assoc key partial)
    [partial idx]))
(defn diff [poly store order]
  (letfn [(diff-vars [poly store idx]
            (map #(partial-diff poly store (conj idx %))
                 (range 1 (count (first poly)))))
          (diff-loop [poly n]
            (when (<= n order)
              (doseq [x poly]
                (diff-loop
                 (diff-vars (first x) store (update (second x) 0 inc))
                 (inc n)))))]
  (swap! store assoc 0 poly)
  (diff-loop [[poly [0]]] 0)))

(defn partial-int [poly store idx]
  (let [i (peek idx)
        key (Long/parseLong (apply str idx))
        partial (vec
                 (for [expr poly
                       :let [v (get expr i)]
                       :when (not (zero? v))] 
                   (-> expr
                       (update 0 / (inc v))
                       (update i inc))))]
    (swap! store assoc key partial)
    [partial idx]))
(defn anti-diff [poly store order]
  (letfn [(int-vars [poly store idx]
            (map #(partial-int poly store (conj idx %))
                 (range 1 (count (first poly)))))
          (int-loop [poly n]
            (when (<= n order)
              (doseq [x poly]
                (int-loop 
                 (int-vars (first x) store (update (second x) 0 inc))
                 (inc n)))))]
    (swap! store assoc 0 poly)
    (int-loop [[poly [0]]] 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; HELPER FUNCTIONS

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

(defn search-map [val map]
  (select [MAP-VALS #(= % val)] @map))

;; filter empty vectors
(defn de-null [map]
  (setval [MAP-VALS #(= [] %)] NONE @map))

(defn map-to-seq [map]
  (select [MAP-VALS #(not= [] %)] @map))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ARITHMETIC

(defn add [poly1 poly2]
  (sort-terms
   (denull
    (union (intersection poly1 poly2) poly1 poly2))))

(defn sub [poly1 poly2]
  (add poly1 (negate poly2)))

(defn scale [poly scalar]
  (mapv #(update % 0 * scalar) poly))
               
(defn mul [poly1 poly2]
  (simplify
   (sort-terms
    (for [term1 poly1
          term2 poly2
          :let [coeff (* (first term1) (first term2))]]
      (vec
       (cons coeff
             (for [idx (range 1 (count (first poly1)))]
               (+ (get term1 idx) (get term2 idx)))))))))

(defn linear-transform [m [weight1 key1] [weight2 key2]]
  (mul
   (scale (get @m key1) weight1)
   (scale (get @m key2) weight2)))

(defn compose [f g var]
  (loop [f f
         result []]
    (let [term (first f)]
      (if (nil? term)
        result
        (recur (next f) (if (zero? (nth term var))
                          (add [term] result)
                          (simplify (sort-terms
                                     (vec (concat result
                                                  (nth (iterate (partial mul [(assoc term var 0)]) g)
                                                       (nth term var))))))))))))

;; (defn add-maps [& maps]
;;   (merge-with add (map deref maps)))

;; (defn mul-maps [& maps]
;;   (merge-with mul (map deref maps)))

(defn transform-map [f map]
  (transform MAP-VALS f @map))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TAYLOR SERIES

(defn integrate-series [s]
  (map / s (drop 1 (range))))

(defn negate-series [s]
  (map - s))

(defn sparse-to-dense [s]
  (vec
   (rseq
    (filterv some?
             (mapv #(if (not= %1 0) [%1 %2]) s (range))))))

(defn exp-series []
  (->> (exp-series)
       (integrate-series)
       (lazy-cat [1])))

(declare cos-series)
(defn sin-series []
  (->> (cos-series)
       (integrate-series)
       (lazy-cat [0])))

(defn cos-series []
  (->> (sin-series)
       (negate-series)
       (integrate-series)
       (lazy-cat [1])))

(defn atan-series []
  (integrate-series
   (cycle [1 0 -1 0])))

(declare cosh-series)
(defn sinh-series []
  (->> (cosh-series)
       (integrate-series)
       (lazy-cat [0])))

(defn cosh-series []
  (->> (sinh-series)
       (integrate-series)
       (lazy-cat [1])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; MACROS

(defmacro diff-once [poly order]
  (let [store (gensym)]
    `(do
       (def ~store (atom (i/int-map)))
       (diff ~poly ~store ~order)
       (pprint @~store))))

(defmacro anti-diff-once [poly order]
  (let [store (gensym)]
    `(do
       (def ~store (atom (i/int-map)))
       (anti-diff ~poly ~store ~order)
       (pprint @~store))))

(defmacro print-map [map]
  `(pprint @~map
           (clojure.java.io/writer
            (str (quote ~map) ".txt"))))

(defn -main []
  )
