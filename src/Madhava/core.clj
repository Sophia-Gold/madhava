(ns Madhava.core
  (:require [clojure.pprint :refer [pprint]]
            [clojure.data.int-map :as i]
            [com.rpl.specter :refer :all]
            [MonteCarlo.core :refer :all]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; DIFFERENTIATION & INTEGRATION

(defn partial-diff [p m idx]
  (let [partial (vec
                 (for [expr p
                       :let [v (get expr (peek idx))]
                       :when (not (zero? v))] 
                      (-> expr
                          (update 0 * v)
                          (update (peek idx) dec))))]
  (swap! m assoc-in idx partial)
  [partial idx]))  
(defn diff [p m order]
  (letfn [(diff-vars [p m idx]
            (map #(partial-diff p m (conj idx %))
                 (range 1 (count (first p)))))
          (diff-loop [n p]
            (when (<= n order)
              (doseq [x p]
                (diff-loop (inc n) (diff-vars (first x) m (update (second x) 0 inc))))))]
    (swap! m assoc 0 p)
    (diff-loop 0 [[p [0]]])))

(defn partial-int [p m idx]
  (let [partial (vec
                 (for [expr p
                       :let [v (get expr (peek idx))]
                       :when (not (zero? v))] 
                   (-> expr
                       (update 0 / (inc v))
                       (update (peek idx) inc))))]
    (swap! m assoc-in idx partial)
    [partial idx]))
(defn int [p m order]
  (letfn [(int-vars [p m idx]
            (map #(partial-int p m (conj idx %))
                 (range 1 (count (first p)))))
          (int-loop [n p]
            (when (<= n order)
              (doseq [x p]
                (int-loop (inc n) (int-vars (first x) m (update (second x) 0 inc))))))]
    (swap! m assoc 0 p)
    (int-loop 0 [[p [0]]])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; HELPER FUNCTIONS

(defn add-dim [poly dim]
  (mapv #(vec (concat (take dim %) [0] (drop dim %))) poly))

(defn remove [term poly]
  (filterv #(not= (next term) (next %)) poly))

(defn denull [poly]
  (filterv #(not= 0 (first %)) poly))

(defn negate [poly]
  (mapv #(update % 0 -) poly))

(defn sort [poly]
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
      (recur (inc i) (next test) (mapv (partial remove (first test)) difference)))))

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

(defn deep-merge-with [f & maps]
  (apply
    (fn m [& maps]
      (if (every? map? maps)
        (apply i/merge-with m maps)
        (apply f maps)))
    maps))

;; navigate to values
(def LEAVES
  (recursive-path [] p
    (if-path map?
      [MAP-VALS p]
      STAY
      )))

;; navigate to lowest level map
(def MAP-NODES
  (recursive-path [] p
    (if-path map?
      (continue-then-stay MAP-VALS p))
    ))

(defn search-map [val map]
  (select [LEAVES #(= % val)] @map))

;; filter empty vectors
(defn de-null [map]
  (setval [LEAVES #(= [] %)] NONE @map))

(defn sort-map [map]
  (transform MAP-NODES #(into (sorted-map) %) @map))

;;remove keys
(defn de-key [map]
  (transform MAP-NODES vals @map))

(defn map-to-seq [map]
  (select [LEAVES ALL] @map))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ARITHMETIC

(defn add [poly1 poly2]
  (sort
   (denull
    (union (intersection poly1 poly2) poly1 poly2))))

(defn sub [poly1 poly2]
  (add poly1 (negate poly2)))

(defn scale [poly scalar]
  (mapv #(update % 0 * scalar) poly))
               
(defn mul [poly1 poly2]
  (simplify
   (sort
    (for [term1 poly1
          term2 poly2
          :let [coeff (* (first term1) (first term2))]]
      (vec
       (cons coeff
             (for [idx (range 1 (count (first poly1)))]
               (+ (get term1 idx) (get term2 idx)))))))))

(defn linear-transform [m [weight1 & keys1] [weight2 & keys2]]
  (mul
   (scale (get-in @m (vec keys1)) weight1)
   (scale (get-in @m (vec keys2)) weight2)))

(defn compose [f g var]
  (loop [f f
         result []]
    (let [term (first f)]
      (if (nil? term)
        result
        (recur (next f) (if (zero? (nth term var))
                          (add [term] result)
                          (simplify (sort
                                     (vec (concat result
                                                  (nth (iterate (partial mul [(assoc term var 0)]) g)
                                                       (nth term var))))))))))))

(defn add-maps [& maps]
  (deep-merge-with add maps))

(defn mul-maps [& maps]
  (deep-merge-with mul maps))

(defn transform-map [f map]
  (transform [LEAVES ALL] f @map))

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
       (pprint ~store))))

(defmacro int-once [poly order]
  (let [store (gensym)]
    `(do
       (def ~store (atom (i/int-map)))
       (int ~poly ~store ~order)
       (pprint ~store))))

(defmacro print-map [map]
  `(pprint @~map
           (clojure.java.io/writer
            (str (quote ~map) ".txt"))))

(defn -main []
  )
