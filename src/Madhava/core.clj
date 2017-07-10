(ns Madhava.core
  (:require [Madhava.taylorseries :refer :all]
            [clojure.pprint :refer [pprint]]
            [clojure.string :as str]
            [clojure.data.int-map :as i]
            [com.rpl.specter :refer :all]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; DIFFERENTIATION & INTEGRATION

(defn diff [poly order]
  (let [tape (atom (i/int-map))
        vars (count (first poly))]
    (letfn [(partial-diff [poly key idx]
              (let [partial (vec
                             (for [expr poly
                                   :let [v (get expr idx)]
                                   :when (not (zero? v))] 
                               (-> expr
                                   (update 0 * v)
                                   (update idx dec))))] 
                (swap! tape assoc key partial)
                (list partial key)))
            (diff-vars [poly key]
              (map #(partial-diff poly (+ key %) %)
                   (range 1 vars)))
            (diff-loop [poly n]
              (when (<= n order)
                (doseq [x poly]
                  (diff-loop
                   (diff-vars (first x) (* 10 (second x)))
                   (inc n)))))]
      (swap! tape assoc 1 poly)
      (diff-loop [[poly 1]] 0)
      @tape)))

(defn anti-diff [poly order]
  (let [tape (atom (i/int-map))
        vars (count (first poly))]
    (letfn [(partial-int [poly key idx]
              (let [partial (vec
                             (for [expr poly
                                   :let [v (get expr idx)]
                                   :when (not (zero? v))] 
                               (-> expr
                                   (update 0 / (inc v))
                                   (update idx inc))))]
                (swap! tape assoc key partial)
                (list partial key)))
            (int-vars [poly key]
              (map #(partial-int poly (+ key %) %)
                   (range 1 vars)))
            (int-loop [poly n]
              (when (<= n order)
                (doseq [x poly]
                  (int-loop 
                   (int-vars (first x) (* 10 (second x)))
                   (inc n)))))]
      (swap! tape assoc 1 poly)
      (int-loop [[poly 1]] 0)
      @tape)))
  
(defn vdiff [vfunc order]
  (let [tape (atom (i/int-map))]
    (letfn [(partial-diff [poly key idx]
              (let [partial (vec
                             (for [expr poly
                                   :let [v (get expr idx)]
                                   :when (not (zero? v))] 
                               (-> expr
                                   (update 0 * v)
                                   (update idx dec))))] 
                (swap! tape assoc key partial)
                (list partial key)))
            (diff-vars [poly key]
              (map #(partial-diff poly (+ key %) %)
                   (range 1 (count (first poly)))))
            (diff-loop [poly n]
              (when (<= n order)
                (doseq [x poly]
                  (diff-loop
                   (diff-vars (first x) (* 10 (second x)))
                   (inc n)))))]
      (doseq [poly (map-indexed #(vector %2 (inc %1)) vfunc)]
        (swap! tape assoc (second poly) (first poly)) 
        (diff-loop [poly] 0))
      @tape)))

(defn anti-vdiff [vfunc order]
  (let [tape (atom (i/int-map))]
    (letfn [(partial-int [poly key idx]
              (let [partial (vec
                             (for [expr poly
                                   :let [v (get expr idx)]
                                   :when (not (zero? v))] 
                               (-> expr
                                   (update 0 / (inc v))
                                   (update idx inc))))]
                (swap! tape assoc key partial)
                (list partial key)))
            (int-vars [poly key]
              (map #(partial-int poly (+ key %) %)
                   (range 1 (count (first poly)))))
            (int-loop [poly n]
              (when (<= n order)
                (doseq [x poly]
                  (int-loop 
                   (int-vars (first x) (* 10 (second x)))
                   (inc n)))))]
      (doseq [poly (map-indexed #(vector %2 (inc %1)) vfunc)]
        (swap! tape assoc (second poly) (first poly)) 
        (int-loop [poly] 0))
      @tape)))

(defn pdiff [poly order]
  (let [tape (agent (i/int-map))
        vars (count (first poly))]
    (letfn [(partial-diff [poly key idx]
              (let [partial (vec
                             (for [expr poly
                                   :let [v (get expr idx)]
                                   :when (not (zero? v))] 
                               (-> expr
                                   (update 0 * v)
                                   (update idx dec))))] 
                (send tape assoc key partial)
                (list partial key)))
            (diff-vars [poly key]
              (map #(partial-diff poly (+ key %) %)
                   (range 1 vars)))
            (diff-loop [poly n]
              (when (<= n order)
                (doseq [x poly]
                  (diff-loop
                   (diff-vars (first x) (* 10 (second x)))
                   (inc n)))))]
      (send tape assoc 1 poly)
      (diff-loop [[poly 1]] 0)
      (await tape)
      @tape)))

(defn anti-pdiff [poly order]
  (let [tape (agent (i/int-map))
        vars (count (first poly))]
    (letfn [(partial-int [poly key idx]
              (let [partial (vec
                             (for [expr poly
                                   :let [v (get expr idx)]
                                   :when (not (zero? v))] 
                               (-> expr
                                   (update 0 / (inc v))
                                   (update idx inc))))]
                (send tape assoc key partial)
                (list partial key)))
            (int-vars [poly key]
              (map #(partial-int poly (+ key %) %)
                   (range 1 vars)))
            (int-loop [poly n]
              (when (<= n order)
                (doseq [x poly]
                  (int-loop 
                   (int-vars (first x) (* 10 (second x)))
                   (inc n)))))]
      (send tape assoc 1 poly)
      (int-loop [[poly 1]] 0)
      (await tape)
      @tape)))

(defn vpdiff [vfunc order]
  (let [tape (agent (i/int-map))]
    (letfn [(partial-diff [poly key idx]
              (let [partial (vec
                             (for [expr poly
                                   :let [v (get expr idx)]
                                   :when (not (zero? v))] 
                               (-> expr
                                   (update 0 * v)
                                   (update idx dec))))] 
                (send tape assoc key partial)
                (list partial key)))
            (diff-vars [poly key]
              (map #(partial-diff poly (+ key %) %)
                   (range 1 (count (first poly)))))
            (diff-loop [poly n]
              (when (<= n order)
                (doseq [x poly]
                  (diff-loop
                   (diff-vars (first x) (* 10 (second x)))
                   (inc n)))))]
      (doseq [poly (map-indexed #(vector %2 (inc %1)) vfunc)]
        (send tape assoc (second poly) (first poly)) 
        (diff-loop [poly] 0))
      (await tape)
      @tape)))

(defn anti-vpdiff [vfunc order]
  (let [tape (agent (i/int-map))]
    (letfn [(partial-int [poly key idx]
              (let [partial (vec
                             (for [expr poly
                                   :let [v (get expr idx)]
                                   :when (not (zero? v))] 
                               (-> expr
                                   (update 0 / (inc v))
                                   (update idx inc))))]
                (send tape assoc key partial)
                (list partial key)))
            (int-vars [poly key]
              (map #(partial-int poly (+ key %) %)
                   (range 1 (count (first poly)))))
            (int-loop [poly n]
              (when (<= n order)
                (doseq [x poly]
                  (int-loop 
                   (int-vars (first x) (* 10 (second x)))
                   (inc n)))))]
      (doseq [poly (map-indexed #(vector %2 (inc %1)) vfunc)]
        (send tape assoc (second poly) (first poly)) 
        (int-loop [poly] 0))
      (await tape)
      @tape)))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ARITHMETIC

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; MAP FUNCTIONS

(defn search-map [map val]
  (into (i/int-map)
        (select [ALL (fn [[k v]] (= v val))] map)))

;; filter empty vectors
(defn denull-map [map]
  (setval [MAP-VALS #(= [] %)] NONE map))

(defn transform-map [map f]
  (transform MAP-VALS f map))

(defn add-maps [& maps]
  (apply merge-with add maps))

(defn mul-maps [& maps]
  (apply merge-with mul maps))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; VECTOR OPERATIONS

(defn magnitude [v]
  (sqrt
   (apply add
          (mapv #(mul [%] [%]) v))))

;; (defn normal [f]
;;   (let [g (grad f)]
;;     (divide
;;      (apply add g)
;;      (magnitude g))))

(defn jacobian [f]
  (select [ALL (fn [[k v]]
                 (and (> k 9) (< k 100)))]
          (diff f 1)))
  
(defn hessian [f]
  (select [ALL (fn [[k v]]
                 (and (> k 99) (< k 1000)))]
          (diff f 2)))

(defn grad [f]
  (vals
   (jacobian f)))

(defn div [f & n]
  (let [all-partials (vdiff f 1)
        partials (map #(get all-partials (+ (* 10 %) %))
                      (range 1 (inc (count f))))]
    (apply add
           (if n
             (map #(scale %2 %1)
                  n
                  (map mul f partials))                   
             (map mul f partials)))))

(defn curl [f & n]
  (let [vars (count f)
        range1 (range 1 (inc vars))
        range2 (range (dec vars) (+ vars (dec vars)))
        partials (vdiff f 1)
        partials-1 (mapv (fn [r1 r2]
                           (get partials
                                (+ (* 10 (inc (mod r2 vars)))
                                   (inc (mod r1 vars)))))
                         range1
                         range2)
        partials-2 (mapv (fn [r1 r2]
                           (get partials
                                (+ (* 10 (inc (mod r1 vars)))
                                   (inc (mod r2 vars)))))
                         range1
                         range2)]
    (apply add
           (if n
             (map #(scale %2 %1)
                  n
                  (map sub partials-1 partials-2))
             (map sub partials-1 partials-2)))))

(defn directional-diff [f n]
  (apply add
         (map #(scale %1 %2) (grad f) n)))

(defn laplacian [f]
  (let [all-partials (diff f 2)]
    (map #(get all-partials (+ 100 (* 10 %) %))
         (range 1 (count (first f))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(defn chain [f g]
  (let [vars (count (first f))
        grad-f (grad f)
        grad-g (grad g)
        f-g (map #(compose f g (inc %)) (range (dec vars)))]
    (map add
         (map-indexed #(mul (nth grad-f %1) %2) f-g)
         (map-indexed #(mul (nth grad-g %1) %2) f-g))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;        

(defn -main []
  )
