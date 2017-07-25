(ns madhava.diff
  (:require [madhava.arithmetic :refer :all]
            [clojure.pprint :refer [pprint]]
            [clojure.data.int-map :as i]
            [com.rpl.specter :refer :all]))
              
(defn diff [poly order]
  (let [tape (atom (transient (i/int-map)))
        vars (count (first poly))]
    (letfn [(partial-diff [poly key idx]
              (let [partial (vec
                             (for [expr poly
                                   :let [v (get expr idx)]
                                   :when (not (zero? v))] 
                               (-> expr
                                   (update 0 * v)
                                   (update idx dec))))] 
                (swap! tape assoc! key partial)
                (list partial key)))
            (diff-loop [poly n]
              (when (<= n order)
                (doseq [p poly]
                  (diff-loop (map #(partial-diff (first p) (+ (* 10 (second p)) %) %)
                                   (range 1 vars))
                             (inc n)))))]
      (swap! tape assoc! 1 poly)
      (diff-loop [[poly 1]] 0)
      (persistent! @tape))))

(defn anti-diff [poly order]
  (let [tape (atom (transient (i/int-map)))
        vars (count (first poly))]
    (letfn [(partial-int [poly key idx]
              (let [partial (vec
                             (for [expr poly
                                   :let [v (get expr idx)]
                                   :when (not (zero? v))] 
                               (-> expr
                                   (update 0 / (inc v))
                                   (update idx inc))))] 
                (swap! tape assoc! key partial)
                (list partial key)))
            (int-loop [poly n]
              (when (<= n order)
                (doseq [p poly]
                  (int-loop (map #(partial-int (first p) (+ (* 10 (second p)) %) %)
                                   (range 1 vars))
                             (inc n)))))]
      (swap! tape assoc! 1 poly)
      (int-loop [[poly 1]] 0)
      (persistent! @tape))))
            
(defn pdiff [poly order]
  (let [*tape* (agent (transient (i/int-map)))
        vars (count (first poly))]
    (letfn [(partial-diff [poly key idx]
              (let [partial (vec
                             (for [expr poly
                                   :let [v (get expr idx)]
                                   :when (not (zero? v))] 
                               (-> expr
                                   (update 0 * v)
                                   (update idx dec))))] 
                (send *tape* assoc! key partial)
                (list partial key)))
            (diff-loop [poly n]
              (when (< n order)
                (doseq [p poly]
                  (diff-loop (pmap #(partial-diff (first p) (+ (* 10 (second p)) %) %)
                                   (range 1 vars))
                             (inc n)))))]
      (dosync
       (send *tape* assoc! 1 poly)
       (diff-loop [[poly 1]] 0))
      (await *tape*)
      (persistent! @*tape*))))

(defn anti-pdiff [poly order]
  (let [*tape* (agent (transient (i/int-map)))
        vars (count (first poly))]
    (letfn [(partial-int [poly key idx]
              (let [partial (vec
                             (for [expr poly
                                   :let [v (get expr idx)]
                                   :when (not (zero? v))] 
                               (-> expr
                                   (update 0 / (inc v))
                                   (update idx inc))))] 
                (send *tape* assoc! key partial)
                (list partial key)))
            (int-loop [poly n]
              (when (< n order)
                (doseq [p poly]
                  (int-loop (pmap #(partial-int (first p) (+ (* 10 (second p)) %) %)
                                  (range 1 vars))
                            (inc n)))))]
      (dosync
       (send *tape* assoc! 1 poly)
       (int-loop [[poly 1]] 0))
      (await *tape*)
      (persistent! @*tape*))))

(defn vector-diff [vf order]
  (pmap #(diff % order) vf))

(defn vector-antidiff [vf order]
  (apply merge
         (map-indexed
          (fn [idx tape]
            (transform ALL (fn [[k v]]
                             [(if (> k 10)
                                (+ k (* 10 idx))
                                k)
                              v]) tape))
          (pmap #(diff % order) vf))))

(defmacro print-tape [filename tape]
  `(pprint ~tape
           (clojure.java.io/writer
            (str ~filename ".txt"))))

(defn search-tape [tape val]
  (into (i/int-map)
        (select [ALL (fn [[k v]] (= v val))] tape)))

;; filter empty vectors
(defn denull-tape [tape]
  (setval [MAP-VALS #(= [] %)] NONE tape))

(defn transform-tape [tape f]
  (transform MAP-VALS f tape))

(defn add-tapes [& tapes]
  (apply merge-with add tapes))

(defn mul-tapes [& tapes]
  (apply merge-with mul tapes))
