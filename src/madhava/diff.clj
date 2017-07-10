(ns madhava.diff
  (:require [madhava.arithmetic :refer :all]
            [clojure.pprint :refer [pprint]]
            [clojure.data.int-map :as i]
            [com.rpl.specter :refer :all]))

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
