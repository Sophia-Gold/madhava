(ns madhava.diff
  (:require [madhava.arithmetic :refer :all]
            [clojure.pprint :refer [pprint]]
            [clojure.data.int-map :as i]
            [com.rpl.specter :refer :all]))

(defn dims [vars]
  (loop [v vars
         i 0
         d 1]
    (let [q (quot vars d)]
      (if (zero? q)
        i
        (recur q (inc i) (* d 10))))))

(defn int-nth [i idx dims]
  (loop [i i
         n (dec dims)]
    (cond
       (zero? n) i
       (> n idx) (recur (quot i 10) (dec n))
       :else (recur (mod i 10) (dec n)))))

(defn diff [poly order]
  (let [tape (atom (transient (i/int-map)))
        dims (dims (ffirst poly))]
    (letfn [(partial-diff [poly key idx]
              (let [partial (into {}
                                  (map #(let [vars (first %)
                                              coeff (second %) 
                                              v (int-nth vars idx dims)]
                                          (when (not (zero? v))
                                            [(- vars (int (* v (Math/pow 10 (dec (- dims idx)))))) ;; ugly line
                                             (* coeff v)]))
                                       poly))]
                (swap! tape assoc! key partial)
                (list partial key)))
            (diff-loop [poly n]
              (when (<= n order)
                (doseq [p poly]
                  (diff-loop
                   (map #(partial-diff (first p) (+ (* 10 (second p)) (inc %)) %)
                        (range dims))
                   (inc n)))))]
      (diff-loop (list (list poly 0)) 0)
      (persistent! @tape))))

(defn anti-diff [poly order]
  (let [tape (atom (transient (i/int-map)))
        vars (count (ffirst poly))]
    (letfn [(partial-int [poly key idx]
              (let [partial (into {}
                                  (map #(let [vars (first %)
                                              coeff (second %)
                                              v (nth vars idx)]
                                          (when (not (zero? v))
                                            [(update vars idx inc)
                                             (/ coeff (inc v))]))
                                       poly))]
                (swap! tape assoc! key partial)
                (list partial key)))
            (int-loop [poly n]
              (when (<= n order)
                (doseq [p poly]
                  (int-loop (map #(partial-int (first p) (+ (* 10 (second p)) (inc %)) %)
                                 (range vars))
                            (inc n)))))]
      (int-loop (list (list poly 0)) 0)
      (persistent! @tape))))
            
(defn pdiff [poly order]
  (let [*tape* (agent (transient (i/int-map)))
        vars (count (ffirst poly))]
    (letfn [(partial-diff [poly key idx]
              (let [partial (into {}
                                  (map #(let [vars (first %)
                                              coeff (second %) 
                                              v (nth vars idx)]
                                          (when (not (zero? v))
                                            [(update vars idx dec)
                                             (* coeff v)]))
                                       poly))]
                (send *tape* assoc! key partial)
                (list partial key)))
            (diff-loop [poly n]
              (when (< n order)
                (doseq [p poly]
                  (diff-loop (pmap #(partial-diff (first p) (+ (* 10 (second p)) (inc %)) %)
                                   (range vars))
                             (inc n)))))]
      (diff-loop (list (list poly 0)) 0)
      (await *tape*)
      (persistent! @*tape*))))

(defn anti-pdiff [poly order]
  (let [*tape* (agent (transient (i/int-map)))
        vars (count (ffirst poly))]
    (letfn [(partial-int [poly key idx]
              (let [partial (into {}
                                  (map #(let [vars (first %)
                                              coeff (second %)
                                              v (nth vars idx)]
                                          (when (not (zero? v))
                                            [(update vars idx inc)
                                             (/ coeff (inc v))]))
                                       poly))]
                (send *tape* assoc! key partial)
                (list partial key)))
            (int-loop [poly n]
              (when (< n order)
                (doseq [p poly]
                  (int-loop (pmap #(partial-int (first p) (+ (* 10 (second p)) (inc %)) %)
                                  (range vars))
                            (inc n)))))]
      (int-loop (list (list poly 0)) 0)
      (await *tape*)
      (persistent! @*tape*))))

(defn vector-diff [vf order]
  (pmap #(diff % order) vf))

(defn vector-antidiff [vf order]
  (pmap #(anti-diff % order) vf))

(defmacro print-tape [filename tape]
  `(pprint ~tape
           (clojure.java.io/writer
            (str ~filename ".txt"))))

(defn search-tape [tape val]
  (into (i/int-map)
        (select [ALL (fn [[k v]] (= v val))] tape)))

(defn denull-tape [tape]
  ;; remove empty partials
  (setval [MAP-VALS #(= {} %)] NONE tape))

(defn transform-tape [tape f]
  (transform MAP-VALS f tape))

(defn add-tapes [& tapes]
  (apply merge-with add tapes))

(defn mul-tapes [& tapes]
  (apply merge-with mul tapes))
