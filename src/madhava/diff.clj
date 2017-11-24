(ns madhava.diff
  (:require [madhava.util :refer :all]
            [madhava.arithmetic :refer :all]
            [clojure.pprint :refer [pprint]]
            [clojure.data.int-map :as i]
            [com.rpl.specter :refer :all]))

(defn diff [poly order]
  (let [tape (atom (transient (i/int-map)))
        dims (dims (first (last poly)))]
    (letfn [(partial-diff [poly key idx]
              (let [partial (transform [ALL] (fn [[k v]]
                                               (let [var (int-nth k idx dims)]
                                                 (when (not (zero? var))
                                                   [(- k (* var (long (Math/pow 10 (dec (- dims idx))))))
                                                    (* v var)])))
                                       poly)]
                (swap! tape assoc! key partial)
                (list partial key)))
            (diff-loop [poly n]
              (when (< n order)
                (run! #(diff-loop (partial-diff (first poly) (+ (* 10 (second poly)) (inc %)) %)
                                  (inc n))
                      (range dims))))]
      (diff-loop (list poly 0) 0)
      (persistent! @tape))))

(defn pdiff [poly order]
  (let [*tape* (agent (transient (i/int-map)))
        dims (dims (first (last poly)))]
    (letfn [(partial-diff [poly key idx]
              (let [partial (transform [ALL] (fn [[k v]]
                                               (let [var (int-nth k idx dims)]
                                                 (when (not (zero? var))
                                                   [(- k (* var (long (Math/pow 10 (dec (- dims idx))))))
                                                    (* v var)])))
                                       poly)]
                (send *tape* assoc! key partial)
                (list partial key)))
            (diff-loop [poly n]
              (when (< n order)
                (run! #(diff-loop (partial-diff (first poly) (+ (* 10 (second poly)) (inc %)) %)
                                  (inc n))
                      (range dims))))]
      (diff-loop (list poly 0) 0)
      (await *tape*)
      (persistent! @*tape*))))

(defn vector-diff [vf order]
  (pmap #(diff % order) vf))

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
