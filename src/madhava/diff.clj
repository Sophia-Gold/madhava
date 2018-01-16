(ns madhava.diff
  (:require [madhava.arithmetic :refer :all]
            [madhava.util :refer :all]
            [clojure.pprint :refer [pprint]]
            [clojure.data.int-map :as i]
            [clojure.core.reducers :as r]
            [com.rpl.specter :refer :all]
            [clj-tuple :refer [vector]])
  (:refer-clojure :exclude [vector]))

(defn diff [poly order]
  (let [tape (atom (transient (i/int-map)))
        dims (count (ffirst poly))]
    (letfn [(partial-diff [poly key idx]
              (let [partial (transform [ALL] (fn [[k v]]
                                               (let [var (nth k idx)]
                                                 (when (not (zero? var))
                                                   [(update k idx dec)
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

(defn anti-diff [poly order]
  (let [tape (atom (transient (i/int-map)))
        dims (count (ffirst poly))]
    (letfn [(partial-diff [poly key idx]
              (let [partial (transform [ALL] (fn [[k v]]
                                               (let [var (nth k idx)]
                                                 (when (not (zero? var))
                                                   [(update k idx inc)
                                                    (/ v (inc var))])))
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
        dims (count (ffirst poly))]
    (letfn [(partial-diff [poly key idx]
              (let [partial (transform [ALL] (fn [[k v]]
                                               (let [var (nth k idx)]
                                                 (when (not (zero? var))
                                                   [(update k idx dec)
                                                    (* v var)])))
                                       poly)]
                (send *tape* assoc! key partial)
                (list partial key)))
            (diff-loop [poly n]
              (when (< n order)
                (doall
                 (pmap #(diff-loop (partial-diff (first poly) (+ (* 10 (second poly)) (inc %)) %)
                                   (inc n))
                       (range dims)))))]
      (diff-loop (list poly 0) 0)
      (await *tape*)
      (persistent! @*tape*))))

(defn anti-pdiff [poly order]
  (let [*tape* (agent (transient (i/int-map)))
        dims (count (ffirst poly))]
    (letfn [(partial-diff [poly key idx]
              (let [partial (transform [ALL] (fn [[k v]]
                                               (let [var (nth k idx)]
                                                 (when (not (zero? var))
                                                   [(update k idx inc)
                                                    (/ v (inc var))])))
                                       poly)]
                (send *tape* assoc! key partial)
                (list partial key)))
            (diff-loop [poly n]
              (when (< n order)
                (doall
                 (pmap #(diff-loop (partial-diff (first poly) (+ (* 10 (second poly)) (inc %)) %)
                                   (inc n))
                       (range dims)))))]
      (diff-loop (list poly 0) 0)
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
  (->> tape
       (select [ALL (fn [[k v]] (= v val))])
       (r/fold i/merge conj)))

(defn denull-tape [tape]
  ;; remove empty partials
  (setval [MAP-VALS #(= {} %)] NONE tape))

(defn transform-tape [tape f]
  (transform MAP-VALS f tape))

(defn add-tapes [& tapes]
  (apply merge-with add tapes))

(defn mul-tapes [& tapes]
  (apply merge-with mul tapes))
