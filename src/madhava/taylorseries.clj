(ns madhava.taylorseries
  (:require [madhava.util :refer :all]
            [clojure.data.avl :refer [sorted-map-by]]
            [clj-tuple :refer [vector]])
  (:refer-clojure :exclude [vector sorted-map-by]))

(defn integrate-series [s]
  (map / s (drop 1 (range))))

(defn negate-series [s]
  (map - s))

(defn dense-to-sparse [s]
  ;; only for univariate polys
  (->> s
       (map-indexed #(if (not= %2 0) [[%1] %2]))
       (filterv some?)
       (into (sorted-map-by grevlex))))

(defn sparse-to-dense [poly]
  ;; only for univariate polys
  (let [order (->> poly
                   (keys)
                   (map first))
        diff-terms (concat (map (comp dec -') order (next order))
                           (list (last order)))]
    (reverse
     (mapcat #(cons %1 (repeat %2 0)) (vals poly) diff-terms ))))

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
