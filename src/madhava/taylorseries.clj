(ns madhava.taylorseries
  (:require [clojure.data.int-map :as i]
            [clojure.core.reducers :as r]))

(defn integrate-series [s]
  (map / s (drop 1 (range))))

(defn negate-series [s]
  (map - s))

(defn dense-to-sparse [s]
  ;; only for univariate polys
  (->> s
       (map-indexed #(if (not= %2 0) [%1 %2]))
       (filter some?)
       (r/fold i/merge conj)))

(defn sparse-to-dense [poly]
  ;; only for univariate polys
  (let [order (keys poly)
        diff-terms (cons (first order) (map (comp dec -') (next order) order))]
    (mapcat #(concat (repeat %1 0) (list %2)) diff-terms (vals poly))))
 
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
