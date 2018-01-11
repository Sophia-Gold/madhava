(ns madhava.taylorseries
  (:require [clojure.data.avl :as avl]))

(defn integrate-series [s]
  (map / s (drop 1 (range))))

(defn negate-series [s]
  (map - s))

(defn dense-to-sparse [s]
  (->> s
       (map-indexed #(if (not= %2 0) [%1 %2]))
       (filterv some?)
       (into (avl/sorted-map))))

(defn sparse-to-dense [poly]
  ;; only for univariate polys
  (let [poly (into (avl/sorted-map) poly)
        order (keys poly)
        diff-terms (concat (map #(dec (- %1 %2)) (next order) order) (list 0))]
    (mapcat #(cons %2 (repeat %1 0)) diff-terms (vals poly))))
      
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
