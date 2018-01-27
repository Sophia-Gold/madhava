(ns madhava.taylorseries
  (:require [madhava.util :refer :all]
            [clojure.data.avl :refer [sorted-map-by]]
            [clj-tuple :refer [vector]])
  (:refer-clojure :exclude [vector sorted-map-by]))

(defn integrate-series
  "Computes integral of formal power series, generating functions, and dense univariate polynomials."
  [s]
  (map / s (drop 1 (range))))

(defn negate-series
  "Negates all terms in series."
  [s]
  (map - s))

(defn mul-series
  "Cauchy product."
  [s1 s2]
  (cons (* (first s1)
           (first s2))
        (lazy-seq
         (map +'
              (map * (rest s2) (repeat (first s1)))
              (mul-series (rest s1) s2)))))

(defn dense-to-sparse
  "Converts univariate series or polynomial from dense to sparse form."
  [s]
  (->> s
       (map-indexed #(if (not= %2 0) [[%1] %2]))
       (filterv some?)
       (into (sorted-map-by grevlex))))

(defn sparse-to-dense
  "Converts univariate series or polynomial from sparse to dense form."
  [poly]
  (let [order (->> poly
                   (keys)
                   (map first))
        diff-terms (concat (map (comp dec -') order (next order))
                           (list (last order)))]
    (reverse
     (mapcat #(cons %1 (repeat %2 0)) (vals poly) diff-terms ))))

(defn exp-series
  "Taylor series of exponential function."
  []
  (->> (exp-series)
       (integrate-series)
       (lazy-cat [1])))

(declare cos-series)
(defn sin-series
  "Taylor series of sine."
  []
  (->> (cos-series)
       (integrate-series)
       (lazy-cat [0])))

(defn cos-series
  "Taylor series of cosine."
  []
  (->> (sin-series)
       (negate-series)
       (integrate-series)
       (lazy-cat [1])))

(defn atan-series
  "Taylor series of arctangent."
  []
  (integrate-series
   (cycle [1 0 -1 0])))

(declare cosh-series)
(defn sinh-series
  "Taylor series of hyperbolic sine function."
  []
  (->> (cosh-series)
       (integrate-series)
       (lazy-cat [0])))

(defn cosh-series
  "Taylor series of hyperbolic cosine function."
  []
  (->> (sinh-series)
       (integrate-series)
       (lazy-cat [1])))

(defn partitions
  "Partition function for positive integers."
  [n]  
  (letfn [(p [n]
            (cons 1
                  (lazy-seq
                   (map +'
                        (p (+ n 1))
                        (concat (repeat (- n 1) 0) (p n))))))]
    (nth (p 1) (dec n))))

(defn bell
  "Bell numbers: partitions of a set containing n elements where n > 0."
  [n]
  (int
   (Math/ceil
    (/ (reduce +'
               (map #(/ (Math/pow % n)
                        (reduce *' (range 1 (inc %)))) ;; factorial
                    (range (* 2 n))))
       (Math/E)))))
