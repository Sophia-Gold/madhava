(ns madhava.vectormath
  (:require [madhava.diff :refer :all]
            [madhava.arithmetic :refer :all]
            [clojure.core :as cc]
            [com.rpl.specter :refer :all]
            [primitive-math]
            [clj-tuple :refer [vector]])
  (:refer-clojure :exclude [vector]))

(primitive-math/use-primitive-operators)

(defn jacobian
  "Computes Jacobian matrix. 
  Returns an int-map of first-order partial derivatives."
  [f]
  (diff f 1))

(defn hessian
  "Computes Hessian matrix. 
  Returns an int-map of second-order partial derivatives."
  [f]
  (select [ALL (fn [[^long k ^long v]] (and (> k 9) (< k 100)))]
          (diff f 2)))

(defn grad
  "Gradient (∇).
  Returns a sequence of functions in Cartesian form."
  [f]
  (->> f
       jacobian
       vals
       (into (vector))))

(defn directional-diff
  "Directional derivative of vector f with magnitude n."
  [f n]
  (->> f
       grad
       (map #(scale % n))
       (#(apply add %))))

(defn magnitude
  "Computes the magnitude of a vector in Cartesian form."
  [v]
  (->> v
       (map #(mul % %))
       (#(apply add %))
       sqrt))

(defn normal
  "Computes normal of n-manifold represented by f."
  [f]
  (let [g (grad f)]
    (quotient (apply add g)
            (magnitude g))))

(defn laplacian
  "Laplace operator (∇^2)."
  [f]
  (->> f
      (#(diff-unmixed % 2))
      vals
      (map peek)
      (apply add)))

(defn div
  "Divergence operator.
  Takes a sequence of functions in Cartesian form."
  [vf]
  (let [dims (count vf)]
    (->> vf
         (#(vector-diff % 1))
         (map-indexed #(get %2 (inc (long %1))))
         (apply add))))
 
(defn curl
  "Curl (rotation) operator.
  Returns a sequence of functions in Cartesian form."
  [f]
  (let [dims (count (first (ffirst f)))
        range1 (range 1 (inc dims))
        range2 (range (dec dims) (+ dims (dec dims)))
        partials (vector-diff f 1)
        partials-1 (map (fn [r1 r2]
                          (get (nth partials (mod r1 dims))
                               (inc (long (mod r2 dims)))))
                        range2
                        range1)
        partials-2 (map (fn [r1 r2]
                          (get (nth partials (mod r1 dims))
                               (inc (long (mod r2 dims)))))
                        range1
                        range2)]
    (mapv sub partials-1 partials-2)))
