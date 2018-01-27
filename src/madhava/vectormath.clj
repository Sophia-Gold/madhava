(ns madhava.vectormath
  (:require [madhava.diff :refer :all]
            [madhava.arithmetic :refer :all]
            [com.rpl.specter :refer :all]
            [clj-tuple :refer [vector]])
  (:refer-clojure :exclude [vector]))

(defn jacobian
  "Computes Jacobian matrix. 
  Returns an int-map of first-order partial derivatives."
  [f]
  (diff f 1))  

(defn hessian
  "Computes Hessian matrix. 
  Returns an int-map of first-order partial derivatives."
  [f]
  (select [ALL (fn [[k v]] (and (> k 9) (< k 100)))]
          (diff f 2)))

(defn grad
  "Gradient. Returns a list of functions in Cartesian form."
  [f]
  (->> f
       (jacobian)
       (vals)))

(defn directional-diff
  "Directional derivative of vector f with magnitude n."
  [f n]
  (->> f
       (grad)
       (map #(scale % n))
       (#(apply add %))))

(defn magnitude
  "Computes the magnitude of a vector in Cartesian form."
  [v]
  (->> v
       (map #(mul % %))
       (#(apply add %))
       (sqrt)))

(defn normal
  "Computes normal of n-manifold represented by f."
  [f]
  (let [g (grad f)]
    (divide (apply add g)
            (magnitude g))))

(defn laplacian [f]
  (let [partials (diff f 2)
        vars (inc (count (ffirst f)))]
    (->> (range 1 vars)
         (map #(get partials (+ (* 10 %) %)))
         (#(apply add %)))))

(defn div
  "Divergence in Cartesian form."
  [f]
  (->> f
       (#(vector-diff % 1))
       (map-indexed #(get %2 (inc %1)))
       (#(apply add %))))

(defn curl
  "Curl (rotation) in Cartesian form."
  [f]
  (let [vars (count (first (ffirst f)))
        range1 (range 1 (inc vars))
        range2 (range (dec vars) (+ vars (dec vars)))
        partials (vector-diff f 1)
        partials-1 (map (fn [r1 r2]
                          (get (nth partials (mod r1 vars))
                               (inc (mod r2 vars))))
                        range2
                        range1)
        partials-2 (map (fn [r1 r2]
                          (get (nth partials (mod r1 vars))
                               (inc (mod r2 vars))))
                        range1
                        range2)]
    (map sub partials-1 partials-2)))
