(ns madhava.vectormath
  (:require [madhava.diff :refer :all]
            [madhava.arithmetic :refer :all]
            [com.rpl.specter :refer :all]))

(defn jacobian [f]
  (diff f 1))  

(defn hessian [f]
  (select [ALL (fn [[k v]] (and (> k 9) (< k 100)))]
          (diff f 2)))

(defn grad [f]
  (->> f
       (jacobian)
       (vals)))

(defn directional-diff [f n]
  (->> f
       (grad)
       (map #(scale % n))
       (#(apply add %))))

(defn magnitude [v]
  (->> v
       (map #(mul % %))
       (#(apply add %))
       (sqrt)))

(defn normal [f]
  (let [g (grad f)]
    (divide (apply add g)
            (magnitude g))))

(defn laplacian [f]
  (let [partials (diff f 2)
        vars (inc (count (ffirst f)))]
    (map #(get partials (+ (* 10 %) %))
         (range 1 vars))))

(defn div [f]
  (->> f
       (#(vector-diff % 1))
       (map-indexed #(get %2 (inc %1)))))

(defn curl [f]
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
