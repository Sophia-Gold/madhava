(ns madhava.vectormath
  (:require [madhava.diff :refer :all]
            [madhava.arithmetic :refer :all]
            [madhava.util :refer :all]
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

(defn laplacian [f]
  (let [partials (diff f 2)
        vars (dims (first (last f)))]
    (->> (range 1 (inc vars))
         (map #(get partials (+ (* 10 %) %)))
         (#(apply add %)))))

(defn div [f]
  (->> f
       (#(vector-diff % 1))
       (map-indexed #(get %2 (inc %1)))
       (#(apply add %))))

(defn curl [f]
  (let [vars (dims (first (last (first f))))
        range1 (range 1 (inc vars))
        range2 (range (dec vars) (+ vars (dec vars)))
        partials (vector-diff f 1)
        partials-1 (map (fn [r1 r2] 
                          (get (nth partials (mod r1 vars) vars)  ;; '(2 0 1)
                               (inc (mod r2 vars))))  ;; '(1 2 0)
                        range2
                        range1)
        partials-2 (map (fn [r1 r2]
                          (get (nth partials (mod r1 vars) vars)  ;; '(1 2 0)
                               (inc (mod r2 vars))))  ;; '(2 0 1)
                        range1
                        range2)]
    (map sub partials-1 partials-2)))
