(ns madhava.vectormath
  (:require [madhava.diff :refer :all]
            [madhava.arithmetic :refer :all]
            [com.rpl.specter :refer :all]))

(defn magnitude [v]
  (sqrt
   (apply add
          (mapv #(mul [%] [%]) v))))

;; (defn normal [f]
;;   (let [g (grad f)]
;;     (divide
;;      (apply add g)
;;      (magnitude g))))

(defn jacobian [f]
  (select [ALL (fn [[k v]]
                 (and (> k 9) (< k 100)))]
          (diff f 1)))
  
(defn hessian [f]
  (select [ALL (fn [[k v]]
                 (and (> k 99) (< k 1000)))]
          (diff f 2)))

(defn grad [f]
  (vals
   (jacobian f)))

(defn div [f]
  (map #(get %1 (+ 10 %2))
       (vector-diff f 1)
       (range 1 (inc (count (ffirst f))))))

(defn curl [f]
  (let [vars (count f)
        range1 (range 1 (inc vars))
        range2 (range (dec vars) (+ vars (dec vars)))
        partials (vector-diff f 1)
        partials-1 (mapv (fn [r1 r2]
                           (get (nth partials
                                     (mod r1 vars))
                                (+ 10
                                   (inc (mod r2 vars)))))
                         range2
                         range1)
        partials-2 (mapv (fn [r1 r2]
                           (get (nth partials
                                     (mod r1 vars))
                                (+ 10
                                   (inc (mod r2 vars)))))
                         range1
                         range2)]
    (map sub partials-1 partials-2)))

(defn directional-diff [f n]
  (apply add
         (map #(scale %1 %2) (grad f) n)))

(defn laplacian [f]
  (let [all-partials (diff f 2)]
    (map #(get all-partials (+ 100 (* 10 %) %))
         (range 1 (count (first f))))))

(defn compose [f g var]
  (loop [f f
         result []]
    (let [term (first f)]
      (if (nil? term)
        result
        (recur (next f) (if (zero? (nth term var))
                          (add [term] result)
                          (simplify (sort-terms
                                     (vec (concat result
                                                  (nth (iterate (partial mul [(assoc term var 0)]) g)
                                                       (nth term var))))))))))))

(defn chain [f g]
  (let [vars (count (first f))
        grad-f (grad f)
        grad-g (grad g)
        f-g (map #(compose f g (inc %)) (range (dec vars)))]
    (map add
         (map-indexed #(mul (nth grad-f %1) %2) f-g)
         (map-indexed #(mul (nth grad-g %1) %2) f-g))))
