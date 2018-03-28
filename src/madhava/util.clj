(ns madhava.util
  (:require [clojure.core :as cc]
            [clojure.math.combinatorics :as combo]
            [com.rpl.specter :refer :all]
            [primitive-math]))

(primitive-math/use-primitive-operators)

(defn add-dim
  "Projects a function into the next higher dimension
  by appending zeros to tuples of variables."
  [poly]
  (transform [MAP-KEYS] #(conj % 0) poly))

(defn denull
  "Removes terms with zero coefficients."
  [poly]
  (setval [MAP-VALS #(= % 0)] NONE poly))  

(defn grevlex
  "Comparator for tuple-keyed sorted-maps.
  Uses graded reverse lexicographic ordering
  for efficiency in computing Gröbner bases."
  [term1 term2]
  (let [grade1 (long (reduce +' term1))
        grade2 (long (reduce +' term2))
        comp (- grade2 grade1)] ;; total degree
    (if (not= 0 comp)
      comp
      (loop [term1 term1
             term2 term2]
        (if (empty? term1)
          0
          (let [grade1 (long (last term1))
                grade2 (long (last term2))
                comp (- grade1 grade2)] ;; differs from grlex because terms are flipped from above
            (if (not= 0 comp)
            comp
            (recur (pop term1)
                   (pop term2)))))))))

(defn factorial
  [n]
  (reduce *' (range 1 (inc n))))

(defn partition-set
  [n]
  (->> (range 1 (cc/inc n))
       combo/partitions))

(defn partition-multiset
  "Useful for 'collapsing' partitions."
  [s]
  (->> s
       frequencies
       (map #(vector (first %) (partition-set (second %))))
       (into {})))
 
(defn partition-int
  [n]
  (->> (repeat n 1)
       combo/partitions
       (mapv (fn [p] (mapv #(reduce +' %) p)))))
