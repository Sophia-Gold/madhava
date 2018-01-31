(ns madhava.util
  (:require [clojure.core :as cc]
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
  (let [grade1 (reduce +' term1)
        grade2 (reduce +' term2)
        comp (- grade2 grade1)] ;; total degree
    (if (not= 0 comp)
      comp
      (loop [term1 term1
             term2 term2]
        (if (empty? term1)
          0
          (let [grade1 (last term1)
                grade2 (last term2)
                comp (- grade1 grade2)] ;; differs from grlex because terms are flipped from above
            (if (not= 0 comp)
            comp
            (recur (pop term1)
                   (pop term2)))))))))
