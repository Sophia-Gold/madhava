(ns madhava.util
  (:require [com.rpl.specter :refer :all]))

(defn add-dim [poly]
  ;; projects into next higher dimension by appending zero to tuples of variables
  (transform [MAP-KEYS] #(conj % 0) poly))

(defn denull [poly]
  (setval [MAP-VALS #(= % 0)] NONE poly))  

(defn grevlex [term1 term2]
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
