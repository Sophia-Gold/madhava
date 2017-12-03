(ns madhava.util
  (:require [com.rpl.specter :refer :all]
            [clojure.data.int-map :as i]))

(defn add-dim [poly]
  ;; projects into next higher dimension by appending zero to keys
  (transform [MAP-KEYS] #(* % 10) poly))

(defn denull [poly]
  (setval [MAP-VALS #(= % 0)] NONE poly))
  
(defn dims [vars]
  (loop [v vars
         i 0
         d 1]
    (let [q (quot vars d)]
      (if (zero? q)
        i
        (recur q (inc i) (* d 10))))))

(defn int-nth [i idx dims]
  (loop [i i
         n (dec dims)]
    (cond
      (zero? n) i
      (> n idx) (recur (quot i 10) (dec n))
      :else (recur (mod i 10) (dec n)))))
