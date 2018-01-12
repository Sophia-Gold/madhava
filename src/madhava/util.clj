(ns madhava.util
  (:require [com.rpl.specter :refer :all]
            [clojure.data.avl :as avl]))

(defn add-dim [poly]
  ;; projects into next higher dimension by appending zero to tuples of variables
  (transform [MAP-KEYS] #(conj % 0) poly))

(defn denull [poly]
  (setval [MAP-VALS #(= % 0)] NONE poly))  

(defn grevlex [poly1 poly2]
  (let [grade1 (reduce +' poly1)
        grade2 (reduce +' poly2)
        comp (- grade1 grade2)]
    (if (not= 0 comp)
      comp
      (loop [poly1 poly1
             poly2 poly2]
        (let [grade1 (first poly1)
              grade2 (first poly2)
              comp (- grade1 grade2)]
          (cond
            (empty? poly1) 0
            (not= 0 comp) comp
            :else (recur (next poly1)
                         (next poly2))))))))
