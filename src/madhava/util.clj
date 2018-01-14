(ns madhava.util
  (:require [com.rpl.specter :refer :all]))

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
  (if (<= dims idx)  ;; no leading zeros means different digit lengths
    0
    (loop [i i
           n (dec dims)]
      (cond
        (zero? n) i
        (> n idx) (recur (quot i 10) (dec n))
        :else (recur (mod i 10) (dec n))))))

(defn grevlex [term1 term2]
  (letfn [(grade [i]
            (let [dims (dims i)]
              (loop [count dims
                     result 0]
                (if (zero? count)
                  result
                  (recur (inc count) (+ result
                                        (int-nth i count)))))))]                      
    (let [grade1 (grade term1)
          grade2 (grade term2)
          comp (- grade2 grade1)] ;; total degree
      (if (not= 0 comp)
        comp
        (loop [term1 term1
               term2 term2]
          (let [dims1 (dims term1)
                dims2 (dims term2)]
            (cond
              (and
               (zero? dims1)
               (zero? dims2)) 0
              (zero? dims1)  term1
              (zero? dims2)  (- term2)
              :else (let [grade1 (int-nth term1 (dec dims1))
                          grade2 (int-nth term2 (dec dims2))
                          comp (- grade1 grade2)] ;; differs from grlex because terms are flipped from above
                      (if (not= 0 comp)
                        comp
                        (recur (quot term1 10)
                               (quot term2 10)))))))))))
