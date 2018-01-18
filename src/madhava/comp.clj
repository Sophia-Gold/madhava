(ns madhava.comp
  (:require [madhava.arithmetic :refer :all]
            [madhava.util :refer :all]
            [madhava.vectormath :refer :all]
            [clojure.data.avl :refer [sorted-map-by]]
            [clj-tuple :refer [vector]])
  (:refer-clojure :exclude [vector sorted-map-by]))
  
(defn compose [f g idx]
  (let [idx (dec idx)]  ;; x == 1st var, 0th element in tuple 
    (loop [f f
           result {}]
      (let [term (first f)
            vars (first term)
            coeff (second term)
            v (nth vars idx)]
        (cond
          (nil? term) (into (sorted-map-by grevlex) result)
          (zero? v) (recur (dissoc f vars) (add {vars coeff} result))
          :else (recur (dissoc f vars) (add (apply mul
                                                   {(assoc vars idx 0) coeff}
                                                   (repeat v g))  ;; raise g to exponent
                                            result)))))))

(defn revert [f]
  ;; compositional inverse
  ;; aka Horner's rule
  ;; see Knuth TAOCP vol 2 pp. 486-488
  (let [dense-f (atom f)]
    (run! #(let [term1 (first %1)]
             (when (not= 0 (- (reduce +' term1)
                              (reduce +' (first %2))))
               (let [max-exp (max term1)]
                 (swap! dense-f assoc
                        (update term1 (.indexOf max-exp) (dec max-exp))
                        0))))
          f
          (next f))
    @dense-f))

(defn ruffini-horner [f & rest]
  (->> f
       (map #(->> %2
                  (revert)
                  (reduce (fn [x y]
                            (+ y (* x %1))))) ;; substitute values in rest args for variables in f
            rest)
       (reduce *'))) ;; multiply result of evaluating each variable
       
(defn chain
  ;; transducer form of compose
  ([f]  ;; completion
   (fn
     ([] f)
     ([g] (chain f g))
     ([g & more] (chain f g more))))
  ([f g]
   (let [vars (count (ffirst f))
         grad-f (grad f)
         grad-g (grad g)
         f-g (map #(compose f g (inc %)) (range vars))]
     (map add
          (map-indexed #(mul (nth grad-f %1) %2) f-g)
          (map-indexed #(mul (nth grad-g %1) %2) f-g))))
  ([f g & more]
   (reduce chain (chain f g) more)))

(defn rchain
  ;; transducer form of compose
  ([f]  ;; completion
   (fn
     ([] f)
     ([f g] (rchain f g))
     ([f g & more] (apply rchain f g more))))
  ([f g]
   (let [vars (count (ffirst f))
         grad-f (grad f)
         grad-g (grad g)
         g-f (map #(compose g f (inc %)) (range vars))]  ;; transpose matroid
     (map add                                                  
          (map-indexed #(mul (nth grad-f %1) %2) g-f)
          (map-indexed #(mul (nth grad-g %1) %2) g-f))))
  ([f g & more]
   (reduce rchain (rseq (conj more (rchain f g))))))  ;; reverse mode AD is just reversing a vector!
