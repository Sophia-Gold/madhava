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

(defn chain1 [f g idx]
  ;; faster for univariate functions
  (let [i (dec idx)]
    (-> f
        (grad)
        (nth i)
        (compose g idx)
        (mul (nth (grad g) i)))))

(defn chain
  ;; transducer
  ([f]  ;; completion
   (fn
     ([] f)
     ([g] (chain f g))
     ([g & more] (chain f g more))))
  ([f g] 
   (->> f
        (grad)
        (map-indexed #(compose %2 g (inc %1)))
        (map mul (grad g))
        (reduce add)))
  ([f g & more]
   (reduce chain (chain f g) more)))
