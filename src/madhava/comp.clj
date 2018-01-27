(ns madhava.comp
  (:require [madhava.arithmetic :refer :all]
            [madhava.diff :refer :all]
            [madhava.util :refer :all]
            [madhava.taylorseries :refer :all]
            [madhava.vectormath :refer :all]
            [clojure.data.avl :refer [sorted-map-by]]
            [clj-tuple :refer [vector]])
  (:refer-clojure :exclude [vector sorted-map-by]))

(defn pow [f exp]
  (->> f
       (repeat exp)
       (apply mul)))

(defn compose
  "Functional composition. 
  Third argument is index of variable, starting from 1."
  [f g idx]
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

(defn revert 
  "Computes compositional inverse, aka Horner's rule.
  See Knuth TAOCP vol 2 pp. 486-488."
  [f]
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

(defn chain1
  "Faster implementation of chain rule for univariate functions."
  [f g idx]
  (let [i (dec idx)]
    (-> f
        (grad)
        (nth i)
        (compose g idx)
        (mul (nth (grad g) i)))))

(defn chain
  "Chain rule. 
  Variadic, unary version returns transducer."
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

(defn chain-higher1
  "Faster implementation of higher-order chain rule for univariate functions."
  [f g order]
  (let [block-num (partitions order)
        block-size (bell order)
        f' (diff f order)
        g' (diff g order)]
    (mul (reduce add (map #(compose (get f' %) g 1)
                          (range 1 (inc block-num))))
         (reduce mul (map #(get g' %1)
                          (range 1 (inc block-size)))))))

;; Example:
;; (comp f g)''''
;; = 1f'''' * g'^4
;; + 6f'''  * g''    * g'^2
;; + 3 f''  * g''^2
;; + 4 f''  * g'''   * g'
;; + 1f'    * g''''

(defn chain-higher
  "Higher-order chain rule using Faà di Bruno's formula."
  [f g order]
  (let [dims (count (ffirst f))
        n! (reduce *' (range 1 (dec order)))
        m! '()
        f' (vals (diff f order))
        g' (vals (diff g order))
        f*g (map-indexed #(compose %2 g (inc %1)) f')
        g'' (->> g'
                 (map #(mul %1 (repeat %1 (reduce *' %2)))
                      g'
                      (take 10 (mapcat #(repeat dims %) (range))))  ;; exponentiation by order
                 (reduce *'))]
    (scale (mul f*g g'')
           (/ n! m!))))

