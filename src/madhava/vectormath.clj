(ns madhava.vectormath
  (:require [madhava.diff :refer :all]
            [madhava.arithmetic :refer :all]
            [com.rpl.specter :refer :all]))

(defn jacobian [f]
  (select [ALL (fn [[k v]]
                 (and (> k 9) (< k 100)))]
          (diff f 1)))

(defn hessian [f]
  (select [ALL (fn [[k v]]
                 (and (> k 99) (< k 1000)))]
          (diff f 2)))

(defn grad [f]
  (->> f
       (jacobian)
       (vals)))

(defn directional-diff [f n]
  (->> f
       (grad)
       (map #(scale % n))
       (#(apply add %))))  

(defn magnitude [v]
  (->> v
       (map #(mul % %))
       (#(apply add %))
       (sqrt)))

(defn normal [f]
  (let [g (grad f)]
    (divide (apply add g)
            (magnitude g))))

(defn laplacian [f]
  (let [partials (diff f 2)
        vars (inc (count (ffirst f)))]
    (map #(get partials (+ 100 (* 10 %) %))
         (range 1 vars))))

(defn div [f]
  (->> f
       (#(vector-diff % 1))
       (map-indexed #(get %2 (+ 10 (inc %1))))))

(defn curl [f]
  (let [vars (count (first (ffirst f)))
        range1 (range 1 (inc vars))
        range2 (range (dec vars) (+ vars (dec vars)))
        partials (vector-diff f 1)
        partials-1 (map (fn [r1 r2]
                          (get (nth partials (mod r1 vars))
                               (+ 10 (inc (mod r2 vars)))))
                        range2
                        range1)
        partials-2 (map (fn [r1 r2]
                          (get (nth partials (mod r1 vars))
                               (+ 10 (inc (mod r2 vars)))))
                        range1
                        range2)]
    (map sub partials-1 partials-2)))

(defn compose [f g idx]
  (let [idx (dec idx)]  ;; x == 1st var, 0th element in tuple 
    (loop [f f
           result {}]
      (let [term (first f)
            vars (first term)
            coeff (second term)
            v (nth vars idx)]
        (cond
          (nil? term) (into (sorted-map-by (comp - compare)) result)
          (zero? v) (recur (dissoc f vars) (add {vars coeff} result))
          :else (recur (dissoc f vars) (add (apply mul
                                                   {(assoc vars idx 0) coeff}
                                                   (repeat v g))  ;; raise g to exponent
                                            result)))))))

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
   (reduce chain (chain f g) more)))  ;; binary arity doesn't take vector args :(
                                      ;; should it return dot product or do multi-dimensional composition?

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
