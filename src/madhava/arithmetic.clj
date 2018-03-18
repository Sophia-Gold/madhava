(ns madhava.arithmetic
  (:require [madhava.util :refer :all]
            [clojure.core :as cc]
            [clojure.data.avl :refer [sorted-map-by]]
            [primitive-math]
            [clj-tuple :refer [vector]])
  (:refer-clojure :exclude [vector sorted-map-by]))

(primitive-math/use-primitive-operators)

(defn add
  "Polynomial addition.
  Variadic, nullary version returns transducer."
  ([]  ;; init
   (fn
     ([poly] poly)
     ([poly1 poly2] (add poly1 poly2))
     ([poly1 poly2 & more] (reduce add (add poly1 poly2) more))))
  ([poly] poly)  ;; completion 
  ([poly1 poly2]  ;; step
   (denull
    (into (sorted-map-by grevlex)
          (merge-with +' poly1 poly2))))
  ([poly1 poly2 & more]
   (reduce add (add poly1 poly2) more)))

(defn negate
  "Negates all terms."
  [poly]
  (->> poly
       (map #(update % 1 -'))
       (into {})))

(defn sub
  "Polynomial subtraction.
  Consistent with scalar subtraction: 
  arity 1 applies negation rather than returning transducer."
  ([poly] (negate poly))
  ([poly1 poly2]
   (add poly1 (negate poly2)))
  ([poly1 poly2 & more]
   (reduce sub (add poly1 (negate poly2)) more)))

(defn scale [poly scalar]
  (->> poly
       (map #(update % 1 *' scalar))
       (into {})
       (denull)))

(defn monomul
  "Efficiently multiply a polynomial by a monomial."
  [poly term]
  (let [vars (first term)
        coeff (second term)]
  (->> poly
       (map #(vector (mapv +' vars (first %))
                     (*' coeff (second %))))
       (into (sorted-map-by grevlex)))))

(defn mul
  "Polynomial multiplication.
  Variadic, nullary version returns transducer."
  ([]  ;; init
   (fn
     ([poly] poly)
     ([poly1 poly2] (mul poly1 poly2))
     ([poly1 poly2 & more] (reduce mul (mul poly1 poly2) more))))
  ([poly] poly)  ;; completion
  ([poly1 poly2]  ;; step
   (let [product (atom (transient (sorted-map-by grevlex)))]
     (doall  ;; `for` is lazy so must to be forced for side-effects 
      (for [term1 poly1
            term2 poly2
            :let [vars (mapv +' (key term1) (key term2))
                  coeff (* (long (val term1)) (long (val term2)))]]
        (if (contains? @product vars)
          (swap! product assoc! vars (+ (long (get @product vars)) coeff))
          (swap! product assoc! vars coeff))))
     (->> product
          (deref)
          (persistent!)
          (denull))))
  ([poly1 poly2 & more]
   (reduce mul (mul poly1 poly2) more)))

(defn mul-linear
  "Polynomial multiplication.
  Variadic, nullary version returns transducer.
  Based on an algorithm by Henry Baker."
  ([]  ;; init
   (fn
     ([poly] poly)
     ([poly1 poly2] (mul-linear poly1 poly2))
     ([poly1 poly2 & more] (reduce mul-linear (mul-linear poly1 poly2) more))))
  ([poly] poly)  ;; completion 
  ([poly1 poly2]  ;; step
   (->> poly1
        (map #(monomul poly2 %))
        (reduce add)))
  ([poly1 poly2 & more]
   (reduce mul-linear (mul-linear poly1 poly2) more)))

(defn pmul
  "Experimental - parallel version of `mul` using agents."
  ([]  ;; init
   (fn
     ([poly] poly)
     ([poly1 poly2] (pmul poly1 poly2))
     ([poly1 poly2 & more] (reduce (mul poly1 poly2) more))))
  ([poly] poly)  ;; completion
  ([poly1 poly2]  ;; step
   (let [*product* (agent (transient (sorted-map-by grevlex)))]
     (do  ;; `for` is lazy so must to be forced for side-effects 
      (for [term1 poly1
            term2 poly2
            :let [vars (mapv +' (key term1) (key term2))
                  coeff (* (long (val term1)) (long (val term2)))]]
        (if (contains? @*product* vars) 
          (send *product* assoc! vars (+ (long (get @*product* vars)) coeff))
          (send *product* assoc! vars coeff))))
     (await *product*)
     (->> *product*
          (deref)
          (persistent!)
          (denull))))
  ([poly1 poly2 & more]
   (reduce pmul (pmul poly1 poly2) more)))

(defn pow
  "Raises polynomial to exponent."
  ([poly ^long exp]
   {:pre [(>= exp 0)]}
   (cond
     (> exp 1) (->> poly
                    (repeat exp)
                    (apply mul))
     (= exp 1) poly
     (zero? exp) {(into (vector) (repeat (count (ffirst poly)) 0)) 1})))

(defn pow-linear
  "Raises polynomial to exponent.
  Based on an algorithm by Henry Baker."
  ([poly ^long exp]
   {:pre [(>= exp 0)]}
   (if (zero? exp)
     {(into (vector) (repeat (count (ffirst poly)) 0)) 1}
      (loop [exp exp
             result [poly]]
        (cond
          (= exp 1) (apply mul-linear result)
          (even? exp) (recur (quot exp 2)
                             (conj result (nth (iterate #(mul-linear poly %) poly)
                                               (dec (quot exp 2))))) 
          :else (recur (quot exp 2)
                       (conj result (nth (iterate #(mul-linear poly %) poly)
                                         (quot exp 2)))))))))

(defn sqrt
  "Polynomial square root.
  Returns exponents as rationals and coefficients as doubles."
  [poly]
  (->> poly
       (map (fn [v]
              {(mapv #(cc// % 2) (first v)) ;; polymorphic divide: coefficiants can be longs or doubles
               (Math/sqrt (second v))}))
       (into {})))

(defn compl
  "Computes the complement of two monomials.
  Similar to GCD over a Euclidean domain."
  [term1 term2]
  (map (fn [^long x ^long y]
         (cond
           (and (zero? x) (not= 0 y)) nil
           (< x y) nil
           (>= x y) (- x y)))
       term1
       term2))
     
(defn s-poly
  "Computes S-polynomial for finding Gröbner bases."
  [f g]
  (let [f-vars (first f)
        g-vars (first g)
        lcm (compl f-vars g-vars)]
    (if (not-any? nil? lcm)
      (vector (vec lcm)
              (cc// (second f) (second g))))))  ;; polymorphic divide: coefficiants can be longs or doubles

(defn divide
  "Polynomial long division using Buchberger's algorithm.
  Binary only, returns a tuple of quotient and remainder.
  Not guaranteed to be the inverse of multiplication."
  [f g]
  (loop [f f
         g g
         result (transient {})
         remainder {}]
    (if (empty? f)
      (list (persistent! result)
            (->> remainder
                 (filter #(not (nil? %)))
                 (into (sorted-map-by grevlex))))
      (let [term1 (first f)
            term2 (first g)
            s-term (s-poly term1 term2)]
        (if (nil? s-term)
          (recur (dissoc f (first term1))
                 (dissoc g (first term2))
                 result
                 (conj remainder term1))
          (recur (sub f (monomul g s-term))
                 g
                 (conj! result s-term)
                 remainder))))))
