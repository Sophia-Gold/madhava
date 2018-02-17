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
  Variadic, unary version returns transducer."
  ([poly1]  ;; completion
   (fn
     ([] poly1)
     ([poly2] (add poly1 poly2))
     ([poly2 & more] (add poly1 poly2 more))))
  ([poly1 poly2]
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

(defn mul
  "Polynomial multiplication.
  Variadic, unary version returns transducer."
  ([poly1]  ;; completion
   (fn
     ([] poly1)
     ([poly2] (mul poly1 poly2))
     ([poly2 & more] (mul poly1 poly2 more))))
  ([poly1 poly2]
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

(defn pmul
  "Experimental - parallel version of `mul` using agents."
  ([poly1]  ;; completion
   (fn
     ([] poly1)
     ([poly2] (pmul poly1 poly2))
     ([poly2 & more] (pmul poly1 poly2 more))))
  ([poly1 poly2]
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
      {(vec lcm)
       (cc// (second f) (second g))}))) ;; polymorphic divide: coefficiants can be longs or doubles

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
          (recur (sub f (mul g s-term))
                 g
                 (conj! result s-term)
                 remainder))))))
