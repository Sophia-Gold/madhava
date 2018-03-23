(ns madhava.comp
  (:require [madhava.arithmetic :refer :all]
            [madhava.diff :refer :all]
            [madhava.util :refer :all]
            [madhava.vectormath :refer :all]
            [clojure.core :as cc]
            [clojure.data.avl :refer [sorted-map-by]]
            [clojure.math.combinatorics :as combo]
            [primitive-math]
            [clj-tuple :refer [vector]])
  (:refer-clojure :exclude [vector sorted-map-by]))

(primitive-math/use-primitive-operators)

(defn compose
  "Functional composition. 
  Third argument is index of variable, starting from 1."
  [f g ^long idx]
  (let [idx (dec idx)]  ;; x == 1st var, 0th element in tuple 
    (loop [f f
           result {}]
      (let [term (first f)
            vars (first term)
            coeff (second term)
            v (nth vars idx)]
        (cond
          (nil? term) (into (sorted-map-by grevlex) result)
          (zero? (long v)) (recur (dissoc f vars) (add {vars coeff} result))
          :else (recur (dissoc f vars) (add (apply mul
                                                   {(assoc vars idx 0) coeff}
                                                   (repeat v g))  ;; raise g to exponent
                                            result)))))))

(defn compose2
  "Functional composition.
  Third argument is index of variable, starting from 1."
  [f g ^long idx]
  (let [idx (dec idx)]  ;; x == 1st var, 0th element in tuple
    (->> f
         (map (fn [term]
                (let [vars (first term)
                      coeff (second term)
                      v (nth vars idx)]
                  (if (zero? v)
                    {vars coeff}
                    (-> g
                        (pow v)
                        (monomul (vector (assoc vars idx 0) coeff)))))))
         (apply add)
         (into (sorted-map-by grevlex)))))

(defn multi-compose
  "Functional composition.
  Substitutes `g` for all variables in `f`.
  Variadic, unary version returns transducer."
  ([]  ;; init
   (fn
     ([f] f)
     ([f g] (multi-compose f g))
     ([f g & more] (reduce multi-compose (multi-compose f g) more))))
  ([f] f)  ;; completion
  ([f g]  ;; step
   (let [const (into (vector) (repeat (count (ffirst f)) 0))] ;; dims
     (->> f
          (#(dissoc % const)) ;; remove constant term from `f` (idempotent if not present)
          (map (fn [term]
                 (->> term
                      first
                      (map #(if (zero? %)
                              {}
                              (pow g %)))
                      (apply mul)
                      (#(scale % (second term))))))
          (cons {const (get f const)})  ;; add constant term from `f` (idempotent if not present)
          (apply add)
          (into (sorted-map-by grevlex)))))
  ([f g & more]
   (reduce multi-compose (multi-compose f g) more)))

(defn revert1
  "Computes compositional inverse, aka Horner's rule.
  Only for univariate polynomials.
  See Knuth TAOCP vol 2 pp. 486-488."
  [f] 
  (loop [f f
         result (transient (vector))] 
    (if (empty? (next f))
      (-> result
          (conj! (second (first f)))
          (persistent!))
      (let [term (first f)
            var (ffirst term)
            coeff (second term)]
        (if (zero? (- (long var) (long (ffirst (second f)))))
          (recur (next f) result) 
          (recur (next f) (conj! result coeff)))))))

(defn eval-horner
  "Quickly evaluates a univariate polynomial in Horner's form."
  [f x]
  (->> f
       (revert1)
       (reduce #(cc/+' (cc/*' %1 x) %2))))

(defn eval-poly
  "Evaluates a polynomial function at a vector of points."
  [f points]
  (let [vars (keys f)
        coeffs (vals f)]
    (->> vars
         (map (fn [v] (map #(Math/pow %1 %2) points v)))  ;; substitute points for variables in f 
         (map #(reduce *' %))  ;; multiply vars in monomial
         (map *' coeffs)  ;; multiply by coefficients   
         (reduce +'))))  ;; sum monomials

(defn chain
  "Chain rule. 
  Variadic, unary version returns transducer."
  ([]  ;; init
   (fn
     ([f] f)
     ([f g] (chain f g))
     ([f g & more] (reduce chain (chain f g) more))))
  ([f] f)  ;; completion
  ([f g]  ;; step
   (->> f
        (grad)
        (map-indexed #(compose %2 g (inc (long %1))))
        (map mul (grad g))
        (reduce add)))
  ([f g & more]
   (reduce chain (chain f g) more)))

(defn chain2
  "Faster implementation of chain rule for univariate functions.
  Variadic, unary version returns transducer."
  ([]  ;; init
   (fn
     ([f] f)
     ([f g] (chain2 f g))
     ([f g & more] (reduce chain2 (chain2 f g) more))))
  ([f] f)  ;; completion
  ([f g]  ;; step
   (->> f
        ffirst
        count
        range 
        (map #(-> f
                  (grad)
                  (nth %)
                  (compose g (inc %))
                  (mul (nth (grad g) %))))
        (reduce add)))
  ([f g & more]
   (reduce chain2 (chain2 f g) more)))

;; (defn partition-set' [n]
;;   (->> (repeat n 1)
;;        (combo/partitions)
;;        (mapv (fn [x] (mapv #(reduce +' %) x)))))

;; (defn chain-higher1'
;;   "Faster implementation of higher-order chain rule for univariate functions."
;;   [f g ^long order]
;;   (let [f' (diff f order)
;;         g' (diff g order)
;;         partitions (reverse (partition-set' order)) 
;;         coeff (map (fn [p] (->> order     ;; number of partitions of set with elements
;;                                (inc)     ;; equal to order into sets of x elements
;;                                (range 1)
;;                                (combo/partitions)
;;                                (map sort)  ;; TO DO: don't sort
;;                                (filter #(= (map count %) p))
;;                                (count))) 
;;                    (map sort partitions))] ;; TO DO: don't sort
;;     (reduce add
;;             (map #(mul {[0] %1}
;;                        (compose (get f' %2) g 1)
;;                        (reduce mul
;;                                (map (fn [x] (pow (get g' (first x))
;;                                                 (second x)))
;;                                     %3)))
;;                  coeff
;;                  (reverse (range 1 (inc order))) 
;;                  (map frequencies partitions)))))

;; (defn chain-higher'
;;   "Higher-order chain rule using Faà di Bruno's formula."
;;   [f g ^long order ^long idx]
;;   (let [f' (diff f order)
;;         g' (diff g order)
;;         dims (count (ffirst f))]
;;     (reduce add
;;             (map (fn [degree partitions]
;;                    (let [f-partials (compose (get f' (->> degree 
;;                                                           (range)
;;                                                           (map #(* (long (Math/pow 10 %)) idx))
;;                                                           (reduce +')))
;;                                              g idx)
;;                          g-idxs (map (fn [x] (->> x
;;                                                  (range)
;;                                                  (map (fn [k] (map #(* (long (Math/pow 10 k)) (long %)) 
;;                                                                   (range 1 (inc dims)))))
;;                                                  (apply map +')))
;;                                      partitions)
;;                          g-partials (if (< 1 (count g-idxs))
;;                                       (apply (fn [x] (map-indexed #(sort
;;                                                                    (assoc (into (vector) (first g-idxs)) %1 %2))
;;                                                                  x))
;;                                              (next g-idxs))
;;                                       g-idxs)] 
;;                      (->> g-partials
;;                           (mapcat (fn [k] (map #(get g' %) k)))
;;                           (map #(mul f-partials %))
;;                           (reduce add))))
;;                  (range 1 (inc order))
;;                  (map distinct (partition-set' order))))))

;; (defn chain-higher
;;   "Multivariate higher-order chain rule."
;;   [f g ^long order]
;;   (->> f
;;        ffirst
;;        count
;;        range 
;;        (map #(chain-higher' f g order %))
;;        (reduce add)))

(defn partition-set [n]
  (->> (range 1 (inc n))
       (combo/partitions)))

;; 2nd order: (f''(g) * g' * g'') + (f'(g) * g') + (f'(g) * g'')
(defn chain-higher1
  "Higher-order chain rule using Faà di Bruno's formula."
  [f g ^long order]
  (let [f' (vals (diff f order))  ;; strip keys since all derivatives are total and indexed by order 
        g' (vals (diff g order))]
    (->> order
         partition-set
         (map (fn [p] (mul (multi-compose (nth f' (dec (count p))) g)
                          (apply mul                            
                                 (mapcat (fn [b] (map #(nth g' (dec %)) b)) p)))))
         (apply add))))

(defn chain-higher
  "Higher-order chain rule using Faà di Bruno's formula.
  Works over multiple variables using the \"collapsing partitions\" technique
  developed by Michael Hardy in \"Combinatorics of Partial Derivatives.\""
  [f g ^long order]
  (let [f' (diff f order)
        g' (diff g order)]
    ))
