(ns madhava.arithmetic
  (:require [madhava.util :refer :all]
            [clojure.data.avl :refer [sorted-map-by]]))

(defn add
  ;; transducer
  ([poly1]  ;; completion
   (fn
     ([] poly1)
     ([poly2] (add poly1 poly2))
     ([poly2 & more] (add poly1 poly2 more))))
  ([poly1 poly2]
   (denull
    (into (sorted-map-by (comp - compare))
          (merge-with +' poly1 poly2))))
  ([poly1 poly2 & more]
   (reduce add (add poly1 poly2) more)))

(defn negate [poly]
  (->> poly
       (map #(update % 1 -))
       (into {})))

(defn sub
  ;; consistent with scalar subtraction: arity 1 negates polynomial rather than transducer
  ([poly] (negate poly))
  ([poly1 poly2]
   (add poly1 (negate poly2)))
  ([poly1 poly2 & more]
   (reduce sub (add poly1 (negate poly2)) more)))

(defn scale [poly scalar]
  (->> poly
       (map #(update % 1 * scalar))
       (into {})))

(defn mul
  ;; transducer
  ([poly1]  ;; completion
   (fn
     ([] poly1)
     ([poly2] (mul poly1 poly2))
     ([poly2 & more] (mul poly1 poly2 more))))
  ([poly1 poly2]
   (let [product (atom (transient (sorted-map-by (comp - compare))))]
     (doall  ;; `for` is lazy so must to be forced for side-effects 
      (for [term1 poly1
            term2 poly2
            :let [vars (mapv +' (key term1) (key term2))
                  coeff (* (val term1) (val term2))]]
        (if (contains? @product vars)
          (swap! product assoc! vars (+ (get @product vars) coeff))
          (swap! product assoc! vars coeff))))
     (persistent! @product)))
  ([poly1 poly2 & more]
   (reduce mul (mul poly1 poly2) more)))

(defn pmul
  ;; transducer
  ([poly1]  ;; completion
   (fn
     ([] poly1)
     ([poly2] (pmul poly1 poly2))
     ([poly2 & more] (pmul poly1 poly2 more))))
  ([poly1 poly2]
   (let [*product* (agent (transient (sorted-map-by (comp - compare))))]
     (dosync
      (doall;; `for` is lazy so must to be forced for side-effects 
       (for [term1 poly1
             term2 poly2
             :let [vars (mapv +' (key term1) (key term2))
                   coeff (* (val term1) (val term2))]]
         (if (contains? @*product* vars) 
           (send *product* assoc! vars (+ (get @*product* vars) coeff))
           (send *product* assoc! vars coeff)))))
     (await *product*)
     (persistent! @*product*)))
  ([poly1 poly2 & more]
   (reduce pmul (pmul poly1 poly2) more)))

(defn sqrt [poly]
  (->> poly
       (map (fn [v]
              {(mapv #(/ % 2) (first v))
               (Math/sqrt (second v))}))
       (into {})))

(defn compl [term1 term2] 
  (map (fn [x y]
         (cond
           (and (zero? x) (not= 0 y)) nil
           (< x y) nil
           (>= x y) (- x y)))
       term1
       term2))
     
(defn s-poly [f g]
  (let [f-vars (first f)
        g-vars (first g)
        lcm (compl f-vars g-vars)]
    (if (not-any? nil? lcm)
      {(vec lcm)
       (/ (second f) (second g))})))

(defn divide [f g]
  ;; *not* transducer! binary only
  ;; returns a tuple of quotient and remainder
  ;; not the inverse of multiplication (welcome to computer algebra)
  (loop [f f
         g g
         result (transient {})
         remainder {}]
    (if (empty? f)
      (list (persistent! result)
            (->> remainder
                 (filter #(not (nil? %)))
                 (into (sorted-map-by (comp - compare)))))
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
