(ns madhava.arithmetic
  (:require [madhava.util :refer :all]
            [clojure.data.int-map :as i]))

(defn add
  ;; transducer
  ([poly1]  ;; completion
   (fn
     ([] poly1)
     ([poly2] (add poly1 poly2))
     ([poly2 & more] (add poly1 poly2 more))))
  ([poly1 poly2]
   (denull
    (i/merge-with +' poly1 poly2)))
  ([poly1 poly2 & more]
   (reduce add (add poly1 poly2) more)))

(defn negate [poly]
  (reduce #(i/update %1 %2 -) poly (keys poly)))

(defn sub
  ;; consistent with scalar subtraction: arity 1 negates polynomial rather than transducer
  ([poly] (negate poly))
  ([poly1 poly2]
   (add poly1 (negate poly2)))
  ([poly1 poly2 & more]
   (reduce sub (add poly1 (negate poly2)) more)))

(defn scale [poly scalar]
  (denull
   (reduce #(i/update %1 %2 * scalar) poly (keys poly))))

(defn mul
  ;; transducer
  ([poly1]  ;; completion
   (fn
     ([] poly1)
     ([poly2] (mul poly1 poly2))
     ([poly2 & more] (mul poly1 poly2 more))))
  ([poly1 poly2]
   (let [product (atom (transient (i/int-map)))]
     (doall  ;; `for` is lazy so must to be forced for side-effects 
      (for [term1 poly1
            term2 poly2
            :let [vars (+ (key term1) (key term2))
                  coeff (* (val term1) (val term2))]]
        (if (contains? @product vars) 
          (swap! product i/update! vars + coeff)
          (swap! product assoc! vars coeff))))
     (denull (persistent! @product))))
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
   (let [*product* (agent (transient (i/int-map)))]
     (dosync
      (doall  ;; `for` is lazy so must to be forced for side-effects 
       (for [term1 poly1
             term2 poly2
             :let [vars (+ (key term1) (key term2))
                   coeff (* (val term1) (val term2))]]
         (if (contains? @*product* vars)
           (send *product* i/update! vars + coeff)
           (send *product* assoc! vars coeff)))))
     (await *product*)
     (denull (persistent! @*product*))))
  ([poly1 poly2 & more]
   (reduce pmul (pmul poly1 poly2) more)))

;; (defn sqrt [poly]
;;   (->> poly
;;        (map (fn [v]
;;               {(mapv #(/ % 2) (first v))
;;                (Math/sqrt (second v))}))
;;        (into (i/int-map))))

(defn compl [term1 term2]
  (let [dims (dims term1)]
    (map #(let [x (int-nth term1 % dims)
                y (int-nth term2 % dims)]
            (cond
              (and (zero? x) (not= 0 y)) nil
              (< x y) nil
              (>= x y) (* (- x y)
                          (int (Math/pow 10 (- dims %))))))
         (range dims))))
     
(defn s-poly [f g]
  (let [f-vars (first f)
        g-vars (first g)
        lcm (compl f-vars g-vars)]
    (if (not-any? nil? lcm)
      (i/int-map (reduce +' lcm)
                 (/ (second f) (second g))))))

(defn divide [f g]
  ;; *not* transducer! binary only
  ;; returns a tuple of quotient and remainder
  ;; not the inverse of multiplication (welcome to computer algebra)
  (loop [f f
         g g
         result (transient (i/int-map))
         remainder (i/int-map)]
    (if (empty? f)
      (list (persistent! result)
            (->> remainder
                 (filter #(some? %))
                 (into (i/int-map))))
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
