(ns madhava.diff
  (:require [madhava.arithmetic :refer :all]
            [madhava.util :refer :all]
            [clojure.core :as cc]
            [clojure.core.reducers :as r] 
            [clojure.data.int-map :as i]
            [clojure.pprint :refer [pprint]]
            [com.rpl.specter :refer :all]
            [primitive-math]
            [clj-tuple :refer [vector]])
  (:refer-clojure :exclude [vector]))

(primitive-math/use-primitive-operators)

(defn diff
  "Computes all partials derivates of a function up to a given order.
  Functions are represented as sorted-maps of monomials in graded 
  reverse lexicographic order with tuples of exponents as keys and 
  corresponding coefficients as values. Returns a map ('tape') with
  integer keys where number of digits represents order and least
  significant digits represent differentiated variables. Input is
  limited to functions of at most 9 variables."
  [poly ^long order]
  (let [tape (atom (transient (i/int-map)))
        dims (count (ffirst poly))]
    (letfn [(partial-diff [poly ^long key ^long idx]
              (let [partial (transform [ALL] (fn [[k v]]
                                               (let [var (long (nth k idx))]
                                                 (when (not (zero? var))
                                                   [(update k idx cc/dec)
                                                    (cc/* v var)])))
                                       poly)]
                (swap! tape assoc! key partial)
                (list partial key)))
            (diff-loop [poly ^long n]
              (when (< n order)
                (run! #(diff-loop (partial-diff (first poly)
                                                (+ (* 10 (long (second poly)))
                                                   (inc (long %)))
                                                %)
                                  (inc n))
                      (range dims))))]
      (diff-loop (list poly 0) 0)
      (persistent! @tape))))

(defn anti-diff
  "Computes all indefinite integrals of a function up to a given order.
  Functions are represented as sorted-maps of monomials in graded 
  reverse lexicographic order with tuples of exponents as keys and 
  corresponding coefficients as values. Returns a map ('tape') with
  integer keys where number of digits represents order and least
  significant digits represent differentiated variables. Input is
  limited to functions of at most 9 variables."
  [poly ^long order]
  (let [tape (atom (transient (i/int-map)))
        dims (count (ffirst poly))]
    (letfn [(partial-diff [poly ^long key ^long idx]
              (let [partial (transform [ALL] (fn [[k v]]
                                               (let [var (long (nth k idx))]
                                                 (when (not (zero? var))
                                                   [(update k idx cc/inc)
                                                    (cc// v (inc var))])))
                                       poly)]
                (swap! tape assoc! key partial)
                (list partial key)))
            (diff-loop [poly ^long n]
              (when (< n order)
                (run! #(diff-loop (partial-diff (first poly)
                                                (+ (* 10 (long (second poly)))
                                                   (inc (long %)))
                                                %)
                                  (inc n))
                      (range dims))))]
      (diff-loop (list poly 0) 0)
      (persistent! @tape))))

(defn pdiff
  "Experimental - parallel version of `diff` using agents."
  [poly ^long order]
  (let [*tape* (agent (transient (i/int-map)))
        dims (count (ffirst poly))]
    (letfn [(partial-diff [poly ^long key ^long idx]
              (let [partial (transform [ALL] (fn [[k v]]
                                               (let [var (long (nth k idx))]
                                                 (when (not (zero? var))
                                                   [(update k idx cc/dec)
                                                    (cc/* v var)])))
                                       poly)]
                (send *tape* assoc! key partial)
                (list partial key)))
            (diff-loop [poly ^long n]
              (when (< n order)
                (doall
                 (pmap #(diff-loop (partial-diff (first poly)
                                                 (+ (* 10 (long (second poly)))
                                                    (inc (long %)))
                                                 %)
                                   (inc n))
                       (range dims)))))]
      (diff-loop (list poly 0) 0)
      (await *tape*)
      (persistent! @*tape*))))

(defn anti-pdiff
  "Experimental - parallel version of `anti-diff` using agents."
  [poly ^long order]
  (let [*tape* (agent (transient (i/int-map)))
        dims (count (ffirst poly))]
    (letfn [(partial-diff [poly ^long key ^long idx]
              (let [partial (transform [ALL] (fn [[k v]]
                                               (let [var (long (nth k idx))]
                                                 (when (not (zero? var))
                                                   [(update k idx cc/inc)
                                                    (cc// v (inc var))])))
                                       poly)]
                (send *tape* assoc! key partial)
                (list partial key)))
            (diff-loop [poly ^long n]
              (when (< n order)
                (doall
                 (pmap #(diff-loop (partial-diff (first poly)
                                                 (+ (* 10 (long (second poly)))
                                                    (inc (long %)))
                                                 %)
                                   (inc n))
                       (range dims)))))]
      (diff-loop (list poly 0) 0)
      (persistent! @*tape*))))

(defn vector-diff
  "Computes the partial derivatives of a vector in Cartesian form.
  Coordinates are processed in parallel.
  Returns a list of int-maps."
  [vf order]
  (pmap #(diff % order) vf))

(defn vector-antidiff
   "Computes the indefinite integrals of a vector in Cartesian form.
  Coordinates are processed in parallel.
  Returns a list of int-maps."
  [vf order]
  (pmap #(anti-diff % order) vf))

(defmacro print-tape
  "Prints tape to text-file."
  [filename tape]
  `(pprint ~tape
           (clojure.java.io/writer
            (str ~filename ".txt"))))

(defn search-tape
  "Searches tape for a particular derivative.
  Will return multiple results if present."
  [tape val]
  (->> tape
       (select [ALL (fn [[k v]] (= v val))])
       (r/fold i/merge conj)))

(defn denull-tape
  "Removes empty partials from tape."
  [tape]
  (setval [MAP-VALS #(= {} %)] NONE tape))

(defn transform-tape
  "Applies a function to all derivatives on tape."
  [tape f]
  (transform MAP-VALS f tape))

(defn add-tapes
  "Takes multiple tapes and returns one with sums of corresponding derivatives."
  [& tapes]
  (apply merge-with add tapes))

(defn mul-tapes
  "Takes multiple tapes and returns one with products of corresponding derivatives."
  [& tapes]
  (apply merge-with mul tapes))
