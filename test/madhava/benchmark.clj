(ns madhava.benchmark
  (:require [clojure.test :refer :all]
            [criterium.core :refer [quick-bench]]
            [madhava.arithmetic :refer :all]
            [madhava.comp :refer :all]
            [madhava.diff :refer :all]
            [madhava.vectormath :refer :all]
            [clj-tuple :refer [vector]])
  (:refer-clojure :exclude [vector]))

(deftest ^:benchmark add-bench
  (println "\nAddition")
  (quick-bench
   (add {[4 3 3] 5, [2 1 2] 8, [0 4 0] 1, [0 0 3] 2, [1 0 0] 5}
        {[3 3 3] 5, [2 1 2] 8, [0 4 0] 1, [0 0 3] 2})))

(deftest ^:benchmark mul-bench
  (println "\nMultiplication")
  (quick-bench
   (mul {[4 3 3] 5, [2 1 2] 8, [0 4 0] 1, [0 0 3] 2, [1 0 0] 5}
        {[3 3 3] 5, [2 1 2] 8, [0 4 0] 1, [0 0 3] 2})))

(deftest ^:benchmark mul-bench2
  (println "\nMultiplication w/ Henry Baker algorithm")
  (quick-bench
   (mul-linear {[4 3 3] 5, [2 1 2] 8, [0 4 0] 1, [0 0 3] 2, [1 0 0] 5}
               {[3 3 3] 5, [2 1 2] 8, [0 4 0] 1, [0 0 3] 2})))

(deftest ^:benchmark pow-bench
  (println "\nExponentiation")
  (quick-bench
   (pow {[4 3 3] 5, [2 1 2] 8, [0 4 0] 1, [0 0 3] 2, [1 0 0] 5} 8)))

(deftest ^:benchmark pow-bench2
  (println "\nExponentiation w/ Henry Baker algorithm")
  (quick-bench
   (pow-linear {[4 3 3] 5, [2 1 2] 8, [0 4 0] 1, [0 0 3] 2, [1 0 0] 5} 8)))

(deftest ^:benchmark division-bench
  (println "\nDivision")
  (quick-bench
   (divide {[6 4 5] 40, [4 3 6] 10, [5 3 3] 25, [2 1 5] 16, [0 0 6] 4, [1 0 3] 10}
           {[2 1 2] 8, [0 0 3] 2, [1 0 0] 5})))

(deftest ^:benchmark comp-bench
  (println "\nComposition")
  (quick-bench
   (compose {[4 3 3] 5, [2 1 2] 8, [0 4 0] 1, [0 0 3] 2, [1 0 0] 5}
            {[3 3 3] 5, [2 1 2] 8, [0 4 0] 1, [0 0 3] 2} 2)))

(deftest ^:benchmark grad-bench
  (println "\nGradient")
  (quick-bench
   (grad {[4 3 3] 5, [2 1 2] 8, [0 4 0] 1, [0 0 3] 2, [1 0 0] 5})))

(deftest ^:benchmark laplacian-bench
  (println "\nLaplacian")
  (quick-bench
   (laplacian {[4 3 3] 5, [2 1 2] 8, [0 4 0] 1, [0 0 3] 2, [1 0 0] 5})))

(deftest ^:benchmark diff-bench
  (println "\n4th Order Diff Map")
  (quick-bench
   (diff {[4 3 3] 5, [2 1 2] 8, [0 4 0] 1, [0 0 3] 2, [1 0 0] 5} 4)))
