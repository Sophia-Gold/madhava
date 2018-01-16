(ns madhava.benchmark
  (:require [clojure.test :refer :all]
            [criterium.core :refer [quick-bench]]
            [clojure.data.int-map :as i]
            [madhava.arithmetic :refer :all]
            [madhava.comp :refer :all]
            [madhava.diff :refer :all]
            [madhava.vectormath :refer :all]))

(deftest ^:benchmark add-bench
  (println "\nAddition")
  (quick-bench
   (add (i/int-map 3 2, 40 1, 100 5, 212 8, 433 5)
        (i/int-map 3 2, 40 1, 212 8, 233 5))))

(deftest ^:benchmark mul-bench
  (println "\nMultiplication")
  (quick-bench
   (mul (i/int-map 3 2, 40 1, 100 5, 212 8, 433 5)
        (i/int-map 3 2, 40 1, 212 8, 233 5))))

(deftest ^:benchmark division-bench
  (println "\nDivision")
  (quick-bench
   (divide (i/int-map 3 2, 40 1, 100 5, 212 8, 433 5)
           (i/int-map 3 2, 40 1, 100 5, 212 8, 433 5))))

(deftest ^:benchmark comp-bench
  (println "\nComposition")
  (quick-bench
   (compose (i/int-map 3 2, 40 1, 100 5, 212 8, 433 5)
            (i/int-map 3 2, 40 1, 212 8, 233 5) 2)))

(deftest ^:benchmark grad-bench
  (println "\nGradient")
  (quick-bench
   (grad (i/int-map 3 2, 40 1, 100 5, 212 8, 433 5))))

(deftest ^:benchmark laplacian-bench
  (println "\nLaplacian")
  (quick-bench
   (laplacian (i/int-map 3 2, 40 1, 100 5, 212 8, 433 5))))

(deftest ^:benchmark diff-bench
  (println "\n4th Order Diff Map")
  (quick-bench
   (diff (i/int-map 3 2, 40 1, 100 5, 212 8, 433 5) 4)))
