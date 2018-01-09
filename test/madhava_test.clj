(ns madhava-test
  (:require [clojure.test :refer :all]
            [madhava.arithmetic :refer :all]
            [madhava.vectormath :refer :all]
            [madhava.comp :refer :all]
            [madhava.taylorseries :refer :all]
            [clojure.data.int-map :as i]))

(deftest add-tests
  (is (= (add (i/int-map 0 7, 1 5, 10 3, 11 2)
              (i/int-map 1 1, 10 4, 21 1))
         {0 7, 1 6, 10 7, 11 2, 21 1}))
  (is (= (add (i/int-map 0 7, 1 5, 10 3, 11 2)
              (i/int-map 1 1, 10 4, 21 1) 
              (i/int-map 0 10))
         {0 17, 1 6, 10 7, 11 2, 21 1}))
  (is (= (add (i/int-map 0 7, 1 5, 10 3, 11 2)
              (i/int-map))
         {0 7, 1 5, 10 3, 11 2})))

(deftest sub-tests
  (is (= (sub (i/int-map 0 7, 1 5, 10 3, 11 2))
         {0 -7, 1 -5, 10 -3, 11 -2})) 
  (is (= (sub (i/int-map 0 7, 1 5, 10 3, 11 2)
              (i/int-map 0 7, 1 5, 10 3, 11 2)) 
         {}))
  (is (= (sub (i/int-map 0 7, 1 5, 10 3, 11 2)
              (i/int-map 0 7, 1 5, 10 3, 11 1))
         {11 1}))
  (is (= (sub (i/int-map 0 7, 1 5, 10 3, 11 2)
              (i/int-map 0 2, 1 5, 10 3, 11 1))
         {0 5, 11 1})))

(deftest scale-tests
  (is (= (scale (i/int-map 0 7, 1 5, 10 3, 11 2) 1)
         {0 7, 1 5, 10 3, 11 2}))
  (is (= (scale (i/int-map 0 7, 1 5, 10 3, 11 2) 2)
         {0 14, 1 10, 10 6, 11 4}))
  (is (= (scale (i/int-map 0 7, 1 5, 10 3, 11 2) 0)
         {})))

(deftest mul-tests
  (is (= (mul (i/int-map 0 7, 1 5, 10 3, 11 2)
              (i/int-map 1 1, 10 4, 21 1))
         {1 7, 2 5, 10 28, 11 23, 12 2, 20 12, 21 15, 22 5, 31 3, 32 2}))
  (is (= (mul (i/int-map 0 7, 1 5, 10 3, 11 2)
              (i/int-map 0 1))
         (i/int-map 0 7, 1 5, 10 3, 11 2)))
  (is (= (mul (i/int-map 0 7, 1 5, 10 3, 11 2)
              (i/int-map 0 0))
         {}))
  (is (= (mul (i/int-map 0 7, 1 5, 10 3, 11 2)
              (i/int-map 1 1, 10 4, 21 1)
              (i/int-map 0 2))
         {1 14, 2 10, 10 56, 11 46, 12 4, 20 24, 21 30, 22 10, 31 6, 32 4})))

;; (deftest pmul-tests
;;   (is (= (pmul (i/int-map 0 7, 1 5, 10 3, 11 2)
;;                (i/int-map 1 1, 10 4, 21 1))
;;          {1 7, 2 5, 10 28, 11 23, 12 2, 20 12, 21 15, 22 5, 31 3, 32 2}))
;;   (is (= (pmul (i/int-map 0 7, 1 5, 10 3, 11 2)
;;                (i/int-map 0 1))
;;          (i/int-map 0 7, 1 5, 10 3, 11 2)))
;;   (is (= (pmul (i/int-map 0 7, 1 5, 10 3, 11 2)
;;                (i/int-map 0 0))
;;          {}))
;;   (is (= (pmul (i/int-map 0 7, 1 5, 10 3, 11 2)
;;                (i/int-map 1 1, 10 4, 21 1)
;;                (i/int-map 0 2))
;;          {1 14, 2 10, 10 56, 11 46, 12 4, 20 24, 21 30, 22 10, 31 6, 32 4})))

(deftest divide-tests
  (is (= (divide (i/int-map 0 7, 1 5, 10 3, 11 2)
                 (i/int-map 0 7, 1 5, 10 3, 11 2))
         (list {0 1} {}))))
  ;; (is (= (divide (i/int-map 0 7, 1 5, 10 3, 11 2)
  ;;                (i/int-map 0 1))
  ;;        (list {0 7, 1 5, 10 3, 11 2} {})))
  ;; (is (= (divide (i/int-map 0 15, 1 3, 10 10, 11 2)
  ;;                (i/int-map 0 5, 1 1))
  ;;        (list {0 3, 10 2} {})))
  ;; (is (= (divide (i/int-map 0 15, 1 3, 10 10, 11 2)
  ;;                (i/int-map 0 3, 10 2))
  ;;        (list {0 5, 1 1} {}))))
         
;; (deftest compose-tests
;;   (is (= (compose (i/int-map 0 7, 1 5, 10 3, 11 2)
;;                   (i/int-map 0 7, 1 5, 10 3, 11 2) 1)
;;          {0 28, 1 34, 2 10, 10 9, 11 12, 12 4}))
;;   (is (= (compose (i/int-map 0 7, 1 5, 10 3, 11 2)
;;                   (i/int-map 0 7, 1 5, 10 3, 11 2) 2)
;;          {0 42, 1 25, 10 32, 11 20, 20 6, 21 4})))

;; (deftest chain-tests
;;   (is (= (chain (i/int-map 0 7, 1 5, 10 3, 11 2)
;;                 (i/int-map 0 7, 1 5, 10 3, 11 2))
;;          (list {0 168, 1 316, 2 196, 3 40, 10 54, 11 108, 12 72, 13 16}
;;            {0 420, 1 250, 10 488, 11 300, 20 188, 21 120, 30 24, 31 16})))
;;   (is (= (chain (i/int-map 1 5, 11 2)
;;                 (i/int-map 10 4, 21 1))
;;          (list {1 20, 2 10, 11 32, 12 26, 22 24, 23 4, 33 4}
;;            {10 100, 20 80, 21 25, 30 36, 31 20, 40 8, 41 9, 51 2})))
;;   (is (= (chain (i/int-map 10 4, 21 1)
;;                 (i/int-map 1 5, 11 2))
;;          (list {1 80, 2 40, 3 100, 4 50, 11 32, 12 56, 13 80, 14 90, 22 16, 23 16, 24 48, 34 8}
;;            {10 20, 20 8, 21 25, 30 4, 31 20, 41 9, 51 2}))))

;; (deftest rchain-tests
;;   (is (= (rchain (i/int-map 0 7, 1 5, 10 3, 11 2)
;;                  (i/int-map 0 7, 1 5, 10 3, 11 2))
;;          (list {0 168, 1 316, 2 196, 3 40, 10 54, 11 108, 12 72, 13 16}
;;            {0 420, 1 250, 10 488, 11 300, 20 188, 21 120, 30 24, 31 16})))
;;   (is (= (rchain (i/int-map 1 5, 11 2)
;;                  (i/int-map 10 4, 21 1))
;;          (list {1 80, 2 40, 3 100, 4 50, 11 32, 12 56, 13 80, 14 90, 22 16, 23 16, 24 48, 34 8}
;;            {10 20, 20 8, 21 25, 30 4, 31 20, 41 9, 51 2})))
;;   (is (= (rchain (i/int-map 10 4, 21 1)
;;                  (i/int-map 1 5, 11 2))
;;          (list {1 20, 2 10, 11 32, 12 26, 22 24, 23 4, 33 4}
;;            {10 100, 20 80, 21 25, 30 36, 31 20, 40 8, 41 9, 51 2}))))
  
(deftest grad-tests
  (is (= (grad (i/int-map 3 2, 40 1, 100 5, 212 8))
         (list {0 5, 112 16} {30 4, 202 8} {2 6, 211 16}))))

(deftest directional-diff-tests
  (is (= (directional-diff (i/int-map 0 7, 1 5, 10 3, 11 2) 1)
         {0 8, 1 2, 10 2}))
  (is (= (directional-diff (i/int-map 0 7, 1 5, 10 3, 11 2) 2)
         {0 16, 1 4, 10 4}))
  (is (= (directional-diff (i/int-map 3 2, 40 1, 212 8, 433 5) 3)
         {2 18, 30 12, 112 48, 202 24, 211 48, 333 60, 423 45, 432 45})))

(deftest laplacian-tests
  (is (= (laplacian (i/int-map 3 2, 40 1, 100 5, 212 8))
         (list {12 16} {20 12} {1 12, 210 16}))))

(deftest div-tests
  (is (= (div (list (i/int-map 433 5) (i/int-map 212 8) (i/int-map 40 1)))
         {202 8, 333 20})))

(deftest curl-tests
  (is (= (curl (list (i/int-map 433 5) (i/int-map 212 8) (i/int-map 40 1)))
         (list {30 4, 211 -16} {432 15} {112 16, 423 -15}))))

(deftest series-tests
  (is (= (dense-to-sparse (take 10 (exp-series)))
         {0 1, 1 1, 2 1/2, 3 1/6, 4 1/24, 5 1/120, 6 1/720, 7 1/5040, 8 1/40320, 9 1/362880}))
  (is (= (dense-to-sparse (take 10 (sin-series)))
         {1 1, 3 -1/6, 5 1/120, 7 -1/5040, 9 1/362880}))
  (is (= (dense-to-sparse (take 10 (cos-series)))
         {0 1, 2 -1/2, 4 1/24, 6 -1/720, 8 1/40320}))
  (is (= (dense-to-sparse (take 10 (atan-series)))
         {0 1, 2 -1/3, 4 1/5, 6 -1/7, 8 1/9}))
  (is (= (dense-to-sparse (take 10 (sinh-series)))
         {1 1, 3 1/6, 5 1/120, 7 1/5040, 9 1/362880}))
  (is (= (dense-to-sparse (take 10 (cosh-series)))
         {0 1, 2 1/2, 4 1/24, 6 1/720, 8 1/40320})))
