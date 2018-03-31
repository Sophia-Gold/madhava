(ns madhava.test
  (:require [clojure.test :refer :all]
            [madhava.arithmetic :refer :all]
            [madhava.comp :refer :all] 
            [madhava.taylorseries :refer :all]
            [madhava.util :refer [add-dim]]
            [madhava.vectormath :refer :all]
            [clj-tuple :refer [vector]])
  (:refer-clojure :exclude [vector]))

(deftest add-tests
  (is (= (add {[1 1] 2, [1 0] 3, [0 1] 5, [0 0] 7}
              {[2 1] 1, [1 0] 4, [0 1] 1})
         {[2 1] 1, [1 1] 2, [1 0] 7, [0 1] 6, [0 0] 7}))
  (is (= (add {[1 1] 2, [1 0] 3, [0 1] 5, [0 0] 7}
              {[2 1] 1, [1 0] 4, [0 1] 1}
              {[0 0] 10})
         {[2 1] 1, [1 1] 2, [1 0] 7, [0 1] 6, [0 0] 17}))
  (is (= (add {[1 1] 2, [1 0] 3, [0 1] 5, [0 0] 7}
              {})
         {[1 1] 2, [1 0] 3, [0 1] 5, [0 0] 7})))

(deftest sub-tests
  (is (= (sub {[1 1] 2, [1 0] 3, [0 1] 5, [0 0] 7})
         {[1 1] -2, [1 0] -3, [0 1] -5, [0 0] -7}))
  (is (= (sub {[1 1] 2, [1 0] 3, [0 1] 5, [0 0] 7}
              {[1 1] 2, [1 0] 3, [0 1] 5, [0 0] 7})
         {}))
  (is (= (sub {[1 1] 2, [1 0] 3, [0 1] 5, [0 0] 7}
              {[1 1] 1, [1 0] 3, [0 1] 5, [0 0] 7})
         {[1 1] 1}))
  (is (= (sub {[1 1] 2, [1 0] 3, [0 1] 5, [0 0] 7}
              {[1 1] 1, [1 0] 3, [0 1] 5, [0 0] 2})
         {[1 1] 1, [0 0] 5})))

(deftest scale-tests
  (is (= (scale {[1 1] 2, [1 0] 3, [0 1] 5, [0 0] 7} 2)
         {[1 1] 4, [1 0] 6, [0 1] 10, [0 0] 14}))
  (is (= (scale {[1 1] 2, [1 0] 3, [0 1] 5, [0 0] 7} 1)
         {[1 1] 2, [1 0] 3, [0 1] 5, [0 0] 7}))
  (is (= (scale {[1 1] 2, [1 0] 3, [0 1] 5, [0 0] 7} 0)
         {})))

(deftest mul-tests
  (is (= (mul {[1 1] 2, [1 0] 3, [0 1] 5, [0 0] 7}
              {[2 1] 1, [1 0] 4, [0 1] 1})
         {[3 2] 2, [3 1] 3, [2 2] 5, [2 1] 15, [1 2] 2, [2 0] 12, [1 1] 23, [0 2] 5, [1 0] 28, [0 1] 7}))
  (is (= (mul {[1 1] 2, [1 0] 3, [0 1] 5, [0 0] 7}
              {[0 0] 1})
         {[1 1] 2, [1 0] 3, [0 1] 5, [0 0] 7}))
  (is (= (mul {[1 1] 2, [1 0] 3, [0 1] 5, [0 0] 7}
              {[0 0] 0})
         {}))
  (is (= (mul {[1 1] 2, [1 0] 3, [0 1] 5, [0 0] 7}
              {[2 1] 1, [1 0] 4, [0 1] 1}
              {[0 0] 2})
         {[3 2] 4, [3 1] 6, [2 2] 10, [2 1] 30, [1 2] 4, [2 0] 24, [1 1] 46, [0 2] 10, [1 0] 56, [0 1] 14})))

;; (deftest pmul-tests
;;   (is (= (pmul {[1 1] 2, [1 0] 3, [0 1] 5, [0 0] 7}
;;                {[2 1] 1, [1 0] 4, [0 1] 1})
;;          {[3 2] 2, [3 1] 3, [2 2] 5, [2 1] 15, [1 2] 2, [2 0] 12, [1 1] 23, [0 2] 5, [1 0] 28, [0 1] 7}))
;;   (is (= (pmul {[1 1] 2, [1 0] 3, [0 1] 5, [0 0] 7}
;;                {[0 0] 1})
;;          {[1 1] 2, [1 0] 3, [0 1] 5, [0 0] 7}))
;;   (is (= (pmul {[1 1] 2, [1 0] 3, [0 1] 5, [0 0] 7}
;;                {[0 0] 0})
;;          {}))
;;   (is (= (pmul {[1 1] 2, [1 0] 3, [0 1] 5, [0 0] 7}
;;                {[2 1] 1, [1 0] 4, [0 1] 1}
;;                {[0 0] 2})
;;          {[3 2] 4, [3 1] 6, [2 2] 10, [2 1] 30, [1 2] 4, [2 0] 24, [1 1] 46, [0 2] 10, [1 0] 56, [0 1] 14})))

(deftest pow-tests
  (is (= (pow {[1 1] 2, [1 0] 3, [0 1] 5, [0 0] 7} 0)
         {[0 0] 1}))
  (is (= (pow {[1 1] 2, [1 0] 3, [0 1] 5, [0 0] 7} 1)
         {[1 1] 2, [1 0] 3, [0 1] 5, [0 0] 7})) 
  (is (= (pow {[1 1] 2, [1 0] 3, [0 1] 5, [0 0] 7} 2)
         (mul {[1 1] 2, [1 0] 3, [0 1] 5, [0 0] 7}
              {[1 1] 2, [1 0] 3, [0 1] 5, [0 0] 7})))
  (is (= (pow {[1 1] 2, [1 0] 3, [0 1] 5, [0 0] 7} 3)
         (mul {[1 1] 2, [1 0] 3, [0 1] 5, [0 0] 7}
              {[1 1] 2, [1 0] 3, [0 1] 5, [0 0] 7}
              {[1 1] 2, [1 0] 3, [0 1] 5, [0 0] 7})))
  (is (= (pow {[1 1] 2, [1 0] 3, [0 1] 5, [0 0] 7} 8)
         (nth (iterate #(mul {[1 1] 2, [1 0] 3, [0 1] 5, [0 0] 7} %)
                       {[1 1] 2, [1 0] 3, [0 1] 5, [0 0] 7})
              7))))

(deftest sqrt-tests
  (is (= (sqrt {[1 1] 2, [1 0] 3, [0 1] 5})
         {[1/2 1/2] 1.4142135623730951, [1/2 0] 1.7320508075688772, [0 1/2] 2.23606797749979}))
  (is (= (sqrt {[4 4] 4, [2 0] 9, [0 2] 25})
         {[2 2] 2.0, [1 0] 3.0, [0 1] 5.0})))

(deftest divide-tests
  (is (= (divide {[1 1] 2, [1 0] 3, [0 1] 5, [0 0] 7}
                 {[1 1] 2, [1 0] 3, [0 1] 5, [0 0] 7})
         '({[0 0] 1} {})))
  (is (= (divide {[1 1] 2, [1 0] 3, [0 1] 5, [0 0] 7}
                 {[0 0] 1})
         '({[1 1] 2, [1 0] 3, [0 1] 5, [0 0] 7} {})))
  (is (= (divide {[1 1] 2, [1 0] 10, [0 1] 3, [0 0] 15}
                 {[0 1] 1, [0 0] 5})
         '({[1 0] 2, [0 0] 3} {})))
  (is (= (divide {[1 1] 2, [1 0] 10, [0 1] 3, [0 0] 15}
                 {[1 0] 2, [0 0] 3})
         '({[0 1] 1, [0 0] 5} {}))))

(deftest compose-tests
  (is (= (compose {[1 1] 2, [1 0] 3, [0 1] 5, [0 0] 7}
                  {[1 1] 2, [1 0] 3, [0 1] 5, [0 0] 7} 1)
         {[1 2] 4, [1 1] 12, [0 2] 10, [1 0] 9, [0 1] 34, [0 0] 28}))
  (is (= (compose {[1 1] 2, [1 0] 3, [0 1] 5, [0 0] 7}
                  {[1 1] 2, [1 0] 3, [0 1] 5, [0 0] 7} 2)
         {[2 1] 4, [2 0] 6, [1 1] 20, [1 0] 32, [0 1] 25, [0 0] 42})))

(deftest multi-compose-tests
  (is (= (compose {[2] 2, [1] 3, [0], 7}
                  {[2] 2, [1] 3, [0], 7}
                  1)
         (multi-compose {[2] 2, [1] 3, [0], 7}
                        {[2] 2, [1] 3, [0], 7}))))

(deftest chain-tests
  (is (= (chain {[1 1] 2, [1 0] 3, [0 1] 5, [0 0] 7}
                {[1 1] 2, [1 0] 3, [0 1] 5, [0 0] 7})
         {[2 0] 4, [0 2] 4, [1 0] 20, [0 1] 12, [0 0] 34})) 
  (is (= (chain {[1 1] 2, [0 1] 5}
                {[2 1] 1, [1 0] 4}) 
         {[3 0] 2, [1 2] 4, [2 0] 5, [0 1] 8})) 
  (is (= (chain {[2 1] 1, [1 0] 4}
                {[1 1] 2, [0 1] 5})
         {[1 3] 8, [3 0] 2, [0 3] 20, [2 0] 5, [0 1] 8})))

(deftest chain-higher1-tests
  (is (= (first (grad (multi-compose {[2] 1, [1] 5, [0] 7}
                                     {[2] 1, [1] 5, [0] 7}))) 
         (chain-higher1 {[2] 1, [1] 5, [0] 7}
                        {[2] 1, [1] 5, [0] 7}
                        1))))

;; (deftest chain-higher-tests
;;   (is (= (laplacian (multi-compose (add-dim {[2] 1, [1] 5, [0] 7})
;;                                    {[2 1] 1, [1 0] 4}))
;;          (chain-higher' {[2] 1, [1] 5, [0] 7}
;;                         {[2 1] 1, [1 0] 4} 
;;                         2))))

(deftest grad-tests
  (is (= (grad {[2 1 2] 8, [0 4 0] 1, [0 0 3] 2, [1 0 0] 5})
         [{[1 1 2] 16, [0 0 0] 5}
          {[2 0 2] 8, [0 3 0] 4}
          {[2 1 1] 16, [0 0 2] 6}])))

(deftest directional-diff-tests
  (is (= (directional-diff {[1 1] 2, [1 0] 3, [0 1] 5, [0 0] 7} 1)
         {[1 0] 2, [0 1] 2, [0 0] 8}))
  (is (= (directional-diff {[1 1] 2, [1 0] 3, [0 1] 5, [0 0] 7} 2)
         {[1 0] 4, [0 1] 4, [0 0] 16}))
  (is (= (directional-diff {[4 3 3] 5, [2 1 2] 8, [0 4 0] 1, [0 0 3] 2} 3)
         {[4 3 2] 45, [4 2 3] 45, [3 3 3] 60, [2 1 1] 48, [2 0 2] 24, [1 1 2] 48, [0 3 0] 12, [0 0 2] 18})))

(deftest magnitude-tests
  (is (= (magnitude '({[4 3 3] 5}
                      {[2 1 2] 8}
                      {[0 4 0] 1}))
         {[4 3 3] 5.0, [2 1 2] 8.0, [0 4 0] 1.0})))

(deftest laplacian-tests
  (is (= (laplacian {[2 1 2] 8, [0 4 0] 1, [0 0 3] 2, [1 0 0] 5})
         {[2 1 0] 16, [0 1 2] 16, [0 2 0] 12, [0 0 1] 12}))
  (is (= (laplacian {[2 1 2] 8, [0 4 0] 1, [0 0 3] 2, [1 0 0] 5})
         (div (grad {[2 1 2] 8, [0 4 0] 1, [0 0 3] 2, [1 0 0] 5})))))

(deftest div-tests
  (is (= (div [{[4 3 3] 5}
               {[2 1 2] 8}
               {[0 4 0] 1}])
         {[3 3 3] 20, [2 0 2] 8}))
  (is (= (div (curl [{[4 3 3] 5}
                     {[2 1 2] 8}
                     {[0 4 0] 1}]))
         {})))

(deftest curl-tests
  (is (= (curl [{[4 3 3] 5}
                {[2 1 2] 8}
                {[0 4 0] 1}])
         [{[2 1 1] -16, [0 3 0] 4}
          {[4 3 2] 15}
          {[4 2 3] -15, [1 1 2] 16}]))
  (is (= (curl (grad {[2 1 2] 8, [0 4 0] 1, [0 0 3] 2, [1 0 0] 5}))
         [{} {} {}])))

(deftest series-tests
  (is (= (dense-to-sparse (take 10 (exp-series)))
         {[9] 1/362880, [8] 1/40320, [7] 1/5040, [6] 1/720, [5] 1/120, [4] 1/24, [3] 1/6, [2] 1/2, [1] 1, [0] 1}))
  (is (= (dense-to-sparse (take 10 (sin-series)))
         {[9] 1/362880, [7] -1/5040, [5] 1/120, [3] -1/6, [1] 1}))
  (is (= (dense-to-sparse (take 10 (cos-series)))
         {[8] 1/40320, [6] -1/720, [4] 1/24, [2] -1/2, [0] 1}))
  (is (= (dense-to-sparse (take 10 (atan-series)))
         {[8] 1/9, [6] -1/7, [4] 1/5, [2] -1/3, [0] 1}))
  (is (= (dense-to-sparse (take 10 (sinh-series)))
         {[9] 1/362880, [7] 1/5040, [5] 1/120, [3] 1/6, [1] 1}))
  (is (= (dense-to-sparse (take 10 (cosh-series)))
         {[8] 1/40320, [6] 1/720, [4] 1/24, [2] 1/2, [0] 1})))
