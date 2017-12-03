(ns madhava.core
  (:gen-class)
  (:require [madhava.diff :refer :all]
            [madhava.arithmetic :refer :all]
            [madhava.vectormath :refer :all]
            [madhava.comp :refer :all]
            [madhava.taylorseries :refer :all]
            [madhava.parser :refer :all]
            [clojure.data.int-map :as i]))

(defn -main []
  (compose (i/int-map 0 7, 1 5, 10 3, 11 2)
           (i/int-map 0 7, 1 5, 10 3, 11 2) 1))
