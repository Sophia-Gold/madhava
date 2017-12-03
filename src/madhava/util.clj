(ns madhava.util
  (:require [com.rpl.specter :refer :all]
            [clojure.data.int-map :as i]))

(defn add-dim [poly]
  ;; projects into next higher dimension by appending zero to tuples of variables
  (transform [MAP-KEYS] #(conj % 0) poly))

(defn denull [poly]
  (setval [MAP-VALS #(= % 0)] NONE poly))  
