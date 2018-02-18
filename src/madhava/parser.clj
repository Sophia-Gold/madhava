(ns madhava.parser
  (:require [madhava.ast :refer [ast example]]
            [clojure.core.async.impl.ioc-macros :refer [parse-to-state-machine]]
            [clojure.pprint :refer [pprint]]))

(defmacro ssa [f]
  `(-> ~f
       ast
       parse-to-state-machine
       second
       :blocks
       vals
       first))

(defn ret [f]
  (->> f
       (filter #(= (class %) clojure.core.async.impl.ioc_macros.Return))
       (first)
       :value))

(defn exprs [f]
  (filter #(= (class %) clojure.core.async.impl.ioc_macros.Call) f))

(defn expr-iter [f next]
  (->> f
       (filter #(= (:id %) next))
       first
       :refs))

(def t (ssa example))
