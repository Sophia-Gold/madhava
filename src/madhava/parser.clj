(ns madhava.parser
  (:require [madhava.ast :refer [ast example]]
            [madhava.util :refer :all]
            [clojure.core.async.impl.ioc-macros :refer [parse-to-state-machine]]
            [clojure.pprint :refer [pprint]]
            [clojure.data.avl :refer [sorted-map-by]]
            [clj-tuple :refer [vector]])
  (:refer-clojure :exclude [vector sorted-map-by]))

(defn ** [base exp]
  (long (Math/pow base exp)))

(def dict {'clojure.core/+ 'add
           'clojure.core/- 'sub
           'clojure.core/* 'add
           'clojure.core// 'div
           'madhava.parser/** 'pow})

(defmacro ssa [f]
  `(-> ~f
       ast
       parse-to-state-machine
       second
       :blocks
       vals
       first))

(defmacro ssa2 [f] 
  `(-> ~f
       quote
       clojure.repl/source-fn
       read-string
       nnext 
       parse-to-state-machine
       second
       :blocks
       vals
       first))

(defn get-block [id asm]
  (->> asm
       (filterv #(= (:id %) id))
       first
       :refs))

(defn args [asm]
  (->> asm
       first
       :refs
       next
       (into (vector))))

(defn ret [asm]
  (-> asm
      last
      :value
      (get-block asm)))

(defn trace-control-flow [asm]
  (let [args (args asm)]
    (letfn [(trace [block]
              (let [op (keyword (name (first block))) 
                    children (mapv #(if (and (symbol? %)
                                             (empty? (filter (partial = %) args)))
                                      (trace (get-block % asm))
                                      %)
                                   (next block))]
                {op children}))]
      (trace (ret asm)))))

;; (defn trace-control-flow2 [f]
;;   (let [asm (ssa f)
;;         args (args asm)]
;;     (letfn [(trace [block]
;;               (let [op (keyword (name (first block))) 
;;                     children (mapv #(if (and (symbol? %)
;;                                              (empty? (filter (partial = %) args)))
;;                                       (trace (get-block % asm))
;;                                       %)
;;                                    (next block))]
;;                 {op children}))]
;;       (trace (ret asm)))))

(def t1 (ssa example))
(def t2 (ssa2 example))
