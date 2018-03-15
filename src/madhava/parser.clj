(ns madhava.parser
  (:require [madhava.arithmetic :refer :all]
            [madhava.ast :refer [ast example]]
            [madhava.util :refer :all]
            [clojure.core.async.impl.ioc-macros :refer [parse-to-state-machine]]
            [clojure.core.match :refer [match]]
            [com.rpl.specter :refer :all]
            [clojure.pprint :refer [pprint]]
            [clojure.data.avl :refer [sorted-map-by]]
            [clj-tuple :refer [vector]])
  (:refer-clojure :exclude [vector sorted-map-by]))

(defn ** [base exp]
  (long (Math/pow base exp)))

(defmacro ssa [f]
  `(-> ~f
       ast
       parse-to-state-machine
       second
       :blocks
       vals
       first))

;; (defmacro ssa2 [f] 
;;   `(-> ~f
;;        quote
;;        clojure.repl/source-fn
;;        read-string
;;        nnext 
;;        parse-to-state-machine
;;        second
;;        :blocks
;;        vals
;;        first))

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

;; (defn trace-control-flow [asm]
;;   (let [args (args asm)]
;;     (letfn [(trace [block]
;;               (let [op (keyword (name (first block))) 
;;                     children (mapv #(if (and (symbol? %)
;;                                              (empty? (filter (partial = %) args)))
;;                                       (trace (get-block % asm))
;;                                       %)
;;                                    (next block))]
;;                 {op children}))]
;;       (trace (ret asm)))))

(defn trace [block asm]
  (let [args (args asm)
        op (keyword (name (first block)))
        children (mapv #(if (and (symbol? %)
                                   (empty? (filter (partial = %) args)))
                            (trace (get-block % asm) asm)
                            %)
                         (next block))]
     {op children}))

(defmacro trace-control-flow [f]
  `(let [asm# (ssa ~f)]
     (trace (ret asm#) asm#)))

;; (def dict {clojure.core/+ 'add
;;            'clojure.core/- 'sub
;;            'clojure.core/* 'add
;;            'clojure.core// 'div
;;            'madhava.parser/** 'pow})

(defn parse-to-mono [expr]
  (let [op (first expr)
        args (second expr)]
    (match [op args]
     [:+ args]         (->> args
                            (parse-to-mono)
                            (merge-with +'))
     [:- args]         (->> args
                            (parse-to-mono)
                            (merge-with -')) 
     [:* [coeff vars]] {(parse-to-mono vars) coeff} 
     [:/ [a1 a2]]      (cond
                         (and (number? a1)
                              (number? a2)) (/ a1 a2)
                         (number? a1)       (->> a2
                                                 (transform [MAP-KEYS] #(map (partial / a1) %))
                                                 (transform [MAP-VALS] #(/ % a1)))
                         (number? a2)       (transform [MAP-VALS] #(/ % a2) a1)
                         :else              (divide a1 a2))
     [:** [vars exp]]  (->> vars
                            (apply conj (vector))
                            (mapv (partial * coeff))))))
