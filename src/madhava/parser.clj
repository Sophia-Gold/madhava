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

(defn var-args [asm]
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
  
(defn match-expr [expr]
  (let [op (first expr)
        args (second expr)] 
    (match [op args]
           [:+ args]         (->> args
                                  (match-expr)
                                  (match-expr +'))
           [:- args]         (->> args
                                  (match-expr)
                                  (merge-with -'))
           [:* args]         (let [term (group-by number? args)
                                   coeff (reduce *' (get term true)) 
                                   vars (group-by symbol? (get term false)) 
                                   v1 (->> (get vars true) 
                                           frequencies
                                           (merge-with +' var-args)
                                           vals
                                           (into (vector)))
                                   v2 (apply match-expr (get vars false))]
                               {coeff (mapv +' v1 v2)})
           [:/ [a1 a2]]      (cond
                               (and (number? a1)
                                    (number? a2)) (/ a1 a2)
                               (number? a1)       (->> a2
                                                       (transform [MAP-KEYS] #(map (partial / a1) %))
                                                       (transform [MAP-VALS] #(/ % a1)))
                               (number? a2)       (transform [MAP-VALS] #(/ % a2) a1)
                               :else              (divide a1 a2))  ;; `:else` doesn't catch exceptions
           [:** [base exp]]  (cond 
                               (symbol? base) (->> {base exp}
                                                   (merge-with +' var-args)
                                                   vals
                                                   (into (vector)))
                               (number? base) (Math/pow base exp)
                               (map? base)    (pow base exp))))) 

(defn parse-mono [var-args expr]
  (let [var-args (zipmap var-args (repeat 0))]
    (match-expr expr)))

;; (defn trace-control-flow [asm]
;;   (let [args (var-args asm)]
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
  (let [args (var-args asm)
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
