(ns madhava.parser
  (:require [madhava.ast :refer [ast example]]
            [clojure.core.async.impl.ioc-macros :refer [parse-to-state-machine]]
            [clojure.pprint :refer [pprint]]))

(defmacro ssa [f]
  `(-> ~f
       ast
       second
       parse-to-state-machine
       second
       :blocks
       vals
       first))

(def t (ssa example))
