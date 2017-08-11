(ns madhava.parser
  (:require [madhava.statemachines :refer :all]
            [clojure.pprint :refer [pprint]])) 

(def async-custom-terminators
  {'clojure.core.async/<! `take!
   'clojure.core.async/>! `put!
   'clojure.core.async/alts! 'clojure.core.async/ioc-alts!
   :Return `return-chan})

(defn pause [x]
  x)

(defn pause-run [state blk val]
  (aset-all! state STATE-IDX blk VALUE-IDX val)
  :recur)

(defmacro runner [& body]
  (let [terminators {`pause `pause-run}
        crossing-env (zipmap (keys &env) (repeatedly gensym))]
    `(let [captured-bindings# (clojure.lang.Var/getThreadBindingFrame)
           ~@(mapcat (fn [[l sym]] [sym `(^:once fn* [] ~l)]) crossing-env)
           state# (~(state-machine `(do ~@body) 0 [crossing-env &env] terminators))]
       (aset-all! state#
                      ~BINDINGS-IDX
                      captured-bindings#)
       (run-state-machine state#)
       (aget-object state# VALUE-IDX))))

(defmacro defn [name & decls]
  `(def
     ~(with-meta name {:ast (list 'quote decls)})
     (fn ~decls)))

(defmacro ast [f]
  `(get (meta #'~f) :ast))

(defn poly [x y]
  (+ (* 2 x y)
     (* 3 x)
     (* 5 y)
     7))
