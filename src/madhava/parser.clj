(ns madhava.parser
  [madhava.core :refer :all])

(defmacro defn [name & decls]
  `(def
     ~(with-meta name {:ast (list 'quote decls)})
     (fn ~decls)))

(defmacro ast [f]
  `(get (meta #'f) :ast))

(defn poly [x y]
  (+ (* 2 x y)
     (* 3 x)
     (* 5 y)
     7))
