(ns madhava.comp
  (:require [madhava.util :refer :all] 
            [clojure.core :as cc] 
            [clojure.data.avl :refer [sorted-map-by]]
            [com.rpl.specter :refer :all]
            [primitive-math]
            [clj-tuple :refer [vector]])
  (:refer-clojure :exclude [vector sorted-map-by]))

(primitive-math/use-primitive-operators)

(deftype Var [^Number v])

(definterface ITerm
  (coeff [])
  (vars [])
  (dims [])
  (insert [^Number coeff & ^Number vars]))
  
(deftype Term [^Number coeff & ^Number vars]
  ITerm
  (coeff [this] coeff)
  (vars [this] vars)
  (dims [this] (count vars))
  (insert [this ^Number coeff & ^Number vars]
    (into (vector)
          (into (vector) vars)
          coeff)))

(definterface IPoly
  (denull [this])
  (add-dims [this this])
  (transform [this xf])
  (insert [this ^Term term])
  (realize [this])

(deftype Poly [& ^Term terms]
  IPoly
  (denull [this]
    ;; move from util
    )
  (add-dims [this ^long n]
    ;; move from util, project into arbitrary higher dimensions
    )
  (transform [this xf]
    ;; replicate `map` from clojure.core with `transform` from specter
    )
  (insert [this ^Term term]
    ;; lazy insertion à la difference lists
    )
  (realize ^AVLMap [this]
    ;; batch insertion to replace `(->> f (apply add) (insert (sorted-map-by) grevlex))`
    ))

(defn project [^Poly f ^Poly g]
  ;; compare dimensions and call `add-dims` on lower poly with n = difference 
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Pretty Printing for LHS
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod print-method Term [v ^java.io.Writer w]
  (print-method (:coeff v) w)
  (print-method (:vars v) w)
  ;; map-indexed over vars: (cond-> v
  ;;                          0 ""
  ;;                          1 %
  ;;                          :else "(" % "^v)"
  )

(defmethod print-method Poly [v ^java.io.Writer w]
  (print-method (:terms v) w)
  ;; insert a "+" in between terms
  ;; (.write w "+")
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Difference List Example (for reference)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  
(defn dl 
  [& elements] 
  (fn [x] (concat elements x)))

(defn dl-un 
  [l] 
  (l nil))
(defn dl-concat 
  [& lists] 
  (fn [x] ((apply comp lists) x)))

(def example (dl-un (dl-concat (dl 1) (dl 2 3) (dl) (dl 4))))
