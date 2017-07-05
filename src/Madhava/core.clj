(ns Madhava.core
  (:require [Madhava.taylorseries :refer :all]
            [clojure.pprint :refer [pprint]]
            [clojure.string :as str]
            [clojure.data.int-map :as i]
            [com.rpl.specter :refer :all]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; DIFFERENTIATION & INTEGRATION

(defn vec-to-int [v]
  (Long/parseLong (apply str v)))

(defn int-to-vec [i]
  (mapv #(Long/parseLong %)
         (str/split (str i) #"")))

(defn diff [poly tape order]
  (letfn [(partial-diff [poly tape idx]
            (let [i (peek idx)
                  key (vec-to-int idx)
                  partial (vec
                           (for [expr poly
                                 :let [v (get expr i)]
                                 :when (not (zero? v))] 
                             (-> expr
                                 (update 0 * v)
                                 (update i dec))))] 
              (swap! tape assoc key partial)
              [partial idx]))
          (diff-vars [poly tape idx]
            (map #(partial-diff poly tape (conj idx %))
                 (range 1 (count (first poly)))))
          (diff-loop [poly n]
            (when (<= n order)
              (doseq [x poly]
                (diff-loop
                 (diff-vars (first x) tape (update (second x) 0 inc))
                 (inc n)))))]
  (swap! tape assoc 1 poly)
  (diff-loop [[poly [0]]] 0)))

(defn anti-diff [poly tape order]
  (letfn [(partial-int [poly tape idx]
            (let [i (peek idx)
                  key (vec-to-int idx)
                  partial (vec
                           (for [expr poly
                                 :let [v (get expr i)]
                                 :when (not (zero? v))] 
                             (-> expr
                                 (update 0 / (inc v))
                                 (update i inc))))]
              (swap! tape assoc key partial)
              [partial idx]))
          (int-vars [poly tape idx]
            (map #(partial-int poly tape (conj idx %))
                 (range 1 (count (first poly)))))
          (int-loop [poly n]
            (when (<= n order)
              (doseq [x poly]
                (int-loop 
                 (int-vars (first x) tape (update (second x) 0 inc))
                 (inc n)))))]
    (swap! tape assoc 1 poly)
    (int-loop [[poly [0]]] 0)))

(defn vdiff [vfunc tape order]
  (letfn [(partial-diff [poly tape idx]
            (let [i (peek idx)
                  key (vec-to-int idx)
                  partial (vec
                           (for [expr poly
                                 :let [v (get expr i)]
                                 :when (not (zero? v))] 
                             (-> expr
                                 (update 0 * v)
                                 (update i dec))))] 
              (swap! tape assoc key partial)
              [partial idx]))
          (diff-vars [poly tape idx]
            (map #(partial-diff poly tape (conj idx %))
                 (range 1 (count (first poly)))))
          (diff-loop [poly n]
            (when (<= n order)
              (doseq [x poly]
                (diff-loop
                 (diff-vars (first x) tape (update (second x) 1 inc))
                 (inc n)))))]
    (doseq [poly (map-indexed #(vector (inc %1) %2) vfunc)]
      (swap! tape assoc (first poly) (second poly))
      (diff-loop [[(second poly) [(first poly) 0]]] 0))))
  
(defn anti-vdiff [vfunc tape order]
  (letfn [(partial-int [poly tape idx]
            (let [i (peek idx)
                  key (vec-to-int idx)
                  partial (vec
                           (for [expr poly
                                 :let [v (get expr i)]
                                 :when (not (zero? v))] 
                             (-> expr
                                 (update 0 / (inc v))
                                 (update i inc))))]
              (swap! tape assoc key partial)
              [partial idx]))
          (int-vars [poly tape idx]
            (map #(partial-int poly tape (conj idx %))
                 (range 1 (count (first poly)))))
          (int-loop [poly n]
            (when (<= n order)
              (doseq [x poly]
                (int-loop 
                 (int-vars (first x) tape (update (second x) 1 inc))
                 (inc n)))))]
    (doseq [poly (map-indexed #(vector (inc %1) %2) vfunc)]
      (swap! tape assoc (first poly) (second poly))
      (int-loop [[(second poly) [(first poly) 0]]] 0))))

(defn pdiff [poly tape order]
  (letfn [(partial-diff [poly tape idx]
            (let [i (peek idx)
                  key (vec-to-int idx)
                  partial (vec
                           (for [expr poly
                                 :let [v (get expr i)]
                                 :when (not (zero? v))] 
                             (-> expr
                                 (update 0 * v)
                                 (update i dec))))] 
              (send tape assoc key partial)
              ;; (await tape)
              [partial idx]))
          (diff-vars [poly tape idx]
            (pmap #(partial-diff poly tape (conj idx %))
                 (range 1 (count (first poly)))))
          (diff-loop [poly n]
            (when (< n order)
              (doseq [x poly]
                (diff-loop
                 (diff-vars (first x) tape (update (second x) 0 inc))
                 (inc n)))))]
    (send tape assoc 1 poly)
    (diff-loop [[poly [0]]] 0)))

(defn anti-pdiff [poly tape order]
  (letfn [(partial-int [poly tape idx]
            (let [i (peek idx)
                  key (vec-to-int idx)
                  partial (vec
                           (for [expr poly
                                 :let [v (get expr i)]
                                 :when (not (zero? v))] 
                             (-> expr
                       (update 0 / (inc v))
                       (update i inc))))]
              (send tape assoc key partial)
              [partial idx]))
          (int-vars [poly tape idx]
            (pmap #(partial-int poly tape (conj idx %))
                 (range 1 (count (first poly)))))
          (int-loop [poly n]
            (when (< n order)
              (doseq [x poly]
                (int-loop 
                 (int-vars (first x) tape (update (second x) 0 inc))
                 (inc n)))))]
    (send tape assoc 1 poly)
    (int-loop [[poly [0]]] 0)))

(defn vpdiff [vfunc tape order]
  (letfn [(partial-diff [poly tape idx]
            (let [i (peek idx)
                  key (vec-to-int idx)
                  partial (vec
                           (for [expr poly
                                 :let [v (get expr i)]
                                 :when (not (zero? v))] 
                             (-> expr
                                 (update 0 * v)
                                 (update i dec))))] 
              (send tape assoc key partial)
              [partial idx]))
          (diff-vars [poly tape idx]
            (pmap #(partial-diff poly tape (conj idx %))
                  (range 1 (count (first poly)))))
          (diff-loop [poly n]
            (when (< n order)
              (doseq [x poly]
                (diff-loop
                 (diff-vars (first x) tape (update (second x) 1 inc))
                 (inc n)))))]
    (doseq [poly (map-indexed #(vector (inc %1) %2) vfunc)]
      (send tape assoc (first poly) (second poly))
      (diff-loop [[(second poly) [(first poly) 0]]] 0))))
  
(defn anti-vpdiff [vfunc tape order]
  (letfn [(partial-int [poly tape idx]
            (let [i (peek idx)
                  key (vec-to-int idx)
                  partial (vec
                           (for [expr poly
                                 :let [v (get expr i)]
                                 :when (not (zero? v))] 
                             (-> expr
                                 (update 0 / (inc v))
                                 (update i inc))))]
              (send tape assoc key partial)
              [partial idx]))
          (int-vars [poly tape idx]
            (pmap #(partial-int poly tape (conj idx %))
                  (range 1 (count (first poly)))))
          (int-loop [poly n]
            (when (< n order)
              (doseq [x poly]
                (int-loop 
                 (int-vars (first x) tape (update (second x) 1 inc))
                 (inc n)))))]
    (doseq [poly (map-indexed #(vector (inc %1) %2) vfunc)]
      (send tape assoc (first poly) (second poly))
      (int-loop [[(second poly) [(first poly) 0]]] 0))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ONE SHOTS

(defn diff-once [poly order]
  (let [tape (atom (i/int-map))]
    (diff poly tape order)
    @tape))

(defn anti-diff-once [poly order]
  (let [tape (atom (i/int-map))]
    (anti-diff poly tape order)
    @tape))

(defn vdiff-once [vfunc order]
  (let [tape (atom (i/int-map))]
    (vdiff vfunc tape order)
    @tape))

(defn anti-vdiff-once [vfunc order]
  (let [tape (atom (i/int-map))]
    (anti-vdiff vfunc tape order)
    @tape))

(defn pdiff-once [poly order]
  (let [tape (agent (i/int-map))]
    (pdiff poly tape order)
    (await tape)
    @tape))

(defn anti-pdiff-once [poly order]
  (let [tape (agent (i/int-map))]
    (anti-pdiff poly tape order)
    (await tape)
    @tape))

(defn vpdiff-once [poly order]
  (let [tape (agent (i/int-map))]
    (vpdiff poly tape order)
    (await tape)
    @tape))

(defn anti-vpdiff-once [poly order]
  (let [tape (agent (i/int-map))]
    (anti-vpdiff poly tape order)
    (await tape)
    @tape))

(defmacro print-map [map]
  `(pprint @~map
           (clojure.java.io/writer
            (str (quote ~map) ".txt"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; HELPER FUNCTIONS

(defn add-dim [poly dim]
  (mapv #(vec (concat (take dim %) [0] (drop dim %))) poly))

(defn remove-terms [term poly]
  (filterv #(not= (next term) (next %)) poly))

(defn denull [poly]
  (filterv #(not= 0 (first %)) poly))

(defn negate [poly]
  (mapv #(update % 0 -) poly))

(defn sort-terms [poly]
;; graded lexicographic order
  (vec
   (sort-by #(- (reduce + (next %)))
            (sort-by (comp - fnext) poly))))

(defn union [intersection & sets]
  (loop [i 0
         test intersection
         difference sets]
    (if (> i (count intersection))
      (into intersection (mapcat identity difference))
      (recur (inc i) (next test) (mapv (partial remove-terms (first test)) difference)))))

(defn intersection [poly1 poly2]
  (vec
   (for [term1 poly1
         term2 poly2
         :when (= (next term1) (next term2))]
     (update term1 0 + (first term2)))))

(defn simplify [poly]
  (loop [idx 0
         test poly
         result []]
    (if (> idx (dec (count poly)))
      result
      (if (= (nfirst test) (nfirst (next test)))
        (recur (+ 2 idx) (nnext test)
               (conj result (vec (cons (+ (ffirst test) (ffirst (next test))) (nfirst test)))))
        (recur (inc idx) (next test) (conj result (first test)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ARITHMETIC

(defn add
  ([] [])
  ([poly] poly)
  ([poly1 poly2]
   (sort-terms
    (denull
     (union (intersection poly1 poly2) poly1 poly2))))
  ([poly1 poly2 & more]
   (reduce add (add poly1 poly2) more)))

(defn sub
  ([] [])
  ([poly] (negate poly))
  ([poly1 poly2]
   (add poly1 (negate poly2)))
  ([poly1 poly2 & more]
   (reduce sub (add poly1 (negate poly2)) more)))

(defn scale [poly scalar]
  (mapv #(update % 0 * scalar) poly))

(defn mul
  ([] [])
  ([poly] poly)
  ([poly1 poly2]
   (simplify
    (sort-terms
     (for [term1 poly1
           term2 poly2
           :let [coeff (* (first term1) (first term2))]]
       (vec
        (cons coeff
              (for [idx (range 1 (count (first poly1)))]
                (+ (get term1 idx) (get term2 idx)))))))))
  ([poly1 poly2 & more]
   (reduce mul (mul poly1 poly2) more)))

(defn sqrt [f]
  (mapv (fn [v]
          (conj (vector (Math/sqrt (first v)))
                (mapv #(/ % 2) v)))
        f))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; DIVISION

(defn lcm [term1 term2]
  (sub
   (mapv #(max %1 %2) term1 term2)
   term1))

(defn spoly [f g]
  (let [f-vars (nfirst f)
        g-vars (nfirst g)]
    (sub
     (mul [(vec (concat [1]
                        (lcm f-vars g-vars) f-vars))]
          f)
     (mul [(vec (concat (/ (ffirst f) (ffirst g))
                        (lcm g-vars f-vars) g-vars))]
          g))))

(defn normal-form [f s]
  (loop [f f]
    (cond
      (zero? f) []
      (nil? s) f
      :else (recur (spoly f s)))))

(defn product-criterion [f g]
  (= [false]
     (distinct
      (map #(or (and (zero? %1) (not= 0 %2))
                (and (not= 0 %1) (zero? %2)))
           (nfirst f) (nfirst g)))))

(defn groebner [f]
  (let [s (for [f f
                g f
                :when (and (not= f g)
                           (product-criterion f g))]
            (list f g))]
    (letfn [(groebner-loop [f s]
              (let [nf (normal-form (spoly (ffirst s)
                                           (nfirst s)) f)]
      (cond
        (nil? s) f
        (zero? nf) (groebner-loop f (next s))
        :else (recur (cons nf (nfirst s))
                     (concat (next s) (for [new-f f
                                            :when (product-criterion nf new-f)]
                                        (list nf new-f)))))))]
      (groebner-loop f s))))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; MAP FUNCTIONS

(defn search-map [map val]
  (into (i/int-map)
        (select [ALL (fn [[k v]] (= v val))] map)))

;; filter empty vectors
(defn denull-map [map]
  (setval [MAP-VALS #(= [] %)] NONE map))

(defn vec-keys [map]
  (transform ALL (fn [[k v]] [(last (int-to-vec k)) v]) map))

(defn transform-map [map f]
  (transform MAP-VALS f map))

(defn add-maps [& maps]
  (apply merge-with add maps))

(defn mul-maps [& maps]
  (apply merge-with mul maps))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; VECTOR OPERATIONS

(defn magnitude [v]
  (sqrt
   (apply add
          (mapv #(mul [%] [%]) v))))

;; (defn normal [f]
;;   (let [g (grad f)]
;;     (divide
;;      (apply add g)
;;      (magnitude g))))

(defn jacobian [f]
  (select [ALL (fn [[k v]]
                 (and (> k 9) (< k 100)))]
          (diff-once f 1)))
  
(defn hessian [f]
  (select [ALL (fn [[k v]]
                 (and (> k 99) (< k 1000)))]
          (diff-once f 2)))

(defn grad [f]
  (vals
   (jacobian f)))

(defn div [f & n]
  (let [all-partials (vdiff-once f 1)
        partials (mapv #(get all-partials
                             (vec-to-int (vector % 1 %)))
                       (range 1 (inc (count f))))]    
    (apply add
           (if n
             (map #(scale %2 %1)
                  n
                  (map mul f partials))                   
             (map mul f partials)))))

(defn curl [f & n]
  (let [vars (count f)
        range1 (range (dec vars) (+ vars (dec vars)))
        range2 (range (- vars 2) (+ vars (- vars 2)))
        partials (vdiff-once f 1)
        partials-1 (mapv (fn [r1 r2]
                           (get partials
                                (vec-to-int (vector (inc (mod r1 vars))
                                                    1
                                                    (inc (mod r2 vars))))))
                              range1
                              range2)
        partials-2 (mapv (fn [r1 r2]
                           (get partials
                                (vec-to-int (vector (inc (mod r1 vars))
                                                    1
                                                    (inc (mod r2 vars))))))
                         range2
                         range1)]
    (apply add
           (if n
             (map #(scale %2 %1)
                  n
                  (map sub partials-1 partials-2))
             (map sub partials-1 partials-2)))))

(defn directional-diff [f n]
  (apply add
         (map #(scale %1 %2) (grad f) n)))

(defn laplacian [f]
  (let [all-partials (diff-once f 2)]
    (map #(get all-partials
               (vec-to-int (vector 2 % %)))
         (range 1 (count (first f))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; make variadic and multivariate
(defn compose [f g var]
  (loop [f f
         result []]
    (let [term (first f)]
      (if (nil? term)
        result
        (recur (next f) (if (zero? (nth term var))
                          (add [term] result)
                          (simplify (sort-terms
                                     (vec (concat result
                                                  (nth (iterate (partial mul [(assoc term var 0)]) g)
                                                       (nth term var))))))))))))

(defn chain [f g]
  (let [grad-f (grad 1)
        grad-g (diff-once g 1)]
    (map mul
         (map compose grad-f (repeat g))
         grad-g)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;        

(defn -main []
  )
