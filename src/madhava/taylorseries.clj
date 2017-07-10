(ns madhava.taylorseries)

(defn integrate-series [s]
  (map / s (drop 1 (range))))

(defn negate-series [s]
  (map - s))

(defn sparse-to-dense [s]
  (vec
   (rseq
    (filterv some?
             (mapv #(if (not= %1 0) [%1 %2]) s (range))))))

(defn dense-to-sparse [poly var]
    (loop [poly poly
           result []]
      (let [term (first poly)
            coeff (vector (first term))
            rest (next poly)
            next-term (first rest)]
        (if (nil? (next rest))
          (vec
           (concat (repeat (nth (last poly) var) 0)
                   (vector (first next-term))
                   (repeat (- (dec (nth term var)) (nth next-term var)) 0)
                   coeff
                   result))
          (recur rest
                 (concat (repeat (- (dec (nth term var)) (nth next-term var)) 0)
                         coeff
                         result))))))
      
(defn exp-series []
  (->> (exp-series)
       (integrate-series)
       (lazy-cat [1])))

(declare cos-series)
(defn sin-series []
  (->> (cos-series)
       (integrate-series)
       (lazy-cat [0])))

(defn cos-series []
  (->> (sin-series)
       (negate-series)
       (integrate-series)
       (lazy-cat [1])))

(defn atan-series []
  (integrate-series
   (cycle [1 0 -1 0])))

(declare cosh-series)
(defn sinh-series []
  (->> (cosh-series)
       (integrate-series)
       (lazy-cat [0])))

(defn cosh-series []
  (->> (sinh-series)
       (integrate-series)
       (lazy-cat [1])))
