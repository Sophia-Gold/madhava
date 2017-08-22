(ns madhava.taylorseries)

(defn integrate-series [s]
  (map / s (drop 1 (range))))

(defn negate-series [s]
  (map - s))

(defn multi-to-univariate [poly var]
  (->> poly
       (map #(if (and (not= 0 (nth % var))
                      (not= 0 (next %)))
               (vector (first %) (nth % var))))
       (filterv some?)))

(defn dense-to-sparse [s]
  (->> s
       (map-indexed #(if (not= %2 0) [%2 %1]))
       (filterv some?)
       (rseq)
       (vec)))

(defn sparse-to-dense [poly]
  (let [poly (rseq poly)
        diff-terms (map #(vector (first %1) (dec (- (second %1) (second %2))))
                        (next poly) poly)]
    (->> diff-terms
         (cons (first poly))
         (mapcat #(concat (repeat (second %) 0) [(first %)]))
         (vec))))
      
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
