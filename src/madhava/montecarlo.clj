(ns madhava.montecarlo)

(defn rand-pairs [low1 high1 low2 high2]
  (lazy-seq
   (cons (list (+ low1 (rand high1))
               (+ low2 (rand high2)))
         (rand-pairs low1 high1 low2 high2))))

(defn monte-carlo [test passed failed]
  (letfn [(next-test [passed failed]
                     (lazy-seq
                      (cons (/ passed (+ passed failed))
                            (monte-carlo (rest test) passed failed))))]
    (if (first test)
      (next-test (inc passed) failed)
      (next-test passed (inc failed)))))

(defn estimate-integral [p x1 x2 y1 y2]estimate-integral
  (let [area (* (- x2 x1) (- y2 y1))
        rands (rand-pairs x1 x2 y1 y2)]
    (map #(* area %) (monte-carlo (map p rands) 0.0 0.0))))


;; TEST: decimal approximation of pi
(defn pi []
  (letfn [(sum-of-squares [x y]
            (+ (* x x) (* y y)))]
    (estimate-integral  #(>= 1 (sum-of-squares (dec (first %))
                                               (dec (fnext %)))) 0 2 0 2)))
