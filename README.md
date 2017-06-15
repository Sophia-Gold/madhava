# Madhava v2

>”Multiply the arc by the square of the arc, and take the result of repeating that (any number of times). Divide (each of the above numerators) by the squares of the successive even numbers increased by that number and multiplied by the square of the radius. Place the arc and the successive results so obtained one below the other, and subtract each from the one above. These together give the jiva, as collected together in the verse beginning with "vidvan" etc."

-[Madhava of Sangamagrama](https://en.wikipedia.org/wiki/Madhava_of_Sangamagrama) (c. 1350 – c. 1425), founder of the Kerala school of astronomy and mathematics

---

Madhava is a Clojure library for [automatic differentiation](https://en.wikipedia.org/wiki/Automatic_differentiation) and integration of partial differential equations. As opposed to many other functional AD libraries, Madhava takes a stream processing approach by generating all partials up to a given order at once and storing them in integer-keyed radix tries. As functions are represented as dense collections of n-tuples stored in Clojure vectors, this approach is both simple and extremely fast: capable of generating four orders of partial derivatives from hairy three dimensional functions in less than 0.5ms on commodity CPUs.

Additional functions are included for basic arithmetic operations, linear transformations, functional composition, and several common Taylor series. Since partials can be composed after they've been generated as data (as opposed to using the language's built-in composition function) the chain rule can be applied in arbitrary order, making reverse and mixed mode as simple as forward mode&mdash;a major distinction compared to other AD packages.

Many thanks to Doug McIlroy for feedback and encouragement along the way. His [Power Serious](http://www.cs.dartmouth.edu/~doug/powser.html) package for Haskell will always be an inspiration for elegant software design.

---

## Usage

Generating partial derivatives:

```
;; 2xy + 3x + 5y + 7
=> (def diff-map (atom (i/int-map)))
=> (diff [[2 1 1] [3 1 0] [5 0 1] [7 0 0]] diff-map 2)
=> (pprint @diff-map)
{0 [[2 1 1] [3 1 0] [5 0 1] [7 0 0]],
 11 [[2 0 1] [3 0 0]],
 12 [[2 1 0] [5 0 0]],
 211 [],
 212 [[2 0 0]],
 221 [[2 0 0]],
 222 []}
;; or in one shot:
=> (diff-once [[2 1 1] [3 1 0] [5 0 1] [7 0 0]] 2)
{0 [[2 1 1] [3 1 0] [5 0 1] [7 0 0]],
 11 [[2 0 1] [3 0 0]],
 12 [[2 1 0] [5 0 0]],
 211 [],
 212 [[2 0 0]],
 221 [[2 0 0]],
 222 []}
```

Integrals:

```
=> (def diff-map (atom (i/int-map)))
=> (anti-diff [[2 1 1] [3 1 0] [5 0 1] [7 0 0]] diff-map 3)
=> (pprint int-map)
{0 [[2 1 1] [3 1 0] [5 0 1] [7 0 0]],
 11 [[1 2 1] [3/2 2 0]],
 12 [[1 1 2] [5/2 0 2]],
 211 [[1/3 3 1] [1/2 3 0]],
 212 [[1/2 2 2]],
 221 [[1/2 2 2]],
 222 [[1/3 1 3] [5/6 0 3]],
 3111 [[1/12 4 1] [1/8 4 0]],
 3112 [[1/6 3 2]],
 3121 [[1/6 3 2]],
 3122 [[1/6 2 3]],
 3211 [[1/6 3 2]],
 3212 [[1/6 2 3]],
 3221 [[1/6 2 3]],
 3222 [[1/12 1 4] [5/24 0 4]]}
=> (anti-diff-once [[2 1 1] [3 1 0] [5 0 1] [7 0 0]] 3)
{0 [[2 1 1] [3 1 0] [5 0 1] [7 0 0]],
 11 [[1 2 1] [3/2 2 0]],
 12 [[1 1 2] [5/2 0 2]],
 211 [[1/3 3 1] [1/2 3 0]],
 212 [[1/2 2 2]],
 221 [[1/2 2 2]],
 222 [[1/3 1 3] [5/6 0 3]],
 3111 [[1/12 4 1] [1/8 4 0]],
 3112 [[1/6 3 2]],
 3121 [[1/6 3 2]],
 3122 [[1/6 2 3]],
 3211 [[1/6 3 2]],
 3212 [[1/6 2 3]],
 3221 [[1/6 2 3]],
 3222 [[1/12 1 4] [5/24 0 4]]}
```

Arithmetic:

```
;; (2xy + 3x + 5y + 7) + (x^2y + 4x + y) = x^2y + 2xy + 7x + 6y +7
=> (add [[2 1 1] [3 1 0] [5 0 1] [7 0 0]] [[1 2 1] [4 1 0] [1 0 1]])
[[1 2 1] [2 1 1] [7 1 0] [6 0 1] [7 0 0]]

;; (2xy + 3x + 5y + 7) - (2xy + 3x + 5y + 7) = 0
=> (sub [[2 1 1] [3 1 0] [5 0 1] [7 0 0]] [[2 1 1] [3 1 0] [5 0 1] [7 0 0]])
[]

;; 2 * (2xy + 3x + 5y + 7) = 4xy + 6x + 10y + 14
=> (scale [[2 1 1] [3 1 0] [5 0 1] [7 0 0]] 2)
[[4 1 1] [6 1 0] [10 0 1] [14 0 0]]

;; (2xy + 3x + 5y + 7) * (x^2y + 4x + y)
;; = 2x^3y^2 + 3x^3y + 5x^2y^2 + 15x^2y + 2xy^2 + 12x^2 + 23xy + 5y^2 + 28x + 7y
=> (mul [[2 1 1] [3 1 0] [5 0 1] [7 0 0]] [[1 2 1] [4 1 0] [1 0 1]])
[[2 3 2] [3 3 1] [5 2 2] [15 2 1] [2 1 2] [12 2 0] [23 1 1] [5 0 2] [28 1 0] [7 0 1]]
```

Linear transforms:

```
=> (def diff-map (atom (i/int-map)))
=> (diff [[2 1 1] [3 1 0] [5 0 1] [7 0 0]] diff-map 1)
=> (linear-transform diff-map [(Math/cos 0.5) 1 1] [(Math/sin 0.5) 1 2])
[[1.682941969615793 1 1] [2.5244129544236897 1 0] [4.207354924039483 0 1] [6.311032386059225 0 0]]
```

Composition:

```
;; f = 2xy + 3x + 5y + 7
=> (def f [[2 1 1] [3 1 0] [5 0 1] [7 0 0]])
;; f(f(x)) = 4xy^2 + 10y^2 + 12xy + 9x + 34y + 28
=> (compose f f 1)
=> [[4 1 2] [12 1 1] [10 0 2] [9 1 0] [34 0 1] [28 0 0]]
;; f(f(y)) = 4x^2y + 6x^2 + 10xy + 22x + 45y + 42
=> (compose f f 2)
=> [[4 2 1] [6 2 0] [20 1 1] [32 1 0] [25 0 1] [42 0 0]]
```

Taylor Series:

```
=> (sparse-to-dense (take 10 (exp-series)))
[[1/362880 9] [1/40320 8] [1/5040 7] [1/720 6] [1/120 5] [1/24 4] [1/6 3] [1/2 2] [1 1] [1 0]]

=> (sparse-to-dense (take 10 (sin-series)))
[[1/362880 9] [-1/5040 7] [1/120 5] [-1/6 3] [1 1]]

=> (sparse-to-dense (take 10 (cos-series)))
[[1/40320 8] [-1/720 6] [1/24 4] [-1/2 2] [1 0]]

=> (sparse-to-dense (take 10 (atan-series)))
[[1/9 8] [-1/7 6] [1/5 4] [-1/3 2] [1 0]]

=> (sparse-to-dense (take 10 (sinh-series)))
[[1/362880 9] [1/5040 7] [1/120 5] [1/6 3] [1 1]]

=> (sparse-to-dense (take 10 (cosh-series)))
[[1/40320 8] [1/720 6] [1/24 4] [1/2 2] [1 0]]
```

Printing to a text file:

```
=> (def diff-map (atom (i/int-map)))
=> (diff [[2 1 1] [3 1 0] [5 0 1] [7 0 0]] diff-map 2)
=> (print-map diff-map)
```

Benchmarking:

```
;; 3 dimensions, 5 terms, 4 orders tested on 2.6GHz Core i7 
=> (use 'criterium.core)
=> (quick-bench (doall (diff [[5 4 3 3] [8 2 1 2] [1 0 4 0] [2 0 0 3] [5 1 0 0]] (atom (i/int-map)) 4)))
Evaluation count : 1848 in 6 samples of 308 calls.
             Execution time mean : 326.552184 µs
    Execution time std-deviation : 7.665505 µs
   Execution time lower quantile : 318.624896 µs ( 2.5%)
   Execution time upper quantile : 337.507200 µs (97.5%)
                   Overhead used : 7.373353 ns
```
