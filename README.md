# Madhava v2 [![Build Status](https://travis-ci.org/Sophia-Gold/Madhava-v2.svg?branch=master)](https://travis-ci.org/Sophia-Gold/Madhava-v2)

>”Multiply the arc by the square of the arc, and take the result of repeating that (any number of times). Divide (each of the above numerators) by the squares of the successive even numbers increased by that number and multiplied by the square of the radius. Place the arc and the successive results so obtained one below the other, and subtract each from the one above. These together give the jiva, as collected together in the verse beginning with "vidvan" etc."

-[Madhava of Sangamagrama](https://en.wikipedia.org/wiki/Madhava_of_Sangamagrama) (c. 1350 – c. 1425), founder of the Kerala school of astronomy and mathematics

---

Madhava is a Clojure library for [automatic differentiation](https://en.wikipedia.org/wiki/Automatic_differentiation) of partial differential equations. As opposed to many other functional AD libraries, Madhava takes a stream processing approach by generating all partials up to a given order at once and storing them in integer-keyed radix tries. Functions are similarly represented as maps of monomials in reverse lexicographical order with exponents encoded as integer keys and corresponding coefficients as values. This approach is both simple and extremely fast: capable of generating four orders of partial derivatives from hairy three dimensional functions in ~0.1ms on commodity CPUs.

Additional functions are included for arithmetic operations, functional composition, divergence, gradients, curl, directional derivatives, normal vectors, Laplacians, and several common Taylor series. Since functions can be composed after they've been generated as data (as opposed to using Clojure's built-in composition function) the chain rule can be applied in arbitrary order, making reverse and mixed mode as simple as forward mode&mdash;a major distinction compared to other AD packages.

Many thanks to Doug McIlroy for feedback and encouragement along the way. His [Power Serious](http://www.cs.dartmouth.edu/~doug/powser.html) package for Haskell will always be an inspiration for elegant software design.

---

## Usage

Generating partial derivatives:

```
;; 2xy + 3x + 5y + 7
=> (pprint (diff (i/int-map 0 7, 1 5, 10 3, 11 2) 2))
{1 {0 3, 1 2},
 2 {0 5, 10 2},
 11 {},
 12 {0 2},
 21 {0 2},
 22 {}}
```

Parallel (NOTE: usually slower unless using very high dimensions or heavy processing):

```
=> (pprint (pdiff (i/int-map 0 7, 1 5, 10 3, 11 2) 2))
{1 {0 3, 1 2},
 2 {0 5, 10 2},
 11 {},
 12 {0 2},
 21 {0 2},
 22 {}}
```

Arithmetic (NOTE: output is in lexicographic order):

```
;; (2xy + 3x + 5y + 7) + (x^2y + 4x + y) = x^2y + 2xy + 7x + 6y + 7
=> (add (i/int-map 0 7, 1 5, 10 3, 11 2) (i/int-map 1 1, 10 4, 21 1))
{0 7, 1 6, 10 7, 11 2, 21 1}

;; variadic:
;; (2xy + 3x + 5y + 7) + (x^2y + 4x + y) + 10 = x^2y + 2xy + 7x + 6y + 17
=> (add (i/int-map 0 7, 1 5, 10 3, 11 2) (i/int-map 1 1, 10 4, 21 1) (i/int-map 0 10))
{0 17, 1 6, 10 7, 11 2, 21 1}

;; (2xy + 3x + 5y + 7) - (2xy + 3x + 5y + 7) = 0
=> (sub (i/int-map 0 7, 1 5, 10 3, 11 2) (i/int-map 0 7, 1 5, 10 3, 11 2))
{}

;; -(2xy + 3x + 5y + 7) = -2xy - 3x -5y -7
=> (sub (i/int-map 0 7, 1 5, 10 3, 11 2))
{11 -2, 10 -3, 1 -5, 0 -7}

;; 2 * (2xy + 3x + 5y + 7) = 4xy + 6x + 10y + 14
=> (scale (i/int-map 0 7, 1 5, 10 3, 11 2) 2)
{0 14, 1 10, 10 6, 11 4}

;; (2xy + 3x + 5y + 7) * (x^2y + 4x + y)
;; = 2x^3y^2 + 3x^3y + 5x^2y^2 + 15x^2y + 2xy^2 + 12x^2 + 23xy + 5y^2 + 28x + 7y
=> (mul (i/int-map 0 7, 1 5, 10 3, 11 2) (i/int-map 1 1, 10 4, 21 1))
{1 7, 2 5, 10 28, 11 23, 12 2, 20 12, 21 15, 22 5, 31 3, 32 2}

;; (2xy + 3x + 5y + 7) / (2xy + 3x + 5y + 7) = 1 + (no remainder)
=> (divide (i/int-map 0 7, 1 5, 10 3, 11 2) (i/int-map 0 7, 1 5, 10 3, 11 2))
({0 1} {})
```

Composition:

```
;; f = 2xy + 3x + 5y + 7
=> (def f (i/int-map 0 7, 1 5, 10 3, 11 2))
;; f(f(x)) = 4xy^2 + 10y^2 + 12xy + 9x + 34y + 28
=> (compose f f 1)
{0 28, 1 34, 2 10, 10 9, 11 12, 12 4}
;; f(f(y)) = 4x^2y + 6x^2 + 10xy + 22x + 45y + 42
=> (compose f f 2)
{0 42, 1 25, 10 32, 11 20, 20 6, 21 4}
```

Gradient:

```
;; f = 8(x^2)y(z^2) + y^4 + 2z^3 + 5x
=> (grad (i/int-map 3 2, 40 1, 100 5, 212 8))
;; (16xyz^2 + 5, 8(x^2)(y^2) + 4y^3, 12(x^2)yz + 6z^2
({0 5, 112 16} {30 4, 202 8} {2 6, 211 16})
```

Laplacian:

```
=> (laplacian (i/int-map 3 2, 40 1, 100 5, 212 8))
;; (16yz^2, 12y^2, 12(x^2)y + 12z)
{1 12, 12 16, 20 12, 210 16}
=> (= (laplacian (i/int-map 3 2, 40 1, 100 5, 212 8))
      (div (grad (i/int-map 3 2, 40 1, 100 5, 212 8))))
true
```

Divergence (in Cartesian coordinates):

```
;; f(x,y,z) = 5(x^4)(y^3)(z^3) + 8(x^2)y(z^2) + y^4
=> (div [(i/int-map 433 5) (i/int-map 212 8) (i/int-map 40 1)])
;; 8(x^2)(z^2) + 20(x^3)(y^3)(z^3)
{202 8, 333 20}
```

Curl (in Cartesian coordinates):

```
=> (curl [(i/int-map 433 5) (i/int-map 212 8) (i/int-map 40 1)])
;; (- 16xy(z^2) + 4y^3, 15(x^4)(y^3)(z^2) - 15(x^4)(y^2)(z^3), 16(x^2)yz)
({30 4, 211 -16} {432 15} {112 16, 423 -15})
```

["The boundary of a boundary is zero."](http://cqi.inf.usi.ch/qic/wheeler.pdf)

```
=> (div (curl [(i/int-map 433 5) (i/int-map 212 8) (i/int-map 40 1)]))
{}
=> (curl (grad (i/int-map 3 2, 40 1, 100 5, 212 8)))
({} {} {})
```

Taylor Series:

```
=> (dense-to-sparse (take 10 (exp-series)))
{0 1, 1 1, 2 1/2, 3 1/6, 4 1/24, 5 1/120, 6 1/720, 7 1/5040, 8 1/40320, 9 1/362880}

=> (dense-to-sparse (take 10 (sin-series)))
{1 1, 3 -1/6, 5 1/120, 7 -1/5040, 9 1/362880}

=> (dense-to-sparse (take 10 (cos-series)))
{0 1, 2 -1/2, 4 1/24, 6 -1/720, 8 1/40320}

=> (dense-to-sparse (take 10 (atan-series)))
{0 1, 2 -1/3, 4 1/5, 6 -1/7, 8 1/9}

=> (dense-to-sparse (take 10 (sinh-series)))
{1 1, 3 1/6, 5 1/120, 7 1/5040, 9 1/362880}

=> (dense-to-sparse (take 10 (cosh-series)))
{0 1, 2 1/2, 4 1/24, 6 1/720, 8 1/40320}
```

Printing to a text file:

```
=> (print-tape "my_derivatives" (diff (i/int-map 0 7, 1 5, 10 3, 11 2) 2))
```

Benchmarking:

```
;; 3 dimensions, 5 terms, 4 orders tested on 2.6GHz Core i7 
=> (use 'criterium.core)
=> (bench (diff (i/int-map 3 2, 40 1, 100 5, 212 8, 433 5) 4))
Evaluation count : 552540 in 60 samples of 9209 calls.
             Execution time mean : 108.857714 µs
    Execution time std-deviation : 1.092826 µs
   Execution time lower quantile : 107.648204 µs ( 2.5%)
   Execution time upper quantile : 111.676714 µs (97.5%)
                   Overhead used : 1.732578 ns
```