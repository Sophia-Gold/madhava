# Madhava v2 [![Build Status](https://travis-ci.org/Sophia-Gold/Madhava-v2.svg?branch=master)](https://travis-ci.org/Sophia-Gold/Madhava-v2)

>”Multiply the arc by the square of the arc, and take the result of repeating that (any number of times). Divide (each of the above numerators) by the squares of the successive even numbers increased by that number and multiplied by the square of the radius. Place the arc and the successive results so obtained one below the other, and subtract each from the one above. These together give the jiva, as collected together in the verse beginning with "vidvan" etc."

-[Madhava of Sangamagrama](https://en.wikipedia.org/wiki/Madhava_of_Sangamagrama) (c. 1350 – c. 1425), founder of the Kerala school of astronomy and mathematics

---

Madhava is a Clojure library for [automatic differentiation](https://en.wikipedia.org/wiki/Automatic_differentiation) and integration of partial differential equations. Madhava uses a different technique than most existing AD libraries. All partials up to a given order are generated at once and stored in integer-keyed radix tries. Functions are represented as hash-maps of monomials in [graded reverse lexicographic order](https://en.wikipedia.org/wiki/Monomial_order#Graded_reverse_lexicographic_order) with tuples of exponents as keys and corresponding coefficients as values. This approach is both simple and extremely fast: capable of generating four orders of partial derivatives from hairy three dimensional functions in ~0.1ms on commodity CPUs.

Additional functions are included for arithmetic operations, functional composition, divergence, gradients, curl, directional derivatives, normal vectors, Laplacians, and several common Taylor series. Since functions can be composed after they've been generated as data (as opposed to using Clojure's built-in composition function) the chain rule can be applied in arbitrary order, making reverse and mixed mode as simple as forward mode&mdash;a major distinction compared to other AD packages.

Many thanks to Doug McIlroy for feedback and encouragement along the way. His [Power Serious](http://www.cs.dartmouth.edu/~doug/powser.html) package for Haskell will always be an inspiration for elegant software design.

---

## Usage

Generating partial derivatives:

```
;; 2xy + 3x + 5y + 7
=> (pprint (diff {[1 1] 2, [1 0] 3, [0 1] 5, [0 0] 7} 2))
{1 {[0 1] 2, [0 0] 3},
 2 {[1 0] 2, [0 0] 5},
 11 {},
 12 {[0 0] 2},
 21 {[0 0] 2},
 22 {}}
```

Integrals:

```
=> (pprint (anti-diff {[1 1] 2, [1 0] 3, [0 1] 5, [0 0] 7} 3))
{1 {[2 1] 1, [2 0] 3/2},
 2 {[1 2] 1, [0 2] 5/2},
 11 {[3 1] 1/3, [3 0] 1/2},
 12 {[2 2] 1/2},
 21 {[2 2] 1/2},
 22 {[1 3] 1/3, [0 3] 5/6},
 111 {[4 1] 1/12, [4 0] 1/8},
 112 {[3 2] 1/6},
 121 {[3 2] 1/6},
 122 {[2 3] 1/6},
 211 {[3 2] 1/6},
 212 {[2 3] 1/6},
 221 {[2 3] 1/6},
 222 {[1 4] 1/12, [0 4] 5/24}}
```

Parallel (NOTE: usually slower unless using very high dimensions or heavy processing):

```
=> (pprint (pdiff {[1 1] 2, [1 0] 3, [0 1] 5, [0 0] 7} 2))
{1 {[0 1] 2, [0 0] 3},
 2 {[1 0] 2, [0 0] 5},
 11 {},
 12 {[0 0] 2},
 21 {[0 0] 2},
 22 {}}
=> (pprint (anti-pdiff {[1 1] 2, [1 0] 3, [0 1] 5, [0 0] 7} 3))
{1 {[2 1] 1, [2 0] 3/2},
 2 {[1 2] 1, [0 2] 5/2},
 11 {[3 1] 1/3, [3 0] 1/2},
 12 {[2 2] 1/2},
 21 {[2 2] 1/2},
 22 {[1 3] 1/3, [0 3] 5/6},
 111 {[4 1] 1/12, [4 0] 1/8},
 112 {[3 2] 1/6},
 121 {[3 2] 1/6},
 122 {[2 3] 1/6},
 211 {[3 2] 1/6},
 212 {[2 3] 1/6},
 221 {[2 3] 1/6},
 222 {[1 4] 1/12, [0 4] 5/24}}
```

Arithmetic:

```
;; (2xy + 3x + 5y + 7) + (x^2y + 4x + y) = x^2y + 2xy + 7x + 6y + 7
=> (add {[1 1] 2, [1 0] 3, [0 1] 5, [0 0] 7} {[2 1] 1, [1 0] 4, [0 1] 1})
{[2 1] 1, [1 1] 2, [1 0] 7, [0 1] 6, [0 0] 7}

;; variadic:
;; (2xy + 3x + 5y + 7) + (x^2y + 4x + y) + 10 = x^2y + 2xy + 7x + 6y + 17
=> (add {[1 1] 2, [1 0] 3, [0 1] 5, [0 0] 7} {[2 1] 1, [1 0] 4, [0 1] 1} {[0 0] 10})
{[2 1] 1, [1 1] 2, [1 0] 7, [0 1] 6, [0 0] 17}

;; (2xy + 3x + 5y + 7) - (2xy + 3x + 5y + 7) = 0
=> (sub {[1 1] 2, [1 0] 3, [0 1] 5, [0 0] 7} {[1 1] 2, [1 0] 3, [0 1] 5, [0 0] 7})
{}

;; -(2xy + 3x + 5y + 7) = -2xy - 3x -5y -7
=> (sub {[1 1] 2, [1 0] 3, [0 1] 5, [0 0] 7})
{[1 1] -2, [1 0] -3, [0 1] -5, [0 0] -7}

;; 2 * (2xy + 3x + 5y + 7) = 4xy + 6x + 10y + 14
=> (scale {[1 1] 2, [1 0] 3, [0 1] 5, [0 0] 7} 2)
{[1 1] 4, [1 0] 6, [0 1] 10, [0 0] 14}

;; (2xy + 3x + 5y + 7) * (x^2y + 4x + y)
;; = 2x^3y^2 + 3x^3y + 5x^2y^2 + 15x^2y + 2xy^2 + 12x^2 + 23xy + 5y^2 + 28x + 7y
=> (mul {[1 1] 2, [1 0] 3, [0 1] 5, [0 0] 7} {[2 1] 1, [1 0] 4, [0 1] 1})
{[3 2] 2, [3 1] 3, [2 2] 5, [2 1] 15, [1 2] 2, [2 0] 12, [1 1] 23, [0 2] 5, [1 0] 28, [0 1] 7}

;; (2xy + 3x + 5y + 7) / (2xy + 3x + 5y + 7) = 1 + (no remainder)
=> (divide {[1 1] 2, [1 0] 3, [0 1] 5, [0 0] 7} {[1 1] 2, [1 0] 3, [0 1] 5, [0 0] 7})
({[0 0] 1} {})
;; (2xy + 10x + 3y + 15) / (y + 5) = 2x + 3 + (no remainder)
=> (divide {[1 1] 2, [1 0] 10, [0 1] 3, [0 0] 15} {[0 1] 1, [0 0] 5})
({[1 0] 2, [0 0] 3} {})
```

Composition:

```
;; f = 2xy + 3x + 5y + 7
=> (def f {[1 1] 2, [1 0] 3, [0 1] 5, [0 0] 7})
;; f(f(x)) = 4xy^2 + 10y^2 + 12xy + 9x + 34y + 28
=> (compose f f 1)
{[1 2] 4, [1 1] 12, [0 2] 10, [1 0] 9, [0 1] 34, [0 0] 28}
;; f(f(y)) = 4x^2y + 6x^2 + 10xy + 22x + 45y + 42
=> (compose f f 2)
{[2 1] 4, [2 0] 6, [1 1] 20, [1 0] 32, [0 1] 25, [0 0] 42}
```

Gradient:

```
;; f = 8(x^2)y(z^2) + y^4 + 2z^3 + 5x
=> (grad {[2 1 2] 8, [0 4 0] 1, [0 0 3] 2, [1 0 0] 5})
;; (16xyz^2 + 5, 8(x^2)(y^2) + 4y^3, 12(x^2)yz + 6z^2)
({[1 1 2] 16, [0 0 0] 5} {[2 0 2] 8, [0 3 0] 4} {[2 1 1] 16, [0 0 2] 6})
```

Laplacian:

```
=> (laplacian {[2 1 2] 8, [0 4 0] 1, [0 0 3] 2, [1 0 0] 5})
;; 16yz^2 + 12(x^2)y + 12y^2 + 12z
{[2 1 0] 16, [0 1 2] 16, [0 2 0] 12, [0 0 1] 12}
=> (= (laplacian {[2 1 2] 8, [0 4 0] 1, [0 0 3] 2, [1 0 0] 5})
      (div (grad {[2 1 2] 8, [0 4 0] 1, [0 0 3] 2, [1 0 0] 5})))
true
```

Divergence (in Cartesian coordinates):

```
;; f(x,y,z) = 5(x^4)(y^3)(z^3) + 8(x^2)y(z^2) + y^4
=> (div [{[4 3 3] 5} {[2 1 2] 8} {[0 4 0] 1}])
;; 20(x^3)(y^3)(z^3) + 8(x^2)(z^2)
{[3 3 3] 20, [2 0 2] 8}
```

Curl (in Cartesian coordinates):

```
=> (curl [{[4 3 3] 5} {[2 1 2] 8} {[0 4 0] 1}])
;; (- 16xy(z^2) + 4y^3, 15(x^4)(y^3)(z^2), - 15(x^4)(y^2)(z^3) + 16(x^2)yz)
({[2 1 1] -16, [0 3 0] 4} {[4 3 2] 15} {[4 2 3] -15, [1 1 2] 16})
```

[The boundary of a boundary is zero:](http://cqi.inf.usi.ch/qic/wheeler.pdf)

```
=> (div (curl [{[4 3 3] 5} {[2 1 2] 8} {[0 4 0] 1}]))
{}
=> (curl (grad {[2 1 2] 8, [0 4 0] 1, [0 0 3] 2, [1 0 0] 5}))
({} {} {})
```

Taylor Series:

```
=> (dense-to-sparse (take 10 (exp-series)))
{[9] 1/362880, [8] 1/40320, [7] 1/5040, [6] 1/720, [5] 1/120, [4] 1/24, [3] 1/6, [2] 1/2, [1] 1, [0] 1}

=> (dense-to-sparse (take 10 (sin-series)))
{[9] 1/362880, [7] -1/5040, [5] 1/120, [3] -1/6, [1] 1}

=> (dense-to-sparse (take 10 (cos-series)))
{[8] 1/40320, [6] -1/720, [4] 1/24, [2] -1/2, [0] 1}

=> (dense-to-sparse (take 10 (atan-series)))
{[8] 1/9, [6] -1/7, [4] 1/5, [2] -1/3, [0] 1}

=> (dense-to-sparse (take 10 (sinh-series)))
{[9] 1/362880, [7] 1/5040, [5] 1/120, [3] 1/6, [1] 1}

=> (dense-to-sparse (take 10 (cosh-series)))
{[8] 1/40320, [6] 1/720, [4] 1/24, [2] 1/2, [0] 1}
```

Printing to a text file:

```
=> (print-tape "my_derivatives" (diff {[1 1] 2, [1 0] 3, [0 1] 5, [0 0] 7} 2))
```
