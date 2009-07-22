
(import (numero symbolic infix)
        (numero symbolic to-alg)
        (numero symbolic simplify)
        (numero symbolic derivative)
        (numero symbolic expand)
        (numero symbolic solve-for)
        (numero symbolic subst))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; simplify
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(to-alg
 (simplify
  (infix '(a + b + 2 a))))

"3 * a + b"

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; derivative
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(to-alg
 (simplify
  (D (infix '( 7 x ^ 5 - 3 x ^ 4 + 6 x ^ 2 + 3 x + 4 )) 'x)))

"3 + 35 * x ^ 4 - 12 * x ^ 3 + 12 * x"

(to-alg
 (simplify
  (D (infix '( ( 3 x - 2 ) / ( x ^ 2 + 7 ) )) 'x)))

"(3 * x ^ 2 + 21 - 2 * x * (3 * x - 2)) / (x ^ 2 + 7) ^ 2"

(to-alg
 (simplify
  (simplify
   (D (infix '( (3 x - 2) / (2 x + 5) )) 'x))))

"19 / (2 * x + 5) ^ 2"

;; Example from Mathematica reference manual

(to-alg
 (simplify
  (expand
   (simplify
    (D (D (D (D (infix '(sin(x) ^ 10)) 'x) 'x) 'x) 'x)))))

"-4680 * (cos x) ^ 2 * (sin x) ^ 8 + 280 * (sin x) ^ 10 + 5040 * (cos x) ^ 4 * (sin x) ^ 6"

;; 8.41 Mendelson

(to-alg
 (simplify
  (expand
   (simplify
    (D (infix '(x * (2 x - 1) * (x + 2))) 'x)))))

"6 * x ^ 2 + 6 * x - 2"

;; 8.49 Mendelson

(to-alg
 (simplify
  (expand
   (D (infix '( (x ^ 2 - 3) / (x + 4) )) 'x))))

"(3 + x ^ 2 + 8 * x) / (8 * x + x ^ 2 + 16)"

;; 8.50 Mendelson

(to-alg
 (simplify
  (D (infix '( (x ^ 5 - x + 2) / (x ^ 3 + 7) )) 'x)))

"  ((5 * x ^ 4 - 1) * (x ^ 3 + 7) - 3 * x ^ 2 * (x ^ 5 - x + 2)) /
   (x ^ 3 + 7) ^ 2  "

;; 8.51 Mendelson

(to-alg
 (simplify
  (simplify
   (expand
    (simplify
     (D (infix '( (3 x ^ 7 + x ^ 5 - 2 x ^ 4 + x - 3) / x ^ 4 )) 'x))))))

"-3 / x ^ 4 + 9 * x ^ 2 + 1 + 12 / x ^ 5"

;; 9.7

(to-alg
 (simplify
  (D (infix '(1 / (3 x ^ 2 + 5) ^ 4)) 'x)))

"-24 * x / (3 * x ^ 2 + 5) ^ 5"

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(to-alg
 (simplify
  (simplify
   (expand
    (infix '((a - b) ^ 3))))))

"-3 * a ^ 2 * b + 3 * a * b ^ 2 + a ^ 3 - b ^ 3"

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; solve-for
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(solve-for (infix '(2 x = 6)) 'x)

(= x (/ 6 2))

(solve-for (infix '(a b c / d = e)) 'c)

(= c (/ (* d e) (* a b)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Calculus - Gilbert Strang
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 4.2 Example 1 (implicit differentiation)

;; y^5 + x y = 3

;; Find dy/dx at x = 2 and y = 1

;; Define the equation:

(define eq-1 (infix '( y(x) ^ 5 + x y(x) = 3 )))

;; Take the derivative of the whole equation:

(set! eq-2 (simplify (D eq-1 'x)))

(to-alg eq-2)

"5 * (D (y x) x) * (y x) ^ 4 + (y x) + x * (D (y x) x) = 0"

;; Let's use 'subst' to make it prettier:

(set! eq-3
      (subst eq-2
             ((D (y x) x) 'dy/dx)
             ((y x) 'y)))

(to-alg eq-3)

"5 * dy/dx * y ^ 4 + y + x * dy/dx = 0"

;; Plug in x=2 and y=1 :

(set! eq-4
      (simplify
       (subst eq-3
              (x 2)
              (y 1))))

(to-alg eq-4)

"7 * dy/dx + 1 = 0"

;; Solve for dy/dx:

(to-alg
 (simplify
  (solve-for eq-4 'dy/dx)))

"dy/dx = -1/7"

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Vector Calculus by Michael Corral
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Example 2.10

(define f (infix '(x ^ 2 y + y ^ 3)))

(to-alg
 (simplify
  (D f 'x)))

"2 * x * y"

(to-alg
 (simplify
  (D f 'y)))

"x ^ 2 + 3 * y ^ 2"

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Example 2.11

(define f (infix '( sin( x y ^ 2 ) / (x ^ 2 + 1) )))

(to-alg
 (simplify
  (D f 'x)))

"(y ^ 2 * cos (x * y ^ 2) * (x ^ 2 + 1) - 2 * x * sin (x * y ^ 2)) / (x ^ 2 + 1) ^ 2"

(to-alg
 (simplify
  (D f 'y)))

"2 * x * y * cos (x * y ^ 2) / (x ^ 2 + 1)"

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Example 2.12

(define f (infix '( e ^ (x ^ 2 y) + x y ^ 3 )))

(to-alg
 (simplify
  (D f 'x)))

"2 * e ^ (x ^ 2 * y) * x * y + y ^ 3"

(to-alg
 (simplify
  (D f 'y)))

"e ^ (x ^ 2 * y) * x ^ 2 + 3 * x * y ^ 2"

(to-alg
 (simplify
  (D (D f 'x) 'x)))

"e ^ (x ^ 2 * y) * (2 * x * y) ^ 2 + 2 * e ^ (x ^ 2 * y) * y"

(to-alg
 (simplify
  (D (D f 'y) 'y)))

"e ^ (x ^ 2 * y) * x ^ 4 + 6 * x * y"

(to-alg
 (simplify
  (D (D f 'x) 'y)))

"3 * y ^ 2 + 2 * e ^ (x ^ 2 * y) * x ^ 3 * y + 2 * e ^ (x ^ 2 * y) * x"

(to-alg
 (simplify
  (D (D f 'y) 'x)))

"3 * y ^ 2 + 2 * e ^ (x ^ 2 * y) * x ^ 3 * y + 2 * e ^ (x ^ 2 * y) * x"

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Example 2.13

;; Find the equation of the tangent plane to the surface z = x^2 + y^2 at the
;; point (1, 2, 5).

(define f (infix '(x ^ 2 + y ^ 2)))

(define (df/dx x y)
  (simplify
   (subst (D f 'x) (x x) (y y))))

(define (df/dy x y)
  (simplify
   (subst (D f 'y) (x x) (y y))))

(define a 1)
(define b 2)
(define c (simplify (subst f (x 1) (y 2))))

(to-alg
 (simplify
  (simplify
   (expand
    (infix
     `( ,(df/dx a b) * (x - ,a) + ,(df/dy a b) * (y - ,b) - z + ,c = 0 ))))))

"-5 + 2 * x + 4 * y - z = 0"

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

