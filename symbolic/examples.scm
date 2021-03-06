
(import (numero symbolic infix)
        (numero symbolic alg)
        (numero symbolic to-alg)
        (numero symbolic rewrite)
        (numero symbolic simplify)
        (numero symbolic simplify-parameters)
        (numero symbolic derivative)
        (numero symbolic expand)
        (numero symbolic solve-for)
        (numero symbolic subst)

        (numero symbolic simplify-util)

        (srfi :64))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(simplify '(+ #(1 2) #(3 4)))

(simplify '(+ #(1 2) 3))

(simplify '(+ 3 #(1 2)))


(simplify '(- #(3 4) #(1 2)))

(simplify '(- #(3 4) 2))

(simplify '(- 5 #(3 4)))


(simplify '(* #(1 2) #(3 4)))

(simplify '(* #(2 3) 4))

(simplify '(* 4 #(2 3)))


(simplify '(/ #(4 5) #(2 3)))

(simplify '(/ #(4 5) 2))

(simplify '(/ 2 #(4 5)))


(simplify '(norm #(1 1)))

(simplify '(norm #(2 0)))

(simplify '(norm #(0 2)))


(simplify '(dot #(1 2 3) #(-18 21 -8)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; simplify
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(to-alg
 (simplify
  (alg "a + b + 2 a")))

"3 * a + b"

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; derivative
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(to-alg
 (simplify
  (D (alg "7 x^5 - 3 x^4 + 6 x^2 + 3 x + 4") 'x)))

"3 + 35 * x ^ 4 - 12 * x ^ 3 + 12 * x"

(to-alg
 (simplify
  (D (alg "( 3 x - 2 ) / ( x^2 + 7 )") 'x)))

"(3 * x ^ 2 + 21 - 2 * x * (3 * x - 2)) / (x ^ 2 + 7) ^ 2"

(to-alg
 (simplify
  (D (alg "(3 x - 2) / (2 x + 5)") 'x)))

"19 / (2 * x + 5) ^ 2"

;; Example from Mathematica reference manual

(to-alg
 (simplify
  (expand
   (simplify
    (D (D (D (D (alg "sin(x) ^ 10") 'x) 'x) 'x) 'x)))))

"-4680 * (cos x) ^ 2 * (sin x) ^ 8 + 280 * (sin x) ^ 10 + 5040 * (cos x) ^ 4 * (sin x) ^ 6"

;; 8.41 Mendelson

(to-alg
 (simplify
  (expand
   (simplify
    (D (alg "x * (2 x - 1) * (x + 2)") 'x)))))

"6 * x ^ 2 + 6 * x - 2"

;; 8.49 Mendelson

(to-alg
 (simplify
  (expand
   (D (alg "(x^2 - 3) / (x+4)") 'x))))

"(3 + x ^ 2 + 8 * x) / (8 * x + x ^ 2 + 16)"

;; 8.50 Mendelson

(to-alg
 (simplify
  (D (alg "(x^5 - x + 2) / (x^3 + 7)") 'x)))

"  ((5 * x ^ 4 - 1) * (x ^ 3 + 7) - 3 * x ^ 2 * (x ^ 5 - x + 2)) /
   (x ^ 3 + 7) ^ 2  "

;; 8.51 Mendelson

(to-alg
 (simplify
  (expand
   (simplify
    (D (alg "(3 x^7 + x^5 - 2 x^4 + x - 3) / x^4 ") 'x)))))

"-3 / x ^ 4 + 9 * x ^ 2 + 1 + 12 / x ^ 5"

;; 9.7

(to-alg
 (simplify
  (D (alg "1 / (3 x^2 + 5)^4") 'x)))

"-24 * x / (3 * x ^ 2 + 5) ^ 5"

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(to-alg
 (simplify
  (expand
   (alg "(a-b)^3"))))

"-3 * a ^ 2 * b + 3 * a * b ^ 2 + a ^ 3 - b ^ 3"

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; solve-for
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(solve-for (alg "2 x = 6") 'x)

(= x (/ 6 2))

(solve-for (alg "a b c / d = e") 'c)

(= c (/ (* d e) (* a b)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Calculus - Gilbert Strang
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 4.2 Example 1 (implicit differentiation)

;; y^5 + x y = 3

;; Find dy/dx at x = 2 and y = 1

;; Define the equation:

(define eq-1 (infix '( y(x) ^ 5 + x y(x) = 3 )))

(define eq-1 (alg "y(x)^5 + x y(x) = 3 "))

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

(define f (alg "x^2 y + y^3"))

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

(define f (alg "sin( x y^2 ) / (x^2 + 1) "))

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

(define f (alg "e^(x^2 y) + x y^3 )"))

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
;; 2.3 Tangent Plane to a Surface
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Example 2.13

;; Find the equation of the tangent plane to the surface z = x^2 + y^2 at the
;; point (1, 2, 5).

(define f (alg "x^2 + y^2"))

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
  (expand
   (infix
    `( ,(df/dx a b) * (x - ,a) + ,(df/dy a b) * (y - ,b) - z + ,c = 0 )))))

"-5 + 2 * x + 4 * y - z = 0"

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Example 2.14

;; Find the equation of the tangent plane to the surface x^2 + y^2 + z^2 = 9
;; at the point (2, 2, -1).

(define F (alg "x^2 + y^2 + z^2 - 9)"))

(define (dF/dx x y z)
  (simplify
   (subst (D F 'x) (x x) (y y) (z z))))

(define (dF/dy x y z)
  (simplify
   (subst (D F 'y) (x x) (y y) (z z))))

(define (dF/dz x y z)
  (simplify
   (subst (D F 'z) (x x) (y y) (z z))))

(define a  2)
(define b  2)
(define c -1)

(to-alg
 (simplify
  (expand
   (simplify
    (infix `( ,(dF/dx a b c) * (x - ,a)
              +
              ,(dF/dy a b c) * (y - ,b)
              +
              ,(dF/dz a b c) * (z - ,c)
              = 0 ))))))

"-18 + 4 * x + 4 * y - 2 * z = 0"

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 2.3 Exercises
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (define (tangent-plane F)

;;   (define (dF/dx x y z)
;;     (simplify
;;      (subst (D F 'x) (x x) (y y) (z z))))

;;   (define (dF/dy x y z)
;;     (simplify
;;      (subst (D F 'y) (x x) (y y) (z z))))

;;   (define (dF/dz x y z)
;;     (simplify
;;      (subst (D F 'z) (x x) (y y) (z z))))

;;   (lambda (a b c)

;;     (simplify

;;      (expand

;;       (infix `( ,(dF/dx a b c) * (x - ,a)
;;                 +
;;                 ,(dF/dy a b c) * (y - ,b)
;;                 +
;;                 ,(dF/dz a b c) * (z - ,c)
;;                 = 0 ))))))

(define (tangent-plane F)

  (define (dF/dx x y z)
    (subst (D F 'x) (x x) (y y) (z z)))

  (define (dF/dy x y z)
    (subst (D F 'y) (x x) (y y) (z z)))

  (define (dF/dz x y z)
    (subst (D F 'z) (x x) (y y) (z z)))

  (define formula
    (infix
     '( dF/dx * (x - a) + dF/dy * (y - b) + dF/dz * (z - c) = 0 )))

  (lambda (a b c)

    (simplify

     (expand

      (subst formula
             (dF/dx (dF/dx a b c))
             (dF/dy (dF/dy a b c))
             (dF/dz (dF/dz a b c))
             (a a)
             (b b)
             (c c))))))

;; 1

(let ((F (infix '( x ^ 2 + y ^ 3 - z ))))

  (to-alg

   ((tangent-plane F) 1 1 2)))

"-3 + 2 * x + 3 * y - z = 0"

;; 3

(let ((F (infix '( x ^ 2 y - z ))))

  (to-alg

   ((tangent-plane F) -1 1 1)))

"-2 * x - 2 + y - z = 0"

;; 5

(let ((F (infix '( x + 2 y - z ))))

  (to-alg

   ((tangent-plane F) 2 1 4)))

"x + 2 * y - z = 0"

;; 7

(parameterize ((simplify-exact? #f))

  (let ((F (infix '( x ^ 2 / 4 + y ^ 2 / 9 + z ^ 2 / 16 - 1 ))))

    (to-alg

     ((tangent-plane F) 1 2 (/ (* 2 (sqrt 11)) 3)))))

"-2.0 + 1/2 * x + 4/9 * y + 0.2763853991962833 * z = 0"

;; 9

(let ((F (infix '( x ^ 2 + y ^ 2 - z ^ 2 ))))

  (to-alg

   ((tangent-plane F) 3 4 5)))

"6 * x + 8 * y - 10 * z = 0"

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

