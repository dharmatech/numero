
(import (numero symbolic infix)
        (numero symbolic to-alg)
        (numero symbolic simplify)
        (numero symbolic derivative)
        (numero symbolic expand))

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
  (D (infix '( (3 x - 2) / (2 x + 5) )) 'x)))

"19 / (2 * x + 5) ^ 2"

;; Example from Mathematica reference manual

(to-alg
 (simplify
  (expand-loop
   (simplify
    (expand-loop
     (D (D (D (D (infix '(sin(x) ^ 10)) 'x) 'x) 'x) 'x))))))

"-4680 * (cos x) ^ 2 * (sin x) ^ 8 + 5040 * (cos x) ^ 4 * (sin x) ^ 6 + 280 * (sin x) ^ 10"

;; 8.41 Mendelson

(to-alg
 (simplify
  (expand-loop
   (D (infix '(x * (2 x - 1) * (x + 2))) 'x))))

"6 * x ^ 2 + 6 * x - 2"

;; 8.49 Mendelson

(to-alg
 (simplify
  (expand-loop
   (D (infix '( (x ^ 2 - 3) / (x + 4) )) 'x))))

"(x ^ 2 + 8 * x + 3) / (8 * x + x ^ 2 + 16)"

;; 8.50 Mendelson

(to-alg
 (simplify
  (D (infix '( (x ^ 5 - x + 2) / (x ^ 3 + 7) )) 'x)))

"((5 * x ^ 4 - 1) * (x ^ 3 + 7) - 3 * x ^ 2 * (x ^ 5 - 1 * x + 2)) / (x ^ 3 + 7) ^ 2"

;; 8.51 Mendelson

(to-alg
 (simplify
  (expand-loop
   (simplify
    (expand-loop
     (D (infix '( (3 x ^ 7 + x ^ 5 - 2 x ^ 4 + x - 3) / x ^ 4 )) 'x))))))

"9 * x ^ 2 + 1 - 3 * x ^ -4 + 12 * x ^ -5"