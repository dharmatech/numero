
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
  (infix '(a + b + 2 * a))))

"3 * a + b"

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; derivative
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(to-alg
 (simplify
  (D (infix '( 7 x ^ 5 - 3 x ^ 4 + 6 x ^ 2 + 3 x + 4 )) 'x)))

"35 * x ^ 4 + -12 * x ^ 3 + 12 * x + 3"

(to-alg
 (simplify
  (D (infix '( ( 3 x - 2 ) / ( x ^ 2 + 7 ) )) 'x)))

"(3 * x ^ 2 + 21 - 2 * x * (3 * x + -2)) / (x ^ 2 + 7) ^ 2"

(to-alg
 (simplify
  (D (infix '( (3 x - 2) / (2 x + 5) )) 'x)))

"(6 * x + 15 + -6 * x + 4) / (2 * x + 5) ^ 2"

;; Example from Mathematica reference manual

(to-alg
 (simplify
  (expand-loop
   (simplify
    (expand-loop
     (D (D (D (D (infix '(sin(x) ^ 10)) 'x) 'x) 'x) 'x))))))

"-4680 * (cos x) ^ 2 * (sin x) ^ 8 + 5040 * (cos x) ^ 4 * (sin x) ^ 6 + 280 * (sin x) ^ 10"

