
(library (numero symbolic simplify-neg)

  (export simplify-neg)

  (import (rnrs)
          (xitomatl AS-match))

  (define (simplify-neg expr)
    (match expr

      (('- (? number? x)) (- x))

      ;; -(-x)   x

      ( ('- ('- x)) x )

      ;; ( ('- x) `(* -1 ,x) )

      ;; (- (* 5 (* x y)))	(* -5 (* x y))

      ((and ('- ('* a ('* b c)))
            (? (lambda (_) (number? a))))
       `(* ,(- a) (* ,b ,c)))
      
      (else expr)))
  
  )