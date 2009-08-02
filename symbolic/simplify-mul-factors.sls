
(library (numero symbolic simplify-mul-factors)

  (export simplify-mul-factors)

  (import (rnrs)
          (xitomatl AS-match)
          (numero symbolic simplify-util))

  (define (simplify-mul-factors expr)
    (match expr
      ;; combine like factors (a^2 * a^3) => a^5
      ( (and ('* a b)
             (? (lambda (_) (equal? (base a) (base b)))))
        `(^ ,(base a) (+ ,(exponent a) ,(exponent b))) )
      ;; (a * (a * b))	a ^ 2 * b
      ( (and ('* a ('* b c))
             (? (lambda (_) (equal? (base a) (base b)))))
        `(* (^ ,(base a) (+ ,(exponent a) ,(exponent b))) ,c) )
      ;; a * (b + c)    where a and c are numbers
      ( ('* (? number? a) ('+ b (? number? c)))
        `(+ (* ,a ,b) ,(* a c)) )

      ;; a * (b - c)    where a and c are numbers
      ((and ('* a ('- b c))
            (? (lambda (_) (and (number? a)
                                (number? c)))))
       
       `(- (* ,a ,b) (* ,a ,c)))

      ;; a * (b * c)   where a and b are numbers

      ( ('* (? number? a) ('* (? number? b) c))
        `(* ,(* a b) ,c) )
      
      (else expr)))

  )