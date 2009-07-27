
(library (numero symbolic simplify-pretty)

  (export simplify-pretty)

  (import (rnrs)
          (xitomatl AS-match))

  (define (negative-number? x)
    (and (number? x)
         (negative? x)))

  (define (simplify-pretty expr)

    (match expr

      ;; ( ('* -1 x) `(- ,x) )

      ;; ( ('+ a ('* (? negative-number? b) c)) `(- ,a (* ,(- b) ,c)) )

      ;; ( ('+ a (? negative-number? b)) `(- ,a ,(- b)) )

      ;; a * b ^ n    a / b ^ n

      ( (or ('* a ('^ b (? negative-number? n)))
            ('* ('^ b (? negative-number? n)) a))

        `(/ ,a (^ ,b ,(- n))) )

      (else expr)))

  )