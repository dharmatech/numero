
(library (numero symbolic simplify-pretty)

  (export simplify-pretty)

  (import (rnrs)
          (xitomatl AS-match))

  (define (negative-number? x)
    (and (number? x)
         (negative? x)))

  (define (simplify-pretty expr)

    (match expr

      ( ('* -1 x) `(- ,x) )

      ( ('+ a ('* (? negative-number? b) c)) `(- ,a (* ,(- b) ,c)) )

      ( ('+ a (? negative-number? b)) `(- ,a ,(- b)) )

      ((? list?) (map simplify-pretty expr))

      (else expr)))

  )