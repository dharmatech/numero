
(library (numero symbolic simplify-pow)

  (export simplify-pow)

  (import (rnrs) (xitomatl AS-match))

  (define (simplify-pow expr)
    (match expr
      (('^ a 0) 1)
      (('^ a 1) a)

      ( ('^ (? number? a) (? number? b)) (expt a b) )

      ;; (a^b)^c    a^(b*c)
      (('^ ('^ a (? number? b)) (? number? c)) `(^ ,a ,(* b c)))

      (('^ a b) `(^ ,(simplify-pow a) ,(simplify-pow b)))
      ((? list?) (map simplify-pow expr))
      (else expr)))

  )