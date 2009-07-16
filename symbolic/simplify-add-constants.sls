
(library (numero symbolic simplify-add-constants)

  (export simplify-add-constants)

  (import (rnrs) (xitomatl AS-match))

  (define (simplify-add-constants expr)
    (match expr
      (('+ (? number? a) (? number? b)) (+ a b))
      ((or ('+ x 0) ('+ 0 x)) x)
      ((? list?) (map simplify-add-constants expr))
      (else expr)))

  )