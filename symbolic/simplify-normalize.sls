
(library (numero symbolic simplify-normalize)

  (export simplify-normalize)

  (import (rnrs) (xitomatl AS-match))

  (define (simplify-normalize expr)
    (match expr
      (('normalize (? vector? v))
       `(/ ,v (norm ,v)))
      (else expr)))

  )