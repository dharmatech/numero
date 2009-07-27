
(library (numero symbolic simplify-pow-vector)

  (export simplify-pow-vector)

  (import (rnrs)
          (xitomatl AS-match)
          (numero symbolic simplify-parameters))

  (define (simplify-pow-vector expr)
    (match expr
      (('^ (? vector? v) x)
       (vector-map (lambda (elt) `(^ ,elt ,x)) v))
      (else expr)))

  )