
(library (numero symbolic simplify-norm)

  (export simplify-norm)

  (import (rnrs)
          (only (srfi :43) vector-fold)
          (xitomatl AS-match)
          (numero symbolic simplify-parameters))

  (define (simplify-norm expr)
    (match expr
      (('norm (? vector? v))
       `(sqrt
         ,(vector-fold (lambda (i x y) `(+ ,x ,y))
                       0
                       (vector-map (lambda (elt) `(^ ,elt 2))
                                   v))))
      (else expr)))

  )