
(library (numero symbolic simplify-dot)

  (export simplify-dot)

  (import (rnrs)
          (only (srfi :43) vector-fold)
          (xitomatl AS-match))

  (define (simplify-dot expr)
    (match expr
      (('dot (? vector? u) (? vector? v))
        (vector-fold (lambda (i x y) `(+ ,x ,y))
                     0
                     (vector-map (lambda (a b) `(* ,a ,b))
                                 u
                                 v)))
      (else expr)))

  )