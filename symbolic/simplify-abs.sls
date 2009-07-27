
(library (numero symbolic simplify-abs)

  (export simplify-abs)

  (import (rnrs)
          (only (srfi :43) vector-fold)
          (xitomatl AS-match))

  (define (simplify-abs expr)

    (match expr

      ( ('abs (? vector? v))

        `(sqrt
          ,(vector-fold (lambda (i a b) `(+ ,a ,b))
                        0
                        (vector-map (lambda (elt) `(^ ,elt 2)) v))) )

      ( ('abs (? number? n))

        (abs n) )

      ( (? list?) (map simplify-abs expr) )

      ( else expr )))

  )