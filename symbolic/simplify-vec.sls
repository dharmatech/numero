
(library (numero symbolic simplify-vec)

  (export simplify-vec)

  (import (rnrs)
          (xitomatl AS-match))

  (define (simplify-vec expr)

    (match expr

      ( (? vector?) (vector-map simplify exp) )

      ( (? list?) (map simplify-vec expr) )

      ( else expr )))

  )