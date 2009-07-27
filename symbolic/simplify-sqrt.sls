
(library (numero symbolic simplify-sqrt)

  (export simplify-sqrt)

  (import (rnrs)
          (xitomatl AS-match)
          (numero symbolic simplify-parameters))

  (define (simplify-sqrt expr)

    (match expr

      ( ('sqrt (? number? n))

        (let ((val (sqrt n)))

          (if (simplify-exact?)

              (if (exact? val) val expr)

              val)))

      ( (? list?) (map simplify-sqrt expr) )

      ( else expr )))

  )