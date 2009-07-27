
(library (numero symbolic simplify-sub)

  (export simplify-sub)

  (import (rnrs)
          (xitomatl AS-match)
          (numero symbolic simplify-parameters))

  (define (simplify-sub expr)

    (match expr

      (('- (? number? a) (? number? b))
       (let ((val (- a b)))
         (if (simplify-exact?)
             (if (exact? val) val expr)
             val)))

      (('- x 0) x)

      (('- 0 x) `(- ,x))

      (else expr)))

  )