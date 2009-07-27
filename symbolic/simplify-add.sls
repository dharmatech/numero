
(library (numero symbolic simplify-add)

  (export simplify-add)

  (import (rnrs) (xitomatl AS-match)
          (numero symbolic simplify-parameters)
          )

  (define (simplify-add expr)

    (match expr

      (('+ (? number? a) (? number? b))
       (let ((val (+ a b)))
         (if (simplify-exact?)
             (if (exact? val) val expr)
             val)))

      ((or ('+ x 0) ('+ 0 x)) x)

      (else expr)))

  )