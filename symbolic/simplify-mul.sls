
(library (numero symbolic simplify-mul)

  (export simplify-mul)

  (import (rnrs)
          (xitomatl AS-match)
          (numero symbolic simplify-parameters))

  (define (simplify-mul expr)
    
    (match expr

      (('* (? number? a) (? number? b))
       (let ((val (* a b)))
         (if (simplify-exact?)
             (if (exact? val) val expr)
             val)))
      ( ('* 0 x) 0 )
      ( ('* 1 x) x )
      (else expr)))

  )