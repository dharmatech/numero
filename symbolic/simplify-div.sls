
(library (numero symbolic simplify-div)

  (export simplify-div)

  (import (rnrs)
          (xitomatl AS-match)
          (numero symbolic simplify-parameters))

  (define (simplify-div expr)
    (match expr
      ( ('/ (? number? a) (? number? b))
        (let ((val (/ a b)))
          (if (simplify-exact?)
              (if (exact? val) val expr)
              val)) )
      ( ('/ 0 x) 0 )
      ( ('/ x 1) x )

      ( ('/ a b)

        `(* ,a (^ ,b -1)) )
      
      ( else expr )))

  )