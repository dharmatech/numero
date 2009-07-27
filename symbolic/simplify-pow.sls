
(library (numero symbolic simplify-pow)

  (export simplify-pow)

  (import (rnrs)
          (xitomatl AS-match)
          (numero symbolic simplify-parameters))

  (define (simplify-pow expr)
    (match expr
      (('^ a 0) 1)
      (('^ a 1) a)

      (('^ (? number? a) (? number? b))
       (let ((val (expt a b)))
         (if (simplify-exact?)
             (if (exact? val) val expr)
             val)))

      ;; (a^b)^c    a^(b*c)
      
      ( ('^ ('^ a (? number? b)) (? number? c))

        `(^ ,a ,(* b c)) )

      ;; (('^ a b) `(^ ,(simplify-pow a) ,(simplify-pow b)))

      ;; (a*b)^-1   a^-1 b^-1

      ( ('^ ('* a b) -1)

        `(* (^ ,a -1) (^ ,b -1)) )
      
      (else expr)))

  )