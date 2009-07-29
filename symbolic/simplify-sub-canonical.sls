
(library (numero symbolic simplify-sub-canonical)

  (export simplify-sub-canonical)

  (import (rnrs)
          (xitomatl AS-match))

  (define (simplify-sub-canonical expr)
    (match expr
      ;; (a - (b - c))	(a - b) + c
      ( ('- a ('- b c)) `(+ (- ,a ,b) ,c) )
      ;; (a - (b + c))	(a - b) - c
      ( ('- a ('+ b c)) `(- (- ,a ,b) ,c) )

      ;; a - (- b)	a + b

      (('- a ('- b))
       `(+ ,a ,b))
      
      (else expr)))

  )
