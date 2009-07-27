
(library (numero symbolic simplify-add-canonical)

  (export simplify-add-canonical)

  (import (rnrs)
          (xitomatl AS-match))

  (define (simplify-add-canonical expr)
    (match expr
      ;; (a + (b + c))	((a + b) + c)
      ( ('+ a ('+ b c)) `(+ (+ ,a ,b) ,c) )
      ;; (a + (b - c))  ((a + b) - c)
      ( ('+ a ('- b c)) `(- (+ ,a ,b) ,c) )
      (else expr)))

  )
