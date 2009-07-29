
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

      ;; a + - b	a - b
      (('+ a ('- b))
       `(- ,a ,b))

      ;; a + (* -5 b)	a - (* 5 b)

      ((and ('+ a ('* b c))
            (? (lambda (_) (and (number? b)
                                (negative? b)))))
       `(- ,a (* ,(- b) ,c)))
      
      (else expr)))

  )
