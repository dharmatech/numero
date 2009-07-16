
(library (numero symbolic simplify-mul-constants)

  (export simplify-mul-constants)

  (import (rnrs) (xitomatl AS-match))

  (define (simplify-mul-constants expr)
    (match expr
      ( ('* (? number? a) (? number? b)) (* a b) )
      ( ('* (? number? a) ('* (? number? b) c)) `(* ,(* a b) ,c) )
      ( ('* 0 b) 0 )
      ( ('* 1 b) b )
      ((? list?) (map simplify-mul-constants expr))
      (else expr)))

  )