
(library (numero symbolic simplify-remove-sub)

  (export simplify-remove-sub)

  (import (rnrs) (xitomatl AS-match))
  
  (define (simplify-remove-sub expr)
    (match expr
      (('- a b) `(+ ,a (- ,b)))
      (('- a) `(* -1 ,a))
      ((? list?) (map simplify-remove-sub expr))
      (else expr)))

  )