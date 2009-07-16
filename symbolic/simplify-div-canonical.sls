
(library (numero symbolic simplify-div-canonical)

  (export simplify-div-canonical)

  (import (rnrs)
          (xitomatl AS-match))

  (define (simplify-div-canonical expr)

    (match expr

      ;; a / b    a b^-1

      ( ('/ a b) `(* ,a (^ ,b -1)) )

      ((? list?) (map simplify-div-canonical expr))

      (else expr)))

  )