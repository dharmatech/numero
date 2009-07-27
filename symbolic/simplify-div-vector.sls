
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

      ( ('/ (? vector? a) (? vector? b))

        (vector-map (lambda (elt-a elt-b) `(/ ,elt-a ,elt-b))
                    a
                    b) )

      ( ('/ (? vector? a) (? number? b))

        (vector-map (lambda (elt) `(/ ,elt ,b))
                    a) )

      ( (? list?) (map simplify-div expr) )

      ( else expr )))

  )