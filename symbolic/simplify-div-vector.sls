
(library (numero symbolic simplify-div-vector)

  (export simplify-div-vector)

  (import (rnrs)
          (xitomatl AS-match)
          (numero symbolic simplify-parameters))

  (define (simplify-div-vector expr)

    (match expr

      (('/ (? vector? a) (? vector? b))

       (vector-map (lambda (elt-a elt-b) `(/ ,elt-a ,elt-b))
                   a
                   b))

      (('/ (? vector? a) (? number? n))
       (vector-map (lambda (elt-a) `(/ ,elt-a ,n)) a))

      (('/ (? number? n) (? vector? a))
       (vector-map (lambda (elt-a) `(/ ,n ,elt-a)) a))

      (('/ (? vector? a) x)
       (vector-map (lambda (elt-a) `(/ ,elt-a ,x)) a))

      (else expr)))

  )