
(library (numero symbolic simplify-add-vector)

  (export simplify-add-vector)

  (import (rnrs)
          (xitomatl AS-match)
          (numero symbolic simplify-parameters))

  (define (simplify-add-vector expr)

    (match expr

      (('+ (? vector? a) (? vector? b))

       (vector-map (lambda (elt-a elt-b) `(+ ,elt-a ,elt-b))
                   a
                   b))

      ((or ('+ (? vector? a) (? number? n))
           ('+ (? number? n) (? vector? a)))

       (vector-map (lambda (elt-a) `(+ ,elt-a ,n)) a))

      (else expr)))

  )