
(library (numero symbolic simplify-parameters)

  (export simplify-exact?)

  (import (rnrs)
          (srfi :39)
          )

  (define simplify-exact? (make-parameter #t))

  )
