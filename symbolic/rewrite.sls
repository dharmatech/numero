
(library (numero symbolic rewrite)

  (export rewrite)

  (import (rnrs))

  (define (rewrite rule old)
    (let ((new (rule old)))
      (if (equal? old new)
          old
          (rewrite rule new))))

  )