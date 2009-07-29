
(library (numero symbolic simplify)

  (export simplify simplify-step simplify-pre simplify-post)

  (import (rnrs)
          (xitomatl AS-match)

          (numero symbolic rewrite)

          (numero symbolic simplify-add-canonical)
          (numero symbolic simplify-sub-canonical)
          (numero symbolic simplify-mul-canonical)
          
          (numero symbolic simplify-add)
          (numero symbolic simplify-sub)
          (numero symbolic simplify-mul)
          (numero symbolic simplify-div)

          (numero symbolic simplify-neg)

          (numero symbolic simplify-add-like-terms)
          (numero symbolic simplify-sub-like-terms)

          (numero symbolic simplify-mul-factors)

          (numero symbolic simplify-remove-sub)

          (numero symbolic simplify-pow)

          (numero symbolic simplify-sqrt)

          (numero symbolic simplify-add-vector)
          (numero symbolic simplify-sub-vector)
          (numero symbolic simplify-mul-vector)
          (numero symbolic simplify-div-vector)

          (numero symbolic simplify-pow-vector)

          (numero symbolic simplify-norm)
          (numero symbolic simplify-dot)
          (numero symbolic simplify-normalize)
          
          ;; (numero symbolic simplify-pretty)
          
          ;; (numero symbolic simplify-abs)

          )

  (define-syntax compose-in-order
    (syntax-rules ()
      ( (compose g) g )
      ( (compose g f ...)
        (lambda (x)
          ((compose-in-order f ...) (g x))))))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define simplify-step

    (compose-in-order simplify-add-canonical
                      simplify-sub-canonical
                      simplify-mul-canonical

                      simplify-add
                      simplify-sub
                      simplify-mul
                      simplify-div

                      simplify-neg

                      simplify-add-like-terms
                      simplify-sub-like-terms

                      simplify-mul-factors

                      simplify-pow

                      simplify-sqrt

                      simplify-add-vector
                      simplify-sub-vector
                      simplify-mul-vector
                      simplify-div-vector

                      simplify-pow-vector

                      simplify-norm
                      simplify-dot
                      simplify-normalize

                      ))

  (define simplify-step-pre  (compose-in-order simplify-step simplify-from-div))
  (define simplify-step-post (compose-in-order simplify-step simplify-to-div))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define (simplify-pre expr)

    (let ((expr (cond ((list?   expr) (map        simplify-pre expr))
                      ((vector? expr) (vector-map simplify-pre expr))
                      (else expr))))

      (simplify-step-pre expr)))

  (define (simplify-post expr)

    (let ((expr (cond ((list?   expr) (map        simplify-post expr))
                      ((vector? expr) (vector-map simplify-post expr))
                      (else expr))))

      (simplify-step-post expr)))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define (simplify expr)
    
    (let ((expr (rewrite simplify-pre expr)))

      (let ((expr (rewrite simplify-post expr)))

        expr)))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  )