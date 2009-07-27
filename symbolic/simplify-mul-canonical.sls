
(library (numero symbolic simplify-mul-canonical)

  (export simplify-mul-canonical)

  (import (rnrs)
          (numero symbolic simplify-util)
          (xitomatl AS-match))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define (mul-element<? a b)

    (or (and (number? a)
             (not (number? b)))

        (and (var-term? a)
             (not (number? b))
             (not (var-term? b)))

        (and (var-term? a)
             (var-term? b)
             (symbol<? (base a)
                       (base b)))

        (and (fun-term? a)
             (fun-term? b)
             (symbol<? (fun-name a)
                       (fun-name b)))))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define (simplify-mul-canonical expr)
    (match expr
      (('* ('* a b) c) `(* ,a (* ,b ,c)))
      ((and ('* a b) (? (lambda (x) (mul-element<? b a)))) `(* ,b ,a))
      ((and ('* a ('* b c)) (? (lambda (x) (mul-element<? b a)))) `(* ,b (* ,a ,c)))
      (else expr)))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  )