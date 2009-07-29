
(library (numero symbolic simplify-util)

  (export base
          var-term?
          symbol<?
          fun-name?
          fun-term?
          fun-name
          coefficient
          strip-coefficient
          like-terms?
          contains-like-term?
          exponent)

  (import (rnrs)
          (xitomatl AS-match)
          (numero symbolic simplify-parameters)
          )

  (define (coefficient expr)
    (match expr
      ( (? number?) expr )
      ( ('* (? number? a) b) a )
      ( else 1 )))

  (define (strip-coefficient expr)
    (match expr
      ( (? number?) 1 )
      ( ('* (? number?) b) b )
      ( else expr )))

  (define (strip-pos-neg expr)
    (match expr
      (('- x) x)
      (('+ x) x)
      (else expr)))

  ;; (define (like-terms? a b)
  ;;   (if (and (number? a)
  ;;            (number? b))
  ;;       (if (simplify-exact?)
  ;;           (and (exact? a)
  ;;                (exact? b))
  ;;           #t)
  ;;       (equal? (strip-coefficient a)
  ;;               (strip-coefficient b))))

  (define (like-terms? a b)
    (equal? (strip-coefficient a)
            (strip-coefficient b)))

  (define (contains-like-term? expr term)
    (or (like-terms? expr term)
        (match expr
          ( ((or '+ '-) a b)
            (or (contains-like-term? a term)
                (contains-like-term? b term)) )
          ( else #f ))))
  
  (define (base expr)
    (match expr
      ( ('^ a b) (base a) )
      ( else expr )))

  (define (exponent expr)
    (match expr

      ;; (a ^ b) ^ c    a ^ (b * c)

      ( ('^ ('^ a b) c)

        (exponent `(^ ,a (* ,b ,c))))
      
      ( ('^ a b) b )
      ( else 1 )))
  
  (define (var-term? expr)
    (symbol? (base expr)))

  (define (symbol<? a b)
    (string<? (symbol->string a)
              (symbol->string b)))

  (define (fun-name? expr)
    (and (symbol? expr)
         (char-alphabetic?
          (string-ref (symbol->string expr) 0))))

  (define (fun-term? expr)
    (let ((expr (base expr)))
      (and (list? expr)
           (>= (length expr) 1)
           (fun-name? (car expr)))))

  (define (fun-name expr)
    (car (base expr)))

  )