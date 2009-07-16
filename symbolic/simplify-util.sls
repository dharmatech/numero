
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

  (import (rnrs) (xitomatl AS-match))

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

  (define (like-terms? a b)
    (or (equal? (strip-coefficient a)
                (strip-coefficient b))
        (and (number? a)
             (number? b))))

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