#lang typed/racket
(require (for-syntax racket/list))

(define-syntax (define-type-class stx)
  (syntax-case stx ()
    [(k name T [method : type] ...)
     (with-syntax ([define-name (datum->syntax
                                 #'k
                                 (string->symbol
                                  (string-append "define-"
                                                 (symbol->string (syntax->datum #'name)))))]
                   [with-name (datum->syntax
                               #'k
                               (string->symbol
                                (string-append "with-"
                                               (symbol->string (syntax->datum #'name)))))]
                   [define-name-library
                     (datum->syntax
                      #'k
                      (string->symbol
                       (string-append "define-" (symbol->string (syntax->datum #'name)) "-library")))]
                   [...2 #'(... ...)]
                   [...3 #'((... ...) (... ...))]
                   [(n ...) (range (length (syntax->list #'(method ...))))])
       #'(begin
           (define-syntax (define-name stx2)
             (syntax-case stx2 ()
               [(k2 name2 (T-imp A ...2) body body* ...2)
                #`(define name2
                    (let ()
                      (define-type (T A ...2) (T-imp A ...2))
                      (: #,(datum->syntax #'k2 'method) : type) ...
                      body body* ...2
                      (list #,(datum->syntax #'k2 'method) ...)))]))

           (define-syntax (with-name stx2)
             (define (nth-cdr+car stx3 m)
               (if (zero? m)
                   #`(car #,stx3)
                   (nth-cdr+car #`(cdr #,stx3) (sub1 m))))
             (syntax-case stx2 (import)
               [(k2 s (T-imp A ...2)
                    (import libs ...2)
                    body body* ...2)
                #`(let ()
                    (define-type (T A ...2) (T-imp A ...2))
                    (define #,(datum->syntax #'k2 'method) : type #,(nth-cdr+car #'s n)) ...
                    (libs (T-imp A ...2) s) ...2
                    body body* ...2)]))

           (define-syntax (define-name-library stx2)
             (syntax-case stx2 (export)
               [(k2 name2 (T2 A ...2)
                    (export f ...2)
                    body body* ...2)
                #`(define-syntax (name2 stx3)
                    (define (nth-cdr+car stx3 m)
                      (if (zero? m)
                          #`(car #,stx3)
                          (nth-cdr+car #`(cdr #,stx3) (sub1 m))))
                    (syntax-case stx3 ()
                      [(k3 (T-imp B ...3) s)
                       #`(define-values (#,(datum->syntax #'k3 'f) ...2)
                           (let ()
                             (define-type (T A ...2) (T-imp A ...2))
                             (define-type (T2 B ...3) (T-imp B ...3))
                             (define #,(datum->syntax #'k2 'method) : type #,(nth-cdr+car #'s n)) ...
                             body body* ...2
                             (values f ...2)))]))]))))]))
(provide define-type-class)
