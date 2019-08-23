#lang typed/racket
(require (for-syntax racket/list))

(define-syntax (define-type-class stx)
  (syntax-case stx ()
    [(k name T ([method : type] ...))
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
                   [(expr ...) (generate-temporaries #'(method ...))]
                   [(n ...) (range (length (syntax->list #'(method ...))))])
       #'(begin
           (define-syntax (define-name stx2)
             (syntax-case stx2 ()
               [(k2 name2 (T-imp A (... ...)) body body* (... ...))
                #`(define name2
                    (let ()
                      (define-type (T A (... ...)) (T-imp A (... ...)))
                      body body* (... ...)
                      (list (ann  #,(datum->syntax #'k2 'method) : type) ...)))]))
           
           (define-syntax (with-name stx2)
             (define (nth-cdr+car stx3 m)
               (if (zero? m)
                   #`(car #,stx3)
                   (nth-cdr+car #`(cdr #,stx3) (sub1 m))))
             (syntax-case stx2 ()
               [(k2 (T-imp A (... ...)) s body body* (... ...))
                #`(let ()
                    (define-type (T A (... ...)) (T-imp A (... ...)))
                    (define #,(datum->syntax #'k2 'method) : type #,(nth-cdr+car #'s n)) ...
                    body body* (... ...))]))

           (define-syntax (define-name-library stx2)
             (syntax-case stx2 (export)
               [(k2 name2 (T2 A (... ...))
                    (export f (... ...))
                    body body* (... ...))
                #`(define-syntax (name2 stx3)
                    (define (nth-cdr+car stx3 m)
                      (if (zero? m)
                          #`(car #,stx3)
                          (nth-cdr+car #`(cdr #,stx3) (sub1 m))))
                    (syntax-case stx3 ()
                      [(k3 (T-imp B ((... ...) (... ...))) s)
                       #`(define-values (#,(datum->syntax #'k3 'f) (... ...))
                           (let ()
                             (define-type (T A (... ...)) (T-imp A (... ...)))
                             (define-type (T2 B ((... ...) (... ...))) (T-imp B ((... ...) (... ...))))
                             (define #,(datum->syntax #'k2 'method) : type #,(nth-cdr+car #'s n)) ...
                             body body* (... ...)
                             (values f (... ...))))]))]))))]))
(provide define-type-class)
