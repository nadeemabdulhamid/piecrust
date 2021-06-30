#lang racket

(provide (all-defined-out))

(require json net/url
         (for-syntax syntax/parse))


(define (status-line->code line)
  (string->number (second (string-split (bytes->string/utf-8 line)))))

(define-syntax (make-api-request/status stx)
  (syntax-parse stx
    [(_ url rest ...)

     #'(let-values ([(resp hdrs prt) (http-sendrecv/url (string->url url) rest ...)])
         resp)]))

(define-syntax (make-api-request/jsexpr stx)
  (syntax-parse stx
    [(_ x:integer url:str rest ...)
     ; if first argument is a number, checks that the response status code matches that, or else raises an error

     #'(let-values ([(resp hdrs prt) (http-sendrecv/url (string->url url) rest ...)])
         (define status-code (status-line->code resp))
         (unless (= status-code x) (error (format "unexpected status code ~a" status-code)))
         (string->jsexpr (port->string prt)))]
    
    [(_ url:str rest ...)

     #'(let-values ([(resp hdrs prt) (http-sendrecv/url (string->url url) rest ...)])
         (string->jsexpr (port->string prt)))]))



