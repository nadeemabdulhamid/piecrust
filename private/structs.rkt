#lang racket

(require db
         json
         web-server/http
         (for-syntax syntax/parse))

(provide (all-defined-out))


(define BAD-REQUEST-CODE 400)
(define SERVER-ERROR-CODE 500)

(struct exn:fail:api:error exn:fail () #:transparent)

(define-syntax (raise-api-error stx)
  (syntax-parse stx
    [(_ msg:expr) #`(raise (exn:fail:api:error msg (current-continuation-marks)))]))


(define crud-op/c (symbols 'create 'query 'read 'update 'delete))

(struct/contract db-field
                 ([col-name string?]
                  [json-symbol symbol?]
                  [label string?])
                 #:transparent)

(struct/contract db-bind
                 ([table  string?]      ; name
                  [fields (listof db-field?)]     ; [listof db-field]
                  [primkey string?])   ; string
                 #:transparent)

(struct/contract api ([url-path (listof string?)]
                      [db connection?]
                      [binding db-bind?]
                      [dispatch-gen (-> any/c (-> request? any))] ; really: (-> api? (-> request? any))
                      [handler-wrapper
                       (->i
                        ([ty (symbols 'pre 'post 'error)])
                        [result
                         (ty) (match ty
                                ['pre   (or/c false/c
                                              (-> request? crud-op/c (request? . -> . response?) response?))]
                                ['post  (or/c false/c (jsexpr? crud-op/c . -> . response?))]
                                ['error
                                 (or/c false/c
                                       (exn? crud-op/c response-code/c jsexpr? . -> . response?))])])])
                 #:transparent) 



(define (db-bind-col-names a-dbb)
  (map string-downcase (map db-field-col-name (db-bind-fields a-dbb))))

;; db-bind -> (hashof string db-field)
(define (db-bind-field-map a-dbb)
  (for/hash ([fld (db-bind-fields a-dbb)])
    (match fld
      [(db-field col-name json-symbol label) 
       (values (string-downcase col-name) fld)])))

(define (result-vector->dict field-map field-names value-vector)
  (for/hasheq ([fn field-names]
               [val value-vector])
    (values (db-field-json-symbol (hash-ref field-map fn)) val)))
