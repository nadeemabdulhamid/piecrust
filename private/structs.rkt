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
(define sql-type/c (symbols 'text 'integer 'real 'blob 'null 'boolean 'numeric))

(define field-bind-spec/c
  (or/c (list/c string? sql-type/c) ; 2
        (list/c string? sql-type/c string?) ; 3
        (list/c string? sql-type/c boolean?) ; 3 - ambiguous
        (list/c (list/c string? symbol?) sql-type/c string?) ; 3+
        (list/c (list/c string? symbol?) sql-type/c boolean?) ; 3+ - ambiguous
        (list/c string? sql-type/c boolean? string?) ; 4
        (list/c (list/c string? symbol?) sql-type/c boolean? string?))) ; 4+


(struct/contract db-field
                 ([col-name string?]
                  [json-symbol symbol?]
                  [sql-type sql-type/c]
                  [can-be-null? boolean?]
                  [label string?])
                 #:transparent)


; joins:
; [list json-key   Foreign-table   Foreign-key-(to match this table's PK value)]
; [list json-key  [list Foreign-table  Foreign-table-primary-key] [list Junction-table Junction-table-Foreign-Key-1] Junction-table-Foreign-key-2 (to match this table's PK)]
(define join-spec/c
  (or/c (list/c symbol? (list/c string? (listof field-bind-spec/c)) string?)
        (list/c symbol? (list/c string? (listof field-bind-spec/c) string?)
                (list/c string? string?) string?)))

(define join-fields/c
  (or/c (list/c symbol? (list/c string? (listof db-field?)) string?)
        (list/c symbol? (list/c string? (listof db-field?) string?) (list/c string? string?) string?)))


(struct/contract db-bind
                 ([table  string?]      ; name
                  [fields (listof db-field?)]     ; [listof db-field]
                  [primkey string?]
                  [joins (listof join-fields/c)])   ; string
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
  (db-fields->names (db-bind-fields a-dbb)))


(define (db-fields->names flds)
  (map string-downcase (map db-field-col-name flds)))


;; db-bind -> (hashof string db-field)
(define (db-bind-field-map a-dbb)
  (db-fields->hash (db-bind-fields a-dbb)))


(define (db-fields->hash flds)
  (for/hash ([fld flds])
    (match fld
      [(db-field col-name _ _ _ _)
       (values (string-downcase col-name) fld)])))


;; TODO: this is really loose
;; this is jsexpr -> sql values
(define (jsexpr->sql-type val sql-type)
  (match sql-type
    ['text   (match val
               [(or 'null #f) sql-null]
               [_ val])]
    
    [_ val]))


;; this is sql values -> jsexpr (hash?)
(define (result-vector->dict field-map field-names value-vector)
  (define (sql-null->null x) (if (eq? x sql-null) 'null x))
  
  (for/hasheq ([fn field-names]
               [val value-vector])
    (values (db-field-json-symbol (hash-ref field-map fn)) (sql-null->null val))))


