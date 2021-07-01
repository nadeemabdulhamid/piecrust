#lang racket

;; piecrust

(module+ test
  (require rackunit))

(require db
         json
         web-server/http
         web-server/servlet
         web-server/dispatchers/dispatch
         "private/structs.rkt"
         "private/handlers.rkt"
         (for-syntax syntax/parse
                     racket/string))


(provide create-api-manager
         api-dispatcher
         api-db
         (struct-out exn:fail:api:error))



(define-syntax (create-api-manager stx)
  (syntax-parse stx
    [(_ endpoint-path:str
        database-conn
        table-name:str
        columns
        primary-key:str
        rest ...)

     (define pieces (string-split (syntax->datum #'endpoint-path) "/"))

     #`(_create-api-manager
        (λ (else-dispatcher)
          (λ (an-api)
            (let-values ([(disp gen)
                          (dispatch-rules
                           [#,pieces #:method "post"                (api/create-handler an-api)]
                           [#,(append pieces '("")) #:method "post" (api/create-handler an-api)] ; trailing /
                           
                           [#,pieces #:method "get"                 (api/query-handler an-api)] 
                           [#,(append pieces '("")) #:method "get"  (api/query-handler an-api)] ; trailing /
                           
                           [#,(append pieces #'((integer-arg))) #:method "get"  (api/read-handler an-api)] ;((api-handler an-api) 'read)]
                           [#,(append pieces #'((integer-arg))) #:method "put"  (api/update-handler an-api)] ;((api-handler an-api) 'update)]
                           [#,(append pieces #'((integer-arg))) #:method "delete"  (api/delete-handler an-api)] ;((api-handler an-api) 'delete)]
                           [else (or else-dispatcher api/default-else)])])
              disp)))
        endpoint-path
        database-conn
        table-name
        columns
        primary-key
        rest ...)]))



(define/contract
  (_create-api-manager dispatch-gen
                       api-path
                       database-conn
                       table-name
                       columns
                       primary-key
                       #:fallback-request-handler (else-req-handler #f)
                       #:request-wrapper (pre-wrapper #f)
                       #:response-wrapper (post-wrapper #f)
                       #:error-wrapper (error-wrapper #f)
                       #:field-name-generator (field-name-gen (compose string->symbol string-downcase)))
  (->* (((or/c false/c (request? . -> . any))
         . -> . (api? . -> . (request? . -> . any)))
        string?
        connection?
        string?
        (listof (or/c (list/c string? sql-type/c) ; 2
                      (list/c string? sql-type/c string?) ; 3
                      (list/c string? sql-type/c boolean?) ; 3 - ambiguous
                      (list/c (list/c string? symbol?) sql-type/c string?) ; 3+
                      (list/c (list/c string? symbol?) sql-type/c boolean?) ; 3+ - ambiguous
                      (list/c string? sql-type/c boolean? string?) ; 4
                      (list/c (list/c string? symbol?) sql-type/c boolean? string?))) ; 4+
        string?)
       (#:fallback-request-handler (or/c false/c (request? . -> . any))
        #:request-wrapper (or/c false/c (-> request? crud-op/c (request? . -> . response?) response?))
        #:response-wrapper (or/c false/c (jsexpr? crud-op/c . -> . response?))
        #:error-wrapper (or/c false/c (exn? crud-op/c response-code/c jsexpr? . -> . response?))
        #:field-name-generator (string? . -> . symbol?))

      api?)


  (define flds (map (λ(col-info)
                      (match col-info
                        [(list col-name type)   ; 2
                         (db-field col-name (field-name-gen col-name) type #t col-name)]

                        [(list [list col-name field-name] type null-ok?/label) ; 3+
                         (if (boolean? null-ok?/label)
                             (db-field col-name field-name type null-ok?/label col-name)
                             (db-field col-name field-name type #t null-ok?/label))]
                        
                        [(list col-name type null-ok?/label) ; 3
                         (if (boolean? null-ok?/label)
                             (db-field col-name (field-name-gen col-name) type null-ok?/label col-name)
                             (db-field col-name (field-name-gen col-name) type #t null-ok?/label))]

                        [(list [list col-name field-name] type null-ok? label) ; 4+
                         (db-field col-name field-name type null-ok? label)]

                        [(list col-name type null-ok? label) ; 4
                         (db-field col-name (field-name-gen col-name) type null-ok? label)]))
                    columns))

  (define bnd (db-bind table-name flds primary-key))

  (define (wrapper-lookup type)
    (match type
      ['pre pre-wrapper]
      ['post post-wrapper]
      ['error error-wrapper]))
  
  (api (string-split api-path "/")
       database-conn
       bnd
       (dispatch-gen else-req-handler)
       wrapper-lookup))



(define/contract (api-dispatcher an-api)
  (api? . -> . (request? . -> . any))
  ((api-dispatch-gen an-api) an-api))


