#lang racket

(require db
         json
         web-server/http
         web-server/servlet
         web-server/dispatchers/dispatch
         "structs.rkt")

(provide (all-defined-out))



(define (api/default-else req)
  (next-dispatcher))



(define (apply-pre/error-wrapper handler-wrapper crud-op req-handler-func)
  (define w (handler-wrapper 'pre))
  (define ew (handler-wrapper 'error))
  
  (lambda (req)
    (with-handlers ([exn:fail:api:error? (λ(e)
                                           (define msg (exn-message e))
                                           (define code BAD-REQUEST-CODE)
                                           (if ew
                                               (ew e crud-op code msg)
                                               (response/jsexpr msg #:code code))
                                           )]
                    [exn:fail:sql? (λ(e)
                                     (printf "~a" (exn-message e))
                                     (define msg "internal sql error")
                                     (define code SERVER-ERROR-CODE)
                                     (if ew
                                         (ew e crud-op code msg)
                                         (response/jsexpr msg #:code code)))])
      (if w (w req crud-op req-handler-func) (req-handler-func req)))))


(define (apply-post-wrapper handler-wrapper crud-op jsexpr)
  (define w (handler-wrapper 'post))

  (if w
      (w jsexpr crud-op)
      (response/jsexpr jsexpr)))



; [listof string] request -> [listof string]
; produces (validated) list of database columns to select based on the
; json dictionary labels specified by the _fields=<...> query parameter
; in the requeest. If no query parameter, then just the given field names
; are produced as is (order may be important for the calling function)
(define (build-select-field-list field-map all-field-names req)
  (define rbs (request-bindings req))
  ;(printf "Bindings:~n~a~n" rbs)

  (cond
    [(exists-binding? '_fields rbs)
      (define fstr (string-downcase (extract-binding/single '_fields rbs)))
      (validate-req-field-names field-map all-field-names (string-split fstr ","))]
    [else all-field-names]))


; [listof string] [listof string] -> [listof string]
; validates and translates json-symbol names to internal database column names
(define (validate-req-field-names field-map all-field-names provided-names)
  (define json-syms/strs   ; an inverted index of field-map
    (for/hash ([fn all-field-names])
      (values (symbol->string (db-field-json-symbol (hash-ref field-map fn)))
              fn)))

  (for/list ([pn provided-names])
    (unless (member pn (hash-keys json-syms/strs)) (raise-api-error "invalid field name"))
    (hash-ref json-syms/strs pn)))









#|
in the data of the POST:
  look for JSON dictionary with a dictionary of fields to create a record
ignore primary key field if it's in the dictionary
|#
(define (api/create-handler an-api)
  (define crud-op 'create)

  (define (build-insert-sql bndg fvals)   ; [listof (string . any)]
    (define col-names (map car fvals))
    (define val-placeholders (map (λ(fn) "?") fvals))
    
    (format "INSERT INTO ~a (~a) VALUES (~a)"
            (db-bind-table bndg)
            (string-join col-names ", ")
            (string-join val-placeholders ", ")))

  
  (match an-api
    [(api url-path db bndg _ handler-wrapper)
     (apply-pre/error-wrapper
      handler-wrapper crud-op
      
      (lambda (req)
        (define pk (string-downcase (db-bind-primkey bndg)))  ; primary key name
        (define all-field-names (db-bind-col-names bndg))
        (define field-map (db-bind-field-map bndg)) ; (hashof string db-field)

        (define post-data (request-post-data/raw req))
        (define post-json (bytes->jsexpr post-data))

        (unless (hash? post-json) (raise-api-error "bad data"))

        (define field-vals
          (for/list ([fn all-field-names]
                     #:unless (string-ci=? fn pk))
            (match (hash-ref field-map fn)
              [(db-field _ json-sym sql-type null-ok? _)
               (define json-sym (db-field-json-symbol (hash-ref field-map fn)))
               (define sql-type (db-field-sql-type (hash-ref field-map fn)))
               
               (define val
                 (cond [(hash-has-key? post-json json-sym) (hash-ref post-json json-sym)]
                       [null-ok? 'null]
                       [else (raise-api-error (format "missing ~a field" json-sym))]))
               
               (cons fn (jsexpr->sql-type val sql-type))])))

        (define sql-str (build-insert-sql bndg field-vals))
        (define result (apply query (append (list db sql-str) (map cdr field-vals))))
  
        (define pk-symbol (db-field-json-symbol (hash-ref field-map pk)))
        (define new-id (cdr (assoc 'insert-id (simple-result-info result))))
        
        (apply-post-wrapper
         handler-wrapper crud-op
         (make-immutable-hasheq `((,pk-symbol . ,new-id))))))]))









#|
 /api          list all  
 /api?{field}={blah}&_sort={fieldname}&_order={asc|desc}    query
 /api?*={blah}....
 /api?_fields={fieldname,...}    select fields
 /api?_unique=1    select unique
|#
(define (api/query-handler an-api)
  (define crud-op 'query)

  ;;;; A bunch of utility functions first ----------
  ;;;;
  (define (extract-sort-field field-map all-field-names req)
    (define rbs (request-bindings req))
    (cond [(exists-binding? '_sort rbs)
           (define sstr (string-downcase (extract-binding/single '_sort rbs)))
           (and (validate-req-field-names field-map all-field-names (string-split sstr ","))
                sstr)]
          [else #f]))

  (define (extract-sort-order req)
    (define rbs (request-bindings req))
    (cond [(exists-binding? '_order rbs)
           (define ostr (string-downcase (extract-binding/single '_order rbs)))
           (and (member ostr `("asc" "desc")) ostr)]
          [else #f]))

  ; [listof string] request -> (values  string["or"|"and"]  (listof (string . string)))
  (define (extract-where-fields field-map all-field-names req)
    (define rbs (request-bindings req))
    
    (cond
      [(exists-binding? '* rbs)
       (define vstr (extract-binding/single '* rbs))
       (values "or"
               (for/list ([fn (map string-downcase all-field-names)])
                 (cons fn (string-replace vstr "*" "%"))))]
      [else     
       ; search for query params that match any of the field names in the db
       (values
        "and"
        (for/list ([fn (map string-downcase all-field-names)]
                   #:when (exists-binding? (string->symbol fn) rbs))
          (define vstr (extract-binding/single (string->symbol fn) rbs))
          (cons fn (string-replace vstr "*" "%"))))]))

  ; (string . string) -> string
  (define (build-where-condition wfld)
    (format "~a like ?" (car wfld)))
  
  ; (listof (string . string)) -> string
  (define (build-where-clause wflds join-op)
    (if (and wflds (cons? wflds))
        (string-append " WHERE "
                       (string-join (map build-where-condition wflds)
                                    (format " ~a " join-op)))
        ""))
  ;;;;
  ;;;; ----------------- done with the utility functions
  
  
  (match an-api
    [(api url-path db bndg _ handler-wrapper)
     (apply-pre/error-wrapper
      handler-wrapper crud-op
      
      (lambda (req)
        (define all-field-names (db-bind-col-names bndg))
        (define field-map (db-bind-field-map bndg)) ; (hashof string db-field)

        (define select-field-names (build-select-field-list field-map all-field-names req))
        (when (empty? select-field-names) (raise-api-error "no fields selected"))
        
        (define sort-field (extract-sort-field field-map all-field-names req)) ; string or #f
        (define sort-order (extract-sort-order req)) ; string or #f
        (define sort-string (if (not sort-field) ""
                                (format " ORDER BY ~a ~a " sort-field (or sort-order ""))))

        (define-values (join-op where-fields)
          (extract-where-fields field-map all-field-names req)) ; [listof (string . string)] or #f
        (define where-string (build-where-clause where-fields join-op))

        (define unique-string (if (exists-binding? '_distinct (request-bindings req))
                                  " DISTINCT " ""))

        (define sql-str (format "select~a ~a from ~a~a~a"
                                unique-string
                                (string-join select-field-names ", ")
                                (db-bind-table bndg)
                                where-string
                                sort-string))
        (define results (apply query-rows (append (list db sql-str) (map cdr where-fields))))
  
        ;(printf "~nQuery: ~a~nBind: ~a~n ~nResult:~n~a~n" (send stmt get-stmt) (map cdr where-fields) result)
        ;(printf "select field names: ~a~nfield map: ~a~n" select-field-names (db-bind-field-map bndg))

        (define joins-requested? (exists-binding? '_joins (request-bindings req)))
        
        (define result/jsexpr
          (let [(R (map (λ(row)
                          (define fields-hash (result-vector->dict field-map select-field-names row))
                          (cond
                            [joins-requested?
                             (define id (hash-ref fields-hash
                                                  (db-field-json-symbol
                                                   (hash-ref field-map
                                                             (string-downcase (db-bind-primkey bndg))))))
                             (add-joins db bndg crud-op fields-hash id)]
                            [else fields-hash]))
                        results))]
            (cond
              [(and (not joins-requested?) (= 1 (length select-field-names)))
                ; collapse singleton dictionaries to a list of values
               (for/list ([dict R]) (cdar (hash->list dict)))]
               ;(for/list ([(key value) R]) value)]
              [else R])))
        
        (apply-post-wrapper handler-wrapper crud-op result/jsexpr)))]))







#|
 /api/{id}        list {id} 
 /api/{id}?fields={fieldname,...}
                                 list {id}
                               only fields
|#
(define (api/read-handler an-api)
  (define crud-op 'read)
  
  (match an-api
    [(api url-path db bndg _ handler-wrapper)

     (lambda (req id)
       ((apply-pre/error-wrapper
         handler-wrapper crud-op

         (lambda (req)
           (define all-field-names (db-bind-col-names bndg))
           (define field-map (db-bind-field-map bndg)) ; (hashof string db-field)

           (define select-field-names (build-select-field-list field-map all-field-names req))
           (when (empty? select-field-names) (raise-api-error "no fields selected"))
           
           (define stmt (format "select ~a from ~a where ~a = ?"
                                (string-join select-field-names ", ")
                                (db-bind-table bndg)
                                (string-downcase (db-bind-primkey bndg))))
           (define result (query-maybe-row db stmt id))
           
           (unless result (raise-api-error "invalid id"))

           (define fields-hash
             (result-vector->dict (db-bind-field-map bndg) select-field-names result))

           (define joined-hash (add-joins db bndg 'read fields-hash id))

           (apply-post-wrapper handler-wrapper crud-op joined-hash)))
        req))]))


; connection? crud-op/c join-spec/c dict any -> dict
; adds nested dictionaries based on joins
(define (add-joins db bndg crud-op base-hash id)
  (define tbl-name (db-bind-table bndg))
  (define joins (db-bind-joins bndg))
  (define pk/tbl (db-bind-primkey bndg))
  
  (for/fold ([built-hash base-hash])
            [(join joins)]
    ; joins:
    ; [list json-key  [Foreign-table Field-binding]   Foreign-key-(to match this table's PK value)]  <--- one-to-many
    ; [list json-key  [list Foreign-table Field-binding Foreign-table-primary-key]  This-table-Foreign-key-(to match Foreign table PK)]    <--- many-to-one
    ; [list json-key  [list Foreign-table Field-binding Foreign-table-primary-key] [list Junction-table Junction-table-Foreign-Key-1] Junction-table-Foreign-key-2 (to match this table's PK)]
    (match join
      ;;  ONE-TO-MANY
      [(list json-key [list f-tbl flds] f-key/f-tbl)
       (define all-field-names (db-fields->names flds))
       (define field-map (db-fields->hash flds))
          
       (define stmt (format "SELECT ~a FROM ~a WHERE ~a.~a = ?"
                            (string-join
                             (for/list ([fn all-field-names])
                               (format "~a.~a" f-tbl fn)) ", ")
                            f-tbl f-tbl f-key/f-tbl))
       (define result (query-rows db stmt id))
       (define js-list
         (map (λ(row) (result-vector->dict field-map all-field-names row)) result))
          
       (hash-set built-hash json-key js-list)]

      ;; MANY-TO-ONE
      [(list json-key [list f-tbl flds pk/f-tbl] f-key/this-tbl)
       (define all-field-names (db-fields->names flds))
       (define field-map (db-fields->hash flds))

       ; need to get the foreign key value
       (define f-id
         (query-maybe-value db (format "SELECT ~a FROM ~a WHERE ~a = ?" f-key/this-tbl tbl-name pk/tbl) id))

       (cond
         [(or (sql-null? f-id)
              (false? f-id)) built-hash]
         [else 
          (define stmt (format "SELECT ~a FROM ~a WHERE ~a.~a = ?"
                               (string-join
                                (for/list ([fn all-field-names])
                                  (format "~a.~a" f-tbl fn)) ", ")
                               f-tbl
                               ; WHERE
                               f-tbl pk/f-tbl))
       
          (define result (query-maybe-row db stmt f-id))
          (define jsexpr (if (or (sql-null? result) (false? result))
                             'null
                             (result-vector->dict field-map all-field-names result)))
          
          (hash-set built-hash json-key jsexpr)])]
      
      ;; MANY-TO-MANY
      [(list json-key [list f-tbl flds pk/f-tbl] [list j-tbl f-tbl-f-key/j-tbl] f-key/j-tbl)
       (define all-field-names (db-fields->names flds))
       (define field-map (db-fields->hash flds))

       (define stmt (format "SELECT ~a FROM ~a
                                INNER JOIN ~a ON ~a.~a = ~a.~a
                                WHERE ~a.~a = ?"
                            (string-join
                             (for/list ([fn all-field-names])
                               (format "~a.~a" f-tbl fn)) ", ")
                            f-tbl
                            ; inner join
                            j-tbl   ; junction table
                            f-tbl pk/f-tbl  ; f-tbl.primkey =
                            j-tbl f-tbl-f-key/j-tbl
                            ; where
                            j-tbl  f-key/j-tbl))

       (define result (query-rows db stmt id))
       (define js-list
         (map (λ(row) (result-vector->dict field-map all-field-names row)) result))
          
       (hash-set built-hash json-key js-list)])))
  


#|
 PUT  /api/{id}    <json req body>
|#
(define (api/update-handler an-api)
  (define crud-op 'update)
  
  (match an-api
    [(api url-path db bndg _ handler-wrapper)
     (lambda (req id)
       ((apply-pre/error-wrapper
         handler-wrapper crud-op

         (lambda (req)
           (define pk (string-downcase (db-bind-primkey bndg)))  ; primary key name
           (define all-field-names (db-bind-col-names bndg))
           (define field-map (db-bind-field-map bndg)) ; (hashof string db-field)

           (define post-data (request-post-data/raw req))
           (define post-json (bytes->jsexpr post-data))

           (unless (hash? post-json) (raise-api-error "bad data"))

           (define field-vals
             (for/list ([fn all-field-names]
                        #:unless (string-ci=? fn pk)
                        #:when (hash-has-key? post-json (db-field-json-symbol (hash-ref field-map fn))))
               (define val (hash-ref post-json  ;; look up the json-symbol for the field in the post data
                                     (db-field-json-symbol (hash-ref field-map fn))))
               (cons fn val)))

           (when (empty? field-vals) (raise-api-error "no fields to update"))

           (define assign-strs (map (λ(pair) (format "~a = ?" (car pair))) field-vals))
    
           (define sql-str (format "UPDATE ~a SET ~a WHERE ~a = ?"
                                   (db-bind-table bndg)
                                   (string-join assign-strs ", ")
                                   pk))

           (define result (apply query (append (list db sql-str) `(,@(map cdr field-vals) ,id))))

           (define row-count (cdr (assoc 'affected-rows (simple-result-info result))))

           (if (zero? row-count)
               (raise-api-error "no rows found")
               (apply-post-wrapper handler-wrapper crud-op row-count))))
        req))]))

           



#|
 DELETE  /api/{id}  
|#
(define (api/delete-handler an-api)
  (define crud-op 'delete)
  
  (match an-api
    [(api url-path db bndg _ handler-wrapper)
     (lambda (req id)
       ((apply-pre/error-wrapper
         handler-wrapper crud-op

         (lambda (req)
           (define pk (string-downcase (db-bind-primkey bndg)))  ; primary key name

           (define sql-str (format "DELETE FROM ~a WHERE ~a = ?" (db-bind-table bndg) pk))
           (define result (query db sql-str id))

           (define row-count (cdr (assoc 'affected-rows (simple-result-info result))))

           (if (zero? row-count)
               (raise-api-error "no rows found")
               (apply-post-wrapper handler-wrapper crud-op row-count))))
        req))]))





