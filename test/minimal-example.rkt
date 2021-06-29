#lang racket

(require piecrust)
(require "test-utils.rkt")

(require db
         json
         web-server/http
         web-server/servlet
         web-server/servlet-env
         web-server/dispatchers/dispatch)

(module+ test
  (require rackunit))

(define (setup-database dbc)
  (query-exec dbc "DROP TABLE IF EXISTS Stores")
  (query-exec dbc "CREATE TABLE Stores ( Store_id INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL,
                         Name TEXT NOT NULL,
                         Address TEXT);")
  (for ([line
         (string-split #<<END
INSERT INTO Stores (Store_id, Name, Address) VALUES (1, 'Adli', '123 Main');
INSERT INTO Stores (Store_id, Name, Address) VALUES (2, 'BeeJays', '456 First');
INSERT INTO Stores (Store_id, Name) VALUES (3, 'Warmalt');
END
                       "\n")])
    (query-exec dbc line))
  
  (void))


(define (go-live disp-func)
  (serve/servlet disp-func
                 #:servlet-regexp #rx""
                 #:servlets-root "."
                 #:servlet-path "/"
                 #:extra-files-paths (list ".")
                 #:launch-browser? #f))


(define dbc (sqlite3-connect #:database 'memory))
(setup-database dbc)

(define store-api
  (create-api-manager "" ; API endpoint
                      dbc          ; database connection
                      "Stores"     ; table name
                      `(("Store_id" id integer "Internal ID")  
                        ("Name" text #f "Store name")
                        ("Address" text))  ; table columns
                      "Store_id")) ; primary key

(module+ test
  ; launch web server in separate thread
  (define ws-thr
    (thread (λ() (go-live (api-dispatcher store-api)))))

  (sleep 2) ; wait for the web server thread to go live

  (check-equal? (status-line->code
                 (make-api-request/status "http://localhost:8000/?_fields=name,address&_sort=address&_order=desc"))
                200)


  ;; QUERY - list of 3 records
  (check-equal? (make-api-request/jsexpr 200 "http://localhost:8000")
                (string->jsexpr #<<JSON
                                [{"address":"123 Main","id":1,"name":"Adli"},
                                 {"address":"456 First","id":2,"name":"BeeJays"},
                                 {"address":null,"id":3,"name":"Warmalt"}]
JSON
                                ))

  ;; QUERY - field select and sorting
  (check-equal? (make-api-request/jsexpr 200 "http://localhost:8000/?_fields=name,address&_sort=address&_order=desc")
                (string->jsexpr #<<JSON
                                [{"address":"456 First","name":"BeeJays"},
                                 {"address":"123 Main","name":"Adli"},
                                 {"address":null,"name":"Warmalt"}]
JSON
                                )) ; list without id's

  ;; UPDATE - name of record with id=1
  (check-equal? (make-api-request/jsexpr "http://localhost:8000/1"
                                         #:method "PUT"
                                         #:data "{\"name\" : \"Adli, Inc.\"}"
                                         #:headers (list "Content-Type: application/json"))
                1) ; should update a row successfully

  ;; READ - updated record id=1
  (check-equal? (make-api-request/jsexpr 200 "http://localhost:8000/1")
                (string->jsexpr #<<JSON
                                {"address":"123 Main","id":1,"name":"Adli, Inc."}
JSON
                                )) ; single record

  ;; READ - a non-existent row (id=4)
  (check-equal? (make-api-request/jsexpr 400 "http://localhost:8000/4")
                "invalid id")
  
  ;; CREATE - a new record (note: ignores primary key value if supplied) (new id=4)
  (check-equal? (make-api-request/jsexpr "http://localhost:8000/"
                                         #:method "POST"
                                         #:data #<<JSON
                                { "id" : 54,
                                    "name" : "CoCost",
                                    "address" : false }
JSON
                                         #:headers (list "Content-Type: application/json"))
                (string->jsexpr #<<JSON
                                {"id":4}
JSON
                                ))

  ;; READ - the newly recreated row (id=4) with field selects
  (check-equal? (make-api-request/jsexpr 200 "http://localhost:8000/4?_fields=name,address")
                (string->jsexpr #<<JSON
                                {"address": null, "name":"CoCost"}
JSON
                                )) ; single newly added record with field selects

  ;; DELETE - id=4
  (check-equal? (make-api-request/jsexpr 200 "http://localhost:8000/4" #:method "DELETE")
                1) ; deletes that one

  ;; READ - delete row should be non-existent (id=4)
  (check-equal? (make-api-request/jsexpr 400 "http://localhost:8000/4")
                "invalid id")
  
  ;; CREATE - another new one - new id=5,  null-ok field (address) does not need to be supplied in the body
  (check-equal? (make-api-request/jsexpr "http://localhost:8000/"
                                         #:method "POST"
                                         #:data #<<JSON
                                { "name" : "CoCost" }
JSON
                                         #:headers (list "Content-Type: application/json"))
                (string->jsexpr #<<JSON
                                {"id":5}
JSON
                                )) ; add again, only supply name


  
  ; kill the server thread
  (break-thread ws-thr))



; to run:
;    (go-live (api-dispatcher store-api))

