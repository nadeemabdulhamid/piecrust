#lang racket

(require piecrust)

(require db
         json
         web-server/http
         web-server/servlet
         web-server/servlet-env
         web-server/dispatchers/dispatch)

(define (setup-database dbc)
  (query-exec dbc "DROP TABLE IF EXISTS Stores")
  (query-exec dbc "DROP TABLE IF EXISTS Items")
  
  (query-exec dbc "CREATE TABLE Stores ( Store_id INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL,
                         Name TEXT NOT NULL,
                         Address TEXT);")

  (query-exec dbc "CREATE TABLE Items ( ID INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL,
                         Store_id INTEGER NOT NULL,
                         Name TEXT NOT NULL,
                         Brand TEXT,
                         FOREIGN KEY (Store_id) REFERENCES Stores (Store_id));")

  (for ([line
         (string-split #<<END
INSERT INTO Stores (Store_id, Name, Address) VALUES (1, 'Adli', '123 Main');
INSERT INTO Stores (Store_id, Name, Address) VALUES (2, 'BeeJays', '456 First');
INSERT INTO Stores (Store_id, Name) VALUES (3, 'Warmalt');
INSERT INTO Items (ID, Store_id, Name, Brand) VALUES (101, 2, 'Chips', "Louie's");
INSERT INTO Items (ID, Store_id, Name, Brand) VALUES (102, 1, 'Donuts', 'Delites');
INSERT INTO Items (ID, Store_id, Name, Brand) VALUES (103, 1, 'Eggs', 'Fresh');
INSERT INTO Items (ID, Store_id, Name, Brand) VALUES (104, 3, 'Flour', "Baker's");
INSERT INTO Items (ID, Store_id, Name) VALUES (105, 2, 'Grapes');
END
                       "\n")])
    (query-exec dbc line))
  
  dbc)


(define (go-live disp-func)
  (serve/servlet disp-func
                 #:servlet-regexp #rx"" #:servlet-path "/" #:launch-browser? #f))



(define dbc (sqlite3-connect #:database 'memory))
(setup-database dbc)

;; A default generic request handler for root path "/" requests
(define (main-servlet req)
  (if (equal? "" (path/param-path (first (url-path (request-uri req)))))
      (response/xexpr `(body (h1 "Shopping List - API Endpoints")
                             (a ([href "/api/stores"]) "Stores") (br)
                             (a ([href "/api/v1/items"]) "Items")))
      (next-dispatcher))) ; hand everything else off to the next dispatcher

;; Stores API endpoint
(define store-api
  (create-api-manager "api/stores" ; API endpoint
                      dbc          ; database connection
                      "Stores"     ; table name
                      `((["Store_id" id] integer "Internal ID")  
                        ("Name" text "Store name")
                        ("Address" text))  ; table columns
                      "Store_id" ; primary key

                      #:request-wrapper
                      (λ (req crud-op api-handler)
                        (match crud-op
                          ['delete (response/jsexpr "not supported" #:code 400)]
                          [_ (api-handler req)]))

                      #:fallback-request-handler main-servlet))

;; Items API endpoint
(define items-api
  (create-api-manager "api/v1/items"
                      dbc
                      "Items"
                      `(("ID" integer)
                        (["Store_id" store-id] integer #f "Store ID")
                        ("Name" text #f) ; must be non-null
                        ("Brand" text))
                      "ID"

                      #:response-wrapper
                      (λ (js crud-op)
                        (response/jsexpr (make-immutable-hasheq
                                          `((meta . #hasheq((endpoint . "api/v1/items")))
                                            (data . ,js)))))

                      #:error-wrapper
                      (λ (exn crud-op resp-code jsexpr)
                        (response/jsexpr
                         (make-immutable-hasheq `((operation . ,(symbol->string crud-op))
                                                                  (message . ,jsexpr)))
                         #:code resp-code))
                              
                      #:fallback-request-handler (api-dispatcher store-api)))

; (go-live (api-dispatcher items-api))

