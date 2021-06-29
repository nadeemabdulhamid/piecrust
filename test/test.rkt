#lang racket

(require "main.rkt"
         "structs.rkt")

(require db json web-server/http web-server/servlet web-server/servlet-env)


(define dbc
  (sqlite3-connect #:database 'memory))

(query-exec dbc "CREATE TABLE DonationRecipients ( ID INTEGER PRIMARY KEY,
                         Name TEXT NOT NULL,
                         Description TEXT NOT NULL,
                         HeaderName TEXT NOT NULL,
                         TaxID TEXT);")

(start-transaction dbc)
(for ([line 
       (string-split #<<END
INSERT INTO "DonationRecipients" ("ID","Name","Description","HeaderName","TaxID") VALUES (2,'CCM','CCM','Connecticut Council of Masajid, Inc.','06-1349917');
INSERT INTO "DonationRecipients" ("ID","Name","Description","HeaderName","TaxID") VALUES (3,'Hamden masjid','the building of the Hamden masjid','Islamic Center of Hamden, Inc. ','06-1578692');
INSERT INTO "DonationRecipients" ("ID","Name","Description","HeaderName","TaxID") VALUES (4,'Graveyard','CCM Graveyard Project','Connecticut Council of Masajid, Inc.','06-1349917');
INSERT INTO "DonationRecipients" ("ID","Name","Description","HeaderName","TaxID") VALUES (5,'Test Name here','Test Description','header','42424');
END
                     "\n")])
  (query-exec dbc line))
(commit-transaction dbc)



            
(define (url->request u [req-type #"GET"])
  (make-request req-type (string->url u) empty
                (delay empty) #f "1.2.3.4" 80 "4.3.2.1"))
  
(define p (create-api-manager "api/recips"
                              dbc
                              "DonationRecipients"
                              `(("ID" "Internal ID")
                                ("Name" "Recipient Name")
                                ("Description" "Description/Comment")
                                ("HeaderName" displayname "Display Name")
                                ("TaxID" "Tax ID"))
                              "ID"
                              ;#:fallback-request-handler (λ(req) (response/jsexpr "default"))
                              ))

(define q (create-api-manager "api/other"
                              dbc
                              "DonationRecipients"
                              `(("ID" "Internal ID")
                                ("Name" "Recipient Name")
                                ("Description" "Description/Comment")
                                ("HeaderName" displayname "Display Name")
                                ("TaxID" "Tax ID"))
                              "ID"
                              
                              #:response-wrapper
                              (λ (js crud-op)
                                (response/jsexpr (make-immutable-hasheq
                                                  `((meta . #hasheq((endpoint . "api/recips")))
                                                    (data . ,js)))))
                              
                              #:request-wrapper
                              (λ (req crud-op api-handler)
                                (printf "HERE ~v~n" crud-op)
                                (match crud-op
                                  ['read (response/jsexpr "whoa")]
                                  [_ (api-handler req)]))
                              
                              #:fallback-request-handler (api-dispatcher p)))


(serve/servlet (api-dispatcher q)
               #:servlet-regexp #rx"/api/"
               ;;#:server-root-path "."
               #:servlets-root "."
               #:extra-files-paths (list ".")
               #:launch-browser? #f)
