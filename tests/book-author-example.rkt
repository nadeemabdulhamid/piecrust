#lang racket

;; This example demonstrates the 3 types of joins

(require piecrust)

(require db
         json
         web-server/http
         web-server/servlet
         web-server/servlet-env
         web-server/dispatchers/dispatch)


(define (setup-database dbc)
  (query-exec dbc "DROP TABLE IF EXISTS Authors")
  (query-exec dbc "DROP TABLE IF EXISTS Books")
  (query-exec dbc "DROP TABLE IF EXISTS Book_Author")
  (query-exec dbc "DROP TABLE IF EXISTS Publishers")
  
  (query-exec dbc "CREATE TABLE Authors ( ID INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL,
                         Name TEXT);")

  (query-exec dbc "CREATE TABLE Books ( ID INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL,
                         Title TEXT, PubId INTEGER);")

  (query-exec dbc "CREATE TABLE BookAuthor ( ID INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL,
                         Author_ID INTEGER, Book_ID INTEGER);")

  (query-exec dbc "CREATE TABLE Publishers ( ID INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL,
                         CompanyName TEXT);")

  (for ([line
         (string-split #<<END
INSERT INTO Authors (ID, Name) VALUES (1, 'Eve Sutton');
INSERT INTO Authors (ID, Name) VALUES (2, 'Lynley Dodd');
INSERT INTO Authors (ID, Name) VALUES (3, 'J. R. R. Tolkien');
INSERT INTO Publishers (ID, CompanyName) VALUES (21, 'Houghton Mifflin');
INSERT INTO Publishers (ID, CompanyName) VALUES (31, 'Mallinson Rendel');
INSERT INTO Books (ID, Title, PubId) VALUES (100, 'The Hobbit', 21);
INSERT INTO Books (ID, Title, PubId) VALUES (101, 'Slinky Malinki', 31);
INSERT INTO Books (ID, Title, PubId) VALUES (102, 'My Cat Likes to Hide in Boxes', 31);
INSERT INTO Books (ID, Title, PubId) VALUES (103, 'Bad Entry', 0);
INSERT INTO Books (ID, Title) VALUES (104, 'Really bad Entry');
INSERT INTO BookAuthor (Author_ID, Book_ID) VALUES (2, 101);
INSERT INTO BookAuthor (Author_ID, Book_ID) VALUES (2, 102);
INSERT INTO BookAuthor (Author_ID, Book_ID) VALUES (1, 102);
INSERT INTO BookAuthor (Author_ID, Book_ID) VALUES (3, 100);
END
                       "\n")])
    (query-exec dbc line))
  
  dbc)


(define (go-live disp-func)
  (serve/servlet disp-func
                 #:servlet-regexp #rx"" #:servlet-path "/" #:launch-browser? #f))


(define dbc (sqlite3-connect #:database 'memory))
(setup-database dbc)


(define book-api
  (create-api-manager "books" ; API endpoint
                      dbc          ; database connection
                      "Books"     ; table name
                      `(("ID" integer)  
                        ("Title" text))
                      "ID"       ; primary key

                      #:joins `(; many-to-many through junction table
                                (authors ["Authors" (["Name" text]) "ID"]
                                         ["BookAuthor" "Author_ID"]
                                         "Book_ID")
                                
                                ; many-to-one through f-key in this table linked to p-key in secondary
                                ; result is bound to a dictionary, not a list
                                (publisher ["Publishers" (["CompanyName" text]) "ID"] "PubId"))))


(define pubs-api
  (create-api-manager "pubs"
                      dbc
                      "Publishers"
                      `(("ID" integer)
                        ("CompanyName" text))
                      "ID"

                      #:joins `(; one-to-many through f-key in the secondary table link to this one's p-key
                                (books ["Books" (["Title" text])] "PubId"))
                      
                      #:fallback-request-handler (api-dispatcher book-api)))


; to launch:
;     (go-live (api-dispatcher pubs-api))

; look at:
;     http://localhost:8000/books?_joins=1     (note: authors is a *list*, publisher is not)
;     http://localhost:8000/pubs?_joins=1

