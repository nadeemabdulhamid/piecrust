#lang scribble/manual

@require[@for-label[piecrust
                    racket/base
                    web-server/dispatch
                    web-server/servlet-env]]

@title{Pie Crust}
@author[(author+email "Nadeem Abdul Hamid" "nadeem@acm.org")]

@defmodule[piecrust]

Pie Crust is an automated (RESTful) CRUD API generator for use with the library for
@other-manual['(lib "web-server/scribblings/web-server.scrbl")].

@bold{Warning:}
This library should be treated as an @emph{alpha} release. It has not been stress-tested or
bullet-proofed. Bug reports, feature requests, and pull requests are welcomed at the Github
repo.

@section{Quick Start}

Pie Crust provides a lightweight way to bind an SQL database table(s) to a REST-based API
providing responses in JSON format. It enables you to generate API endpoints for creating,
reading/querying, updating, and deleting data in the underlying table(s) by defining a
dispatching function (based on @racket[dispatch-rules]) that can be used with @racket[serve/servlet].

@subsection{Minimal Example}

Consider an SQLite database set up like this:

@verbatim|{
>  CREATE TABLE Stores ( Store_id INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL,
                         Name TEXT NOT NULL,
                         Address TEXT);

   INSERT INTO Stores (Store_id, Name, Address) VALUES (1, 'Adli', '123 Main');
   INSERT INTO Stores (Store_id, Name, Address) VALUES (2, 'BeeJays', '456 First');
   INSERT INTO Stores (Store_id, Name) VALUES (3, 'Warmalt');                      
}|

The following code will launch a JSON API on @tt{http://localhost:8000}.

@#reader scribble/comment-reader
(racketblock
 (require piecrust)
 (require db web-server/servlet-env)

 (define dbc (sqlite3-connect ...))

 (define store-api
   (create-api-manager ""           ; API endpoint
                       dbc          ; database connection
                       "Stores"     ; table name
                         ; table column schema
                       `((["Store_id" id] integer "Internal ID")  
                         ("Name" text #f "Store name")
                         ("Address" text))  
                       "Store_id")) ; primary key

 (serve/servlet (api-dispatcher store-api)
                #:servlet-regexp #rx""
                #:servlet-path "/")
 )




@section{API Documentation}


@defform[#:literals (endpoint-path database-conn table-name columns primary-key)
         (create-api-manager (code:line)
                             [endpoint-path string?]
                             [database-conn connection?]
                             [table-name string?]
                             [columns [listof col-schema]]
                             [primary-key string?]
                             fallback-handler-clause
                             request-wrapper-clause
                             response-wrapper-clause
                             error-wrapper-clause
                             field-name-generator-clause)
         #:grammar
         [(col-schema (code:line ([col-name json-key] sql-type null-ok? col-description))
                      (code:line (col-name sql-type null-ok? col-description))
                      (code:line ([col-name json-key] sql-type col-description))
                      (code:line ([col-name json-key] sql-type null-ok?))
                      (code:line (col-name sql-type col-description))
                      (code:line (col-name sql-type null-ok?))
                      (code:line (col-name sql-type)))
          (fallback-handler-clause (code:line)
                                   (code:line #:fallback-request-handler disp-fun))
          (request-wrapper-clause (code:line)
                                   (code:line #:request-wrapper pre-fun))
          (response-wrapper-clause (code:line)
                                   (code:line #:response-wrapper post-fun))
          (error-wrapper-clause (code:line)
                                   (code:line #:error-wrapper err-fun))
          (field-name-generator-clause (code:line)
                                   (code:line #:field-name-generator gen-fun))]
         #:contracts
         ([col-name string?]
          [json-key symbol?]
          [sql-type (or/c 'text 'integer)]
          [null-ok? boolean?]
          [col-description string?]
          
          [disp-fun  (request? . -> . any)]
          [pre-fun   (request? crud-op/c (request? . -> . response?) . -> . response?)]
          [post-fun  (jsexpr? crud-op/c . -> . response?)]
          [err-fun   (exn? crud-op/c response-code/c jsexpr? . -> . response?)]
          [gen-fun   (string? . -> . symbol?)]
          )]{
 Produces an API manager instance.

 The @code{endpoint-path} should be a slash-separated path string specifying the endpoint
 from which the API will be served, for example, @racket["/myapi/v1"].

}



@defproc[(api-dispatcher [an-api-manager api?])
         (request? . -> . any)]{
Produces the dispatch function for the given API manager instance, suitable for use with
@racket[serve/servlet].
}



