#lang scribble/manual

@require[scribble/core scribble/html-properties]

@require[@for-label[piecrust
                    racket/base
                    web-server/dispatch
                    web-server/servlet-env]]

@(define target-blank
   (style #f (list (attributes '((target . "_blank"))))))

@(define hilite
   (background-color-property "cyan"))


@title{Pie Crust}
@author[(author+email "Nadeem Abdul Hamid" "nadeem@acm.org")]

@defmodule[piecrust]

Pie Crust is an automated (RESTful)
@hyperlink["https://en.wikipedia.org/wiki/Create,_read,_update_and_delete" #:style target-blank]{CRUD}
API generator for use with the library for
@other-manual['(lib "web-server/scribblings/web-server.scrbl")].

@bold{Warning:}
This library should be treated as an @emph{alpha} release. It has not been stress-tested or
bullet-proofed. Bug reports, feature requests, and pull requests are welcomed at the
@hyperlink["https://github.com/nadeemabdulhamid/piecrust" #:style target-blank]{Github repo}.

In particular:
@itemlist[
 @item{This library has only been tested/used with small SQLite databases.}
 @item{It assumes either @tt{TEXT} or @tt{INTEGER} fields.
  Conversion to and from JSON and SQL data types is minimal to non-existent at this point.}
 @item{Although there shouldn't be any (i.e. properly parameterized queries are generated and
  used), it has not been thoroughly vetted for safety from injection attacks.}
 @item{The first time you try to use it, something's bound to not work. (One cannot be
 too pessimistic.)}
 ]

With all that said, feel free to try it out and @bold{have fun}! Use cases and ideas for
improvement are welcome, as noted above.

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

With this server running, the follow endpoints are provided:

@itemlist[
 @item{@element[(style #f (list hilite))]{@elem["(GET)" @hspace[5] @tt{/api-endpoint}]} @(linebreak)
        Retrieves all data records in the table as a list of JSON dictionaries.

        @tabular[#:style 'boxed
                 #:row-properties '(() bottom-border ())
                 (list (list @bold{Query Parameters} 'cont)
                       (list @bold{Name and value pattern} @bold{Example})
                       (list @tt{_fields=<field-name>,...} @tt{/api-endpoint?_fields=id,name})
                       (list "Retrieves only the specified fields from each record. Field names
                              are case-sensitive." 'cont)
                       (list @~ 'cont)
                       (list @tt{_sort=<field-name>} @tt{/api-endpoint?_sort=address})
                       (list @tt{_order=[asc|desc]} @tt{/api-endpoint?_sort=address&_order=desc})
                       (list "Sorts records by the specified field in ascending/descending order."
                             'cont)
                       (list @~ 'cont)
                       (list @tt{*=<data>} @tt{/api-endpoint?*=street})
                       (list "Searches for the specified data in any of the fields of each record."
                             'cont)
                       (list @~ 'cont)
                       (list @tt{<field-name>=<data>} @tt{/api-endpoint?name=LLC})
                       (list "Searches for the specified data in the named field. If multiple fields
                              are specified as separate query parameters, the criteria are combined
                              with AND. If a * query parameter is specified, any named fields
                              are ignored."
                             'cont)
                    )]
  @(linebreak)}

  @item{@element[(style #f (list hilite))]{@elem["(GET)" @hspace[5] @tt{/api-endpoint/<id>}]}
        @(linebreak)
        Retrieves the single data record with the given primary key value (@tt{<id>}
        as a JSON dictionary.

        @tabular[#:style 'boxed
                 #:row-properties '(() bottom-border ())
                 (list (list @bold{Query Parameters} 'cont)
                       (list @bold{Name and value pattern} @bold{Example})
                       (list @tt{_fields=<field-name>,...} @tt{/api-endpoint/42?_fields=name,address})
                       (list "Retrieves only the specified fields. Field names
                              are case-sensitive." 'cont))]
  @(linebreak)}

  @item{@element[(style #f (list hilite))]{@elem["(POST)" @hspace[5] @tt{/api-endpoint}]}
        @(linebreak)
        Adds a new row to the database table. A JSON dictionary must be provided
        in the body of the @tt{POST} request. For example:
  @verbatim|{
    { "name" : "CoCost", "address" : "987 Center Lane" }
    }|

       Upon success, responds with a dictionary containing the primary key field and
       value of the newly added row.       
  @(linebreak)}

 @item{@element[(style #f (list hilite))]{@elem["(PUT)" @hspace[5] @tt{/api-endpoint/<id>}]}
        @(linebreak)
        Updates the row identified by the specified primary key value (@tt{<id>}).
        A JSON dictionary must be provided in the body of the @tt{POST} request with
        the fields and values that are to be updated.
  @(linebreak)}
               
 @item{@element[(style #f (list hilite))]{@elem["(DELETE)" @hspace[5] @tt{/api-endpoint/<id>}]}
        @(linebreak)
        Deletes the row identified by the specified primary key value (@tt{<id>}).
  @(linebreak)}          
]




@subsubsection{Notes on the generated API}

@itemlist[
 @item{The default for the JSON keys generated and used for query parameters is to
      lowercase the specified SQL column names (e.g. @tt{Address} becomes @tt{address}).}
 @item{On a @tt{POST} request to create a new record, the primary key field and its value
  is ignored if it is specified in the JSON dictionary in the body of the request.}
 ]




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



