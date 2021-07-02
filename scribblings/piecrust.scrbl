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

@;@table-of-contents[]

@section{Quick Start}

Pie Crust provides a lightweight way to bind an SQL database table(s) to a REST-based API
providing responses in JSON format. It enables you to generate API endpoints for creating,
reading/querying, updating, and deleting data in the underlying table(s) by defining a
dispatching function (based on @racket[dispatch-rules]) that can be used with @racket[serve/servlet].

@subsection{Minimal Example}

(Running code for this example is in the @tt{piecrust/test/minimal-example.rkt} file.)

Consider an SQLite database set up like this:

@verbatim|{
>  CREATE TABLE Stores ( Store_id INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL,
                         Name TEXT NOT NULL,
                         Address TEXT);

   INSERT INTO Stores (Store_id, Name, Address) VALUES (1, 'Adli', '123 Main');
   INSERT INTO Stores (Store_id, Name, Address) VALUES (2, 'BeeJays', '456 First');
   INSERT INTO Stores (Store_id, Name) VALUES (3, 'Warmalt');                      
}|

The following code will launch a JSON-based API on @tt{http://localhost:8000}.

@#reader scribble/comment-reader
(racketblock
 (require piecrust)

 (define dbc (sqlite3-connect ...))

 (define store-api
   (create-api-manager "/api-endpoint"
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

There are several overloaded forms to specifying the column schema. See the full documentation
for @racket[create-api-manager] below for details.


@subsubsection{Generated endpoints and request formats}

With the server above running, the follow endpoints are provided:

@itemlist[
 @item{@element[(style #f (list hilite))]{@elem["(GET)" @hspace[5] @tt{/api-endpoint}]} @(linebreak)
        Retrieves all data records in the table as a list of JSON dictionaries.

        @tabular[#:style 'boxed
                 #:row-properties '(() bottom-border ())
                 (list (list @bold{Query Parameters} 'cont)
                       (list @bold{Name and value pattern} @bold{Example})
                       (list @tt{_fields=<field-name>,...} @tt{/api-endpoint?_fields=id,name})
                       (list "Retrieves only the specified fields from each record. Field names
                              are case-sensitive. If only one field is specified, then a list
                              of values is returned (rather than a list of dictionaries)." 'cont)
                       (list @~ 'cont)
                       (list @tt{_distinct=1} @tt{/api-endpoint?_distinct=1})
                       (list "Retrieves only unique records with the selected set of
                             fields (i.e. does not include duplicates)." 'cont)
                       (list @~ 'cont)
                       (list @tt{_sort=<field-name>} @tt{/api-endpoint?_sort=address})
                       (list @tt{_order=[asc|desc]} @tt{/api-endpoint?_sort=address&_order=desc})
                       (list "Sorts records by the specified field in ascending/descending order."
                             'cont)
                       (list @~ 'cont)
                       (list @tt{*=<data>} @tt{/api-endpoint?*=street})
                       (list "Searches for the specified data in any of the fields of each record.
                             If <data> contains an asterisk (*), it is treated as a wildcard
                             (corresponding to % in SQL syntax). The asterisk is used because
                             escaping % in a URL can be annoying." 
                             'cont)
                       (list @~ 'cont)
                       (list @tt{<field-name>=<data>} @tt{/api-endpoint?name=LLC})
                       (list "Searches for the specified data in the named field. If multiple fields
                              are specified as separate query parameters, the criteria are combined
                              with AND. If a * query parameter is specified, any named fields
                              are ignored. See the note above about the wildcard character in
                              <data>."
                             'cont)
                       (list @~ 'cont)
                       (list @tt{_joins=1} @tt{/api-endpoint?_joins=1})
                       (list @elem{Includes nested data from joined tables in the output. By
                    default, joins are not performed and included.
                    See @elemref["joins-details"]{the details on the @racket[#:joins] clause}.}
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
        in the @bold{body} of the @tt{POST} request. For example:
  @verbatim|{
    { "name" : "CoCost", "address" : "987 Center Lane" }
    }|

       Upon success, responds with a dictionary containing the primary key field and
       value of the newly added row.       
  @(linebreak)}

 @item{@element[(style #f (list hilite))]{@elem["(PUT)" @hspace[5] @tt{/api-endpoint/<id>}]}
        @(linebreak)
        Updates the row identified by the specified primary key value (@tt{<id>}).
        A JSON dictionary must be provided in the @bold{body} of the @tt{POST} request with
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
 @item{Use @racket[api-dispatcher] procedure to extract a request handling function from the
   API generated by @racket[create-api-manager], suitable for use to launch a server.}
 ]





@subsection{Extended Example}

(Running code for this example is in the @tt{piecrust/test/shopping-list-example.rkt} file.)

Consider an SQLite database set up with two tables like this:

@verbatim|{
>  CREATE TABLE Stores ( Store_id INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL,
                         Name TEXT NOT NULL,
                         Address TEXT);

   CREATE TABLE Items ( ID INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL,
                         Store_id INTEGER NOT NULL,
                         Name TEXT NOT NULL,
                         Brand TEXT,
                         FOREIGN KEY (Store_id) REFERENCES Stores (Store_id));                         

   INSERT INTO Stores (Store_id, Name, Address) VALUES (1, 'Adli', '123 Main');
   INSERT INTO Stores (Store_id, Name, Address) VALUES (2, 'BeeJays', '456 First');
   INSERT INTO Stores (Store_id, Name) VALUES (3, 'Warmalt');                      

   INSERT INTO Items (ID, Store_id, Name, Brand) VALUES (101, 2, 'Chips', "Louie's");
   INSERT INTO Items (ID, Store_id, Name, Brand) VALUES (102, 1, 'Donuts', 'Delites');
   INSERT INTO Items (ID, Store_id, Name, Brand) VALUES (103, 1, 'Eggs', 'Fresh');
   INSERT INTO Items (ID, Store_id, Name, Brand) VALUES (104, 3, 'Flour', "Baker's");
   INSERT INTO Items (ID, Store_id, Name) VALUES (105, 2, 'Grapes');
}|


The following code will set up and
launch a JSON-based API on @tt{http://localhost:8000}.

@#reader scribble/comment-reader
(racketblock
(require piecrust)

(define dbc (sqlite3-connect ...))

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


(serve/servlet (api-dispatcher items-api)
                 #:servlet-regexp #rx"" #:servlet-path "/" #:launch-browser? #f)
)


@subsubsection[#:tag "hooks"]{API hooks}

The extended example above demonstrates several hooks that are supplied with
the @racket[create-api-manager] form.

@itemlist[
 @item{Request handling functions can be chained together using the
  @racket[#:fallback-request-handler]. In this case, the @tt{Items} API endpoint path is
 handled first. If the request URL path does not match that, it will be delegated to the
 @racket[(api-dispatcher store-api)] handler. If the request URL path does not match
  @racket["api/stores"] either, then @racket[main-servlet] will be used to handle the request.}

 @item{The @racket[#:request-wrapper] option provides a hook to install a function that can
  examine and build a response to the request prior to the generated API handlers. The supplied
 function takes the original request instance, a @racket[crud-op/c] symbol, and a callback
  function (which is the one generated by the API to handle that type of request). The supplied
 function may decide to either (1) produce its own response (perhaps based on authentication results, or,
 like in this example, to deny access to the @racket['delete] functionality of the @tt{Stores}
 API), (2) construct a modified request object, or (3) pass the request as it on to the
 default API handler function.}

 @item{In a similar vein, the @racket[#:response-wrapper] hook allows one to supply a function
 that can examine the final JSON result produced by the API and decide how to modify it, if
 desired, to produce the final @racket[response?]. In the example, the @tt{Items} API wraps all
 responses to the CRUD operations in another layer of JSON with some additional metadata.}

 @item{The generated API will handle @racket[exn:fail:api:error] and @racket[exn:fail:sql]
  exceptions with a generic response (and a 400 or 500 code, respectively). To construct an
  alternate error response, the @racket[#:error-wrapper] option allows the user to supply
  a function that takes the exception object, the CRUD operation that was being handled,
  and the response code and message that the API generated, and can decide how to construct
  and produce their own response.}
 ]




@section{API Documentation}

@defthing[#:kind "procedure" crud-op/c contract? #:value (symbols 'create 'query 'read 'update 'delete)]

@defform[#:literals (endpoint-path database-conn table-name columns primary-key)
         (create-api-manager (code:line)
                             [endpoint-path string?]
                             [database-conn connection?]
                             [table-name string?]
                             [columns [listof col-schema]]
                             [primary-key string?]
                             joins-clause
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
          (joins-clause (code:line)
                        (code:line #:joins joins-list))
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

          [joins-list (listof join-spec/c)]
          [disp-fun  (request? . -> . any)]
          [pre-fun   (request? crud-op/c (request? . -> . response?) . -> . response?)]
          [post-fun  (jsexpr? crud-op/c . -> . response?)]
          [err-fun   (exn? crud-op/c response-code/c jsexpr? . -> . response?)]
          [gen-fun   (string? . -> . symbol?)]
          )]{
 Produces an API manager instance.

 The @code{endpoint-path} should be a slash-separated path string specifying the base
 endpoint from which the API will be served, for example, @racket["/myapi/v1"].

The most comprehensive form of the column schema is:
 @defform[#:literals (list)
          (list (list sql-column-name  json-key-name)
                sql-type  null-ok?  col-description)]

 @emph{sql-column-name} should be the name of the column in the SQL table. @emph{json-key-name}
 will be used as the key label for matching fields in the JSON output. Requests to create,
 update, or query data must also use the JSON key name, as the SQL column name is only used
 internally in the API to bind to the database. If a @emph{json-key-name} is not specified,
 the function supplied to the @racket[#:field-name-generator] option is used to build the JSON
 key name from the SQL column name. The default generator is
 @racket[(compose string->symbol string-downcase)]. The @emph{null-ok?} parameter corresponds
 to (the absence of) a
 @tt{NOT NULL} constraint in the SQL table. If not specified, it is assumed to be @racket[#t].
 The column description is only used for metadata purposes, and if not specified is just the
 column name.

 See the @secref{hooks} section for descriptions of the optional keyword arguments.

 @elemtag["joins-details"]
 The @racket[#:joins] clause specifies additional tables to which the primary one may be
 joined, based on its primary key. It supports two scenarios: a simple join to another foreign
 table based on a matching foreign key; or a junction table.

 @defthing[#:kind "procedure"
           join-spec/c contract?
           #:value (or/c (list/c symbol? (list/c string? (listof col-schema)) string?)
                         (list/c symbol? (list/c string? (listof col-schema) string?)
                                 (list/c string? string?) string?))]

 For the simple join, specify the
 JSON key with which the joined data will be associated (as a nested JSON expression in the
 returned dictionary for each row of data), the name of the foreign table, a list of columns to
 include from matching rows in the foreign table, and the name of the foreign key.

 For a join that goes through a junction table, provide again the JSON key with which the data will
 be associated in the output; the name of the foreign table, its list of columns to select, and
 the primary key of the foreign table; and then the name of the junction table and the (foreign)
 key in the junction table that corresponds to the primary key of the foreign table; and, finally,
 the name of the (foreign) key in the junction table that corresponds to the primary key of the
 table associated with this endpoint.
 

}



@defproc[(api-dispatcher [an-api-manager api?])
         (request? . -> . any)]{
Produces the dispatch function for the given API manager instance (generated by
 @racket[create-api-manager]), suitable for use with @racket[serve/servlet].}


@defproc[(api-db [an-api-manager api?])
         connection?]{
Produces the database connection to which the given API manager instance is bound. }


@defstruct[(exn:fail:api:error exn:fail) ()]{
Represents an API error, for example, a missing field name in the body of a request, or an
invalid parameter.
}



