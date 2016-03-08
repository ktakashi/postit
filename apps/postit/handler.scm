;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; postit - Simple postit application
;;;  
;;;   Copyright (c) 2015  Takashi Kato  <ktakashi@ymail.com>
;;;   
;;;   Redistribution and use in source and binary forms, with or without
;;;   modification, are permitted provided that the following conditions
;;;   are met:
;;;   
;;;   1. Redistributions of source code must retain the above copyright
;;;      notice, this list of conditions and the following disclaimer.
;;;  
;;;   2. Redistributions in binary form must reproduce the above copyright
;;;      notice, this list of conditions and the following disclaimer in the
;;;      documentation and/or other materials provided with the distribution.
;;;  
;;;   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;;;   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;;;   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;;   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;;;   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;;   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
;;;   TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;;;   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;;;   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;;   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;;   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;  

#!read-macro=sagittarius/regex
(library (plato webapp postit)
    (export entry-point support-methods mount-paths)
    (import (rnrs) 
	    (paella)
	    (tapas)
	    (plato)
	    (cuberteria)
	    (sagittarius)
	    (sagittarius regex)
	    (maquette)
	    (match)
	    (text json)
	    (dbi)
	    (srfi :19)
	    (srfi :39)
	    (clos user))

  (define-constant +dsn+ (include "dsn.dat"))

  (define script-loader (cuberteria-resource-loader 'text/javascript "./js"))
  (define style-loader (cuberteria-resource-loader 'text/css "./css"))
  (define png-loader (cuberteria-resource-loader 'image/png "./images"))
  
  ;; TODO make it generic
  (define-class <entity> ()
    ((id :init-keyword :id :primary-key #t 
	 :generator (maquette-generator "TODO")))
    :metaclass <maquette-table-meta>)

  (define (timestamp->millisecond t)
    (let ((s (time-second t))
	  (n (time-nanosecond t)))
      (+ (* s 1000) (div n 1000000))))
  (define-class <entity/date> (<entity>)
    ((create-date :init-keyword :create-date :column-name "create_date"
		  :sql-type 'timestamp :default 'current_timestamp
		  :json-element-name "creationDate"
		  :->json timestamp->millisecond))
    :metaclass <maquette-table-meta>)

  (define-class <user> (<entity/date>)
    ((username :init-keyword :username :sql-type '(varchar2 255) :unique #t
	       :not-null? #t)
     (password :init-keyword :password :sql-type '(varchar2 255) :not-null? #t))
    :metaclass <maquette-table-meta>
    :table-name 'users)

  (define-class <color> (<entity>)
    ((rgb :init-keyword :rgb :sql-type 'integer :unique #t :not-null? #t
	  :reader color-rgb)
     (name :init-keyword :name :sql-type '(varchar2 255) :column-name
	   "color_name"
	   :reader color-name))
    :metaclass <maquette-table-meta>
    :table-name 'colors)

  (define-class <postit> (<entity/date>)
    ((user :init-keyword :user :foreign-key (list <user> 'id) :not-null? #t
	   :column-name "userid"
	   :->json cuberteria-object->json)
     (postit :init-keyword :postit :sql-type 'clob
	     :json-element-name "note")
     (x :init-keyword :x :sql-type 'integer :default 0)
     (y :init-keyword :y :sql-type 'integer :default 0)
     (width :init-keyword :width :sql-type 'integer :default 300)
     (height :init-keyword :height :sql-type 'integer :default 300)
     (text-color :init-keyword :text-color :foreign-key (list <color> 'id)
		 :column-name "text_color_id"
		 :json-element-name "textColor"
		 :->json color-rgb)
     (bg-color :init-keyword :bg-color :foreign-key (list <color> 'id)
	       :column-name "bg_color_id"
	       :json-element-name "backgroundColor"
	       :->json color-rgb))
    :metaclass <maquette-table-meta>)

  ;; should we have this here?
  (define maquette-context (make-maquette-context +dsn+ :auto-commit #f))

  ;; TODO should we pool connections?
  (define (postit-loader req)
    (parameterize ((current-directory 
		    (plato-current-path 
		     (plato-parent-context (*plato-current-context*))))
		   (*json-map-type* 'alist))
      (define (json->string json)
	(call-with-string-output-port
	 (lambda (out)
	   (json-write json out))))
      
      (define user-template (make <user> :id 0))
      (let ((r (maquette-query maquette-context <postit> 
			       #f)))
	(values 200 'application/json
		(json->string (list->vector 
			       (map cuberteria-object->json r)))))))

  (define (utf8->integer bv) (string->number (utf8->string bv)))
  (define-class <create-request> (<converter-mixin>)
    ((username :converter utf8->string)
     (note     :converter utf8->string)
     (text-color-id :converter utf8->integer)
     (bg-color-id :converter utf8->integer)))

  (define-constant +sql:insert-postit+
    "insert into postit \
       (userid, postit, text_color_id, bg_color_id) \
     select u.id, ?, ?, ? \
     from users u where username = ?")
    
  (define (create-postit req) 
    (define request (cuberteria-map-http-request! (make <create-request>) req))

    (parameterize ((current-directory 
		    (plato-current-path 
		     (plato-parent-context (*plato-current-context*)))))
      (define conn (dbi-connect +dsn+))
      (let ((q (dbi-prepare conn +sql:insert-postit+)))
	(dbi-bind-parameter! q 4 (slot-ref request 'username))
	(dbi-bind-parameter! q 1 (slot-ref request 'note))
	;; FIXME maybe we should lookup color name here
	(if (slot-bound? request 'text-color-id)
	    (dbi-bind-parameter! q 2 (slot-ref request 'text-color-id))
	    (dbi-bind-parameter! q 2 1)) ;; black
	(if (slot-bound? request 'bg-color-id)
	    (dbi-bind-parameter! q 3 (slot-ref request 'bg-color-id))
	    (dbi-bind-parameter! q 3 0)) ;; white
	(dbi-execute! q)
	(dbi-commit! q)
	(dbi-close q))
      (dbi-close conn)
      (values 200 'text/plan "OK")))

  (define-class <id-request> (<converter-mixin>)
    ((id   :converter utf8->integer)))

  (define-class <position-update-request> (<id-request> <converter-mixin>)
    ((top  :converter utf8->integer)
     (left :converter utf8->integer)))
  (define-method write-object ((o <position-update-request>) out)
    (format out "#<position-update-request id=~s top=~s left=~s>"
	    (slot-ref o 'id) (slot-ref o 'top) (slot-ref o 'left)))

  (define-constant +sql:position-update+
    "update postit set x = ?, y = ? where id = ?")
  (define (update-position req) 
    (define request (cuberteria-map-http-request! 
		     (make <position-update-request>) req))

    (parameterize ((current-directory 
		    (plato-current-path 
		     (plato-parent-context (*plato-current-context*)))))
      (define conn (dbi-connect +dsn+))
      (let ((q (dbi-prepare conn +sql:position-update+)))
	(dbi-bind-parameter! q 1 (slot-ref request 'top))
	(dbi-bind-parameter! q 2 (slot-ref request 'left))
	(dbi-bind-parameter! q 3 (slot-ref request 'id))
	(dbi-execute! q)
	(dbi-commit! q)
	(dbi-close q))
      (dbi-close conn)
      (values 200 'text/plan "OK")))

  (define-class <size-update-request> (<id-request> <converter-mixin>)
    ((width  :converter utf8->integer)
     (height :converter utf8->integer)))
  (define-method write-object ((o <size-update-request>) out)
    (format out "#<size-update-request id=~s width=~s height=~s>"
	    (slot-ref o 'id) (slot-ref o 'width) (slot-ref o 'height)))

  (define-constant +sql:size-update+
    "update postit set width = ?, height = ? where id = ?")
  (define (update-size req) 
    (define request (cuberteria-map-http-request! 
		     (make <size-update-request>) req))

    (parameterize ((current-directory 
		    (plato-current-path 
		     (plato-parent-context (*plato-current-context*)))))
      (define conn (dbi-connect +dsn+))
      (let ((q (dbi-prepare conn +sql:size-update+)))
	(dbi-bind-parameter! q 1 (slot-ref request 'width))
	(dbi-bind-parameter! q 2 (slot-ref request 'height))
	(dbi-bind-parameter! q 3 (slot-ref request 'id))
	(dbi-execute! q)
	(dbi-commit! q)
	(dbi-close q))
      (dbi-close conn)
      (values 200 'text/plan "OK")))

  (define-constant +sql:delete+ "delete from postit where id = ?")
  (define (remove-postit req)
    (define request (cuberteria-map-http-request! 
		     (make <id-request>) req))
    (parameterize ((current-directory 
		    (plato-current-path 
		     (plato-parent-context (*plato-current-context*)))))
      (define conn (dbi-connect +dsn+))
      (let ((q (dbi-prepare conn +sql:delete+)))
	(dbi-bind-parameter! q 1 (slot-ref request 'id))
	(dbi-execute! q)
	(dbi-commit! q)
	(dbi-close q))
      (dbi-close conn)
      (values 200 'text/plan "OK")))

  (define-constant +sql:colors+ "select id, rgb, color_name from colors")
  (define (list-colors req)
    (parameterize ((current-directory 
		    (plato-current-path 
		     (plato-parent-context (*plato-current-context*))))
		   (*json-map-type* 'alist))
      (define conn (dbi-connect +dsn+))
      (define (json->string json)
	(call-with-string-output-port
	 (lambda (out)
	   (json-write json out))))
      (let* ((q (dbi-execute-query-using-connection! conn +sql:colors+))
	     (colors (dbi-query-map q
		      (match-lambda
		       (#(id rgb name)
			`((id  . ,id)
			  (rgb . ,rgb)
			  (name . ,name)))))))
	(dbi-close conn)
	(values 200 'application/json 
		(json->string (list->vector colors))))))

  (define (mount-paths)
    `( 
      ((GET)  #/scripts/ ,script-loader)
      ((GET)  #/styles/  ,style-loader)
      ((GET)  #/styles\/images\/.+?\.png/  ,png-loader)
      ((GET)  "/load-postit" ,(plato-session-handler postit-loader))
      ((GET)  "/colors" ,(plato-session-handler list-colors))
      ((POST) "/create-postit" ,(plato-session-handler create-postit))
      ((POST) "/update-position" ,(plato-session-handler update-position))
      ((POST) "/update-size" ,(plato-session-handler update-size))
      ((POST) "/remove-postit" ,(plato-session-handler remove-postit))
      ))
  (define (support-methods) '(GET))

  (define (main-handler req)
    (call-with-input-file "main.html" html->tapas-component))

  (define entry-point
    (tapas-request-handler main-handler))
)
