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

  ;; TODO I don't want to write SQL like this...
  (define-constant +sql:load-postit+
    "select p.id, p.postit, p.x, p.y, p.width, p.height, \
            tc.rgb, bc.rgb, p.create_date \
     from postit p \
     inner join colors tc on p.text_color_id = tc.id \
     inner join colors bc on p.bg_color_id = bc.id \
     where userid = 0")

  ;; TODO should we pool connections?
  (define (postit-loader req)
    (parameterize ((current-directory 
		    (plato-current-path 
		     (plato-parent-context (*plato-current-context*))))
		   (*json-map-type* 'alist))
      (define conn (dbi-connect +dsn+))
      (define (json->string json)
	(call-with-string-output-port
	 (lambda (out)
	   (json-write json out))))
      
      (define (timestamp->millisecond t)
	(let ((s (time-second t))
	      (n (time-nanosecond t)))
	  (+ (* s 1000) (div n 1000000))))

      ;; TODO user management
      (let* ((q (dbi-execute-query-using-connection! conn +sql:load-postit+))
	     (notes (dbi-query-map q
		      (match-lambda 
		       (#(id postit x y w h tc bc date)
			`((id     . ,id) 
			  ;; clob is may be text
			  (note   . ,(if (string? postit) 
					 postit
					 ;; i think should be string port.
					 (utf8->string 
					  (get-bytevector-all postit))))
			  (top    . ,x)
			  (left   . ,y)
			  (width  . ,w)
			  (height . ,h)
			  (textColor . ,tc)
			  (backgroundColor . ,bc)
			  (creationDate . ,(timestamp->millisecond date))))))))
	(dbi-close conn)
	(values 200 'application/json (json->string (list->vector notes))))))

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

  (define-class <position-update-request> (<converter-mixin>)
    ((id   :converter utf8->integer)
     (top  :converter utf8->integer)
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

  (define-class <size-update-request> (<converter-mixin>)
    ((id     :converter utf8->integer)
     (width  :converter utf8->integer)
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


  (define (mount-paths)
    `( 
      ((GET)  #/scripts/ ,script-loader)
      ((GET)  #/styles/  ,style-loader)
      ((GET)  #/styles\/images\/.+?\.png/  ,png-loader)
      ((GET)  "/load-postit" ,(plato-session-handler postit-loader))
      ((POST) "/create-postit" ,(plato-session-handler create-postit))
      ((POST) "/update-position" ,(plato-session-handler update-position))
      ((POST) "/update-size" ,(plato-session-handler update-size))
      ))
  (define (support-methods) '(GET))

  (define (main-handler req)
    (call-with-input-file "main.html" html->tapas-component))

  (define entry-point
    (tapas-request-handler main-handler))
)
