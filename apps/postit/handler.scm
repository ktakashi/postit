;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; postit - Simple postit application
;;;  
;;;   Copyright (c) 2015-2016  Takashi Kato  <ktakashi@ymail.com>
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
	    (rnrs eval)
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
  (define-constant +auth+ (include "passwd"))

  (define script-loader (cuberteria-resource-loader 'text/javascript "./js"))
  (define style-loader (cuberteria-resource-loader 'text/css "./css"))
  (define png-loader (cuberteria-resource-loader 'image/png "./images"))
  
  (define postit-generator
    (let-values (((driver op alist) (dbi-parse-dsn +dsn+)))
      (eval 'generator (environment `(postit ,(string->symbol driver))))))

  (define-class <entity> ()
    ((id :init-keyword :id :primary-key #t :generator postit-generator))
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
     (top :init-keyword :top :sql-type 'integer :default 0
	  :column-name "x")
     (left :init-keyword :left :sql-type 'integer :default 0
	   :column-name "y")
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
  (define maquette-context 
    (apply make-maquette-context +dsn+ :auto-commit #f +auth+))

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
      
      (define anon (make <user> :id 0))
      (let ((r (maquette-query maquette-context <postit> `(= user ,anon))))
	(values 200 'application/json
		(json->string (list->vector 
			       (map cuberteria-object->json r)))))))

  (define (utf8->integer bv) (string->number (utf8->string bv)))
  (define-class <create-request> (<converter-mixin>)
    ((username :converter utf8->string)
     (note     :converter utf8->string)
     (text-color-id :converter utf8->integer)
     (bg-color-id :converter utf8->integer)))

  (define (create-postit req) 
    (define request (cuberteria-map-http-request! (make <create-request>) req))

    (parameterize ((current-directory 
		    (plato-current-path 
		     (plato-parent-context (*plato-current-context*)))))
      (let ((text-color (make <color> :id (slot-ref request 'text-color-id)))
	    (bg-color (make <color> :id (slot-ref request 'bg-color-id)))
	    (user (car (maquette-query maquette-context <user>
			       `(= username ,(slot-ref request 'username))))))
	(with-maquette-transaction maquette-context
	  (maquette-save maquette-context
			 (make <postit>
			   :user user
			   :text-color text-color
			   :bg-color bg-color
			   :postit (slot-ref request 'note))))
      (values 200 'text/plan "OK"))))

  (define-class <id-request> (<converter-mixin>)
    ((id   :converter utf8->integer)))

  (define-class <position-update-request> (<id-request> <converter-mixin>)
    ((top  :converter utf8->integer)
     (left :converter utf8->integer)))
  (define-method write-object ((o <position-update-request>) out)
    (format out "#<position-update-request id=~s top=~s left=~s>"
	    (slot-ref o 'id) (slot-ref o 'top) (slot-ref o 'left)))

  (define (update-position req) 
    (define request (cuberteria-map-http-request! 
		     (make <position-update-request>) req))

    (parameterize ((current-directory 
		    (plato-current-path 
		     (plato-parent-context (*plato-current-context*)))))
      (with-maquette-transaction maquette-context
        (maquette-save maquette-context
		       (make <postit>
			 :id (slot-ref request 'id)
			 :top (slot-ref request 'top)
			 :left (slot-ref request 'left))))
      (values 200 'text/plan "OK")))

  (define-class <size-update-request> (<id-request> <converter-mixin>)
    ((width  :converter utf8->integer)
     (height :converter utf8->integer)))
  (define-method write-object ((o <size-update-request>) out)
    (format out "#<size-update-request id=~s width=~s height=~s>"
	    (slot-ref o 'id) (slot-ref o 'width) (slot-ref o 'height)))

  (define (update-size req) 
    (define request (cuberteria-map-http-request! 
		     (make <size-update-request>) req))

    (parameterize ((current-directory 
		    (plato-current-path 
		     (plato-parent-context (*plato-current-context*)))))
      (with-maquette-transaction maquette-context
        (maquette-save maquette-context
		       (make <postit>
			 :id (slot-ref request 'id)
			 :width (slot-ref request 'width)
			 :height (slot-ref request 'height))))
      (values 200 'text/plan "OK")))

  (define-constant +sql:delete+ "delete from postit where id = ?")
  (define (remove-postit req)
    (define request (cuberteria-map-http-request! 
		     (make <id-request>) req))
    (parameterize ((current-directory 
		    (plato-current-path 
		     (plato-parent-context (*plato-current-context*)))))
      (with-maquette-transaction maquette-context
        (maquette-remove maquette-context
			 (make <postit>
			   :id (slot-ref request 'id))))
      (values 200 'text/plan "OK")))

  (define-constant +sql:colors+ "select id, rgb, color_name from colors")
  (define (list-colors req)
    (parameterize ((current-directory 
		    (plato-current-path 
		     (plato-parent-context (*plato-current-context*))))
		   (*json-map-type* 'alist))
      (define (json->string json)
	(call-with-string-output-port
	 (lambda (out)
	   (json-write json out))))

      (values 200 'application/json
	      (json->string (list->vector (map cuberteria-object->json
					       (maquette-query maquette-context
							       <color> #f)))))))

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
