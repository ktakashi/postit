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
	    (postit entities)
	    (postit constants)
	    (text json)
	    (srfi :19)
	    (srfi :39)
	    (clos user))

  (define script-loader (cuberteria-resource-loader 'text/javascript "./js"))
  (define style-loader (cuberteria-resource-loader 'text/css "./css"))
  (define png-loader (cuberteria-resource-loader 'image/png "./images"))

  ;; should we have this here?
  (define maquette-context 
    (apply make-maquette-context +dsn+ :auto-commit #f +auth+))

  (define (json->string json)
    (call-with-string-output-port
     (lambda (out)
       (json-write json out))))

  (define (postit-loader req)
    (parameterize ((current-directory 
		    (plato-current-path 
		     (plato-parent-context (*plato-current-context*))))
		   (*json-map-type* 'alist))      
      (define anon (make <user> :id 0))
      (let ((r (maquette-query maquette-context <postit> `(= user ,anon))))
	(values 200 'application/json
		(json->string (list->vector 
			       (map cuberteria-object->json r)))))))

  (define (utf8->integer bv) (string->number (utf8->string bv)))
  ;; this now does update as well.
  (define-class <create-request> (<converter-mixin>)
    ((id       :converter utf8->integer)
     (username :converter utf8->string)
     (note     :converter utf8->string)))

  (define (create-postit req) 
    (define request (cuberteria-map-http-request! (make <create-request>) req))

    (parameterize ((current-directory 
		    (plato-current-path 
		     (plato-parent-context (*plato-current-context*))))
		   (*json-map-type* 'alist))
      (let* ((user (car (maquette-query maquette-context <user>
			  `(= username ,(slot-ref request 'username)))))
	     (r (make <postit>
		  :user user
		  :state (make <state> :name "New" :id 0) ;; FIXME
		  :postit (slot-ref request 'note))))
	(with-maquette-transaction maquette-context
				   (maquette-save maquette-context r))
	(values 200 'application/json 
		(json->string 
		 (cuberteria-object->json
		  (car (maquette-query maquette-context <postit>
				       `(= id ,(slot-ref r 'id))))))))))

  (define-class <id-request> (<converter-mixin>)
    ((id   :converter utf8->integer)))

  (define-class <position-update-request> (<id-request> <converter-mixin>)
    ((top  :converter utf8->integer)
     (left :converter utf8->integer)))
  (define-method write-object ((o <position-update-request>) out)
    (format out "#<position-update-request id=~s top=~s left=~s>"
	    (slot-ref o 'id) (slot-ref o 'top) (slot-ref o 'left)))

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

  (define (mount-paths)
    `( 
      ((GET)  #/scripts/ ,script-loader)
      ((GET)  #/styles/  ,style-loader)
      ((GET)  #/styles\/images\/.+?\.png/  ,png-loader)
      ((GET)  "/load-postit" ,(plato-session-handler postit-loader))
      ((POST) "/create-postit" ,(plato-session-handler create-postit))
      ((POST) "/remove-postit" ,(plato-session-handler remove-postit))
      ))
  (define (support-methods) '(GET))

  (define (main-handler req)
    (call-with-input-file "main.html" html->tapas-component))

  (define entry-point
    (tapas-request-handler main-handler))
)
