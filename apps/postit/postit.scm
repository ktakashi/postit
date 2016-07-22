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
	    (maquette query)
	    (maquette context)
	    (maquette connection)
	    (match)
	    (postit entities)
	    (postit constants)
	    (postit handlers)
	    (postit auth)
	    (text json)
	    (srfi :19)
	    (srfi :39)
	    (clos user))

  (define script-loader (cuberteria-resource-loader 'text/javascript "./js"))
  (define style-loader (cuberteria-resource-loader 'text/css "./css"))
  (define png-loader (cuberteria-resource-loader 'image/png "./images"))
  (define template-loader (cuberteria-resource-loader 'text/html "./tmpl"))

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

  (define create-postit
    (cuberteria-object-mapping-handler
     <create-request>
     (lambda (request req)
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
					  `(= id ,(slot-ref r 'id))))))))))))

  (define-class <id-request> (<converter-mixin>)
    ((id   :converter utf8->integer)))

  (define remove-postit
    (cuberteria-object-mapping-handler <id-request>
     (lambda (request req)
       (parameterize ((current-directory 
		       (plato-current-path 
			(plato-parent-context (*plato-current-context*)))))
	 (with-maquette-transaction maquette-context
	   (maquette-remove maquette-context
			    (make <postit>
			      :id (slot-ref request 'id))))
	 (values 200 'text/plan "OK")))))

  (define-class <username&password> (<converter-mixin>)
    ((username :converter utf8->string)
     (password :converter utf8->string)))

  (define +dashboard+ "/postit/dashboard")

  (define (retrieve-credential username)
    (and-let* ((r (maquette-query maquette-context <user>
				  `(= username ,username)))
	       ( (not (null? r)) ))
      (slot-ref (car r) 'password)))
	  
  (define authorisation-handler
    (plato-session-handler
     (cuberteria-object-mapping-handler <username&password>
      (lambda (request req)
	(define (handle-login request)
	  (plato-session-set! (*plato-current-session*) :auth-token
	    (generate-authorisation-token request
	      (session->salt  (*plato-current-session*))))
	  (values 302 'text/plain +dashboard+))
       (cond ((string=? (slot-ref request 'username) "anonymous")
	      (handle-login req))
	     ((authorise (slot-ref request 'username)
			 (slot-ref request 'password)
			 (retrieve-credential (slot-ref request 'username)))
	      (handle-login req))
	     (else
	      (values 200 'shtml
		      (tapas-render-component
		       (populate-error-message
			"Username or password is invalid"
			(login-handler req))))))))))

  (define (populate-error-message e component)
    (define (error-message? c)
      (and (is-a? c <tapas-component>)
	   (equal? "error_message" (slot-ref c 'id))))
    (let ((c (tapas-find-component error-message? component)))
      ;; FIXME
      (when c (tapas-add-components! c (format "~a" e)))
      (tapas-render-component component)))

  (define create-user-handler
    (cuberteria-object-mapping-handler  <username&password>
      (lambda (request raw-request)
	(define username (slot-ref request 'username))
	(define password (slot-ref request 'password))
	
	(let ((credential (create-credential username password)))
	  (guard (e (else
		     (values 200 'shtml
			     (populate-error-message
			      e (user-handler raw-request)))))
	    (with-maquette-transaction maquette-context
	      (maquette-add maquette-context 
			    (make <user> :username username
				  :password credential)))
	    ;; this would redirect to login anyway
	    (values 302 'text/plain +dashboard+))))))
    
  
  (define (mount-paths)
    `( 
      ((GET)  #/scripts/ ,script-loader)
      ((GET)  #/styles/  ,style-loader)
      ((GET)  #/styles\/images\/.+?\.png/  ,png-loader)
      ((GET)  #/templates/  ,template-loader)
      ((GET)  #/load-postit/ ,(plato-session-handler postit-loader))
      ((POST) "/create-postit" ,(session-expired-redirect-handler
				 "/postit/login" ;; TODO
				 create-postit))
      ((POST) "/remove-postit" ,(session-expired-redirect-handler
				 "/postit/login" ;; TODO
				 remove-postit))
      ((GET)  "/dashboard" ,(session-expired-redirect-handler
			     "/postit/login"
			     (tapas-request-handler main-handler)))
      ((POST) "/login" ,(redirect-handler authorisation-handler))
      ((GET) "/login" ,entry-point)
      ((GET POST) "/logout" ,(plato-session-handler logout-handler))
      ((GET)  "/user" ,(tapas-request-handler user-handler))
      ((POST) "/user" ,(redirect-handler create-user-handler))
      ))
  (define (support-methods) '(GET))

  (define logout-handler
    (redirect-handler
     (lambda (req)
       (plato-session-delete! (*plato-current-session*) :auth-token)
       (values 302 'text/plain +dashboard+))))

  (define (static-handler file)
    (lambda (req)
      (define root-path (plato-current-path (*plato-root-context*)))
      (call-with-input-file (build-path root-path file)
	html->tapas-component)))
  
  (define main-handler (static-handler "main.html"))
  (define login-handler (static-handler "login.html"))
  (define user-handler (static-handler "user.html"))
  
  (define entry-point
    (let ((login (tapas-request-handler login-handler)))
      (plato-session-handler
       (redirect-handler
	(lambda (req)
	  (define session (*plato-current-session*))
	  (define salt (session->salt session))
	  (let ((token (plato-session-ref session :auth-token)))
	    (if (validate-authorisation-token req salt token)
		(values 302 'text/plain +dashboard+)
		(login req))))))))
)
