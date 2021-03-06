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
	    (srfi :13)
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

  (define-syntax with-path-variable
    (syntax-rules ()
      ((_ "gen" regexp (b ...) ((v d n) ...))
       (let ((m regexp))
	 (let ((v (if m (m n) d)) ...)
	   b ...)))
      ((_ "count" regexp () (b ...) (t ...) n)
       (with-path-variable "gen" regexp (b ...) (t ...)))
      
      ((_ "count" regexp ((v d) v* ...) (b ...) (t ...) n)
       (with-path-variable "count" regexp (v* ...) (b ...)
			   ((v d (+ n 1)) t ...) (+ n 1)))
      
      ((_ regexp (variables ...) body ...)
       (with-path-variable "count" regexp (variables ...) (body ...) () 0))))
  (define +anonymous+ "anonymous")

  (define (return-json obj)
    (define (json->string json)
      (call-with-string-output-port
       (lambda (out)
	 (json-write json out))))
    
    (parameterize ((*json-map-type* 'alist))
      (values 200 'application/json (json->string obj))))
  
  (define (object->json o) (cuberteria-object->json o 'alist))
  
  (define (postit-loader req)
    (define uri (http-request-uri req))    
    (with-path-variable (#/load-postit\/(.+)/ uri) ((user +anonymous+))
      (define u (make <user> :username user))
      (let ((r (maquette-query maquette-context <postit> `(= user ,u))))
	(return-json (list->vector (map object->json r))))))

  (define (utf8->integer bv) (string->number (utf8->string bv)))
  ;; this now does update as well.
  (define-class <create-request> (<converter-mixin>)
    (username
     note))
  
  (define create-postit
    (cuberteria-object-mapping-handler
     <create-request>
     (lambda (request req)
       (let* ((user (car (maquette-query maquette-context <user>
			   `(= username ,(slot-ref request 'username)))))
	      (r (make <postit>
		   :user user
		   :state (make <state> :name "New" :id 0) ;; FIXME
		   :postit (slot-ref request 'note))))
	 (with-maquette-transaction maquette-context
				    (maquette-save maquette-context r))
	 (return-json
	  (object->json 
	   (car (maquette-query maquette-context <postit>
				`(= id ,(slot-ref r 'id))))))))
     :json? #t))

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

  (define +login+ "/postit/login")
  (define (dashboard username)
    (string-append "/postit/dashboard/" username))

  (define (retrieve-credential username)
    (and-let* ((r (maquette-query maquette-context <user>
				  `(= username ,username)))
	       ( (not (null? r)) ))
      (slot-ref (car r) 'password)))

  (define authorisation-handler
    (plato-session-handler
     (cuberteria-object-mapping-handler <username&password>
      (lambda (request req)
	(define (handle-login request username)
	  (plato-session-set! (*plato-current-session*) :auth-token
	    (generate-authorisation-token request
	      (session->salt  (*plato-current-session*))))
	  ;; put username for convenience
	  (plato-session-set! (*plato-current-session*) :username username)
	  (values 302 'text/plain (dashboard username)))
       (cond ((string=? (slot-ref request 'username) "anonymous")
	      (handle-login req "anonymous"))
	     ((authorise (slot-ref request 'username)
			 (slot-ref request 'password)
			 (retrieve-credential (slot-ref request 'username)))
	      (handle-login req (slot-ref request 'username)))
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

  (define-class <userinfo> ()
    ((firstnames :converter utf8->string)
     (middlename :converter utf8->string)
     (lastname :converter utf8->string)
     (email :converter utf8->string)))
  (define-class <create-user-request> ()
    ((username :converter utf8->string)
     (password :converter utf8->string)
     (info :json <userinfo>)))
  
  (define create-user-handler
    (cuberteria-object-mapping-handler  <create-user-request>
      (lambda (request raw-request)
	(define username (slot-ref request 'username))
	(define password (slot-ref request 'password))
	(define info (slot-ref request 'info))
	(define (get field)
	  (if (slot-bound? info field)
	      (slot-ref info field)
	      ;; () is the null
	      '())) 
	
	(or (and-let* (( (not (string-null? username)) )
		       ( (not (string-null? password)) )
		       (credential (create-credential username password)))
	      (guard (e (else
			 ;; TODO better error reporting
			 (return-json
			  `((error . ,(format "Failed to create a user"))))))
		(with-maquette-transaction maquette-context
		  (let ((user (maquette-add maquette-context 
					    (make <user> :username username
						  :password credential))))
		    (maquette-add maquette-context
				  (make <user-info>
				    :user user
				    :first-names (get 'firstnames)
				    :middle-name (get 'middlename)
				    :last-names (get 'lastname)
				    :email (get 'email)))))
		(return-json '((success . #t)))))
	    (and (string-null? username)
		 (return-json '((error . "Username is empty"))))
	    (and (string-null? password)
		 (return-json '((error . "Password is empty"))))
	    (return-json '((error . "Something went wrong")))))
      :json? #t))
    
  
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
      ((GET)  #/dashboard/ ,(session-expired-redirect-handler
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
       (plato-session-delete! (*plato-current-session*) :username)
       (values 302 'text/plain +login+))))

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
	  (let ((token (plato-session-ref session :auth-token))
		(user (plato-session-ref session :username)))
	    (if (and (validate-authorisation-token req salt token) user)
		(values 302 'text/plain (dashboard user))
		(login req))))))))
)
