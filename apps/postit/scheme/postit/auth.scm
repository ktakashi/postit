;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; auth.scm - Authorisation
;;;  
;;;   Copyright (c) 2016  Takashi Kato  <ktakashi@ymail.com>
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
(library (postit auth)
  (export authorise
	  generate-authorisation-token
	  validate-authorisation-token
	  session->salt

	  create-credential
	  )
  (import (rnrs)
	  (paella)
	  (plato)
	  (math)
	  (sagittarius)
	  (sagittarius regex)
	  (rfc :5322))

  ;; not sure if we do like this...
  (define (session->salt sesion)
    (integer->bytevector
     (plato-session-created (*plato-current-session*))))
  
  (define (compact bv) (number->string (bytevector->uinteger bv) 32))
  (define (expand s) (uinteger->bytevector (string->number s 32)))
  
  (define (generate-authorisation-token request salt)
    (let* ((h (hash-algorithm SHA-1))
	   (bv (make-bytevector (hash-size h)))
	   ;; FIXME this isn't a good value
	   (ua (rfc5322-header-ref (http-request-headers request)
				   "user-agent")))
      (hash-init! h)
      (hash-process! h (string->utf8 (http-request-remote-address request)))
      (if ua
	  (hash-process! h (string->utf8 ua))
	  ;; FIXME should we do like this?
	  (hash-process! h (read-sys-random (* (hash-size h) 8))))
      (hash-process! h salt)
      (hash-done! h bv)
      (compact bv)))
  
  (define (validate-authorisation-token request salt value)
    (let ((expected (generate-authorisation-token request salt)))
      (equal? expected value)))
  
  (define (authorise username password credential)
    (cond ((and (string? credential)
		(#/([\w\d]+)\$([\w\d]+)\$([\w\d]+)/ credential))
	   => (lambda (m)
		(and-let* ((a (m 1))
			   (s (m 2))
			   (h (m 3))
			   ;; for now
			   ( (string=? a "sha1") )
			   (sv (expand s))
			   (in (%create-credential username password sv))
			   (hv (expand h)))
		  (bytevector=? in hv))))
	  (else #f)))

  (define-constant +algorithm+ "sha1")
  (define (%create-credential username password salt)
    (let* ((h (hash-algorithm SHA-1))
	   (bv (make-bytevector (hash-size h))))
      (hash-init! h)
      (hash-process! h salt)
      (hash-process! h (string->utf8 username))
      (hash-process! h (string->utf8 password))
      (hash-done! h bv)
      bv))
  (define (create-credential username password)
    (let ((salt (read-sys-random (* (hash-size SHA-1) 8)))
	  (h (%create-credential username password salt)))
      (format "~a~$~a$~a" +algorithm+ (compact salt) (compact h))))
    
)
