;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; entities.scm - Entities for postit
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

(library (postit entities)
  (export <entity>
	  <entity/date>
	  <user>
	  <state>
	  <postit>)
  (import (rnrs)
	  (rnrs eval)
	  (postit constants)
	  (cuberteria)
	  (dbi) ;; for dbi-parse-dsn
	  (srfi :19)
	  (clos user)
	  (maquette))

;; sequence generator
(define postit-generator
  (let-values (((driver op alist) (dbi-parse-dsn +dsn+)))
    (eval 'generator (environment `(postit ,(string->symbol driver))))))

;; base class
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

(define-class <state> (<entity>)
  ((name :init-keyword :name :sql-type '(varchar2 50)
	 :unique #t :not-null? #t))
  :metaclass <maquette-table-meta>)

(define-class <postit> (<entity/date>)
  ((user :init-keyword :user :foreign-key (list <user> 'id) :not-null? #t
	 :column-name "userid"
	 :->json cuberteria-object->json)
   (postit :init-keyword :postit :sql-type 'clob
	   :json-element-name "note")
   (state :init-keyword :state :foreign-key (list <state> 'id) :not-null? #t
	  :column-name "stateid"
	  :->json cuberteria-object->json))
  :metaclass <maquette-table-meta>)
  )
