;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; postit/sqlite3 - Id generator for SQLite3
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

(library (postit sqlite3)
    (export generator)
    (import (rnrs)
	    (maquette)
	    (maquette query)
	    (maquette tables)
	    (clos core)
	    (clos user)
	    (srfi :18))

;; generator takes 2 argument object and maquette connection
;; we can take table name from given object. the primary key
;; of the table is autoincreament thus we can retrieve the
;; current value from sqlite_sequence table. so all we need
;; to do is add 1 to that value.
;; NB: sqlite_sequence has name and seq column
;;     name = table name
;;     seq  = current number
(define-class <sqlite_sequence> ()
  ((name :sql-type 'varchar)
   (seq :sql-type 'integer))
  :metaclass <maquette-table-meta>)
(define generator
  (let ((mutex (make-mutex)))
    (lambda (object conn)
      ;; unfortunately we need global lock.
      (mutex-lock! mutex)
      (guard (e (else (mutex-unlock! mutex) (raise e)))
	(let* ((table (symbol->string (maquette-table-name (class-of object))))
	       (r (maquette-select conn <sqlite_sequence> `(= name ,table))))
	  (when (null? r)
	    ;; mis configuration
	    (mutex-unlock! mutex)
	    (assertion-violation 'sqlite-generator
				 "sqlite_sequence doesn't contain the table"
				 table))
	  (let ((seq (slot-ref (car r) 'seq)))
	    (mutex-unlock! mutex)
	    (+ seq 1)))))))
  

)

