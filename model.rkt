#lang racket/base

; A db is a (db (lockers locks))
; where lockers is a (listof locker) and
; locks is a (listof locks)
(struct db (home lockers locks) #:mutable #:prefab)

(struct locker
  (number
   [lock #:mutable]
   location
   [owner #:mutable]
   [note #:mutable]) #:prefab) ; Can be broken/inaccessible/occupied

(struct lock
  (number
   combination
   [status  #:mutable]
   note) #:prefab)

(struct owner
  (email
   last-name
   first-name
   program
   year
   note) #:prefab)


; initialize-db! : path? -> db
; Reads a database from a path, if not present, returns default
(define (initialize-db! home)
  (define (log-missing-exn-handler exn)
    (db
     (path->string home)
     '()
     '()))
  (define the-db
    (with-handlers ([exn? log-missing-exn-handler])
      (with-input-from-file home read)))
  (set-db-home! the-db (path->string home))
  the-db)

; save-db! : db -> void
; Saves the contents of a db to its home
(define (save-db! a-db)
  (define (write-to-db)
    (write a-db))
  (with-output-to-file (db-home a-db)
    write-to-db
    #:exists 'replace))

;; Tools

; Checks if a locker or lock  is in the database
; kind: 'locker or 'lock
; n: lock/locker number, a string
; a-list: a list of locker or lock structs
; returns: #t or #f
(define (in-db? kind n a-list)
  (cond ((null? a-list) #f)
        ((or (and (eq? kind 'locker) (string=? n (locker-number (car a-list))))
             (and (eq? kind 'lock) (string=? n (lock-number (car a-list))))) #t)
        (else (in-db? kind n (cdr a-list)))))

(define (kind->string kind)
  (if (eq? kind 'locker) "locker "
      "lock "))

;(define (my-remove kind number a-db)
;  (cond ((null? a-db) '())
;        ((or (and (eq? kind 'locker) (string=? number (locker-number (car a-db))))
;             (and (eq? kind 'lock) (string=? number (lock-number (car  a-db))))) (cdr a-db))
;        (else (cons (car a-db) (my-remove kind number (cdr a-db))))))

(define (my-get kind number a-list)
  (cond ((or (and (eq? kind 'locker) (string=? number (locker-number (car a-list))))
             (and (eq? kind 'lock) (string=? number (lock-number (car a-list)))))
         (car a-list))
        (else (my-get kind number (cdr a-list)))))

;; Functions that change the state of the database

(define (locker-insert-note! a-db  a-locker note)
  (set-locker-note! a-locker note)
  (save-db! a-db))

(define (locker-insert-owner! a-db a-locker owner)
  (set-locker-owner! a-locker owner)
  (save-db! a-db))

(define (insert-new-locker/lock! a-db kind item)
  (if (eq? kind 'locker) (set-db-lockers! a-db (cons item (db-lockers a-db)))
      (set-db-locks! a-db (cons item (db-locks a-db))))
  (save-db! a-db))

(define (assign-lock2locker! a-db  a-locker a-lock)
  (set-locker-lock! a-locker a-lock)
  (save-db! a-db))

(provide (all-defined-out))


