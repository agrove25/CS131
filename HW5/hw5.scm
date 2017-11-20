#lang racket

(define (null-hd? obj)
	(and (not (null? obj)) (pair? obj) (eq? (car obj) (cdr obj)))
)

(define (listdiff? obj)
  (if (null-hd? obj) 
    #t
    (if (and (not (null? obj)) (pair? obj) (not (pair (car obj?))))
      (listdiff? (cons (cdr (car obj)) (cdr obj)))
      #f
    )
  )
)

(define (cons-ld obj listdiff)
  (if (listdiff? listdiff)
    (cons (cons obj (car listdiff) (cdr listdiff)))
    (error "Second arg must be a Listdiff.")
  )
)

