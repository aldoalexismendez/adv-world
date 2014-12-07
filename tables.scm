;; Section 3.3.3 -- Tables

;;; One-dimensional tables

(define (lookup key table)
  (let ((record (assoc key (cdr table))))
    (if (not record)
        #f
        (cdr record))))

(define (insert! key value table)
  (let ((record (assoc key (cdr table))))
    (if (not record)
        (set-cdr! table
                  (cons (cons key value) (cdr table)))
        (set-cdr! record value)))
  'ok)

(define (make-table)
  (list '*table*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; two dimensional tables from SICP          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#|

(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (cdr record)
                  false))
            false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list key-1
                                  (cons key-2 value))
                            (cdr local-table)))))
      'ok)    
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation - TABLE" m))))
    dispatch))

(define (lookup key-1 key-2 table)
  (let ((subtable (assoc key-1 (cdr table))))
    (if subtable
        (let ((record (assoc key-2 (cdr subtable))))
          (if record
              (cdr record)
              false))
        false)))

(define (insert! key-1 key-2 value table)
  (let ((subtable (assoc (key-1 (cdr table)))))
    (if subtable
	(let ((record (assoc key-2 (cdr subtable)))
	      (if record
		  (set-cdr! record value)
		  (set-cdr! subtable
			    (cons (cons key-2 value)
				  (cdr subtable))))))
	(set-cdr! table
		  (cons (list key-1
			      (cons key-2 value))
			(cdr table)))))
    'ok)
  
;; Operation, type -> procedure
;; Dispatch table.
;; 
#|

(define (put op type proc)
  (hash-table-put! *op-table* (list op type) proc))

(define (get op type)
  (hash-table-get *op-table* (list op type) '()))

|#
