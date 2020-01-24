; Run using mit-scheme --load test.scm
(define (win-prob elo_1 elo_2)
  (let* ((diff (- elo_1 elo_2))
	 (exponent (/ diff (- 400))))
    (/ 1 (+ 1 (expt 10 exponent)))
    )
  )

(define (elo-update elo_winner elo_loser k_factor)
  (let* ((prior-prob (win-prob elo_winner elo_loser))
	 (residual (- 1 prior-prob))
	 (winner-update (* k_factor residual))
	 (loser-update (- winner-update)))
    (cons winner-update loser-update)))

(define winners (list "Federer" "Nadal"))
(define losers (list "Nadal" "Federer"))
    
(define (make-lookup-table entries)
  entries)

(define (find-entry entry-name lookup-table default)
  (if (null? lookup-table)
      default
      (let ((first-entry (car lookup-table)))
	(if (equal? (car first-entry) entry-name)
	    (cdr first-entry)
	    (find-entry entry-name (cdr lookup-table) default)))))

(define table (make-lookup-table (list
				  (cons "Federer" 1500)
				  (cons "Nadal" 1600)
				  )))

(define (insert-entry entry-name entry-value lookup-table)
  (if (null? lookup-table)
      (list (cons entry-name entry-value))
      (let ((first-entry (car lookup-table)))
	(if (equal? (car first-entry) entry-name)
	    ; We need to replace this entry and return the rest unchanged
	    (cons (cons entry-name entry-value) (cdr lookup-table))
					; Otherwise, we recurse
	    (cons first-entry (insert-entry entry-name entry-value (cdr
lookup-table)))))))
  
(define (calculate-ratings winners losers k-factor table)
  (define start-rating 1500)
  (if (null? winners)
      table
      (let* ((cur-winner (car winners))
	     (cur-loser (car losers))
	     (winner-rating (find-entry cur-winner table start-rating))
	     (loser-rating (find-entry cur-loser table start-rating))
	     (updates (elo-update winner-rating loser-rating k-factor))
	     (winner-update (car updates))
	     (loser-update (cdr updates))
	     (new-winner-rating (+ winner-rating winner-update))
	     (new-loser-rating (+ loser-rating loser-update))
	     (table-with-winner (insert-entry cur-winner new-winner-rating
					      table))
	     (table-with-both (insert-entry cur-loser new-loser-rating
					    table-with-winner)))
	(calculate-ratings (cdr winners) (cdr losers) k-factor table-with-both)
	)))
