;cumulative sum over list
(define (sum lst) 
  (if (null? lst) 0 
      (+ (car lst) (sum (cdr lst)))
  )
)

;Generate a list of numbers from n down to 1
(define (range n)
  (if (= n 0) null 
      (cons n (range (- n 1)))
  )
)

;Correct and in right position
(define (B-hits cfg probe)
  (define (check a b) (if (= a b) 1 0) )
  (sum (map check cfg probe))
)

;Correct but not in the right position
(define (W-hits cfg probe)
  (- (hits cfg probe) (B-hits cfg probe))
)

;Size of intersection of shared symbols
(define (hits cfg probe)
  (define (check x)
    (define (count-x lst)
      (sum (map (lambda (i) (if (= x i) 1 0 )) lst))
    )
    (min (count-x cfg) (count-x probe))
  )
  (sum (map check (range Num_colors)))
)


(define (score cfg probe)
  (list (B-hits cfg probe) (W-hits cfg probe))
)

(define (score-to-str scr)
  (let* ((x1 (car scr))
	 (x2 (car (cdr scr)))
	 (B (if (string? x1) (string->number x1) x1))
	 (W (if (string? x2) (string->number x2) x2))
	 )
    (string-append (make-string B #\B) 
		   (make-string W #\W)
		   (make-string (- Num_pegs (+ B W )) #\.)
		   )
    )
  )

(define (create-pool)
  (apply cross (make-list Num_pegs (range Num_colors)))
)

;borrowed from 'Strachey's functional pearl, forty years on'
(define (cross . xss)
  (define (f xs yss)
    (define (g x zss)
      (define (h ys uss)
        (cons (cons x ys) uss))
      (fold-right h zss yss))
    (fold-right g '() xs))
  (fold-right f (list '()) xss))

;apply operator right to left to reduce list 
(define (fold-right op base xs)
  (if (null? xs)
      base
      (op (car xs) (fold-right op base (cdr xs)))))

;each code in pool must produce the same score as the current favorite guess
(define (reduce-pool probes cfg scr)
  (filter (lambda (code) (string=? (score-to-str (score cfg code)) scr) ) probes)
)

(define (solve max_turns probes)
  (let loop ((n max_turns) (pool probes))
    (cond 
     [(null? pool) (inconsistent)]
     [(= 1 (length pool)) (found-answer pool)]
     [(= 0 n) (too-hard)]
     [else (loop (- n 1) (turn pool))]
    )
  )
)

(define (turn probes)
  (let ((guess (get-guess probes)))
    (display guess)
    (let ((curr_scr (score-to-str (get-score))))
      (reduce-pool probes guess curr_scr)
    )
  )
)

(define (get-guess probes) (car (shuffle probes)))

(define (get-score)
  (newline)
  (display "> ")
  (regexp-split #px":" (read-line))
)

(define (inconsistent) (display "You've been lying to me...")(newline))
(define (found-answer probes) (display "Solution: ")(display (car probes))(newline))
(define (too-hard) (display "You got me, this is taking too long")(newline))

(begin 
  (display "Enter the number of pegs for the code: ")
  (define Num_pegs (string->number (read-line)))
  (display "Enter the number of symbols to pick from: ")
  (define Num_colors (string->number (read-line)))
  (display "The choices to create the code from are: ")
  (display (reverse (range Num_colors)))
  (newline)

  (define pool (create-pool))
  (solve 10 pool)
  
)