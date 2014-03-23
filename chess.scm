#lang scheme

(require pict)
(require racket/class)
(require racket/draw)
(require (planet plt/rsvg:1:0))

(define cell-w 24)
(define cell-h 24)

(define use-png? #f)

(define (map-range n func)
  (define (map-range-acc max-n n func)
    (cond ((< n 1) '())
          (else (cons (func (- max-n n)) (map-range-acc max-n (- n 1) func)))))
  (map-range-acc n n func))

(define (hc-append-func sep xs) (apply hc-append (cons sep xs)))
(define (vc-append-func sep xs) (apply vc-append (cons sep xs)))

(define (make-grid xsep ysep width height func)
  (vc-append-func 
   ysep 
   (map-range height (lambda (y)
                       (hc-append-func xsep (map-range width 
                                                       (lambda (x) (func x y))))))))

(define (make-chess-board func) (make-grid 1 1 9 9 func))

(define (gray-col grade) (make-object color% grade grade grade))

(define (background-square x y)
  (cond ((odd? (+ x y)) 
         (cc-superimpose 
          (colorize (filled-rectangle cell-w cell-h) (gray-col 180))
         (rectangle cell-w cell-h)))
        (else 
         (cc-superimpose
          (colorize (filled-rectangle cell-w cell-h) (gray-col 90))
          (rectangle cell-w cell-h)))))

(define (foreground-square x y)
  (let ((fig (lookup (list x y) initial-figures)))
    (if (equal? fig #f)
        (blank cell-w cell-h)
        (scale-to-fit fig cell-w cell-h))))

(define (get-square x y)
  (cc-superimpose (background-square x y) (foreground-square x y)))

(define (get-cell x y)
  (cond ((and (= x 0) (= y 0)) (blank cell-w cell-h))
        ((= y 0) 
         (cc-superimpose 
          (blank cell-w cell-h) 
          (text (make-string 1 (integer->char (+ x 64))))))
        ((= x 0)
         (cc-superimpose
          (blank cell-w cell-h)
          (text (number->string (- 9 y)))))
        (else (get-square x y))))

(define (chess-board) (make-chess-board get-cell))

(define (first-char str) (car (string->list str)))

(define (between? number low high) (and (<= number high) (>= number low)))

(define (to-pos str)
    (let ((pos
           (list
            (- (char->integer (first-char (substring str 0 1))) 64)
            (- 9 (string->number (substring str 1 2))))))
      (if (and (between? (car pos) 1 8) (between? (cadr pos) 1 8))
          pos
          #f)))

(define (get-figure str figures) (lookup (to-pos str) figures))

(define (load-figure name) 
  (let ((filepath 
    (string-append (if use-png? "png/" "svg/") 
                   name 
                   (if use-png? ".png" ".svg"))))
    (bitmap (if use-png?
                filepath
                (load-svg-from-file filepath)))))

(define w-king (load-figure "KingW"))
(define w-queen (load-figure "QueenW"))
(define w-bishop (load-figure "BishopW"))
(define w-pawn (load-figure "PawnW"))
(define w-rook (load-figure "RookW"))
(define w-knight (load-figure "KnightW"))

(define b-king (load-figure "KingB"))
(define b-queen (load-figure "QueenB"))
(define b-bishop (load-figure "BishopB"))
(define b-pawn (load-figure "PawnB"))
(define b-rook (load-figure "RookB"))
(define b-knight (load-figure "KnightB"))

(define initial-figures 
  `(((1 1) ,b-rook)
    ((2 1) ,b-knight)
    ((3 1) ,b-bishop)
    ((4 1) ,b-queen)
    ((5 1) ,b-king)
    ((6 1) ,b-bishop)
    ((7 1) ,b-knight)
    ((8 1) ,b-rook)
    
    ((1 2) ,b-pawn)
    ((2 2) ,b-pawn)
    ((3 2) ,b-pawn)
    ((4 2) ,b-pawn)
    ((5 2) ,b-pawn)
    ((6 2) ,b-pawn)
    ((7 2) ,b-pawn)
    ((8 2) ,b-pawn)
    
    ((1 7) ,w-pawn)
    ((2 7) ,w-pawn)
    ((3 7) ,w-pawn)
    ((4 7) ,w-pawn)
    ((5 7) ,w-pawn)
    ((6 7) ,w-pawn)
    ((7 7) ,w-pawn)
    ((8 7) ,w-pawn)
    
    ((1 8) ,w-rook)
    ((2 8) ,w-knight)
    ((3 8) ,w-bishop)
    ((4 8) ,w-queen)
    ((5 8) ,w-king)
    ((6 8) ,w-bishop)
    ((7 8) ,w-knight)
    ((8 8) ,w-rook)))

(define (lookup key assoclist)
  (if (null? assoclist)
      #f
      (let ((current-key (car (car assoclist))))
        (if (equal? current-key key)
            (car (cdr (car assoclist)))
            (lookup key (cdr assoclist))))))