#lang racket

(require srfi/1)

(define all-directions '(-11 -10 -9 -1 1 9 10 11))

(define empty 0)
(define black 1)
(define white 2)
(define outer 3)

(define (char-of piece)
  (case piece
    ((0) ".")
    ((1) "@")
    ((2) "0")
    ((3) "?")))

(define (opponent player)
  (if (= player black)
      white
      black))

(define (bref board square)
  (vector-ref board square))

(define (bset! board square val)
  (vector-set! board square val))

(define (copy-board board)
  (vector-copy board))

(define all-squares
  (filter (lambda (i) (<= 1 (modulo i 10) 8))
          (iota 78 11)))

(define (initial-board)
  (let ((board (make-vector 100 outer)))
    (for/list ((square all-squares))
      (bset! board square empty))
    (bset! board 44 white) (bset! board 45 black)
    (bset! board 54 black) (bset! board 55 white)
    board))

(define (count-difference player board)
  (- (vector-count (lambda (i) (= player i)) board)
     (vector-count (lambda (i) (= (opponent player) i)) board)))

(define (print-board board)
  (printf "~%     1 2 3 4 5 6 7 8  [~a=~a ~a=~a (~a)]"
          (char-of black) (vector-count (lambda (x) (= black x)) board)
          (char-of white) (vector-count (lambda (x) (= white x)) board)
          (count-difference black board))
  (for ((row (iota 8 1)))
    (printf "~%  ~a " (* 10 row))
    (for ((col (iota 8 1)))
      (printf "~a " (char-of (bref board (+ col (* 10 row)))))))
  (printf "~%"))

(define (valid? move)
  (and (integer? move) (<= 11 move 88) (<= 1 (modulo move 10) 8)))

(define (some? f lst)
  (pair? (filter f lst)))

(define (legal? move player board)
  (and (= (bref board move) empty)
       (some? (lambda (dir) (would-flip? move player board dir))
              all-directions)
       move))

(define (make-move move player board)
  (bset! board move player)
  (for-each (lambda (dir) (make-flips move player board dir))
            all-directions)
  board)

(define (make-flips move player board dir)
  (let ((bracketer (would-flip? move player board dir)))
    (when (integer? bracketer)
      (do ((c (+ move dir) (+ c dir)))
          ((= c bracketer) #t)
        (bset! board c player)))))

(define (would-flip? move player board dir)
  (let ((c (+ move dir)))
    (if (= (bref board c) (opponent player))
        (find-bracketing-piece (+ c dir) player board dir)
        #f)))

(define (find-bracketing-piece square player board dir)
  (cond ((= (bref board square) player) square)
        ((= (bref board square) (opponent player))
         (find-bracketing-piece (+ square dir) player board dir))
        (else #f)))

(define (othello bl-strategy wh-strategy . print?)
  (let ((board (initial-board)))
    (let loop ((p black) (strategy bl-strategy))
      (when p
          (get-move strategy p board print?))
      (loop (next-to-play board p print?)
            (if (= p black) bl-strategy wh-strategy)))
    (when print?
      (printf "~%The game is over. Final result:")
      (print-board board))
    (count-difference black board)))

(define (othello-a-step board cur-pl strategy . print?)
  (if (next-to-play board (opponent cur-pl) print?)
      (get-move strategy cur-pl board print?)
      #f))

(define (next-to-play board previous-player print?)
  (let ((opp (opponent previous-player)))
    (cond ((any-legal-move? opp board) opp)
          ((any-legal-move? previous-player board)
           (when print?
             (printf "~%~a has no moves and must pass."
                     (char-of opp)))
           previous-player)
          (else #f))))

(define (any-legal-move? player board)
  (some? (lambda (move) (legal? move player board))
         all-squares))

(define (get-move strategy player board print?)
  (when print (print-board board))
  (let ((move (strategy player (copy-board board))))
    (when print? (printf "~%~a moves to ~a." (char-of player) move))
    (make-move move player board)))

(define (human player board)
  (printf "~%~a to move: " (char-of player))
  (let ((val (read)))
    val))
