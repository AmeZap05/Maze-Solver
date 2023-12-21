;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname Untitled) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
;LIBRARIES
(require data/queue)
(require 2htdp/universe)
(require 2htdp/image)

;EDITABLE CONSTANT 
(define ROWS 20)
(define COLUMNS 20)

; Default-Setting: -> void
;semply set the default parameter 
(define (Default-Setting)
  (if (or (and (equal? ROWS COLUMNS) (equal? ROWS 20))
        (or (and (equal? ROWS COLUMNS) (equal? ROWS 15)) (and (equal? ROWS 15) (equal? COLUMNS 20)) (and (equal? ROWS 20) (equal? COLUMNS 15)))
        (or (and (equal? ROWS COLUMNS) (equal? ROWS 10)) (and (equal? ROWS 10) (equal? COLUMNS 20)) (and (equal? ROWS 20) (equal? COLUMNS 10)))
        (and (equal? ROWS COLUMNS) (equal? ROWS 25)) (or (and (equal? ROWS 20) (equal? COLUMNS 25)) (and (equal? ROWS 25) (equal? COLUMNS 20))))
    #true
    (begin (set! ROWS 20) (set! COLUMNS 20))))

(Default-Setting) 

;STATIC CONSTANT
(define CELL-SIZE 30)
(define BORDER-SIZE 0)
(define SWITCH #true)
(define WIDTH (* CELL-SIZE COLUMNS))
(define HEIGHT (* CELL-SIZE ROWS))
(define EMPTY-CANVA (empty-scene (+ WIDTH (* 2 BORDER-SIZE)) (+ HEIGHT (* 2 BORDER-SIZE)) (make-color 35 35 65)))
(define PATH-PEN (pen "white" 5 "solid" "projecting" "bevel"))
(define ADJ (list (make-posn 1 0) (make-posn -1 0) (make-posn 0 1) (make-posn 0 -1)))
(define QUE (make-queue)) 
(define NULL (rectangle 0 0 "outline" "black"))
(define QUIT #false)
(define MATRIX '()) 
(define SHOW #false)

;IMAGES
(define WALL   (freeze CELL-SIZE CELL-SIZE (scale (/ CELL-SIZE 16) (bitmap "wall.jpg"))))

(define EMPTY  (freeze CELL-SIZE CELL-SIZE (scale (/ CELL-SIZE 16) (bitmap "bg.jpg"))))

(define ENTRY  (freeze CELL-SIZE CELL-SIZE (scale (/ CELL-SIZE 16) (bitmap "door.jpg"))))

(define EXIT   (freeze CELL-SIZE CELL-SIZE (scale (/ CELL-SIZE 16) (bitmap "end.jpg"))))

#|
Maze is a struct (make-maze List<List<Number>> Number posn)
Interpretation:
 - Matrix is a List<List<Number>> where each list is a row, and each element (number) of the list is a cell.
 - State is a number that indicates the status of the maze, is a Number between -1 and 2 included
 - Posn is a Posn<Posn Posn> where Posn-x is the Posn of the starting point, and Posn-Y is the Posn for the end-point
|#
(define-struct maze [matrix state posn])

; RESIZE  HOMEPAGE SETTING
(define HOMEPAGE1 (scale 0.52 (bitmap "hpfinal.jpg"))) ; ROWS=COLUMNS=20

(define HOMEPAGE2 (scale 0.38 (bitmap "hpfinal.jpg"))) ; ROWS=COLUMNS=15 or (ROWS=15 and COLUMNS=20) or (ROWS=20 and COLUMNS=15)

(define HOMEPAGE3 (scale 0.28 (bitmap "hpfinal.jpg"))) ; ROWS=COLUMNS=10 or (ROWS=10 and COLUMNS=20) or (ROWS=20 and COLUMNS=10)

(define HOMEPAGE4 (scale 0.65 (bitmap "hpfinal.jpg"))) ; ROWS=COLUMNS=25

(define HOMEPAGE5 (scale 0.6 (bitmap "hpfinal.jpg")))  ; (ROWS=20 COLUMNS=25) or (ROWS=25 COLUMNS=20)

(define HOMEPAGE (scale 0.35 (bitmap "hpfinal.jpg")))  ; otherwise (raise an error here is better)

; RESET COSTANT
(define R1 (put-image HOMEPAGE1 (/ (+ (* COLUMNS CELL-SIZE) BORDER-SIZE) 2) (/ (+ (* ROWS CELL-SIZE) BORDER-SIZE) 2) EMPTY-CANVA))  ;ROWS=COLUMNS=20

(define R2 (put-image HOMEPAGE2 (/ (+ (* COLUMNS CELL-SIZE) BORDER-SIZE) 2) (/ (+ (* ROWS CELL-SIZE) BORDER-SIZE) 2) EMPTY-CANVA))  ; ROWS=COLUMNS=15 or (ROWS=15 and COLUMNS=20) or (ROWS=20 and COLUMNS=15)

(define R3 (put-image HOMEPAGE3 (/ (+ (* COLUMNS CELL-SIZE) BORDER-SIZE) 2) (/ (+ (* ROWS CELL-SIZE) BORDER-SIZE) 2) EMPTY-CANVA))  ; ROWS=COLUMNS=10 or (ROWS=10 and COLUMNS=20) or (ROWS=20 and COLUMNS=10)

(define R4 (put-image HOMEPAGE4 (/ (+ (* COLUMNS CELL-SIZE) BORDER-SIZE) 2) (/ (+ (* ROWS CELL-SIZE) BORDER-SIZE) 2) EMPTY-CANVA))  ; ROWS=COLUMNS=25

(define R0 (put-image HOMEPAGE (/ (+ (* COLUMNS CELL-SIZE) BORDER-SIZE) 2) (/ (+ (* ROWS CELL-SIZE) BORDER-SIZE) 2) EMPTY-CANVA))   ; otherwise (raise an error here is better)


#|------- INSIDE -------

Data type:
 pos is a Posn
 interpretation:
 pos is a cell

Signature:
 inside: Posn -> Bool

Purpose statement:
 return #true if the Posn is inside the matrix

Header:
 (define (inside pos) #false)

Template:
 (define (inside pos)(cond
    [(and (>= (posn-x pos) 0)
     (< (posn-x pos) COLUMNS)
     (>= (posn-y pos) 0)
     (< (posn-y pos) ROWS))
     ... pos ...]
    [else ... pos ...]))
Implementation:
|#

(define (inside pos)
  (cond
    [(and
      (>= (posn-x pos) 0)
      (< (posn-x pos) COLUMNS)
      (>= (posn-y pos) 0)
      (< (posn-y pos) ROWS))
     #true]
    [else #false]))

;examople
(check-expect (begin (set! ROWS 10) (set! COLUMNS 10) (inside (make-posn 11 11))) #false)
(check-expect (begin (set! ROWS 15) (set! COLUMNS 10) (inside (make-posn 5 5))) #true)
(check-expect (begin (set! ROWS 20) (set! COLUMNS 20) (inside (make-posn 7 9))) #true)

#|------- LIST-SET -------

Data type:
require:
list is a non-empty list of number

Signature:
 list-set: List Number Number -> Lists

Purpose statement:
 return a list with a value changed at pos with val

Header:
 (define (list-set lst pos val) lst)

Template:
 
(define (list-set lst pos val)
  (if (= pos 0)
    (cons ... (rest lst))
    (cons ... (first lst) ...  (list-set (rest lst) ... ))))

Implementation:
|#

(define (list-set lst pos val)
  (if (= pos 0)
      (cons val (rest lst))
      (cons (first lst) (list-set (rest lst) (- pos 1) val))))

;Example
(check-expect(list-set (list 0 0 0 1 0 2) 3 5 )
             (list 0 0 0 5 0 2))
(check-error (list-set '() 1 2 ))



#| ------- CHANGE-MATRIX -------

Data type:

Signature:
 change-matrix: Matrix Posn Number-> Matrix

Purpose statement:
 change the value of the element in the matrix

Header:
 (define (change-matrix matrix cell val) (list (list 0 0) (list 0 0)))

Template:
 (define (change-matrix matrix cell val)
  (list-set ... matrix ... cell ... val ...)

Implementation:
|#

;change-matrix: Matrix Posn Number-> Matrix
(define (change-matrix matrix cell val)
  (list-set matrix (posn-y cell)
            (list-set (list-ref matrix (posn-y cell)) (posn-x cell) val)))




#|------- ADJ-QUE -------

Data type:
 pos is a posn

Signature:
 posn->posn

Purpose statement:
 return an adjcent posn of the first element in the queue

Header:
 (define (adj-que pos) (make-posn 0 0))

Template:
 (define (adj-que pos)
  (make-posn
   (+ (posn-x (first (queue->list que))) (posn-x pos))
   (+ (posn-y (first (queue->list que))) (posn-y pos))))

Implementation:
|#

(define (adj-que pos)
  (make-posn
   (+ (posn-x (first (queue->list QUE))) (posn-x pos))
   (+ (posn-y (first (queue->list QUE))) (posn-y pos))))



#| ------- GET-VALUE -------
Data type:
 matrix is List<List<Number>>

Signature:
 get-value: Matrix Posn -> Number
 
Purpose statement:
 return the value of matrix at x

Header:
 (define (get-value matrix x) (make-posn 0 0))
 
Template:
 (define (get-value matrix x)
  ( ...  matrix ... (posn-y x) ... (posn-x x) ... )
  
Implementation:
|#

(define (get-value matrix x)
  (list-ref (list-ref matrix (posn-y x)) (posn-x x)))

;Example
(check-expect (get-value (list (list 1 0) (list 1 0)) (make-posn 0 0)) 1)
(check-expect (get-value (list (list 1 0)) (make-posn 1 0)) 0)
(check-error (get-value '() (make-posn 1 0)))



#|------- CHECK-NODE -------

Data type:
matrix is list<list<node>>
lst is a list<Number>

Signature:
 check-node: Matrix List<number> -> Matrix

Purpose statement:
 check every adiacent node of the first element in the queue and update the queue following the BFS alghoritm

Header:
 (define (check-node matrix lst) (list (list 0 0) (list 0 0)))

Template:
 (define (check-node matrix lst)
  (cond
    [(empty? lst) ... matrix ...]
    [(and (cons? lst)
          (inside (adj-que (first lst)))
          (= (get-value matrix (adj-que (first lst))) 0))
            (begin ...(check-node (change-matrix matrix ... (first lst)) ... (rest lst)))]
    [else (check-node matrix (rest lst) ...)]))

Implementation:
|#
(define (check-node matrix lst)
  (cond
    [(empty? lst) (begin (dequeue! QUE) matrix)]
    [(and (cons? lst)
          (inside (adj-que (first lst)))
          (= (get-value matrix (adj-que (first lst))) 0))
     (begin
       (enqueue! QUE (adj-que (first lst)))
       (check-node
        (change-matrix matrix
                       (adj-que (first lst))
                       (+ (get-value matrix (first (queue->list QUE))) 1))
                   (rest lst)))]
    [else (check-node matrix (rest lst))]))



#| ------- BFS -------

Data type:
matrix is a list<list<node>>

Signature:
bfs: Matrix -> Matrix

Purpose statement:
 execute bfs algorithm

Template:
 (define (bfs matrix)
  (cond
    [(non-empty-queue? que) (bfs ... matrix ...))]
    [else matrix]))

Implementation:
|#
; bfs: Matrix -> Matrix
; execute bfs
(define (bfs matrix)
  (cond
    [(non-empty-queue? QUE) (bfs (check-node matrix ADJ))]
    [else matrix]))



#|------- ONTICK -------

Data type:
 state is a Maze

Signature:
 onTick: Maze -> Maze

Purpose statement:
 handle the bfs

Header:
 (define (onTick state) state)

Template:
 (define (onTick state)
  ... state ...)

Implementation:
|#

(define (onTick state)
  (begin
    (enqueue! QUE (posn-x (maze-posn state)))
    (set! MATRIX (bfs (change-matrix (maze-matrix state) (posn-x (maze-posn state)) 1)))
    state))



#|------- GET-CENTER -------

Data type:
 Pos is a Posn

Signature:
 get-center: Posn -> Posn

Purpose statement:
 return a Posn with the coordinates on the canva of pos

Header:
 (define (get-center pos) (make-posn 0 0))

Template:
(define (get-center pos)
  (make-posn
    (+ (* CELL-SIZE ... (posn-x pos) ...) BORDER-SIZE (/ CELL-SIZE 2))
    (+ (* CELL-SIZE ... (posn-y pos) ...) BORDER-SIZE (/ CELL-SIZE 2))))

Implementation:
|#
(define (get-center pos)
  (make-posn
   (+ (* CELL-SIZE (posn-x pos)) BORDER-SIZE (/ CELL-SIZE 2))
   (+ (* CELL-SIZE (posn-y pos)) BORDER-SIZE (/ CELL-SIZE 2))))



#|------- ADJ -------

Data type:
 pos and dir are Posn

Signature:
 adj: Posn Posn -> Posn

Purpose statement:
 return an adiacent Posn of pos based on dir

Header:
 (define (adj pos dir) (make-posn  0 0))

Template:
 (define (adj pos dir)
   (make-posn
     (+ (posn-x pos) (posn-x dir))
     (+ (posn-y pos) (posn-y dir))))

Implementation:
|#
(define (adj pos dir)
  (make-posn
   (+ (posn-x pos) (posn-x dir))
   (+ (posn-y pos) (posn-y dir))))

;Example
(check-expect (adj (make-posn 1 0) (make-posn 3 5)) (make-posn 4 5))




#|------- GET-NEXT -------

Data type:
 Matrix is a List<list<Number>>
 pos is a posn
 lst is a list

Signature:
 get-next: Matrix Posn List -> Image
 
Purpose statement:
 find the next element to show the path

Header:
 (define (get-next matrix pos lst) image)

Template:
 (define (get-next matrix pos lst)
  (cond
    [(and
      (inside (adj pos (first lst)))
      (= (get-value matrix (adj pos (first lst))) (- (get-value matrix pos) 1)))
     ... (first lst) ...]
    [else ... (rest lst) ...]))

Implementation:
|#
(define (get-next matrix pos lst)
  (cond
    [(and
      (inside (adj pos (first lst)))
      (= (get-value matrix (adj pos (first lst))) (- (get-value matrix pos) 1)))
     (adj pos (first lst))]
    [else (get-next matrix pos (rest lst))]))



#|------- FIND-PATH -------

Data type:
- Matrix is a list<list<Number>>
- Canva is an image
- Pos is a posn

Signature:
 find-path: Matrix Canva Posn -> Canva

Purpose statement:
 return the canva solved if it can be solved

Header:
 (define (find-path matrix canva pos) image)

Template:
 (define (find-path matrix canva pos)
  (cond
    [(not (equal? (get-value matrix pos) 1))
     (find-path ... matrix ... ]
    
    [else canva]))


Implementation:
|#
(define (find-path matrix canva pos)
  (cond
    [(equal? (get-value matrix pos) 1) canva]
    [else (find-path matrix
                     (add-line canva
                          (posn-x (get-center pos)) (posn-y (get-center pos))
                          (posn-x (get-center (get-next matrix pos ADJ))) (posn-y (get-center (get-next matrix pos ADJ)))
                          PATH-PEN)
                     (get-next matrix pos ADJ))]))



#|------- SHOW-PATH -------

Data type:
state is a Maze
canva is an Image

Signature:
 show-path: Maze Canva -> Canva

Purpose statement:
 return the canva with the path showed if a path exist

Header:
 (define (show-path state canva) image)

Template:
 (define (show-path state canva)
  (cond
    [(and show (not (equal? (get-value MATRIX (posn-y (maze-posn state))) 0)))
     (find-path ... state ... canva ...))]
    [else ... canva ...)]))

Implementation:
|#
; show-path: Maze Canva -> Canva
; return the canva with the path showed if a path exist
(define (show-path state canva)
  (cond
    [(and SHOW (not (equal? (get-value MATRIX (posn-y (maze-posn state))) 0)))
     (find-path MATRIX canva (posn-y (maze-posn state)))]
    [else (begin (set! SHOW #false) canva)]))





#|------- GENERAL-DRAW -------

Data type:
state is a struct maze

Signature:
general draw: Maze -> image

Purpose Statement:
The purpose of this function is draw the HOMEPAGE if the costant SWITCH is true or the maze if SWITCH is false
interpretation:
 Maze is the combination of the matrix and path, where:
  - matrix is the group of row and column.
  - path is the solution of the maze.

Header:
(define (general-draw state) HOMEPAGE)

How:
 this function use the reset costant

Template:
(define (general-draw state)
  (cond
    [(equal? SWITCH #t) HOMEPAGE]
    [else
          ... (show-path state
              ... (place-image (draw (maze-matrix state) (maze-posn state) 0) ...) ])) 

Implementation:
|#

(define (general-draw state)
  (cond
    [(equal? SWITCH #t)
     (cond
       [(and (equal? ROWS COLUMNS) (equal? ROWS 20))
        R1]
       [(or
         (and (equal? ROWS COLUMNS) (equal? ROWS 15))
         (and (equal? ROWS 15) (equal? COLUMNS 20))
         (and (equal? ROWS 20) (equal? COLUMNS 15)))
        R2]
       [(or
         (and (equal? ROWS COLUMNS) (equal? ROWS 10))
         (and (equal? ROWS 10) (equal? COLUMNS 20))
         (and (equal? ROWS 20) (equal? COLUMNS 10)))
        R3]
       [(and (equal? ROWS COLUMNS) (equal? ROWS 25))
        R4]
       [else R0])] 

    [else 
     (show-path state
             (place-image (draw (maze-matrix state) (maze-posn state) 0)
                          (+(/ (* COLUMNS CELL-SIZE) 2) BORDER-SIZE)
                          (+(/ (* ROWS CELL-SIZE) 2) BORDER-SIZE)
                          EMPTY-CANVA))]))



#| ------- DRAW -------
 
 --> draw is an auxiliary function of general-draw
Data type:
Matrix is a List<List<Number>>
Gates is Posn<Posn>
  - Posn-x is the entry point
  - Posn-y is the exit point
n is a Number

Signature:
draw : matrix gates Number -> image

Purpose statement:
 draw each row of a matrix

How:
 Take the first element of each List<List<Number>>, i.e. a list of Number, call on the first element
 the auxiliary function draw row, and call (recursively) draw on the rest of the list, and with Above
 each recursion is combined.
 Return NULL if the list is empty.
 
Header:
 (define (draw matrix gates n) NULL)

Template:
(define (draw matrix gates n)
  (cond
    ;base case
    [(equal? '() matrix) NULL]
    ;recursive case
    [else
     (combine solution ...
      (draw-row (first matrix) bool bool  gates 0) ...
      (draw (rest matrix) gates n) ... ]))

Implementation:
|#
(define (draw matrix gates n)
  (cond
    ;base case
    [(equal? '() matrix) NULL]
    ;recursive case
    [else
     (above
      (cond
        ;both on the same row
        [(and (= n (posn-y(posn-x gates))) (= n (posn-y(posn-y gates))))
         (draw-row (first matrix) #true #true gates 0)]
        ;entry row
        [(= n (posn-y(posn-x gates)))
         (draw-row (first matrix) #true #false gates 0)]
        ;exit row
        [(= n (posn-y(posn-y gates)))
         (draw-row (first matrix) #false #true gates 0)]
        ;nothing
        [else (draw-row (first matrix) #false #false gates 0)])
      (draw (rest matrix) gates (add1 n)))]))




#|------- DRAW-ROW -------


--> draw-row is an auxiliary function of draw
Data type:
 - lon is a list of number
 - in & out are boolean
 - gates is a posn<posn>
 - n is a number
intrepretation:
 - lon is a row of a matrix
 - in is true if in this row there is the starting point
 - out is true if in this row there is the end point
 - Posn-x gates is the Posn of the entry point,
   Posn-y gates is the Posn of the exit point
 

Signature:
 draw-row: lon boolean boolean gates n -> image

Purpose statement:
 draw the cell of a row

How:
 Call on the first elemt of the list, i.e. Number and call on it the auxiliary function draw-cell.
 Call recursively on the rest of lon the function draw-row.
 Combine each recursion with beside.

Header:
 (define (draw-row lon in out gates n) image)

Template:
 (define (draw-row lon in out gates n)
  (cond
    ;base case
    [(equal? lon '()) NULL]
    ;recursive case
    [else
     (combine solution
      (draw-cell (first lon) ...
      (draw-row (rest lon) in out gates (add1 n))) ... ]))
  
Implementation:
|#
(define (draw-row lon in out gates n)
  (local ((define (draw-cell node)
    (cond
     [(= node -1) WALL]
     [ else EMPTY]
    )))
  (cond
    ;base case
    [(equal? lon '()) NULL]
    ;recursive case
    [else
     (beside
      (cond
        ;entry
        [(and (= (posn-x(posn-x gates)) n) (equal? #t in))
         ENTRY]
        ;exit
        [(and ( = (posn-x(posn-y gates)) n) (equal? #t out))
         EXIT]
        ;other cells
        [else (draw-cell (first lon))])
      (draw-row (rest lon) in out gates (add1 n)))])))




#|------- ZERO-LIST -------

Data type:
lon is a List<Number>
 null-lon in an Empty-List

Signature:
 zero-list: List<Number>  -> List<Number>

interpretation: the output list is full of zeros 

Purpose statement:
 take a list of numbers and return a list with the same length of the input list but all elements are 0

Header:
 (define (zero-list lon null-lon) (list 0 0 0 0 ))

Template:
 (define (zero-list lon)
  (map (lambda ( t ) ... 0 ... ) ... lon ...))

Implementation:
|#

(define (zero-list lon)
  (map (lambda (t) 0) lon))

;Example
(check-expect (zero-list (list 1 2 3 4 5)) (list 0 0 0 0 0))
(check-expect (zero-list (list 1 2 0 0)) (list 0 0 0 0))
(check-expect (zero-list '()) '())



#|------- CLEAR -------

Data type:
ms is of type Maze

Signature:
 clear: Maze -> List<List<Number>>

Purpose statement:
 take a matrix (List<List<Number>>) and return a matrix full of 0

Header:
 (define (clear ms) '())

Template:
(define (clear ms)
  (map (lambda ( ... ) (zero-list ... )) (maze-matrix ms))) 

Implementation:
|#
(define (clear ms)
  (map (lambda (lol) (zero-list lol )) (maze-matrix ms)))   

;Example
(check-expect (zero-list (list 1 2 3 4 5)) (list 0 0 0 0 0))
(check-expect (zero-list (list 1 2 0 0)) (list 0 0 0 0))
(check-expect (zero-list '()) '())



#|------- HANDLE-KEY -------

Data type:
 - ms is of type Maze
 - key is a string
 interpretetion:
 the input k of the keyboard is represented as a string

Signature:
 handle-key: Maze String -> ms

Purpose statement:
 handle keyboard's events, and change the state of the maze
 if key equal:
 - "q" -> quit the maze
 - " " -> change the screen from homepage to mazepage
 - "w" -> change the state to -1 - Wall state
 - "b" -> change the state to 2  - Exit point state
 - "e" -> change the state to 1  - Entry point state
 - "d" -> change the state to 0  - Delete state
 - "c" -> clear the whole maze
 - "s" -> solve the maze

Header:
 (define (ms key) ms)

Template:
 (define (handle-key ms key)
  (cond
    [(and (equal? key "q") ...ms... ] 
    [(equal? key " ") ...ms...]
    [(equal? key "w") ... ms ...]    	
    [(equal? key "b") ... ms ...]     	
    [(equal? key "e") ... ms ... ]     	
    [(equal? key "d") ... ms ...]    	
    [(equal? key "c") ... ms ...]    
    [(and (equal? key "s") ... ms ...]    	
    [else ...ms...]))

Implementation:
|#

(define (handle-key ms key)
  (cond
    [(equal? key "q") (begin (set! QUIT #true) ms)] 
    [(equal? key " ") (begin (set! SWITCH #f) ms)]
    [SWITCH ms]
    [(equal? key "w") (make-maze (maze-matrix ms) -1 (maze-posn ms))]    
    [(equal? key "b") (make-maze (maze-matrix ms) 1 (maze-posn ms))]     
    [(equal? key "e") (make-maze (maze-matrix ms) 2 (maze-posn ms))]     	
    [(equal? key "d") (make-maze (maze-matrix ms) 0 (maze-posn ms))]    	
    [(equal? key "c") (make-maze (clear ms) (maze-state ms) (maze-posn ms))]   
    [(equal? key "s") (begin (set! SHOW (not SHOW)) (make-maze (maze-matrix ms) (maze-state ms) (maze-posn ms)))] 
    [else ms]))



#|------- HANDLE MOUSE -------

Data type:
 ms is a Maze
 x-mouse is a Number
 y-mouse is a Number
 mouse-event is a string
interpretation:
 x-mouse is the x cordinate of the mouse
 y-mouse is the y cordinate of the mouse
 mouse-event rappresent the mouse's input

Signature:
 handle-mouse: Maze Number Number String -> Maze

Purpose statement:
 it handles the mouse's events

Header:
 (define (handle-mouse ms x-mouse y-mouse) ms)

Template:
(define (handle-mouse ms x-mouse y-mouse mouse-event) 
  (local ((define CELL (get-cell x-mouse y-mouse)))
    (cond
      ;;check if the mouse input is in the grid -> check upper left
      [(or (< (posn-x CELL) 0)
           (< (posn-y CELL) 0)))
      ... (make-maze) ...]
      
      ;;check if the mouse input is in the grid -> check lower right
      [(or
        (> (posn-y CELL) (- (length (maze-matrix ms)) 1))
        (> (posn-x CELL) (- (length (list-ref (maze-matrix ms) 0)) 1))))
      ... (make-maze) ...]
      
      ;;handle the Entry point -> check if the position is free in order to set the entry point
      [(equal? mouse-event "button-down")
      ... (make-maze) ...]
      
      ;;handle the Exit point -> check if the position is free in order to set the exit point
      [(and
         (equal? mouse-event "button-down")
         (equal? (maze-state ms) 2)) 
       ... (make-maze) ...]
      
      ;;handle the buttond-down to set or delete a Wall
      [(and
         (equal? mouse-event "button-down")
         (< (maze-state ms) 1))
       ... (make-maze) ...]
      
      ;;handle the Wall's drag
      [(and
         (equal? mouse-event "drag")
         (equal? (maze-state ms) -1))
       ... (make-maze) ...]
      
      ;;handles the case where a Wall is being removed
      [(and
         (equal? mouse-event "drag")
         (equal? (maze-state ms) 0))
         ... (make-maze) ...]
      ;else return state
      [else ms])))

Implementation:
|#

(define (handle-mouse ms x-mouse y-mouse mouse-event) 
  (local ((define CELL (get-cell x-mouse y-mouse)))
    (cond
      [SWITCH ms]
      ;;check if the mouse input is in the grid -> check upper left
      [(or (< (posn-x CELL) 0)
           (< (posn-y CELL) 0))
       ms]
      ;;check if the mouse input is in the grid -> check lower right
      [(or
        (> (posn-y CELL) (- (length (maze-matrix ms)) 1))
        (> (posn-x CELL) (- (length (first (maze-matrix ms))) 1)))
       ms]
      ;;handle the Entry point -> check if the position is free in order to set the entry point
      [(and
         (equal? mouse-event "button-down")
         (equal? (maze-state ms) 1))
       (if (or
            (check-el (maze-matrix ms) CELL) ; check if the position is free or there is a Wall
            (equal? CELL (posn-y (maze-posn ms)))); check if the position is free or there is the Exit point
           ms
          (make-maze (maze-matrix ms) (maze-state ms) (make-posn CELL (posn-y (maze-posn ms)))))]
      ;;handle the Exit point -> check if the position is free in order to set the exit point
      [(and
         (equal? mouse-event "button-down")
         (equal? (maze-state ms) 2)) 
       (if (or
            (check-el (maze-matrix ms) CELL) ; check if the position is free or there is a Wall
            (equal? CELL (posn-x (maze-posn ms)))) ; check if the position is free or there is the Entry poin
           ms
           (make-maze (maze-matrix ms) (maze-state ms) (make-posn (posn-x (maze-posn ms)) CELL)))]
      ;;handle the buttond-down to set or delete a Wall
      [(and
         (equal? mouse-event "button-down")
         (< (maze-state ms) 1))
       (if
         (or
          (equal? CELL (posn-x (maze-posn ms)))
          (equal? CELL (posn-y (maze-posn ms))))
         ms
        (make-maze (change-matrix (maze-matrix ms) CELL (maze-state ms)) (maze-state ms) (maze-posn ms)))]
      ;;handle the Wall's drag
      [(and
         (equal? mouse-event "drag")
         (equal? (maze-state ms) -1))
       (if (or
            (equal? CELL (posn-x (maze-posn ms)))
            (equal? CELL (posn-y (maze-posn ms))))
          ms
          (make-maze (change-matrix (maze-matrix ms) CELL (maze-state ms)) (maze-state ms) (maze-posn ms)))]
      ;;handles the case where a Wall is being removed
      [(and
         (equal? mouse-event "drag")
         (equal? (maze-state ms) 0))
       (if (or
            (equal? CELL (posn-x (maze-posn ms)))
            (equal? CELL (posn-y (maze-posn ms))))
          ms
          (make-maze (change-matrix (maze-matrix ms) CELL (maze-state ms)) (maze-state ms) (maze-posn ms)))]
      ;else return state
      [else ms])))  




#| ------- GET-CELL -------

Data type:
 x is a Number
 y is Number
 interpretation:
 - x stand for the column
 - y stand for the row

Signature:
 get-cell: Number Number -> Posn

Purpose statement:
 given the coordinates x and y, it finds the respectively posn from the canva position

Header:
 (define (get-cell x y) (make-posn x y))

Template:
 (define (get-cell x y)
  (make-posn (... (/ (- x BORDER-SIZE) CELL-SIZE)) ... (... (/ (- y BORDER-SIZE) CELL-SIZE)) ...))

Implementation:
|#

(define (get-cell x y)
  (make-posn (floor (/ (- x BORDER-SIZE) CELL-SIZE)) (floor (/ (- y BORDER-SIZE) CELL-SIZE))))

(check-expect (begin (set! ROWS 20) (set! COLUMNS 20) (get-cell 1.5 2.5)) (make-posn 0 0))
(check-expect (begin (set! ROWS 20) (set! COLUMNS 20) (get-cell 60.3 90.8)) (make-posn 2 3))


#|------- CHECK-EL ------- 

Data type:
 matrix is a List<List<Number>> Posn
 pos is a posn

Signature:
 List<List<Number>> Posn -> Bool

Purpose statement:
 it checks if the element at position (x,y) in the input matrix is a wall or not

Header:
 (define check-el matrix pos) #t)

Template:
(define (check-el matrix pos)
  (local ((define CHECKER ( ...  matrix ... (posn-y pos)... (posn-x pos)))
    (cond
      [(equal? CHECKER -1) #true]
      [else #false])))

Implementation:
|#

(define (check-el matrix pos)
  (local ((define CHECKER (list-ref (list-ref matrix (posn-y pos)) (posn-x pos))))
    (cond
      [(equal? CHECKER -1) #true]
      [else #false])))

(check-expect (check-el (list (list 0 0 0 -1 0) (list 0 0 0 1 0) (list 0 0 0 2 0) (list 0 0 0 0 0)) (make-posn 3 0)) #true)
(check-expect (check-el (list (list -1 0 2 1 2 0) (list -1 0 0 -1 1 0) (list -1 0 0 1 0 0) (list -1 -1 0 -1 2 0)) (make-posn 3 1)) #true)
(check-expect (check-el (list (list -1 0 2 1 2 0) (list -1 0 0 -1 1 0) (list -1 0 0 1 0 0) (list -1 -1 0 -1 2 0)) (make-posn 1 3)) #true)
(check-expect (check-el (list (list -1 0 2 1 2 0) (list -1 0 0 -1 1 0) (list -1 0 0 1 0 0) (list -1 -1 0 -1 2 0)) (make-posn 1 0)) #false)
(check-expect (check-el (list (list -1 0 2 1 2 0) (list -1 0 0 -1 1 0) (list -1 0 0 1 0 0) (list -1 -1 0 -1 2 0)) (make-posn 3 2)) #false)


#|------- GEN-NL -------

--> is an auxiliary function of gen-matrix

Data type:
 n is a number

Signature:
 gen-nl: Number  -> List<Number>

Purpose statement:
 take a number n  and GENERATE a list full of 0 of length n

Header:
 (define (gen-nl n ) '() )

Template:
 (define (gen-nl n)
  (cond
    [(equal? n 0) ... n ...]
    [else  (cons 0 (gen-nl (... n ...)))]))

Implementation:
|#

(define (gen-nl n)
  (cond
    [(equal? n 0) '()]
    [else  (cons 0 (gen-nl (sub1 n)))]))

;; Examples
(check-expect (gen-nl 5) (list 0 0 0 0 0))
(check-expect (gen-nl 7) (list 0 0 0 0 0 0 0))
(check-expect (gen-nl 11) (list 0 0 0 0 0 0 0 0 0 0 0))


#| ------- GEN-MATRIX -------

Data type:
 R and C are number
 interpretation:
 R rappresent the row and C the columns

Signature:
 gen-matrix: Number Number  -> List<List<Zero>>

Purpose statement:
 it generates a matrix of R rows and C columns of 0

Header:
 (gen-matrix R p) (list (list 0 0) (list 0 0)))

Template:
(define (gen-matrix R C)
  (local ((define NEW-LIST (gen-nl p)))
    (cond
      [(equal? R 0) ... n ...]
      [else (cons NEW-LIST (gen-matrix ... R ... C ...) )])))
 
Implementation:
|#

(define (gen-matrix R C)
  (local ((define NEW-LIST (gen-nl C)))
    (cond
      [(equal? R 0) '()]
      [else (cons NEW-LIST (gen-matrix (sub1 R) C ) )])))

;Example
(check-expect (gen-matrix 5 5) (list (list 0 0 0 0 0) (list 0 0 0 0 0) (list 0 0 0 0 0) (list 0 0 0 0 0) (list 0 0 0 0 0)))
(check-expect (gen-matrix 3 6)(list (list 0 0 0 0 0 0) (list 0 0 0 0 0 0) (list 0 0 0 0 0 0)))

#| ------- QUIT? -------

Data type:
state is a Maze

Signature:
 quit?: Maze -> quit

Purpose statement:
 stop the execution of the maze

Header:
 (define (quit? state) quit)

Template:
 (define (quit? state) ... quit ...)

Implementation:
|#

(define (quit? state) QUIT)

#| ---------------------- BIG BANG ----------------------|#

(define (maze-game initial-state)
  (big-bang initial-state
    [to-draw general-draw]
    [on-mouse handle-mouse]
    [on-tick onTick 0.5] 
    [on-key handle-key]
    [name "A-MAZE-ING"]
    [stop-when quit?]
    [close-on-stop #true])) 

(define MAZE (make-maze
    (gen-matrix ROWS COLUMNS)
    -1
    (make-posn (make-posn 0 0)
               (make-posn (- COLUMNS 1) (- ROWS 1)))))

(maze-game MAZE)