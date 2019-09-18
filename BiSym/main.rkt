#lang racket/gui
(provide (all-defined-out))

(define master-type (make-hash '((black . (5 8 10 4.2 0.98 3)) (red . (5 2 10 3.5 0.98 3)) (green . (5 8 10 4.2 0.97 3)) (blue . (5 1 10 3.5 0.98 3)))))

(define (make-2d-vector r c initial)
  (build-vector r (lambda (x) (make-vector c initial))))

(define (ref-2d-vec vec r c)
  (vector-ref (vector-ref vec r) c))

(define (set-2d-vec vec r c val)
  (vector-set! (vector-ref vec r) c val))


(define xsize 20)
(define ysize 20)

(define (poisson lam)
  (define L (exp (* -1 lam)))
  (define k 1)
  (define p (random))
  (define (helper)
    (if (< p L) k
        (begin (set! k (+ k 1)) (set! p (* p (random))) (helper))))
  (helper))


(define (get-units d)
  (cond [(= d 0) '(-1 0)]
        [(= d 1) '(-1 1)]
        [(= d 2) '(0 1)]
        [(= d 3) '(1 1)]
        [(= d 4) '(1 0)]
        [(= d 5) '(1 -1)]
        [(= d 6) '(0 -1)]
        [else '(-1 -1)]))

(define cell%
  (class object%
    (init-field type)
    (init evolved)
    (init params)
    
    (init-field pos)
    (define mass 0)
    (define v-max 0)
    (define velocity 0)
    (define critical-mass 0)
    (define dying-mass 0)
    (define decay-rate 0)
    (define locom-param 0)
    ;(define food-gen 0)
    (define direction (random 8))
    (define prev-distance 0)
    (define loc-decay-rate 0.0001)
    (define evolvescale (list 0.2 0.2)) ;;gen
    
    (super-new)

    (define/public (get-mass)
      mass)

    (define/public (get-vmax)
          v-max)

    (define/public (get-locparam)
          locom-param)

    
    (define (learn)
      (define counter 0)
      (define entry
        (if evolved params (dict-ref master-type type)))
      (define p-list (list 'mass v-max critical-mass dying-mass decay-rate locom-param))
      (define (helper c e)
        (cond [(= c 6) '()]
              [(= c 5) (begin (set! locom-param (car e)) (helper (+ c 1) (cdr e)))]
              [(= c 4) (begin (set! decay-rate (car e)) (helper (+ c 1) (cdr e)))]
              [(= c 3) (begin (set! dying-mass (car e)) (helper (+ c 1) (cdr e)))]
              [(= c 2) (begin (set! critical-mass (car e)) (helper (+ c 1) (cdr e)))]
              [(= c 1) (begin (set! v-max (car e)) (helper (+ c 1) (cdr e)))]
              [(= c 0) (begin (set! mass (car e)) (helper (+ c 1) (cdr e)))]))
      (helper counter entry))

    (learn)

    (define/public (display-all)
      (begin (display "mass ")(display mass) (display " pos ") (display pos) (display " direction ") (display direction)))
    ;(display-all)
    
    (define/public (is-dying?)
      (if (< mass dying-mass) #t #f))

    (define (move-possible? p v units)
      (let* ([x-new (+ (mcar p) (* v (car units)))]
             [y-new (+ (mcdr p) (* v (cadr units)))])
        (cond [(and (> x-new 0) (> y-new 0) (< x-new xsize) (< y-new ysize)) #t]
          [else #f])))

    (define (find-food l)
      (define hop 0)
      (define (helper l h)
        (cond [(> (car l) 0) h]
              [else (helper (cdr l) (+ h 1))]))
      (helper l 0))

    (define (wrong-dir l d)
      (define (helper l d)
        (cond [(= d 0) (car l)]
              [else (helper (cdr l) (- d 1))]))
      (if (helper l d) #f #t))

    (define (all-false l)
      (cond [(null? l) #t]
            [else (if (car l) #f (all-false (cdr l)))]))

    (define (no-food? l)
      (cond [(null? l) #t]
            [(= (car l) 0) (no-food? (cdr l))]
            [else #f]))
    (define (direction-gen prev-d)
      (let* ([temp (random 8)])
        (if (= (abs (- temp prev-d)) 4) (direction-gen prev-d) temp)))

    (define/public (move-old lbool)
      (cond [(all-false lbool) '()]
            [(= prev-distance 0) (begin (set! prev-distance (poisson locom-param))
                                        (set! direction (direction-gen direction))
                                        (move-old lbool))]
            [(wrong-dir lbool direction) (begin (set! prev-distance 0) (move-old lbool))]
            [else (begin (let ([units (get-units direction)])
                           (begin (set-mcar! pos (+ (mcar pos) (car units))) (set-mcdr! pos (+ (mcdr pos) (cadr units)))))
                         (set! prev-distance (- prev-distance 1)))]))

    (define/public (move-new v-cur)
      (define ncall 0)
      (define avail-food (send env food-locs (mcar pos) (mcdr pos) direction (inexact->exact (round v-cur))))
      (define units (get-units direction))
      (define (move v-cur)
         (begin (cond [(= ncall 50) '()]
                      [(= prev-distance 0) (begin (set! prev-distance (poisson locom-param))
                                        (set! direction (direction-gen direction)) (set! ncall (+ ncall 1))
                                        (move v-max))]
                   [(and (= (inexact->exact (round v-cur)) 0) (no-food? avail-food)) (begin (set! prev-distance (poisson locom-param))
                                (set! direction (direction-gen direction)) (set! ncall (+ ncall 1))
                                (move v-max))]
                   [(no-food? avail-food) (if (move-possible? pos (inexact->exact (round v-cur)) units)
                                       (begin (set-mcar! pos (+ (mcar pos) (* (car units) (inexact->exact (round v-cur)))))
                                                                       (set-mcdr! pos (+ (mcdr pos) (* (cadr units) (inexact->exact (round v-cur)))))
                                                                       (set! prev-distance (- prev-distance 1))
                                                                       (set! velocity (inexact->exact (round v-cur))))
                                      (begin (set! ncall (+ ncall 1)) (move (- v-cur 1))))]
            [else (let* ([v-earliest (find-food avail-food)])
                    (if (move-possible? pos v-earliest units) (begin (set-mcar! pos (+ (mcar pos) (* (car units) v-earliest)))
                                                                       (set-mcdr! pos (+ (mcdr pos) (* (cadr units) v-earliest)))
                                                                       (set! prev-distance (- prev-distance 1))
                                                                       (set! velocity v-earliest))
                        (begin (set! prev-distance (poisson locom-param))
                               (set! direction (direction-gen direction)) (set! ncall (+ ncall 1))
                               (move v-max))))])))
      (move v-cur))

    
    (define (eat-food)
      (let* ([food-avail (car (send env food-locs (mcar pos) (mcdr pos) direction v-max))])
        (if (> food-avail 0) (begin (set! mass (+ mass food-avail)) 1) 0)))
                                       

    (define/public (evolve)
      (map (lambda (a b) (+ (* (* a (* b (random))) (- (* 2 (random 2)) 1)) a))  (list v-max locom-param) evolvescale))

    (define (mass-effect)
      (begin
        (set! mass (- mass (+ (* (* (* v-max velocity) loc-decay-rate) (expt mass (/ 1 3)))
                              (* (- 1 decay-rate) mass)))))
        )

    (define/public (reproduce)
      (if (> mass critical-mass) #t #f))

    (define/public (master)
      (define rep (reproduce))
      (define new-param1 (if (reproduce) (let*([e-param (evolve)]) (list (/ mass 2) (car e-param) critical-mass dying-mass decay-rate (cadr e-param)))  '(0 0 0 0 0 0)))
      (define new-param2 (if (reproduce) (let*([e-param (evolve)]) (list (/ mass 2) (car e-param) critical-mass dying-mass decay-rate (cadr e-param)))  '(0 0 0 0 0 0)))
      (mass-effect)
      (move-new v-max)
      (define eat (eat-food))
      (define die (is-dying?))
      (list die rep type new-param1 new-param2 eat pos))))
      



(define environment%
  (class object%

    (init-field cells)
    (init-field xsize)
    (init-field ysize)
    (init-field params)
    (init-field food-gen)
    (define food (make-2d-vector xsize ysize 0))

    (define cell-array (make-2d-vector xsize ysize 0))
    (define cell-count 0)
    (define food-count 0)

    (super-new)


    (define/public (get-food) food)

    (define/public (add-food)
      (let* ([xr (build-list food-gen (lambda (x) (random xsize)))]
             [yr (build-list food-gen (lambda (x) (random ysize)))])
        (for-each (lambda (x y) (set-2d-vec food x y 1)) xr yr)))

    (define (update)
      1)

    (define/public (dir-to-vec dir)
      (cond  [(eq? 0 dir) (cons (- 1) 0)]
             [(eq? 1 dir) (cons (- 1) 1)]
             [(eq? 2 dir) (cons 0 1)]
             [(eq? 3 dir) (cons 1 1)]
             [(eq? 4 dir) (cons 1 0)]
             [(eq? 5 dir) (cons 1 (- 1))]
             [(eq? 6 dir) (cons 0 (- 1))]
             [(eq? 7 dir) (cons (- 1) (- 1))]))
    
    (define/public (food-locs xc yc dir vmax)
      (let* ([dirvec (get-units dir)])
        (map (lambda (x) (if (check-coords
                              (cons (+ xc (* x (car dirvec))) (+ yc (* x (cadr dirvec)))))
                             (ref-2d-vec food
                                         (+ xc (* x (car dirvec))) (+ yc (* x (cadr dirvec))))
                             0))
             (range (+ vmax 1)))))
    
    (define/public (num-cells)
      (define red 0)
      (define black 0)
      (define (helper l)
        (cond [(null? l) (cons red black)]
              [else (begin (if (equal? (get-field type (car l)) 'black) (set! black (+ black 1))
                        (set! red (+ red 1))) (helper (cdr l)))]))
      (helper cells))
        

    (define/public (run-cycle)
      (define new-cells (list ))
      (begin
        (vector-map (lambda (x) (begin
                                (let* ([updates (send x master)])
                                  (begin 
                                    (if (car updates) (set! new-cells new-cells)
                                        (if (cadr updates) (set! new-cells
                                                                 (cons (new cell% (type (third updates)) (evolved #t) (params (fifth updates)) (pos (mcons (mcar (get-field pos x)) (mcdr (get-field pos x)))))
                                                                       (cons (new cell% (type (third updates)) (evolved #t) (params (fourth updates)) (pos (mcons (mcar (get-field pos x)) (mcdr (get-field pos x))))) new-cells)))
                                            (begin
                                              (set-2d-vec food (mcar (seventh updates)) (mcdr (seventh updates))
                                                          (- (ref-2d-vec food (mcar (seventh updates)) (mcdr (seventh updates))) (sixth updates)))
                                              (set! new-cells (cons x new-cells)))))
                                    )))) (list->vector (shuffle cells)))
        (set! cells (reverse new-cells))))

    (define/public (generate-locs)
      (map (lambda (x) (list (get-field type x) (get-field pos x) (expt (send x get-mass) (/ 1 3)))) cells))


    (define/public (check-coords l)
        (let* ([xc (car l)]
               [yc (cdr l)])
          (and (< xc xsize) (and (> xc (- 1)) (and (< yc ysize) (and (> yc (- 1)) (eq? (ref-2d-vec cell-array xc yc ) 0)))))))

    (define/public (get-statistic)
      (define avg-vmax (make-hash (hash-map master-type (lambda (key value) (cons key 0)))))
      (define cell-count (make-hash (hash-map master-type (lambda (key value) (cons key 0)))))
      (define avg-locparam (make-hash (hash-map master-type (lambda (key value) (cons key 0)))))
      (begin
        (map (lambda (x)
               (let* ([cell-type (get-field type x)])
                 (hash-set! cell-count cell-type (+ 1 (hash-ref cell-count cell-type)))
                 (hash-set! avg-vmax cell-type (+ (send x get-vmax) (hash-ref avg-vmax cell-type)))
                 (hash-set! avg-locparam cell-type (+ (send x get-locparam) (hash-ref avg-locparam cell-type))))) cells)
        (hash-map avg-vmax (lambda (key value) (hash-set! avg-vmax key (if (eq? (hash-ref cell-count key) 0) 0
                                                                  (/ (hash-ref avg-vmax key) (hash-ref cell-count key))))))
        (hash-map avg-locparam (lambda (key value) (hash-set! avg-locparam key (if (eq? (hash-ref cell-count key) 0) 0
                                                                  (/ (hash-ref avg-locparam key) (hash-ref cell-count key))))))
        (list cell-count avg-vmax avg-locparam)))
          
    ))


;;;Simulate Function
(define time 0)
(define (simulate)
  (define t 0)
  (define (helper)
    (begin (display " New Step ")
           (set! time (+ time 1))
           (display time)
           ;(display t)
           (display "\n")
           (if (= (modulo t 20) 0) (send env add-food) '()); previously t = 3
           ;(if (= (modulo t 10) 0) (send env get-food) '())
           (send env run-cycle)
           ;(sleep 2)
           (set! t (+ t 1))
           (display (send env get-statistic))
           (display "\n")
           ;(helper)
           ))
  (helper))
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; GUI ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;  SIMULATION FRAME ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define frame-sim (new frame%
                       [label "SIMULATION"]
                       [width 500]
                       [height 500]
                       [stretchable-width #t]
                       [stretchable-height #t]))
;;;;;;;; PANELS IN SSIMULATION FRAME ;;;;;;;;;;
(define main-panel
  (new horizontal-panel%
       [parent frame-sim]
       [alignment '(center center)]
       ))
(define panel-grid
  (new vertical-panel%
       [parent main-panel]
       [alignment '(center center)]
       [stretchable-width #t]
       [style '(border)]
       [stretchable-height #t]
       ))

(define panel-msg
  (new vertical-panel%
       [parent main-panel]
       [alignment '(center center)]
       [stretchable-width #t]
       [style '(border)]
       [stretchable-height #t]
       ))

(define panel-black
  (new horizontal-panel%
       [parent panel-msg]
       [alignment '(center top)]
       [style '(border)]
       ))
(define panel-green
  (new horizontal-panel%
       [parent panel-msg]
       [alignment '(center top)]
       [style '(border)]
       ))
(define panel-red
  (new horizontal-panel%
       [parent panel-msg]
       [alignment '(center top)]
       [style '(border)]
       ))
(define panel-blue
  (new horizontal-panel%
       [parent panel-msg]
       [alignment '(center top)]
       [style '(border)]
       ))

(define (draw-grid1 dc x)
(if (= x 500) (send dc draw-line x 0 x 500)
  (begin
    (send dc draw-line x 0 x 500)
    (draw-grid1 dc (+ x 20)))))

(define (draw-grid2 dc x)
(if (= x 500) (send dc draw-line 0 x 500 x)
  (begin
    (send dc draw-line 0 x 500 x)
    (draw-grid2 dc (+ x 20)))))

(define canvas 
 (new canvas% 
      [parent panel-grid] 
      [paint-callback   
       (lambda (c dc)
         (begin (draw-grid1 dc 0) (draw-grid2 dc 0))
         )]))

;;;;;;; BUTTONS FOR SIMULATION FRAME;;;;;;;;;;;;;;;;
; STOP BUTTON
(define stop-button
  (new button%
       [parent panel-msg]
       [label "STOP"]
       [callback (lambda (b e)
                   (begin (set! stop-state #t)  )
                   )]))
(define stop-state #f)

;;;;;;;;;;;;;;

; START BUTTON
(define blue-brush (new brush% [color "black"]))
(define red-brush (new brush% [color "red"]))
(define green-brush (new brush% [color "green"]))
(define black-brush (new brush% [color "blue"]))

(define start-button
  (new button%
       [parent panel-msg]
       [label "START"]
       [callback (lambda (b e) 
                   (thread (lambda () (define (until)
  (cond [(not (eq? stop-state #t))
           (send canvas on-paint)
           (simulate)
           (map (lambda (x)
                  (cond [(eq? 'red (car x)) (begin (send (send canvas get-dc) set-brush red-brush) (send (send canvas get-dc) draw-ellipse (* 25 (mcar (second x))) (* 25 (mcdr (second x))) (* 8 (third x)) (* 8 (third x))))]
                        [(eq? 'black (car x)) (begin (send (send canvas get-dc) set-brush blue-brush) (send (send canvas get-dc) draw-ellipse (* 25 (mcar (second x))) (* 25 (mcdr (second x))) (* 8 (third x)) (* 8 (third x))))]
                        [(eq? 'green (car x)) (begin (send (send canvas get-dc) set-brush green-brush) (send (send canvas get-dc) draw-ellipse (* 25 (+ (mcar (second x)) 0)) (* 25 (+ (mcdr (second x)) 0 )) (* 8 (third x)) (* 8 (third x))))]
                        [(eq? 'blue (car x)) (begin (send (send canvas get-dc) set-brush black-brush) (send (send canvas get-dc) draw-ellipse (* 25 (mcar (second x))) (* 25 (mcdr (second x))) (* 8 (third x)) (* 8 (third x))))]
                        )                     
                  ) (send env generate-locs))
           (sleep 0.1)           
           (send (send canvas get-dc) erase)
           
           (let ([stats (send env get-statistic)]) 
           (cond [(dict-has-key? (second stats) 'red) 		(send red-msg set-label (number->string (dict-ref (car stats) 'red))) 
								(send red-locom set-label (number->string (dict-ref (third stats) 'red))) 
                                                                (send red-velo set-label (number->string (dict-ref (second stats) 'red))) ])
           (cond [(dict-has-key? (second stats) 'black)
                                                                (send black-msg set-label (number->string (dict-ref (car stats) 'black)))
								(send black-locom set-label (number->string (dict-ref (third stats) 'black))) 
                                                                (send black-velo set-label (number->string (dict-ref (second stats) 'black)))])
           (cond [(dict-has-key? (second stats) 'blue)
                                                                (send blue-msg set-label (number->string (dict-ref (car stats) 'blue)))
								(send blue-locom set-label (number->string (dict-ref (third stats) 'blue))) 
                                                                (send blue-velo set-label (number->string (dict-ref (second stats) 'blue)))])
           (cond [(dict-has-key? (second stats) 'green)
                                                                (send green-msg set-label (number->string (dict-ref (car stats) 'green)))
                                                                (send green-locom set-label (number->string (dict-ref (third stats) 'green)))
                                                                (send green-velo set-label (number->string (dict-ref (second stats) 'green)))])
           )
           (until)
           ]
        [else (begin (set! stop-state #f) "SIMULATION STOPPED")]))
           (until))))]))

(define vmax-list '())

; RESET BUTTON
(define reset-button
  (new button%
       [parent panel-msg]
       [label "RESET"]
       [callback (lambda (b e)
                   (set! env (new environment% (cells (func-build-list cell_list))
                  (food-gen 20) (xsize 20) (ysize 20) (params (list ))))
                   (send canvas refresh))]))

; FUNCTION TO CREATE THE CELL OBJECTS FROM THE CELLS 
(define (func-build-list list_types)
  (cond [(null? (cdr list_types)) (build-list (cdar list_types) (lambda (x) (new cell% [type (string->symbol (caar list_types))] [evolved #f] [params '(0 0 0 0 0 0)] [pos (mcons (random 20) (random 20))])))  ]
        [else
         (append
          (build-list (cdar list_types) (lambda (x) (new cell% [type (string->symbol (caar list_types))] [evolved #f] [params '(0 0 0 0 0 0)] [pos (mcons (random 20) (random 20))])))
          (func-build-list (cdr list_types)))]))

;;;;;;;;;;;;;;;;;;;;;;;;; INPUT FRAME ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define frame-input (new frame%
                         [label "CELL INPUTS"]
                         [width 400]
                         [height 400]))

(define cell-type
  (new combo-field%
       (label "type")
       (parent frame-input)
              (choices (list "black" "red" "green" "blue"))
              (init-value "Type")
              (callback (lambda (t e)
                        (let [(pop (send cell-type get-value))] (set! choice pop) )))))

              
(define (change-cell)
  (set! cell_list (append cell_list (list (cons choice (string->number number_of_cells))))))

(define cell_list '())

(define number_of_cells "")
(define choice "")

(define no_of_cells-field (new text-field%
                        (label "Number of cells")
                        (parent frame-input)
                        (init-value "0")
                        (callback (lambda (t e)
                                    (let [(values (send no_of_cells-field get-value))] (set! number_of_cells values) ) ))))

(define button-next
  (new button%
       [parent frame-input]
       [label "NEXT CELL"]
       [callback (lambda (b e)
                   (change-cell)
                   )
                   ]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define env 0)

(define input-done-button
  (new button%
       [parent frame-input]
       [label "DONE"]
       [callback (lambda (b e)
                   (begin (send frame-input show #f)
                   (send frame-sim show #t)
                   (set! env (new environment% (cells (func-build-list cell_list))
                  (food-gen 20) (xsize 20) (ysize 20) (params (list ))))))]))

(send frame-input show #t)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;DISPLAY VALUES FOR CELLS

(define black
  (new message%
       [parent panel-black]
       [label "BLACK"]
       [auto-resize #t]
))
(define green
  (new message%
       [parent panel-green]
       [label "GREEN"]
       [auto-resize #t]
))
(define red
  (new message%
       [parent panel-red]
       [label "RED"]
       [auto-resize #t]
))
(define blue
  (new message%
       [parent panel-blue]
       [label "BLUE"]
       [auto-resize #t]
))

(define red-msg
  (new message%
       [parent panel-red]
       [label "red"]
       [auto-resize #t]
       ))
(define black-msg
  (new message%
       [parent panel-black]
       [label "black"]
       [auto-resize #t]
       ))
(define green-msg
  (new message%
       [parent panel-green]
       [label "green"]
       [auto-resize #t]
       ))
(define blue-msg
  (new message%
       [parent panel-blue]
       [label "blue"]
       [auto-resize #t]
       ))

(define red-velo
  (new message%
       [parent panel-red]
       [label "red-velocity"]
       [auto-resize #t]
       ))
(define black-velo
  (new message%
       [parent panel-black]
       [label "black-velocity"]
       [auto-resize #t]
))
(define green-velo
  (new message%
       [parent panel-green]
       [label "green-velocity"]
       [auto-resize #t]
       ))
(define blue-velo
  (new message%
       [parent panel-blue]
       [label "blue-velocity"]
       [auto-resize #t]
))

(define red-locom
  (new message%
       [parent panel-red]
       [label "red-locom-param"]
       [auto-resize #t]
       ))
(define black-locom
  (new message%
       [parent panel-black]
       [label "black-locom-param"]
       [auto-resize #t]
))
(define green-locom
  (new message%
       [parent panel-green]
       [label "green-locom-param"]
       [auto-resize #t]
       ))
(define blue-locom
  (new message%
       [parent panel-blue]
       [label "blue-locom-param"]
       [auto-resize #t]
))



