#lang racket

(require 2htdp/image 2htdp/universe)

(define width 500)
(define height 400)
(define background "black")
(define velocity 2)
(define radius 10)
(define paddle-y-loc (- height 10))
(define paddle-height 10)
(define paddle-width 40)
(define brick-wdith 50)
(define brick-height 20)

; constant: world size, paddle size, ball size, brick locations
; variable: paddle x, ball x and y, brick exists (#t or #f)
; variables need to be a part of the world definition

(define (blank)
  (empty-scene width height background))

(define paddle-img
  (rectangle paddle-width paddle-height "solid" "white"))

(define ball-img
  (circle radius "solid" "gray"))

(define brick-img
  (rectangle brick-width brick-height "solid" "white"))

(struct world (paddle ball))
(struct paddle (x))
(struct ball (angle x y))
(struct brick (exists))

(define first-world
  (world (paddle (/ width 2))
         (ball (/ pi 4)(- width 100)(* height (/ 1 4)))
         (brick (#t))))

(module* main #f
  (big-bang first-world
    [on-draw draw-everything]
    [on-tick update-ball 1/56]
    [on-key update-paddle]))

(define (draw-everything w)
  (draw-brick (world-brick w)
  (draw-ball (world-ball w)
  (draw-paddle (world-paddle w) (blank)))))

(define (draw-paddle p img)
  (place-image paddle-img (paddle-x p) paddle-y-loc img) )

(define (draw-ball b img)
  (place-image ball-img (ball-x b) (ball-y b) img) )

(define (draw-brick br img)
  (place-image brick-img (/ width 2) (* height (/ 2 3)) img))

(define (update-ball w)
  (world (world-paddle w)
         (new-ball (world-ball w) (world-paddle w) (world-brick w))))
;         (ball (new-angle (world-ball w))
;               (new-ball-x (ball-x (world-ball w))(ball-angle (world-ball w)))
;               (new-ball-y (ball-y (world-ball w))(ball-angle (world-ball w)))
;               ) ) )

(define (new-ball curr-ball curr-paddle curr-brick)
  (define (curr-paddle-x) (paddle-x curr-paddle))
  (define (curr-x) (ball-x curr-ball))
  (define (curr-y) (ball-y curr-ball))
  (define (curr-angle) (ball-angle curr-ball))
  (define (curr-brick) (brick-exists curr-brick))
  (define (new-x) (+ (curr-x) (* (cos (curr-angle)) velocity)))
  (define (new-y) (+ (curr-y) (* (sin (curr-angle)) velocity)))
  (cond ;calculate new angle
    [(or (>= (new-x) (- width radius))(<= (new-x) radius))
     (ball (- pi (curr-angle))(curr-x)(curr-y))]
    [(<= (new-y) radius)
     (ball (- (* 2 pi) (curr-angle))(curr-x)(curr-y))]
    [(and (>= (+ (new-y) radius) (- paddle-y-loc (/ paddle-height 2)))
          (and (> (new-x)(- (curr-paddle-x)(/ paddle-width 2)))
               (< (new-x)(+ (curr-paddle-x)(/ paddle-width 2)))))
     (ball (- (* 2 pi) (curr-angle))(new-x)(curr-y))]
    [else (ball (curr-angle)(new-x)(new-y))]))


;(define (new-ball-x curr-x curr-angle)
;  (cond
;    [(< (+ curr-x (* (cos curr-angle) velocity)) (- width 10))
;     (+ curr-x (* (cos curr-angle) velocity))]
;    [else (- curr-x 1)]))
;(define (new-ball-y curr-y curr-angle)
;  (cond
;    [(> (+ curr-y (* (sin curr-angle) velocity)) 0)
;     (+ curr-y (* (sin curr-angle) velocity))]
;    [else (curr-y 100)]))
;(define (new-angle curr-ball)
;  (cond 
;    [(and (>= (ball-x curr-ball) (- width 10)) (< (ball-angle curr-ball) (/ pi 2))) ;condition
;     (- pi (ball-angle curr-ball))] ;result
;    [(and (>= (ball-x curr-ball) (- width 10)) (> (ball-angle curr-ball) (/ pi 2))) ;condition
;     (+ pi (ball-angle curr-ball))] ;result
;    [(and (<= (ball-x curr-ball) 0) (< (ball-angle curr-ball) (/ pi 2))) ;condition
;     (+ 0 (ball-angle curr-ball))] ;result
;    [(and (<= (ball-x curr-ball) 0) (> (ball-angle curr-ball) (/ pi 2))) ;condition
;     (- pi (ball-angle curr-ball))] ;result
;    [else (ball-angle curr-ball)]))


(define (update-paddle w ke)
  (match ke
    ["right" (world (paddle (+ (paddle-x (world-paddle w)) 10))
                    (world-ball w))]
    ["left" (world (paddle (- (paddle-x (world-paddle w)) 10))
                   (world-ball w))]
    [_ w]))




(define (reset i ke)
  (match ke
    [" " (cons 0 (cdr i))]
    ["left" (i)]
    ["y" (cons (car i) "yellow")]
    ["p" (cons (car i) "purple")]
    [_ i]))

(define (add5 x)
  (cons (+ (car x) 5)
        (cdr x)))

; function for drawing images
; from: https://docs.racket-lang.org/teachpack/2htdpPlanet_Cute_Images.html
(define (stack imgs)
  (cond
    [(empty? (rest imgs)) (first imgs)]
    [else (overlay/xy (first imgs)
                      0 40
                      (stack (rest imgs)))]))