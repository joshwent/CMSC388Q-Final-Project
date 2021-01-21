#lang racket


(require 2htdp/image 2htdp/universe)

(define width 500)
(define height 400)
(define background "black")
(define velocity 2)

; constant: world size, paddle size, ball size, brick locations
; variable: paddle x, ball x and y, brick exists (#t or #f)
; variables need to be a part of the world definition

(define (blank)
  (empty-scene width height background))

(define paddle-img
  (rectangle 40 10 "solid" "white"))

(define ball-img
  (circle 10 "solid" "gray"))

(struct world (paddle ball))
(struct paddle (x))
(struct ball (x y angle))

(define first-world
  (world (paddle (/ width 2))
         (ball (/ width 2)(/ height 2) 0.785398) ) )

(module* main #f
  (big-bang first-world
    [on-draw draw-everything]
    [on-tick update-ball 1/28]
    [on-key update-paddle]))

(define (draw-everything w)
  (draw-ball (world-ball w)
   (draw-paddle (world-paddle w) (blank))))

(define (draw-paddle p img)
  (place-image paddle-img (paddle-x p) (- height 10) img) )

(define (draw-ball b img)
  (place-image ball-img (ball-x b) (ball-y b) img) )

(define (update-ball w)
  (world (world-paddle w)
         (ball (new-ball-x (ball-x (world-ball w))(ball-angle (world-ball w)))
               (new-ball-y (ball-y (world-ball w))(ball-angle (world-ball w)))
               (ball-angle (world-ball w))) ) )

(define (new-ball-x curr-x curr-angle)
  (+ curr-x (* (cos curr-angle) velocity) ) )
(define (new-ball-y curr-y curr-angle)
  (+ curr-y (* (sin curr-angle) velocity) ) )
  
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