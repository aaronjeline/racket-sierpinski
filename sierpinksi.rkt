#lang racket

(require 2htdp/image)

(define tri-length 50)

(define (tri l) (triangle l "solid" "red" ))

(define bg (empty-scene 500 500))

(define-struct p (x y))
(define-struct pl (x y l))

(define (sierpinski image-size depth)
  (draw-sierpinski image-size (empty-scene image-size image-size)
                  (/ image-size (expt 2 depth))))
                  

(define (draw-sierpinski l bg depth)
  (define data (sierpinski-p l (make-p 250 250) depth))
  (define li (pl-l (first (filter pl? data))))
  (foldr (λ (p i) (place-image (tri li) (pl-x p) (pl-y p) i))
         bg
         data))

;; Number Point -> List[Point U Point-Lengths]
(define (sierpinski-p l p depth)
  (cond [(< l depth) `(,(make-pl (p-x p) (p-y p) l))]
        [else
         (let* ([nl (/ l 2)]
                [d (/ nl 2)])
           (append
            (sierpinski-p nl (make-p (p-x p) (- (p-y p) d)) depth)
            (sierpinski-p nl (make-p (- (p-x p) d) (+ (p-y p) d)) depth)
            (sierpinski-p nl (make-p (+ (p-x p) d) (+ (p-y p) d)) depth)))]))
            
(define (pl->p pl)
  (make-p (pl-x pl) (pl-y pl)))
