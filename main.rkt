;This is free and unencumbered software released into the public domain.
;
;Anyone is free to copy, modify, publish, use, compile, sell, or
;distribute this software, either in source code form or as a compiled
;binary, for any purpose, commercial or non-commercial, and by any
;means.
;
;In jurisdictions that recognize copyright laws, the author or authors
;of this software dedicate any and all copyright interest in the
;software to the public domain. We make this dedication for the benefit
;of the public at large and to the detriment of our heirs and
;successors. We intend this dedication to be an overt act of
;relinquishment in perpetuity of all present and future rights to this
;software under copyright law.
;
;THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR
;OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
;ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;OTHER DEALINGS IN THE SOFTWARE.
;
;For more information, please refer to <http://unlicense.org/>
#lang at-exp racket
(require scribble/srcdoc)
(require (for-doc scribble/manual))
(require
  graph
  (prefix-in p: pict)
  metapict)

(define g0
  (directed-graph
   '((1 2))))
(define g1
  (directed-graph
   '((10 20) (10 30) (20 40) (20 50) (30 60) (40 70) (50 80) (50 90) (60 100) (60 110))))
(define g2
  (directed-graph
   '((1 4) (1 5) (2 6) (2 7) (3 8))))
(define g3
  (directed-graph
   '((1 5) (2 5) (3 6) (3 7) (4 8) (5 9) (6 9) (7 9) (8 9))))
(define g4
  (directed-graph
   '((10 20) (10 30) (20 40) (20 50) (30 60) (40 70) (50 80) (50 90) (60 100) (60 110) (110 120) (120 130) (130 140))))
(define (dag-roots dag)
  (define reached-verts (list->set (map second (get-edges dag))))
  (filter
   (λ (v) (not (set-member? reached-verts v)))
   (get-vertices dag)))
(module+ test
  (require rackunit)
  (check-equal?
   (dag-roots g1)
   '(10))
  (check-equal?
   (dag-roots g2)
   '(1 2 3))
  (check-equal?
   (dag-roots g3)
   '(1 2 3 4)))

(define (map-accumulatel mapping-function acc lst)
  (define with-reverse-mapping
    (foldl (λ (lst-elem fold-acc)
             (match fold-acc
               [(cons mapping map-acc-acc)
                (let* ([mapped-pair (mapping-function lst-elem map-acc-acc)]
                       [mapped-elem (car mapped-pair)]
                       [updated-map-acc-acc (cdr mapped-pair)])
                  (cons (cons mapped-elem mapping) updated-map-acc-acc))]))
           (cons (list) acc)
           lst))
  (cons
   (reverse
    (car with-reverse-mapping))
   (cdr with-reverse-mapping)))

(define (dag->pict dag vert->pict #:xϵ [xϵ 25] #:yϵ [yϵ 100])
  (struct layer-elem (parents vert pict) #:transparent)
  (struct pos-layer-elem (le pt) #:transparent)
  (define roots (dag-roots dag))
  (define (offset-x le acc)
    (match le
      [(layer-elem _ _ p)
       (cons
        (pos-layer-elem le (pt (+ acc (/ (pict-width p) 2)) 0))
        (+ acc (pict-width p) xϵ))]))
  (define (offset-ys off layer)
    (let ([max-height
           (apply
            max
            (map
             (compose
              pict-height
              layer-elem-pict
              pos-layer-elem-le)
             layer))])
      (map
       (λ (e)
         (match e
           [(pos-layer-elem le (pt x 0))
            (pos-layer-elem le (pt x (- off max-height yϵ)))]))
       layer)))
  (define (expand-layers ls) ; note: top layer will be the first element, doesn't matter though
    (let* ([next-layer-elems
            (foldl
             (λ (ple acc)
               (let*-values ([(ple-children) (get-neighbors dag (layer-elem-vert (pos-layer-elem-le ple)))]
                             [(to-be-updated brand-new) (partition (λ (c) (member c (map layer-elem-vert acc))) ple-children)])
                 (map
                  (λ (le) (if (member (layer-elem-vert le) to-be-updated) (struct-copy layer-elem le [parents (append (layer-elem-parents le) (list (layer-elem-vert (pos-layer-elem-le ple))))]) le))
                  (append acc (map (λ (v) (layer-elem (list (layer-elem-vert (pos-layer-elem-le ple))) v (vert->pict v))) brand-new)))))
             empty
             (car ls))])
      (if (not (null? next-layer-elems))
          (expand-layers
           (cons
            (offset-ys
             (pt-y (pos-layer-elem-pt (caar ls))) ; all parent layer elements have same Y, so just take first
             (car
              (map-accumulatel
               offset-x
               0
               next-layer-elems)))
            ls))
          ls)))
  (if (null? roots)
      (draw)
      (let* ([root-layer-elems
              (map (λ (r) (layer-elem empty r (vert->pict r))) roots)]
             [root-layer-elems/x-pos
              (car (map-accumulatel offset-x 0 root-layer-elems))]
             [pos-layers (expand-layers (list root-layer-elems/x-pos))]
             [pos-layer-elems (flatten pos-layers)]
             [min-x
              (apply
               min
               (map
                (λ (ple)
                  (-
                   (pt-x (pos-layer-elem-pt ple))
                   (/ ((compose pict-width layer-elem-pict pos-layer-elem-le) ple) 2)))
                pos-layer-elems)) ; should be 0, then
              ]
             [max-x
              (apply
               max
               (map
                (λ (ple)
                  (+
                   (pt-x (pos-layer-elem-pt ple))
                   (/
                    (pict-width
                     (layer-elem-pict
                      (pos-layer-elem-le ple)))
                    2)))
                pos-layer-elems))]
             [min-y (apply min (map (compose pt-y pos-layer-elem-pt) pos-layer-elems))]
             [max-y
              (apply
               max
               (map
                (λ (ple)
                  (+
                   (pt-y (pos-layer-elem-pt ple))
                   (pict-height (layer-elem-pict (pos-layer-elem-le ple)))))
                pos-layer-elems))])
        (with-window (window min-x max-x min-y max-y)
          ;; this is needed because otherwise picts drawn by metapict are always 100 x 100
          ;; that would mess up the use of plain old pict-width and pict-height for labels, which don't assume this kind of rescaling
          ;; in other words, this undoes the rescaling
          ;; a parameter in metapict for this purpose would be nice, though...
          (set-curve-pict-size (- max-x min-x) (- max-y min-y))
          (draw
           ; the elements themselves
           (for/draw ([ple pos-layer-elems])
             (draw
              (label-top
               (layer-elem-pict
                (pos-layer-elem-le
                 ple))
               (pos-layer-elem-pt ple))))
           ; the connections from parent bottom to element top
           (for/draw ([ple pos-layer-elems])
             (let ([parents (layer-elem-parents (pos-layer-elem-le ple))])
               (for/draw ([parent parents])
                 (curve
                  (pos-layer-elem-pt
                   (findf
                    (λ (e)
                      (equal?
                       parent
                       (layer-elem-vert
                        (pos-layer-elem-le e))))
                    pos-layer-elems))
                  ..
                  (pt+
                   (pos-layer-elem-pt ple)
                   (vec
                    0
                    (pict-height
                     (layer-elem-pict
                      (pos-layer-elem-le ple))))))))))))))
(provide
 (proc-doc/names
  dag->pict
  (->* (dag? (-> any/c pict?)) (#:xϵ (and/c positive? number?) #:yϵ (and/c positive? number?)) pict?)
  ((dag vert->pict) ((xϵ 25) (yϵ 100)))
  @{Converts a directed acyclic graph @racket[dag] into a pict.
 Vertices are converted using the user-supplied function @racket[vert->pict], but connections between vertices are computed. This function is still subject to changes, both in interface and output.}))

