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
  pict/tree-layout)

(define (rdag? g)
  (and (dag? g)
       (let ([tc (transitive-closure g)])
         (ormap
          (λ (v)
            (andmap
             cdr
             (filter
              (λ (edge/bool) (equal? (first (car edge/bool)) v))
              (hash->list tc))))
          (get-vertices g)))))
(provide
 (proc-doc/names
  rdag?
  (-> any/c boolean?)
  (e)
  @{Tests whether @racket[e] is a single-rooted, directed acyclic graph.}))

;; then, for every edge in the original, add appropriate edges to the pict
(define (rdag->pict rdag vert->pict)
  (define (mssubtree->tl verts->picts mst root)
    (let ([edges-from-root (filter (λ (e) (equal? (first e) root)) mst)])
      (apply
       tree-layout
       #:pict (hash-ref verts->picts root)
       (map
        (compose (curry mssubtree->tl verts->picts mst) second)
        edges-from-root))))
  (let* ([mst (mst-kruskal rdag)]
         [Δ-edges (remove* mst (get-edges rdag))]
         [root (first (tsort rdag))]
         [verts->picts
          (for/hash ([v (get-vertices rdag)])
            (values v (vert->pict v)))]
         [tl (mssubtree->tl verts->picts mst root)]
         [mst-pict (naive-layered tl)])
    (foldl
     (λ (e acc)
       (p:pin-line acc
                   (hash-ref verts->picts (first e)) p:cc-find
                   (hash-ref verts->picts (second e)) p:cc-find
                   #:color "gray"
                   #:under? #t))
     mst-pict Δ-edges)))
(provide
 (proc-doc/names
  rdag->pict
  (-> rdag? (-> any/c p:pict?) p:pict?)
  (rdag vert->pict)
  @{Converts a rooted, directed acyclic graph @racket[rdag] into a pict.
 Vertices are converted using the user-supplied function @racket[vert->pict], but connections between vertices are computed.}))