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
#lang scribble/manual
@(require
   (for-label
    racket
    pretty-graphs)
   scribble/extract)
@title{Pretty Graphs}
@author{Vincent Nys}
@;TODO: can I use other-doc without inserting "the [...] documentation"?
Racket has some @hyperlink["http://docs.racket-lang.org/graph/index.html?q=pict" "really cool"] @hyperlink["http://soegaard.github.io/docs/metapict/metapict.html" "drawing tools"]. It has @hyperlink["http://docs.racket-lang.org/graph/index.html?q=graph" "a flexible graph library"]. It has @hyperlink["http://docs.racket-lang.org/scribble/"]{document} @hyperlink["http://docs.racket-lang.org/pollen/"]{producing} @hyperlink["http://docs.racket-lang.org/slideshow/"]{DSL's}. By all logic, it should have good graph visualization tools. Unfortunately, it does not. Pretty Graphs aims to fill in that gap. Not right now, but eventually.
@defmodule[pretty-graphs]

@section{Priorities}
The main goal of Pretty Graphs is to automatically generate picts from generic graphs (as per the @racket[graph] library). Generating efficient layouts for all sorts of graphs is not an easy task, so this project should grow organically. Different types of graphs will be added based on users' needs and the API @emph{will} change over time (pull requests very welcome, see @hyperlink["https://cs.brown.edu/~rt/gdhandbook/"]{The Handbook of Graph Drawing and Visualization} if you want to lend a hand).

The secondary goal is to be able to tweak produced graphs. This is nice to have, but tricky when the tweaks are very fine-grained.

@section{API}
@include-extracted[pretty-graphs]