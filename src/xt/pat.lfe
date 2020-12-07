;;;; Functions supporting Extempore's pattern language.
(defmodule xt.pat
  (export
   (/> 5)
   (// 1) (// 5)))

(defun /> (name length offset patt-expr patt-lists)
  "Define and play a pattern."
  (xt.msg:async
   (xt.lang:sexp
    ":>"
    (string:join
     (list (xt.lang:->types `(,name ,length ,offset))
           patt-expr
           patt-lists)
     " "))))

(defun // (name)
  "Stop a pattern."
  (// name 1 0 (xt.common:play 'nil 0 0 0) (xt.common:scale 0 0)))

(defun // (name length offset patt-expr patt-lists)
  "Stop a pattern."
  (xt.msg:async
   (xt.lang:sexp
    ":|"
    (string:join
     (list (xt.lang:->types `(,name ,length ,offset))
           patt-expr
           patt-lists)
     " "))))