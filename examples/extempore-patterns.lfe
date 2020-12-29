
;; need to load this first, but only once per running session of the Extempore
;; binary; note that it will take a while (~1 minute) and this call will block
;; until the load is finished
(xt:sys-load "examples/sharedsystem/setup.xtm")

;; Pull in the LFE macros for Extempore's pattern language
(include-lib "undertone/include/xt/patterns.lfe")

;; perform the 8-note "ascending scale" loop starting at middle C
(/> 'ascending-scale 4 0 (play 'syn1 '@1 80 'dur) (scale 4 8))

;; change the tempo:
(set-tempo! 72)

;; stop the scale:
(// 'ascending-scale 4 0 (play 'syn1 '@1 80 'dur) (scale 4 8))

;; that step is useful for live coding, as starting / stopping a scale has the
;; exact same form, with just a one character change in the function name; for
;; non-live-coding situations, the following may be more convenient:
(// 'ascending-scale)

;; For more info on the Extempore pattern language, see:
;; * https://extemporelang.github.io/docs/guides/pattern-language/