(ns me.arrdem.irregular
  "Regular expressions.

  As data structures.

  It's a rite of passage I guess."
  (:refer-clojure :exclude [cat])
  (:require [clojure.string :refer [join]]
            [me.arrdem.irregular.char :as char]
            [me.arrdem.irregular.combinators :as comb]))

;; regexp elements:
;;
;; - Combinators
;;   - Concatenation
;;   - Alternation
;;   - Grouping
;;   - Named grouping
;;
;; - Terminal sets
;;   - Ranges
;;   - Range/set union
;;   - Range/set intersection
;;   - Range/set subtraction
;;   - Range/set negation (anything but)
;;
;; Some implementations provide named character classes as a convenience. Eg POSIX defines [:foo:]
;; style character classes, GNU family tools provide standardized \w, \s style named groups and some
;; implementations even provide facilities for referring to the Unicode code-pages.
;;
;; This is all made harder by the question of what's the basic unit of a regular expression.
;;
;; One reasonable answer is a BYTE, defined to be eight consecutive bits. Usually bytes in the ASCII
;; codepage standard are of interest.
;;
;; Another potentially reasonable answer is a BIT. Some protocols and file layouts are specified in
;; terms of bit formats, although this is less common today.
;;
;; The most relevant answer in modernity perhaps is unicode code-points. A code-point is a muti-byte
;; encoding of an integer naming either a glyph, modifier or element of a glyph.
;;
;; 
