(ns me.arrdem.irregular
  "Regular expressions.

  As data structures.

  It's a rite of passage I guess.")

;; regexp elements:
;;
;; - Concatenation
;; - Alternation (of groups or terms)
;; - Character groups
;;   - Set union
;;   - Set intersection
;;   - Set subtraction
;;   - Set negation (anything but)
;;   - Ranges
;; - Capture groups (named)
;;
;; Some implementations provide named character classes as a convenience. Eg POSIX defines [:foo:]
;; style character classes, GNU family tools provide standardized \w, \s style named groups and some
;; implementations even provide facilities for referring to the Unicode codepages.
