(ns me.arrdem.irregular
  "Regular expressions.

  As data structures.

  It's a rite of passage I guess."
  (:refer-clojure :exclude [cat])
  (:require [clojure.string :refer [join]]
            [me.arrdem.irregular.char :as char]))

;; regexp elements:
;;
;; - Concatenation
;; - Alternation (of groups or terms)
;; - Character groups
;;   - Ranges
;;   - Range/set union
;;   - Range/set intersection
;;   - Range/set subtraction
;;   - Range/set negation (anything but)
;; - Capture groups (named)
;;
;; Some implementations provide named character classes as a convenience. Eg POSIX defines [:foo:]
;; style character classes, GNU family tools provide standardized \w, \s style named groups and some
;; implementations even provide facilities for referring to the Unicode codepages.
;;
;; This is all made harder by the question of what's the basic unit of a regular expression.
;;
;; One reasonable answer is a BYTE, defined to be eight consecutive bits. Usually bytes in the ASCII
;; codepage standard are of interest.
;;
;; Another potentially reasonable answer is a BIT. Some protocols and file layouts are specified in
;; terms of bit formats, although this is less common today.
;;
;; The most relevant answer in modernity perhaps is unicode codepoints. A codepoint is a muti-byte
;; encoding of an integer naming either a glyph, modifier or element of a glyph.

;; Concatenation
;;----------------------------------------
(defn cat
  "(con)catenation of patterns.

  Returns a pattern which matches the left first, and then the right."
  [a b]
  {:tag        ::cat
   :multi-byte (or (char/multibyte? a)
                   (char/multibyte? b))
   :pattern1   a
   :pattern2   b})

(defmethod char/multibyte? ::cat [{:keys [pattern1 pattern2]}]
  (or (char/multibyte? pattern1)
      (char/multibyte? pattern2)))

;; Alternation
;;----------------------------------------
(defn alt
  "Alternation of patterns.

  Returns a pattern which matches either the left or the right pattern.

  May also be thought of as union between patterns."
  [a b]
  {:tag        ::alt
   :multi-byte (or (char/multibyte? a)
                   (char/multibyte? b))
   :pattern1   a
   :pattern2   b})

(defmethod char/multibyte? ::alt [{:keys [pattern1 pattern2]}]
  (or (char/multibyte? pattern1)
      (char/multibyte? pattern2)))

(defn group
  "A group wrapping another pattern.

  Used lexically for concatenations to be terminated."
  [a]
  {:tag        ::group
   :multi-byte (char/multibyte? a)
   :pattern    a})
