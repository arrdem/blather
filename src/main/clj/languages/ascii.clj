(ns languages.ascii
  "The ASCII character set and widely adopted ranges thereon."
  (:refer-clojure :only [apply])
  (:require [irregular.core :as i]))

(def any
  "The entire ASCII character range."
  (i/->range \u0000 \u007f))

(def whitespace
  "ASCII whitespace."
  (i/union
   \space
   \tab
   \newline
   \u000B ;; Vertical tab
   \formfeed
   \return))

(def blank
  "NOT ALL WHITESPACE. Just in-line whitespace. `[ \\t]`"
  (i/union \tab \space))

(def lower
  "ASCII lower case."
  (i/->range \a \z))

(def upper
  "ASCII upper case."
  (i/->range \A \Z))

(def alpha
  "ASCII alphas."
  (i/union lower upper))

(def digit
  "Known \"standardized\" digit range `[0-9]` per jdk8 Pattern"
  (i/->range \0 \9))

(def punct
  "ASCII punctuation."
  (apply i/union "!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~"))

(def control
  "A control character"
  (i/union
   (i/->range \u0000 \u001f)
   \u007f))

(def space
  "A whitespace character `[ \\t\\n\\x0B\\f\\r]`"
  (apply i/union "[ \t\n\u000B\f\r]"))

(def graph
  "Any visible (ASCII) character"
  (i/subtraction any control space))

(def print
  "Any printable character"
  (i/union graph \u0020))
