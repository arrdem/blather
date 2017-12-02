(ns languages.jdk-re
  "Compiles an irregular regex IR down to a JDK legal regex string."
  (:refer-clojure :exclude [compile])
  (:require [clojure.java.io :refer [resource]]
            [irregular.core :as i :refer [tag-dx]]
            [irregular.combinators :as c]
            [languages.common :as m]
            [detritus.bimap :refer [bimap]]
            [instaparse.core :refer [parser transform]]))

;; Parsing JDK regex patterns
(def -parser
  "The Instaparse parser used to read RFC5234."
  (parser (slurp (resource "jdk-regex.insta"))
          :start :pattern))

(def -whitespace
  "Known \"standardized\" whitespace range `[ \\t\\n\\x0B\\f\\r]` per jdk8 Pattern"
  (i/union
   \space
   \tab
   \newline
   \u000B ;; Vertical tab
   \formfeed
   \return))

(def -lower
  "Known \"standardized\" lower case range `[a-z]` per jdk8 Pattern"
  (i/->range \a \z))

(def -upper
  "Known \"standardized\" upper case range `[A-Z]` per jdk8 Pattern"
  (i/->range \A \Z))

(def -alpha
  "An alphabetic character `[\\p{Lower}\\p{Upper}]` per jdk8 Pattern"
  (i/union -lower -upper))

(def -digit
  "Known \"standardized\" digit range `[0-9]` per jdk8 Pattern"
  (i/->range \0 \9))

(def -word
  "Known \"standardized\" word range `[a-zA-Z_0-9]` per jdk8 Pattern"
  (i/union
   -lower
   -upper
   \_
   -digit))

(def -alnum
  "Alpha or numeral"
  (i/union -alpha -digit))

(def -punct
  "Punctiation One of !\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~"
  (apply i/union "!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~"))

(def -graph
  "Any visible (ASCII) character"
  (i/union -alnum -punct))

(def -print
  "Any printable character"
  (i/union -graph \u0020))

(def -blank
  "`[ \\t]`"
  (i/union
   \tab
   \space))

(def -cntrl
  "A control character"
  (i/union
   (i/->range \u0000 \u001f)
   \u007f))

(def -xdigit
  "Any hexadecimal digit `[0-9a-zA-Z]`"
  (i/union
   (i/->range \0 \9)
   (i/->range \a \f)
   (i/->range \A \F)))

(def -space
  "A whitespace character `[ \\t\\n\\x0B\\f\\r]`"
  (apply i/union "[ \t\n\u000B\f\r]"))

(def -named-character-classes
  "Known \"standardized\" character classes and their names."
  (bimap
   {"t" \tab
    "n" \newline
    "r" \return
    "f" \formfeed
    "a" \u0007
    "e" \u001b

    ;; ASCII/POSIX character ranges
    "s" -whitespace
    "S" (i/subtraction m/ANY-ASCII -whitespace)
    "w" -word
    "W" (i/subtraction m/ANY-ASCII -word)
    "d" -digit
    "D" (i/subtraction m/ANY-ASCII -digit)

    ;; ASCII/POSIX character classes
    "Lower"  -lower
    "Upper"  -upper
    "ASCII"  m/ANY-ASCII
    "Alpha"  -alpha
    "Digit"  -digit
    "Alnum"  -alnum
    "Punct"  -punct
    "Graph"  -graph
    "Print"  -print
    "Blank"  -blank
    "Cntrl"  -cntrl
    "XDigit" -xdigit
    "Space"  -space}))

(defn invert [& sets]
  (apply i/subtraction m/ANY-UTF8 sets))

(defn parse-named-character-class [name]
  (or (get -named-character-classes name)
      (i/->class name)))

(defn parse-character-class
  "Accepts a structure of the form

  [:character-class
   [(:positive-character-class | :negative-character-class) & clauses]
   [:character-class-arithmetic & clauses]]

  And computes either a valid \"simple\" character interval, or a
  \"complex\" interval dependent on JDK features."
  ([cls] cls)
  ([cls [_ & clauses]]
   (reduce (fn [cls1 [op & clss]]
             (let [xform (get {:character-class-intersection i/intersection
                               :character-class-difference   i/subtraction} op)]
               (apply xform cls1 clss)))
           cls clauses)))

(def -transformer
  "Map from node IDs to node transformer functions.

  Note that nodes cannot be deleted by returning nil when transforming them."
  (merge
   m/recursive-alt
   m/recursive-concat
   m/shorthand-repetition
   m/bounded-repetition
   m/unbounded-repetition
   {;; Wrappers, drop 'em
    :character             identity
    :codepoint             identity
    :repetition            identity
    :named-character-class identity
    :pattern               identity
    :escaped-character     identity
    :atom                  identity

    ;;----------------------------------------
    ;; Decode characters from their representations
    :hex-codepoint    (fn [s] (Long/parseLong s 16))
    :octal-codepoint  (fn [s] (Long/parseLong s 8))
    :simple-character first

    :positive-named-character-class parse-named-character-class
    :negative-named-character-class (fn [name]
                                      (invert (parse-named-character-class name)))

    :character-range          i/->range
    :negative-character-class invert
    :character-class          parse-character-class
    :positive-character-class i/union

    ;; Repetition
    ;;----------------------------------------
    :simple-repetition     identity
    :possessive-repetition c/possessive
    :relucant-repetition   c/reluctant
    }))

(defn parse
  "Consumes a resource, parsing it as a RFC5234 structured text, and generating an analyzed FSM"
  [text-or-resource]
  (->> (-parser text-or-resource)
       (transform -transformer)))

;; Emitting JDK regex patterns
(defmulti -render
  "Render a regexp tree to a legal Java regex string"
  #'tag-dx)

(defn emit [pattern]
  "Renders a regex AST to a JDK regex string."
  ;; FIXME: simplify first.
  (-render pattern))

(defn compile [pattern]
  "Emits a regex AST as a JDK Pattern."
  (re-pattern (emit pattern)))
