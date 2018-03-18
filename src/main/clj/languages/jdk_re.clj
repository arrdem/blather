(ns languages.jdk-re
  "Compiles an irregular regex IR down to a JDK legal regex string."
  (:refer-clojure :exclude [compile])
  (:require [clojure.java.io :refer [resource]]
            [clojure.string :as string]
            [irregular.core :as i :refer [tag-dx]]
            [irregular.combinators :as c]
            [languages.common :as m :refer [*emitting-set* maybe-set charset]]
            [languages.ascii :as ascii]
            [detritus.bimap :refer [bimap]]
            [instaparse.core :refer [parser transform]]))

;; Parsing JDK regex patterns
(def -parser
  "The Instaparse parser used to read RFC5234."
  (parser (slurp (resource "jdk-regex.insta"))
          :start :pattern))

(def -word
  "Known \"standardized\" word range `[a-zA-Z_0-9]` per jdk8 Pattern"
  (i/union ascii/lower ascii/upper \_ ascii/digit))

(def -alnum
  "Alpha or numeral"
  (i/union ascii/alpha ascii/digit))

(def -xdigit
  "Any hexadecimal digit `[0-9a-zA-Z]`"
  (i/union
   (i/->range \0 \9)
   (i/->range \a \f)
   (i/->range \A \F)))

(def -named-character-classes
  "Known \"standardized\" character classes and their names."
  {"t" \tab
   "n" \newline
   "r" \return
   "f" \formfeed
   "a" \u0007
   "e" \u001b

   ;; ASCII/POSIX character ranges
   "s" ascii/whitespace
   "S" (i/subtraction m/ANY-ASCII ascii/whitespace)
   "w" -word
   "W" (i/subtraction m/ANY-ASCII -word)
   "d" ascii/digit
   "D" (i/subtraction m/ANY-ASCII ascii/digit)

   ;; ASCII/POSIX character classes
   "Lower"  ascii/lower
   "Upper"  ascii/upper
   "ASCII"  ascii/any
   "Alpha"  ascii/alpha
   "Digit"  ascii/digit
   "Alnum"  -alnum
   "Punct"  ascii/punct
   "Graph"  ascii/graph
   "Print"  ascii/print
   "Blank"  ascii/blank
   "Cntrl"  ascii/control
   "XDigit" -xdigit
   "Space"  ascii/space
   "$"      ::i/eof
   "^"      ::i/sof})

(defn invert [& sets]
  (apply i/subtraction m/ANY-UTF8 sets))

(defn parse-named-character-class [name]
  (or (get -named-character-classes name)
      (i/->class name)))

(defn parse-character-class
  "Accepts a structure of the form
  o
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
    :hex-codepoint     (fn [s] (Long/parseLong s 16))
    :octal-codepoint   (fn [s] (Long/parseLong s 8))
    :simple-character  first
    :special-character -named-character-classes

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

    :parenthetical (fn paren
                     ([e] (c/group e))
                     ([name e]
                      (c/named-group name e)))
    }))

(defn parse
  "Consumes a resource, parsing it as JDK pattern and generating an analyzed FSM"
  [text-or-resource]
  (->> (-parser text-or-resource)
       (transform -transformer)))

;; (ns-unmap *ns* '-render)

;; Emitting JDK regex patterns
(defmulti -render
  "Render a regexp tree to a legal Java regex string"
  (juxt #'tag-dx #(get % :behavior :default)))

(defmethod -render [::i/character :default] [pattern]
  ;; FIXME (arrdem 2017-12-28):
  ;;   does this need to be more involved? I don't think so....
  (m/pr-ch pattern))

(defmethod -render [::i/empty :default] [pattern]
  ;; FIXME (arrdem 2017-12-28):
  ;;   does this need to be more involved? I don't think so....
  "[]")

(defmethod -render [::i/class :default] [{:keys [name]}]
  (format "\\p{%s}" name))

;; Repetition

(defmethod -render [::c/rep-n+ ::c/greedy] [{:keys [pattern count]}]
  (let [suffix ({0 "*" 1 "+"} count (format "{%d,}" count))]
    (str (-render pattern) suffix)))

(defmethod -render [:default ::c/possessive] [r]
  (str (-render (assoc r :behavior ::c/greedy)) "+"))

(defmethod -render [:default ::c/reluctant] [r]
  (str (-render (assoc r :behavior ::c/greedy)) "?"))

(defmethod -render [::i/range :default] [{:keys [upper lower]}]
  (charset (format "%s-%s" (-render lower) (-render upper))))

(defmethod -render [::i/union :default] [{:keys [terms]}]
  (charset (apply str (mapv -render terms))))

(defmethod -render [::i/intersection :default] [{:keys [terms]}]
  (charset (string/join "&&" (binding [*emitting-set* false]
                               (mapv (comp maybe-set -render) terms)))))

(defmethod -render [::i/subtraction :default] [{:keys [minuend subtrahends]}]
  (if-not *emitting-set*
    (binding [*emitting-set* true]
      (format "[%s%s]"
              (-render minuend)
              (->> subtrahends
                   (map #(binding [*emitting-set* false]
                           (-render %)))(string/join "&&" ))))))

;; Grouping

(defmethod -render [::c/group :default] [{:keys [pattern]}]
  (format "(%s)" (-render pattern)))

(defmethod -render [::c/named-group :default] [{:keys [name pattern]}]
  (format "(P<%s>%s)" name (-render pattern)))

;; Emitting combinators

(defmethod -render [::c/cat :default] [{:keys [pattern1 pattern2]}]
  (str (-render pattern1) (-render pattern2)))

(defmethod -render [::c/alt :default] [{:keys [pattern1 pattern2]}]
  (str (-render pattern1) "|" (-render pattern2)))

(defn emit [pattern]
  "Renders a regex AST to a JDK regex string."
  ;; FIXME: simplify first.
  (-render pattern))

(defn compile [pattern]
  "Emits a regex AST as a JDK Pattern."
  (re-pattern (emit pattern)))
