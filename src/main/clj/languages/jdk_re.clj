(ns languages.jdk-re
  "Compiles an irregular regex IR down to a JDK legal regex string."
  (:refer-clojure :exclude [compile])
  (:require [clojure.java.io :refer [resource]]
            [irregular :as i :refer [tag-dx]]
            [irregular.combinators :as c]
            [irregular.char-sets :as s]
            [languages.common :as m]
            [instaparse.core :refer [parser transform]]))

;; Parsing JDK regex patterns
(def -parser
  "The Instaparse parser used to read RFC5234."
  (parser (slurp (resource "jdk-regex.insta"))
          :start :pattern))

(def -whitespace
  "Known \"standardized\" whitespace range `[ \\t\\n\\x0B\\f\\r]` per jdk8 Pattern"
  (s/char-set-union
   (s/char \space)
   (s/char \tab)
   (s/char \newline)
   (s/char \u000B) ;; Vertical tab
   (s/char \formfeed)
   (s/char \return)))

(def -lower
  "Known \"standardized\" lower case range `[a-z]` per jdk8 Pattern"
  (s/char-range \a \z))

(def -upper
  "Known \"standardized\" upper case range `[A-Z]` per jdk8 Pattern"
  (s/char-range \A \Z))

(def -alpha
  "An alphabetic character `[\\p{Lower}\\p{Upper}]` per jdk8 Pattern"
  (s/char-set-union -lower -upper))

(def -digit
  "Known \"standardized\" digit range `[0-9]` per jdk8 Pattern"
  (s/char-range \0 \9))

(def -word
  "Known \"standardized\" word range `[a-zA-Z_0-9]` per jdk8 Pattern"
  (s/char-set-union
   -lower
   -upper
   (s/char \_)
   -digit))

(def -ascii
  "ASCII character range."
  (s/char-range \u0000 \u007f))

(def -alnum
  "Alpha or numeral"
  (s/char-set-union -alpha -digit))

(def -punct
  "Punctiation One of !\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~"
  (->> (map s/char "!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~")
       (apply s/char-set-union)))

(def -graph
  "Any visible (ASCII) character"
  (s/char-set-union -alnum -punct))

(def -print
  "Any printable character"
  (s/char-set-union -graph (s/char \u0020)))

(def -blank
  "`[ \\t]`"
  (s/char-set-union
   (s/char \tab)
   (s/char \space)))

(def -cntrl
  "A control character"
  (s/char-set-union
   (s/char-range \u0000 \u001f)
   (s/char \u007f)))

(def -xdigit
  "Any hexadecimal digit `[0-9a-zA-Z]`"
  (s/char-set-union
   (s/char-range \0 \9)
   (s/char-range \a \f)
   (s/char-range \A \F)))

(def -space
  "A whitespace character `[ \\t\\n\\x0B\\f\\r]`"
  (->> (map s/char "[ \t\n\u000B\f\r]")
       (apply s/char-set-union)))

(def -named-character-classes
  "Known \"standardized\" character classes and their names."
  {"t" (s/char \tab)
   "n" (s/char \newline)
   "r" (s/char \return)
   "f" (s/char \formfeed)
   "a" (s/char \u0007)
   "e" (s/char \u001b)

   ;; ASCII/POSIX character ranges
   "s" -whitespace
   "S" (s/char-set-difference -ascii -whitespace)
   "w" -word
   "W" (s/char-set-difference -ascii -word)
   "d" -digit
   "D" (s/char-set-difference -ascii -digit)

   ;; ASCII/POSIX character classes
   "Lower"  -lower
   "Upper"  -upper
   "ASCII"  -ascii
   "Alpha"  -alpha
   "Digit"  -digit
   "Alnum"  -alnum
   "Punct"  -punct
   "Graph"  -graph
   "Print"  -print
   "Blank"  -blank
   "Cntrl"  -cntrl
   "XDigit" -xdigit
   "Space"  -space})

(defn parse-named-character-class [name]
  (or (get -named-character-classes name)
      (throw (IllegalArgumentException. (format "No known character class '%s'" name)))))

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
             (let [xform (get {:character-class-intersection s/char-set-intersection
                               :character-class-difference   s/char-set-difference} op)]
               (apply xform cls1 (map s/as-character-class clss))))
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

    ;; An atom is either already a character class, or it is a raw char. Normalize.
    :atom s/as-character-class

    ;;----------------------------------------
    ;; Decode characters from their representations
    :hex-codepoint    (fn [s] (Long/parseLong s 16))
    :octal-codepoint  (fn [s] (Long/parseLong s 8))
    :simple-character first

    :positive-named-character-class parse-named-character-class
    :negative-named-character-class (fn [name]
                                      (s/char-set-difference s/ANY-CHAR-RANGE
                                                             (parse-named-character-class name)))

    :character-range          s/char-range
    :negative-character-class (fn [& clss]
                                (->> (map s/as-character-class clss)
                                     (apply s/char-set-difference s/ANY-CHAR-RANGE)))
    :character-class          parse-character-class
    :positive-character-class (fn [& chars-or-ranges]
                                (->> chars-or-ranges
                                     (map s/as-character-class)
                                     (apply s/char-set-union)))

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

(defmethod -render ::c/alt [{:keys [pattern1 pattern2]}]
  (str (-render pattern1) "|" (-render pattern2)))

(defmethod -render :integer [i]
  (if (or (Character/isISOControl ^int (int i))
          (> i 127))
    (format "\\u%04X" (int i))
    (str (clojure.core/char (int i)))))

(defmethod -render ::s/char-range [{:keys [upper lower]}]
  (if (= upper lower)
    (-render upper)
    (format "[%s-%s]" (-render lower) (-render upper))))

(defmethod -render ::s/char-set [{:keys [ranges]}]
  (let [ranges (mapcat s/as-ranges ranges)]
    (->> ranges
         (map -render)
         (apply str)
         (format "[%s]"))))

(defmethod -render ::c/cat [{:keys [pattern1 pattern2]}]
  (str (-render pattern1) (-render pattern2)))

(defmethod -render ::c/group [{:keys [pattern]}]
  (format "(%s)" (-render pattern)))

(defn -append-behavior [{:keys [behavior] :as node} text]
  (case behavior
    (::c/greedy)     text
    (::c/reluctant)  (str text "?")
    (::c/possessive) (str text "+")))

(defmethod -render ::c/rep-n+ [{:keys [pattern count] :as node}]
  (as-> (-render pattern) s
    (if (= count 1) (format "%s+" s) (format "%s{%s,}" s count))
    (-append-behavior node s)))

(defmethod -render ::c/rep-nm [{:keys [pattern min max] :as node}]
  (as-> (-render pattern) s
    (if (and (= min 0) (= max 1))
      (format "%s?" s)
      (format "%s{%s,%s}" min max))
    (-append-behavior node s)))

(defn emit [pattern]
  "Renders a regex AST to a JDK regex string."
  ;; FIXME: simplify first.
  (-render pattern))

(defn compile [pattern]
  "Emits a regex AST as a JDK Pattern."
  (re-pattern (emit pattern)))
