(ns languages.posix-bre
  "POSIX Basic Regular Expressions (BRE) parsing & emitting."
  (:require [clojure.java.io :refer [resource]]
            [clojure.core.match :refer [match]]
            [irregular.core :as i]
            [clojure.zip :as z]
            [irregular.combinators :as c]
            [languages.ascii :as ascii]
            [languages.common :as m :refer [ANY-ASCII]]
            [instaparse.core :refer [parser transform]]))

;; Parsing BRE patterns
(def -parser
  "The Instaparse parser used to read POSIX BREs."
  (parser (slurp (resource "posix-bre.insta"))
          :start :pattern))

(def -transformer
  (merge
   m/shorthand-repetition
   m/recursive-alt
   m/recursive-concat
   m/bounded-repetition
   m/unbounded-repetition
   {;; Wrappers, drop 'em
    :atom                   identity
    :repetition             identity
    :character-class        identity
    :pattern                identity
    :character              identity
    :escaped-character      identity
    :simple-character-class identity

    ;; Parentheticals become groups
    :parenthetical c/group

    ;; trailing * becomes {0,}
    :star-repetition c/rep*

    ;; In general we just convert single characters to singleton ranges
    :simple-character first

    ;; The special character terminal is used to relieve characters of their semantics, but the
    ;; standard doesn't specify any escape aliasing. So just pop the wrapper off.
    :special-character first

    ;; . has special semantics and means literally any character
    :any-character (fn [& _] ANY-ASCII)

    ;; Character classes are a little tricky because they have arithmetic & POSIX specific features.
    :character-range             i/->range
    :negative-character-class    (partial i/subtraction ANY-ASCII)
    :positive-character-class    i/union
    :named-character-class       i/->class
    :collation-class             i/->collation
    :equivalence-character-class i/->equiv
    }))

(defn parse
  "Consumes a resource, parsing it as a POSIX BRE, and generating an analyzed FSM"
  [text-or-resource]
  (->> (-parser text-or-resource)
       (transform -transformer)))

(defmethod i/multibyte? ::i/string [s]
  (some i/multibyte? s))

(defn simplifier [tree]
  (match tree
    {:tag      ::c/cat
     :pattern1 (:or (a :guard char?)
                    (a :guard string?))
     :pattern2 (:or (b :guard char?)
                    (b :guard string?))}
    ,,(str a b)

    {:tag      ::c/cat
     :pattern1 {:tag      ::c/cat
                :pattern1 a
                :pattern2 b}
     :pattern2 c}
    ,,(c/cat a (c/cat b c))

    ;; Identity by default
    :else
    ,,tree))

(defmulti -render
  "Render a regexp tree to a legal Java regex string"
  #'i/tag-dx)

(defmethod -render ::i/character [pattern]
  ;; FIXME (arrdem 2017-12-28):
  ;;   does this need to be more involved? I don't think so....
  (m/pr-ch pattern))

(defmethod -render ::i/empty [pattern]
  ;; FIXME (arrdem 2017-12-28):
  ;;   does this need to be more involved? I don't think so....
  "[]")

(defmethod -render ::i/range [{:keys [upper lower]}]
  (m/charset (format "%s-%s" (-render lower) (-render upper))))

(defmethod -render ::c/cat [{:keys [pattern1 pattern2]}]
  (str (-render pattern1) (-render pattern2)))

(defmethod -render ::c/alt [{:keys [pattern1 pattern2]}]
  (str (-render pattern1) "|" (-render pattern2)))

(defmethod -render ::c/group [{:keys [pattern]}]
  (format "(%s)" (-render pattern)))

(defmethod -render ::i/union [{:keys [terms]}]
  (m/charset (apply str (map -render terms))))

(defmethod -render ::i/subtraction [{:keys [minuend subtrahends]}]
  (if (= minuend ANY-ASCII)
    (m/charset (str "[^" (apply str (map -render subtrahends)) "]"))))

(defmethod -render ::c/rep-n [{:keys [pattern count behavior]}]
  {:pre [(= behavior ::c/greedy)]}
  (format "%s\\{%s\\}" (-render pattern) count))

(defmethod -render ::c/rep-n+ [{:keys [pattern count behavior]}]
  {:pre [(= behavior ::c/greedy)]}
  (if (= count 0)
    (format "%s*" (-render pattern))
    (format "%s\\{%s,\\}" (-render pattern) count)))

(defmethod -render ::c/rep-nm [{:keys [pattern min max behavior]}]
  {:pre [(= behavior ::c/greedy)]}
  (format "%s\\{%s,%s\\}" (-render pattern) min max))

(defmethod -render ::i/sof [_] "^")

(defmethod -render ::i/eof [_] "$")

(defmethod -render ::i/equivalence [{:keys [name]}]
  (format "[=%s=]" name))

(defmethod -render ::i/collation [{:keys [name]}]
  (format "[.%s.]" name))

(defmethod -render ::c/group [{:keys [pattern]}]
  (format "\\(%s\\)" (-render pattern)))

(defn emit [pattern]
  "Renders a regex AST to a JDK regex string."
  ;; FIXME: simplify first.
  (-render pattern))
