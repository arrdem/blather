(ns languages.posix-bre
  "POSIX Basic Regular Expressions (BRE) parsing & emitting."
  (:require [clojure.java.io :refer [resource]]
            [clojure.core.match :refer [match]]
            [irregular.core :as i]
            [clojure.zip :as z]
            [irregular.combinators :as c]
            [languages.common :as m :refer [ANY-UTF8]]
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
    :any-character (fn [& _] ANY-UTF8)

    ;; Character classes are a little tricky because they have arithmetic & POSIX specific features.
    :character-range             i/->range
    :negative-character-class    (partial i/subtraction ANY-UTF8)
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
