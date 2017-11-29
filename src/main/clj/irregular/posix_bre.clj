(ns irregular.posix-bre
  "POSIX Basic Regular Expressions (BRE) parsing & emitting."
  (:require [clojure.java.io :refer [resource]]
            [irregular.imp :as i :refer [tag-dx]]
            [irregular.combinators :as c]
            [irregular.char-sets :as s]
            [irregular.common :as m]
            [instaparse.core :refer [parser transform]]))

;; Parsing BRE patterns
(def -parser
  "The Instaparse parser used to read POSIX BREs."
  (parser (slurp (resource "posix-bre.insta"))
          :start :pattern))

(defn as-char-set [char-or-set]
  (if (instance? Character char-or-set)
    (s/char char-or-set)
    char-or-set))

(defn -parse-char-class [& components]
  (->> components
       (map as-char-set)
       (apply s/char-set-union)))

(def -transformer
  (merge
   m/shorthand-repetition
   m/recursive-alt
   m/recursive-concat
   m/bounded-repetition
   m/unbounded-repetition
   {;; Wrappers, drop 'em
    :repetition             identity
    :character-class        identity
    :pattern                identity
    :character              identity
    :escaped-character      identity
    :simple-character-class identity

    ;; The contract is that any Atom must become a char set - so ensure that chars (which don't
    ;; become sets for convenience elsewhere) get normalized to sets.
    :atom as-char-set

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
    :any-character (fn [& _] s/ANY-CHAR-RANGE)

    ;; Character classes are a little tricky because they have arithmetic & POSIX specific features.
    :character-range          s/char-range
    :negative-character-class #(s/char-set-difference s/ANY-CHAR-RANGE %)
    :positive-character-class -parse-char-class
    }))

(defn parse
  "Consumes a resource, parsing it as a POSIX BRE, and generating an analyzed FSM"
  [text-or-resource]
  (->> (-parser text-or-resource)
       (transform -transformer)))
