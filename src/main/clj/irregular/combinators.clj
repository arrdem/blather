(ns irregular.combinators
  "Character set (or more!) combiantors."
  (:refer-clojure :exclude [cat])
  (:require [clojure.string :refer [join]]
            [irregular.char :as char]))

;; Concatenation
;;----------------------------------------
(defn cat
  "(con)catenation of patterns.

  Returns a pattern which matches the left first, and then the right."
  [pattern1 pattern2]
  {:tag        ::cat
   :multi-byte (or (char/multibyte? pattern1)
                   (char/multibyte? pattern2))
   :pattern1   pattern1
   :pattern2   pattern2})

(defmethod char/multibyte? ::cat [{:keys [pattern1 pattern2]}]
  (or (char/multibyte? pattern1)
      (char/multibyte? pattern2)))

;; Alternation
;;----------------------------------------
(defn alt
  "Unordered alternation of patterns.

  Returns a pattern which matches either the left or the right pattern.

  May also be thought of as union between patterns."
  [pattern1 pattern2]
  {:tag        ::alt
   :multi-byte (or (char/multibyte? pattern1)
                   (char/multibyte? pattern2))
   :pattern1   pattern1
   :pattern2   pattern2})

(defmethod char/multibyte? ::alt [{:keys [pattern1 pattern2]}]
  (or (char/multibyte? pattern1)
      (char/multibyte? pattern2)))

(defn cut
  "Ordered alternation of patterns.

  Returns a pattern which first tries to match the left and then tries
  to match the right pattern.

  The name \"cut\" is used because like cut in logic programming it
  prevents backtracking & enforces an evaluation order."
  [pattern1 pattern2]
  {:tag        ::cut
   :multi-byte (or (char/multibyte? pattern1)
                   (char/multibyte? pattern2))
   :pattern1   pattern1
   :pattern2   pattern2})

(defmethod char/multibyte? ::cut [{:keys [pattern1 pattern2]}]
  (or (char/multibyte? pattern1)
      (char/multibyte? pattern2)))

;; Grouping
;;----------------------------------------
(defn group
  "A group wrapping another pattern.

  Used lexically for concatenations to be terminated."
  [pattern]
  {:tag        ::group
   :multi-byte (char/multibyte? pattern)
   :pattern    pattern})

(defmethod char/multibyte? ::group [{:keys [pattern]}]
  (char/multibyte? pattern))

(defn named-group
  "A regular expression which captures to a named \"register\"."
  [name pattern]
  {:tag        ::named-group
   :name       name
   :multi-byte (char/multibyte? pattern)
   :pattern    pattern})

(defmethod char/multibyte? ::named-group [{:keys [pattern]}]
  (char/multibyte? pattern))

;; Eagerness
;;----------------------------------------
(def behavior?
  #{::greedy ::reluctant ::possessive})

;; Repetition
;;----------------------------------------
(defn rep-n
  "A repetition of precisely N."
  ([pattern count]
   (rep-n ::greedy pattern count))
  ([behavior pattern count]
   {:pre [(behavior? behavior)]}
   {:tag        ::rep-n
    :behavior   behavior
    :multi-byte (char/multibyte? pattern)
    :pattern    pattern
    :count      count}))

(defn rep-n+
  "A repetition of N or more."
  ([pattern count]
   (rep-n+ ::greedy pattern count))
  ([behavior pattern count]
   {:pre [(behavior? behavior)]}
   {:tag        ::rep-n+
    :behavior   behavior
    :multi-byte (char/multibyte? pattern)
    :pattern    pattern
    :count      count}))

(defn rep-nm
  "A repetition of N to M occurrences"
  ([pattern min max]
   (rep-nm ::greedy pattern min max))
  ([behavior pattern min max]
   {:pre [(behavior? behavior)]}
   {:tag        ::rep-nm
    :behavior   behavior
    :multi-byte (char/multibyte? pattern)
    :pattern    pattern
    :min        min
    :max        max}))

(defn rep+
  "Helper, repetition of 1 or more."
  [pattern]
  (rep-n+ pattern 1))

(defn rep*
  "Helper, repetition of 0 or more."
  [pattern]
  (rep-n+ pattern 0))

(defn rep?
  "Helper, repetition of 0 or 1."
  [pattern]
  (rep-nm pattern 0 1))

(defn reluctant
  "Transforms the given combinator, returning a combinator which will
  match the same string with reluctant semantics."
  [combinator]
  (assoc combinator :behavior ::reluctant))

(defn possessive
  "Transforms the given combinator, returning a combinator which will
  match the same string with possessive semantics."
  [combinator]
  (assoc combinator :behavior ::possessive))