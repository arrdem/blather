(ns me.arrdem.irregular.combinators
  "Character set combiantors."
  (:refer-clojure :exclude [cat])
  (:require [clojure.string :refer [join]]
            [me.arrdem.irregular.char :as char]))

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
  "Alternation of patterns.

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

;; Repetition
;;----------------------------------------

