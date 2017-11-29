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



(def -transformer
  (merge
   m/shorthand-repetition
   m/recursive-alt
   m/recursive-concat
   m/bounded-repetition
   m/unbounded-repetition
   {;; Wrappers, drop 'em
    :character       identity
    :atom            identity
    :repetition      identity
    :character-class identity
    :pattern         identity

    ;; Parentheticals become groups
    :parenthetical c/group

    ;; trailing * becomes {0,}
    :star-repetition c/rep*

    ;; In general we just convert single characters to singleton ranges
    :simple-character (comp s/char first)

    ;; . has special semantics and means literally any character
    :any-character (fn [& _] s/ANY-CHAR-RANGE)

    ;; Character classes are a little tricky because they have arithmetic & POSIX specific features.
    :simple-character-class }))

(defn parse
  "Consumes a resource, parsing it as a POSIX BRE, and generating an analyzed FSM"
  [text-or-resource]
  (->> (-parser text-or-resource)
       (transform -transformer)))
