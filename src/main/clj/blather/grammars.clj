(ns blather.grammars
  "The internal representation of a Blather grammar."
  (:refer-clojure :exclude [str])
  (:require [irregular.core :refer [multibyte?]]))

;; A grammar AST node for rule names
;;--------------------------------------------------------------------------------
(defn rule
  "Constructor. Returns a rule node referencing the named rule."
  [rule-name]
  {:tag  ::rule
   :name rule-name})

(defn rule?
  "Predicate. True if given a valid rule node."
  [{:keys [tag]}]
  (= tag ::rule))

;; We can't statically determine this, so assume the negative.
(defmethod multibyte? ::rule [a]
  false)

;; A grammar AST node for literal strings
;;--------------------------------------------------------------------------------
(declare str?)

(defn str
  "Intermediate node representing a character sequence.

  Produced when simplifying grammars. Frontends should prefer to
  produce character sets and concatenations / alternations, which can
  be simplified back to string literals or compiled to regular
  expressions as may be appropriate for a given target."
  [& strs-and-chars]
  {:tag     ::str
   :pattern (->> strs-and-chars
                 (mapcat (fn [str-or-char]
                           (cond (str? str-or-char)
                                 (recur (:pattern str-or-char))

                                 (instance? Character str-or-char)
                                 [str-or-char]

                                 (instance? String str-or-char)
                                 (seq str-or-char))))
                 (apply clojure.core/str))})

(defn str?
  "Predicate. True if given a valid str node."
  [{:keys [tag]}]
  (= tag ::str))
