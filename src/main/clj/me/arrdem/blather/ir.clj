(ns me.arrdem.blather.ir
  "This namespace exists to specify an internal representation of BNF
  grammars which can be transformed and rendered into other BNF
  dialects."
  (:require [clojure.core :as c]
            [clojure.set :as s]
            [me.arrdem.blather.features :as features]
            [guten-tag.core :refer [deftag]]))

;; Helper function for writing macros.
(defn- take-when
  [f empty col]
  {:pre [(fn? f)]}
  (let [[x & rest] col]
    (if (f x) [x rest] [empty col])))

;; Macro for defining a given kind of "production", being a guten-tag "type"
(defmacro defproduction
  {:arglists '([name docstring? attr-map? slots pre-post?])}
  [& args] 
  (let [[name args]       (take-when symbol? nil args)
        [docstring? args] (take-when string? nil args)
        [attr-map? args]  (take-when map? {} args)
        [slots args]      (take-when vector? nil args)
        [pre-post? args]  (take-when map? {} args)]
    (assert name "Requires a symbol to define!")
    (assert (or (string? docstring?)
                (nil? docstring?))
            "Docstring may be a string!")
    (assert (or (map? attr-map?)
                (nil? attr-map?))
            "Attr-map may be an arbitrary attributes mapping!")
    (assert (vector? slots) "Slots must be a vector!")
    (assert (every? symbol? slots) "Slots must be a vector of symbols naming slots!") 
    `(let [feature-name# ~(keyword (c/name (ns-name *ns*)) (c/name name))]
       (deftag ~name ~docstring? ~attr-map? ~slots ~pre-post?)
       (defmethod features/feature? feature-name# [_#] true))))

;; Define a bunch of features
;;
;; There's a lot of structural similarity between the operational semantics of various BNF
;; dialects. The major differences are in the supported notation for terminals and the supported
;; notation for various sorts of repetitions.
;;
;; The goal of this internal representation is to 
(defproduction rep-n
  "Repetition of N."
  [parser n])

(defproduction rep-n+
  "Repetition of N or more with no upper bound."
  [parser n])

(defproduction rep-nm
  "Repetition of at least N and at most M."
  [parser n m])

(defproduction cat
  "Concatenation of two parsers."
  [parser1 parser2])

(defproduction alt-ord
  "Ordered alternation of two parsers."
  {:added "0.0.0"}
  [parser1 parser2])

(defproduction alt-any
  "Unordered alternation of two parsers."
  {:added "0.0.0"}
  [parser1 parser2])

(defproduction nonterminal
  "The name of a production in a grammar, referred to by name."
  {:added "0.0.0"}
  [name])

(defproduction literal
  "A string literal.

  WARNING: STRING LITERALS MAY CONTAIN ANY VALID UTF-8 SEQUENCE. FRONTENDS MUST FULLY DECODE ESCAPED
  CODE SEQUENCES OR BYTE NUMBERS INTO CHARACTERS. BACKENDS MUST PROVIDE ANY APPROPRIATE DECODING OR 
  ESCAPING DOWN TO A SUPPORTED CODESET."
  {:added "0.0.0"}
  [string])

(defproduction regexp
  "A regex literal.

  WARNING: REGEX LITERALS ARE IN THE POSIX EXTENDED REGULAR EXPRESSIONS (ERE) SYNTAX."
  [regexp])

(defn- check-features [features productions])

(defn- production-features [production])

;; Grammar instances
;;
;; A Grammar consists of a set of features used in the grammar, and a map of names to productions,
;; where the productions may only make use of the features listed in the feature set.
(deftag grammar
  "A type representing an (A)BNF grammar.

  A grammar consists of a set of features, "
  [features productions]
  {:pre [(set? features)
         (every? keyword? features)
         (every? features/feature? features)
         (check-features features productions)]})

(defn add-production
  "Function from a Grammar to a Grammar, which adds a production definition.

  Maintains the grammar invariants."
  [grammar production-name production]
  {:post [(grammar? %)]}
  (->grammar (s/union (:features grammar) (production-features production))
             (assoc (:productions grammar)
                    production-name production)))
