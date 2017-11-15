(ns me.arrdem.blather.ir
  "This namespace exists to specify an internal representation of BNF
  grammars which can be transformed and rendered into other BNF
  dialects."
  (:require [guten-tag.core :refer [deftag]]
            [clojure.set :as s]
            [me.arrdem.blather.features :as features]))

(defn -check-features [features productions])

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
         (every? feature? features)
         (-check-features features productions)]})

(defn defproduction
  "Function from a Grammar to a Grammar, which adds a production definition.

  Maintains the grammar invariants."
  [grammar production-name production]
  {:post [(grammar? %)]}
  (->grammar (s/union (:features grammar) (production-features production))
             (assoc (:productions grammar)
                    production-name production)))
