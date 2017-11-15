(ns me.arrdem.blather.productions
  "Grammars consist of productions, being variants tagged with features.

  "
  (:require [me.arrdem.blather.features :as features]
            [guten-tag.core :refer [tag]]))

(defmulti children
  "Function of a production to the (possibly empty) sequence of its child productions."
  {:arglists '([production])}
  tag)

(defmacro defproduction
  "Macro. Defines a new production type."
  {:arglists '([name docstring? attr-map? members child-fn])}
  [& arglist]
  `(do (deftag ~@(butlast arglist))
       ))
