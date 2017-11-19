(ns me.arrdem.blather.features
  "Features are the names of supported (A)BNF node types.

  A node/operator must be registered as a feature. Features allow us
  to talk meaningfully about the language or language subset which any
  given dialect represents."
  (:require [guten-tag.core :refer [tag tagged?]]))

(defmulti feature?
  "Predicate of a keyword.

  Returns `true` if and only if the keyword names a feature supported
  by some known (A)BNF grammar."
  {:added "0.1.0"}
  (fn [v]
    (cond (keyword? v) v
          (tagged? v)  (tag v)
          :else        :default)))

(defmethod feature? :default [_]
  false)
