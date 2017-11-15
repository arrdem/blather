(ns me.arrdem.blather.features
  "Features are the names of supported (A)BNF node types.

  A node/operator must be registered as a feature. Features allow us
  to talk meaningfully about the language or language subset which any
  given dialect represents."
  (:require [clojure.core :as c]))

(defmulti feature?
  "Predicate of a keyword.

  Returns `true` if and only if the keyword names a feature supported
  by some known (A)BNF grammar."
  {:added "0.1.0"}
  (fn [kw]
    (if (keyword? kw) kw ::default)))

(defmethod feature? ::default [_]
  false)

(defmacro deffeature
  [name docstring? attr-map]
  `(let [feature-name# ~(symbol (c/name (ns-name *ns*)) (c/name name))]
     (defmethod feature? feature-name# true)))
