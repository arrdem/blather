(ns languages.lark
  (:require [clojure.java.io :refer [resource]]
            [irregular.combinators :as c]
            [languages.common :as m]
            [instaparse.core :refer [parser transform]]))

(def -parser
  "The Instaparse parser used to read Lark."
  (parser (slurp (resource "lark.insta"))
          :start :grammar
          :auto-whitespace :standard))

(def -transformer
  "Map from node IDs to node transformer functions.

  Note that nodes cannot be deleted by returning nil when transforming them."
  (merge
   m/recursive-alt
   m/recursive-concat
   {
    ;; Intermediary states with no value and no source representation
    :repeat   identity
    :terminal identity
    :element  identity

    ;; Rewrite repetition as a prefix operation
    :repetition (fn
                  ([subtree] subtree)
                  ([subtree quantifier] [quantifier subtree]))

    :one-or-more  (fn [] :one-or-more)
    :zero-or-more (fn [] :zero-or-more)
    :zero-or-one  (fn [] :zero-or-one)
    })
  )

(defn parse
  "Consumes a resource, parsing it as Lark structured text, and generating an analyzed FSM"
  [text-or-resource]
  (->> (-parser text-or-resource)
       (transform -transformer)))
