(ns me.arrdem.blather.lark
  (:require [instaparse.core :refer [parser transform]]
            [clojure.java.io :refer [resource]]))

(def -parser
  "The Instaparse parser used to read Lark."
  (parser (slurp (resource "lark.insta"))
          :start :grammar 
          :auto-whitespace :standard))

(defn just-subtree [subtree]
  subtree)

(def -transformer
  "Map from node IDs to node transformer functions.

  Note that nodes cannot be deleted by returning nil when transforming them."
  {
   ;; Intermediary states with no value and no source representation
   :repeat   just-subtree
   :terminal just-subtree
   :element  just-subtree

   ;; Simplify one-armed chains
   :alternation (fn
                  ([l] l)
                  ([l r] [:alternation l r]))

   :concatenation (fn
                    ([t] t)
                    ([t y & more] `[:concatenation ~t ~y ~@more]))

   ;; Rewrite repetition as a prefix operation
   :repetition (fn
                 ([subtree] subtree)
                 ([subtree quantifier] [quantifier subtree]))

   :one-or-more  (fn [] :one-or-more)
   :zero-or-more (fn [] :zero-or-more)
   :zero-or-one  (fn [] :zero-or-one)
   }
  )

(defn parse
  "Consumes a resource, parsing it as Lark structured text, and generating an analyzed FSM"
  [text-or-resource]
  (->> (-parser text-or-resource)
       (transform -transformer)))
