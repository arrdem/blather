(ns me.arrdem.blather.lark
  (:require [instaparse.core :refer [parser transform]]
            [clojure.java.io :refer [resource]]))

(def -parser
  "The Instaparse parser used to read Lark."
  (parser (slurp (resource "lark.insta"))
          :start :grammar 
          :auto-whitespace :standard))

(def -transformer
  "Map from node IDs to node transformer functions.

  Note that nodes cannot be deleted by returning nil when transforming them."
  {})

(defn parse
  "Consumes a resource, parsing it as Lark structured text, and generating an analyzed FSM"
  [text-or-resource]
  (->> (-parser text-or-resource)
       (transform -transformer)))
