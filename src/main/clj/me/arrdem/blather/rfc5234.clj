(ns me.arrdem.blather.rfc5234
  "A Blather dialect for ABNF as specified in RFC5234."
  (:require [instaparse.core :refer [parser transform]]
            [clojure.java.io :refer [resource]]))

(def -parser
  "The Instaparse parser used to read RFC5234."
  (parser (slurp (resource "rfc5234.insta"))
          :start :rulelist
          :string-ci true
          :auto-whitespace :standard))

(def -transformer
  "Map from node IDs to node transformer functions.

  Note that nodes cannot be deleted by returning nil when transforming them."
  {})

(defn parse
  "Consumes a resource, parsing it as a RFC5234 structured text, and generating an analyzed FSM"
  [text-or-resource]
  (->> (-parser text-or-resource)
       (transform -transformer)))
