(ns me.arrdem.blather.rfc5234
  "A Blather dialect for ABNF as specified in RFC5234."
  (:require [instaparse.core :refer [parser transform]]
            [clojure.java.io :refer [resource]]))

(def -parser
  (parser (slurp (resource "rfc5234.insta"))
          :start :rulelist
          :auto-whitespace :standard))

(def -transformer
  {:CRLF (fn [& _] nil)})

(defn parse [text-or-resource]
  (->> (-parser text-or-resource)
       (transform -transformer)))
