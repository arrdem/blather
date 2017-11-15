(ns me.arrdem.blather.rfc5234
  "A Blather dialect for ABNF as specified in RFC5234."
  (:require [instaparse.core :refer [parser]]            
            [clojure.java.io :refer [resource]]))

(def -parser
  (parser (slurp (resource "abnf-rfc5234.insta"))
          :start :rulelist
          :auto-whitespace :standard))
