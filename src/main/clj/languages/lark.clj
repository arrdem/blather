(ns languages.lark
  (:require [clojure.java.io :refer [resource]]
            [irregular.combinators :as c]
            [languages.common :as m]
            [blather.grammars :as g]
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
   {:terminal identity
    :char-val first
    :element identity
    :alias nil

    :repetition (fn
                  ([x] x)
                  ([x repeat]
                   :FIXME))

    :symbol g/rule
    :rulename (fn ([symbol] symbol)
                  ([tag symbol] symbol))

    :rule (fn [name elements]
            [name elements])

    :grammar (fn [& rules-pragmas]
               (let [{:keys [rule pragma]}
                     (group-by (fn [e]
                                 (or (when (and (vector? e)
                                                (keyword? (first e)))
                                       (first e))
                                     :rule))
                               rules-pragmas)]
                 {:type ::grammar
                  :pragmas pragma
                  :rules (into {} (map (fn [[rule-name rule]] [(:name rule-name) rule])
                                       rule))}))}))

(defn parse
  "Consumes a resource, parsing it as Lark structured text, and generating an analyzed FSM"
  [text-or-resource]
  (->> (-parser text-or-resource)
       (transform -transformer)))
