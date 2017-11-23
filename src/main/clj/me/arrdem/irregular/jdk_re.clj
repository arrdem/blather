(ns me.arrdem.irregular.jdk-re
  "Compiles an irregular regex IR down to a JDK legal regex string."
  (:require [clojure.java.io :refer [resource]]
            [me.arrdem.irregular.imp :as i :refer [tag-dx]]
            [me.arrdem.irregular.combinators :as c]
            [me.arrdem.irregular.char-sets :as s :refer [char-set*]]
            [instaparse.core :refer [parser transform]]))

;; Parsing JDK regex patterns
(def -parser
  "The Instaparse parser used to read RFC5234."
  (parser (slurp (resource "jdk-regex.insta"))
          :start :pattern))

(def -transformer
  "Map from node IDs to node transformer functions.

  Note that nodes cannot be deleted by returning nil when transforming them."
  {:concatenation (fn
                    ([x] x)
                    ([x y & more] `[:concatenation ~x ~y ~@more]))
   :alternation   (fn
                    ([x] x)
                    ([x y & more] `[:alternation ~x ~y ~@more]))
   :character     identity
   :atom          identity})

(defn parse
  "Consumes a resource, parsing it as a RFC5234 structured text, and generating an analyzed FSM"
  [text-or-resource]
  (->> (-parser text-or-resource)
       (transform -transformer)))

;; Emitting JDK regex patterns
(defmulti -render
  "Render a regexp tree to a legal Java regex string"
  #'tag-dx)

(defmethod -render ::c/alt [{:keys [pattern1 pattern2]}]
  (str (-render pattern1) "|" (-render pattern2)))

(defmethod -render ::i/integer [i]
  (if (or (Character/isISOControl ^int (int i))
          (> i 127))
    (format "\\u%04X" (int i))
    (str (clojure.core/char (int i)))))

(defmethod -render ::s/char-range [{:keys [upper lower]}]
  (if (= upper lower)
    (-render upper)
    (format "[%s-%s]" (-render lower) (-render upper))))

(defmethod -render ::s/char-set [{:keys [ranges]}]
  (let [ranges (mapcat char-set* ranges)]
    (->> ranges
         (map -render)
         (apply str)
         (format "[%s]"))))

(defmethod -render ::c/cat [{:keys [pattern1 pattern2]}]
  (str (-render pattern1) (-render pattern2)))

(defmethod -render ::c/group [{:keys [pattern]}]
  (format "(%s)" (-render pattern)))
