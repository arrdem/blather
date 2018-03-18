(ns languages.rfc5234
  "A Blather dialect for ABNF as specified in RFC5234.

  RFC7405 updates RFC5234 adding explicit support for case-sensitivity."
  (:refer-clojure :exclude [str])
  (:require [clojure.java.io :refer [resource]]
            [clojure.core.match :refer [match]]
            [blather.grammars :as g]
            [irregular.combinators :as c]
            [irregular.core :as i]
            [instaparse.core :refer [parser transform]]))

(def -parser
  "The Instaparse parser used to read RFC5234."
  (parser (slurp (resource "rfc5234.insta"))
          :start :rulelist
          :string-ci true
          :auto-whitespace :standard))

(defn -parse-with-base [base dx]
  (fn [e]
    (let [[tag a b] e
          f         #(char (Long/parseLong % base))]
      (case (count e)
        (2) ((dx tag) (f a))
        (3) ((dx tag) (f a) (f b))))))

(defn -case-insensitive-char-set-class [chr]
  (i/union
   (Character/toUpperCase chr)
   (Character/toLowerCase chr)))

(defn -parse-char [text]
  (reduce c/cat
          (-case-insensitive-char-set-class (last text))
          (map -case-insensitive-char-set-class (reverse (butlast text)))))

(def -num-transformers
  {:range i/->range :pair c/cat :val identity})

(def -transformer
  "Map from node IDs to node transformer functions.

  Note that nodes cannot be deleted by returning nil when transforming them."
  {;; Things that can be thrown out
   :elements identity
   :element  identity
   :DIGIT    identity

   ;; Parsing terminals
   :num-val identity
   :hex     (-parse-with-base 16 -num-transformers)
   :dec     (-parse-with-base 10 -num-transformers)
   :bin     (-parse-with-base 2  -num-transformers)

   :prose-val -parse-char
   :char-val  -parse-char

   :rulename g/rule

   ;; Parsing combinators
   :repeat          identity
   :repetition      (fn
                      ([rule] rule)
                      ([combinator rule]
                       (combinator rule)))
   :specific-repeat (fn [digit]
                      #(c/rep-n % (Long/parseLong 10 digit)))
   :range-repeat    (fn
                      ([star] c/rep*)
                      ([digit-or-star1 digit-or-star2]
                       (if (= digit-or-star1 "*")
                         #(c/rep-n+ % (Long/parseLong digit-or-star2 10))
                         (let [c (Long/parseLong digit-or-star1 10)]
                           #(c/rep-nm % 0 c))))
                      ([digit1 _star digit2]
                       #(c/rep-nm % (Long/parseLong 10 digit1) (Long/parseLong digit2 10))))
   :concatenation   (fn
                      ([x] x)
                      ([x y] (c/cat x y)))
   :alternation     (fn
                      ([x] x)
                      ([x y] (c/alt x y)))
   :option          c/rep?
   :group           c/group

   ;; Parse the rule & additions list into a grammar structure.
   :rulelist (fn [& rules-and-additions]
               {:type ::grammar
                :rules (reduce (fn [grammar [tag {:keys [name]} production]]
                                 (case tag
                                   (:rule)     (assoc grammar name production)
                                   (:addition) (update grammar name c/alt production)))
                               {} rules-and-additions)})})

(defn parse
  "Consumes a resource, parsing it as a RFC5234 structured text, and generating an analyzed FSM"
  [text-or-resource]
  (->> (-parser text-or-resource)
       (transform -transformer)))
