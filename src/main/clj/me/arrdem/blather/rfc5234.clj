(ns me.arrdem.blather.rfc5234
  "A Blather dialect for ABNF as specified in RFC5234.

  RFC7405 updates RFC5234 adding explicit support for case-sensitivity."
  (:require [clojure.java.io :refer [resource]]
            [me.arrdem.irregular.combinators :as c]
            [me.arrdem.irregular.char-sets :as cs]
            [instaparse.core :refer [parser transform]]))

(def -parser
  "The Instaparse parser used to read RFC5234."
  (parser (slurp (resource "rfc5234.insta"))
          :start :rulelist
          :string-ci true
          :auto-whitespace :standard))

(defn -parse-with-base [base dx]
  (fn [e]
    (let [[tag a b] e]
      (case (count e)
        (2) ((dx tag)
             (Long/parseLong a base))
        (3) ((dx tag)
             (Long/parseLong a base)
             (Long/parseLong b base))))))

(defn -char-cat [x y]
  (c/cat (cs/as-character-class x)
         (cs/as-character-class y)))

(defn -case-insensitive-char-set-class [chr]
  (cs/char-set-union
   (cs/as-character-class (Character/toUpperCase chr))
   (cs/as-character-class (Character/toLowerCase chr))))

(defn -parse-char [text]
  (reduce c/cat
          (-case-insensitive-char-set-class (last text))
          (map -case-insensitive-char-set-class (reverse (butlast text)))))

(def -num-transformers
  {:range cs/char-range :pair -char-cat :val cs/as-character-class})

(def -transformer
  "Map from node IDs to node transformer functions.

  Note that nodes cannot be deleted by returning nil when transforming them."
  {;; Things that can be thrown out
   :elements identity
   :element  identity
   
   ;; Parsing terminals
   :num-val identity
   :hex     (-parse-with-base 16 -num-transformers)
   :dec     (-parse-with-base 10 -num-transformers)
   :bin     (-parse-with-base 2  -num-transformers)

   :prose-val -parse-char
   :char-val  -parse-char
   
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
                         #(c/rep-n+ % (Long/parseLong 10 digit-or-star2))
                         (let [c (Long/parseLong 10 digit-or-star1)]
                           #(c/rep-nm % 0 c))))
                      ([digit1 _star digit2]
                       #(c/rep-nm % (Long/parseLong 10 digit1) (Long/parseLong 10 digit2))))
   :concatenation   (fn
                      ([x] x)
                      ([x y] (c/cat x y)))
   :alternation     (fn
                      ([x] x)
                      ([x y] (c/alt x y)))
   :rulelist        (fn [& rules-and-additions]
                      (reduce (fn [grammar [tag [_ name] production]]
                                (case tag
                                  (:rule)     (assoc grammar name production)
                                  (:addition) (update grammar name c/alt production)))
                              {} rules-and-additions))
   })

(defn parse
  "Consumes a resource, parsing it as a RFC5234 structured text, and generating an analyzed FSM"
  [text-or-resource]
  (->> (-parser text-or-resource)
       (transform -transformer)))
