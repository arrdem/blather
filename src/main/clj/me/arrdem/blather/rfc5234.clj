(ns me.arrdem.blather.rfc5234
  "A Blather dialect for ABNF as specified in RFC5234.

  RFC7405 updates RFC5234 adding explicit support for case-sensitivity."
  (:refer-clojure :exclude [str])
  (:require [clojure.java.io :refer [resource]]
            [me.arrdem.blather.grammars :as g]
            [me.arrdem.irregular.combinators :as c]
            [me.arrdem.irregular.char-sets :as cs]
            [me.arrdem.irregular.imp :refer [tag-dx]]
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
               (reduce (fn [grammar [tag {:keys [name]} production]]
                         (case tag
                           (:rule)     (assoc grammar name production)
                           (:addition) (update grammar name c/alt production)))
                       {} rules-and-additions))})

(defn parse
  "Consumes a resource, parsing it as a RFC5234 structured text, and generating an analyzed FSM"
  [text-or-resource]
  (->> (-parser text-or-resource)
       (transform -transformer)))


(defn set? [node]
  (#{ ::cs/char-range ::cs/char-set} (:tag node)))

(defn range? [node]
  [{:keys [upper lower] :as charset}]
  (= upper lower))

(defn range-as-char
  [range]
  {:pre [(range-could-be-char? range)]}
  (char (:lower range)))

(defn str? [{:keys [tag]}]
  (= tag ::str))

(defn str
  "Intermediate node representing a character sequence. Produced when simplifying grammars."
  [& strs-and-chars]
  {:tag     ::str
   :pattern (->> strs-and-chars
                 (mapcat (fn [str-or-char]
                           (cond (str? str-or-char)
                                 (recur (:pattern str-or-char))

                                 (instance? Character str-or-char)
                                 [str-or-char]

                                 (instance? String str-or-char)
                                 (seq str-or-char))))
                 (apply clojure.core/str))})

(defmulti simplify*
  "Case analysis of various single step simplifications."
  tag-dx)

(defmethod simplify* ::c/cat [{:keys [pattern1 pattern2] :as n}]
  (cond (not (and (range? pattern1)
                  (range-could-be-char? pattern1)))
        n

        (and (range? pattern2)
             (set-could-be-char? pattern2))
        (str (range-as-char pattern1) (range-as-char pattern2))


        (str? pattern2)
        (str (range-as-char pattern1) pattern2)

        :else n))

(defmethod simplify* :default [node]
  node)

(defn simplify
  "Attempts to simplify some interesting cases of the grammar."
  [tree]
  (let [do-simplify (fn [tree]
                      (->> tree
                           (map (fn [[rulename pattern]]
                                  (prn pattern)
                                  [rulename (simplify* pattern)]))
                           (into {})))]
    (loop [tree₀ nil
           tree₁ tree]
      (if-not (= tree₀ tree₁)
        (recur tree₁ (do-simplify tree₁))
        tree₁))))
