(ns languages.common
  "Common transformer mappings & character sets."
  (:require [irregular.core :as i]
            [irregular.combinators :as c]
            [languages.ascii :as ascii]))

(defn d1x2 [f]
  (fn
    ([x] x)
    ([x y] (f x y))))

(def recursive-concat
  "A transformer mapping for rewriting

  `concatenation = a concatenation ?`

  to combinators"
  {:concatenation (d1x2 c/cat)})

(def recursive-alt
  "A transformer mapping for rewriting

  `alternation = a (<\"|\"> alternation)?`

  to combinators"
  {:alternation (d1x2 c/alt)})

(def shorthand-repetition
  "A transformer mapping for rewriting

  `shorthand-repetition = a #\"*?+\" ?`

  to combinators"
  {:shorthand-repetition (fn [e shorthand]
                           (({"*" c/rep*
                              "?" c/rep?
                              "+" c/rep+} shorthand)
                            e))})

(def bounded-repetition
  "A transformer mapping for rewriting

  `bounded-repetition = e <\"{\"> lower (<\",\"> upper) ? <\"}\">`

  to combinators"
  {:bounded-repetition (fn
                         ([e lower-limit]
                          (c/rep-n e (Long/parseLong lower-limit)))
                         ([e lower-limit upper-limit]
                          (c/rep-nm e
                                    (Long/parseLong lower-limit)
                                    (Long/parseLong upper-limit))))})

(def unbounded-repetition
  "A transformer for rewriting

  `unbounded-repetition = e <\"{\"> lower <\",}\">`

  to combinators"
  {:unbounded-repetition (fn [e lower-limit]
                           (c/rep-n+ e (Long/parseLong lower-limit)))})

(def ANY-UTF8
  (i/->range \u0000 Character/MAX_VALUE))

(def ANY-ASCII
  "ASCII character range."
  (i/->range \u0000 \u007f))

(defn simplifier
  "Function of a traversal function and a transformer function..

  Builds and returns a recursive tree transformer, which will use the
  supplied visitor (fmap) function to recursively visit the entire
  tree, using the given transformer function to transform all the
  nodes as if via fix."
  [fmap transformer]
  (fn recursive-transformer [node]
    (loop [node  node
           node* (transformer (fmap recursive-transformer node))]
      (if (= node node*) node*
          (recur node* (transformer (fmap recursive-transformer node*)))))))

(defn pr-ch [e]
  (let [v (int e)]
    (if (or (Character/isAlphabetic v)
            (Character/isDigit v)
            (Character/isIdeographic v)) e
        (if (>= v 0xFF)
          (format "\\u%04d" v)
          (format "\\u%02d" v)))))

(def ^:dynamic *emitting-set* false)

(defn maybe-set [^String e]
  (if *emitting-set* e
      (format "[%s]" e)))

(defmacro charset [expr]
  `(maybe-set
    (binding [*emitting-set* true]
      ~expr)))
