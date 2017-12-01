(ns languages.common
  "Common transformer mappings & character sets."
  (:require [irregular.core :as i]
            [irregular.combinators :as c]))

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
