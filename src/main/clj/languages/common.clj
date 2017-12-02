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

(def ANY-ASCII
  "ASCII character range."
  (i/->range \u0000 \u007f))

(def whitespace
  "ASCII whitespace."
  (i/union
   \space
   \tab
   \newline
   \u000B ;; Vertical tab
   \formfeed
   \return))

(def -lower
  "Known \"standardized\" lower case range `[a-z]` per jdk8 Pattern"
  (i/->range \a \z))

(def -upper
  "Known \"standardized\" upper case range `[A-Z]` per jdk8 Pattern"
  (i/->range \A \Z))

(def -alpha
  "An alphabetic character `[\\p{Lower}\\p{Upper}]` per jdk8 Pattern"
  (i/union -lower -upper))

(def -digit
  "Known \"standardized\" digit range `[0-9]` per jdk8 Pattern"
  (i/->range \0 \9))

(def -word
  "Known \"standardized\" word range `[a-zA-Z_0-9]` per jdk8 Pattern"
  (i/union
   -lower
   -upper
   \_
   -digit))

(def -alnum
  "Alpha or numeral"
  (i/union -alpha -digit))

(def -punct
  "Punctiation One of !\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~"
  (apply i/union "!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~"))

(def -graph
  "Any visible (ASCII) character"
  (i/union -alnum -punct))

(def -print
  "Any printable character"
  (i/union -graph \u0020))

(def -blank
  "`[ \\t]`"
  (i/union
   \tab
   \space))

(def -cntrl
  "A control character"
  (i/union
   (i/->range \u0000 \u001f)
   \u007f))

(def -xdigit
  "Any hexadecimal digit `[0-9a-zA-Z]`"
  (i/union
   (i/->range \0 \9)
   (i/->range \a \f)
   (i/->range \A \F)))

(def -space
  "A whitespace character `[ \\t\\n\\x0B\\f\\r]`"
  (apply i/union "[ \t\n\u000B\f\r]"))
