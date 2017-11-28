# Blather

> blath·er
>
> /blaT͟Hər/
>
> verb; To talk long-windedly without making much sense.

[Backus-Naur Form](https://en.wikipedia.org/wiki/Backus%E2%80%93Naur_form) has become the [lingua
franca](https://en.wikipedia.org/wiki/Lingua_franca) with which protocol and language architects
describe their creations. This is for good reason. It's fairly easy to learn, has little ceremony,
and techniques for mechanically translating grammar specifications into implementations are well
studied and widely available.

[Instaparse](https://github.com/Engelberg/instaparse), [Lark](https://github.com/erezsh/lark),
[ANTLR](https://github.com/antlr/antlr4) and other tools like them enable users to rapidly derive
parser implementations from BNF specifications. However because they use their own variations on
BNF, there's still a large and error prone manual step in converting a language specification to an
implementation because invariably they have to be rewritten for each toolkit.

By a large, converting between BNF dialects means adapting syntax features such as comments,
implicit concatenation, ordered and unordered alternation, character set notations, the use of
square brackets rather than `?` to denote optional syntax elements and soforth. These
transformations could be entirely mechanical, given some common intermediary representation.

Blather provides a toolkit for writing precisely such transformations.

## Status

- `[9/10]` Parse the RFC 5234 of RFC 5234 into an AST
- `[9/10]` Parse Lark grammars into an AST
- `[4/16]` Design an intermediate 'common' representation for BNF operational semantics
  - `[9/10]` Design and implement an abstract representation of regular expressions. Regex dialects
    differ, need to be able to convert between them. Escape codes vary, support for quantifiers,
    etc. Note that regular expressions fully subset BNF, and so it's possible to generate states and
    rewrite pretty much any regular expression into any BNF dialect which also features
    quantifiers. This usually isn't desired but is possible.
  - `[0/3]` Figure out what to do with string literals. They're a special case of regexes that only
    match one string. Some dialects (RFC 5234) impose restrictions on what characters can occur
    within a string literal.
  - `[0/3]` Figure out what to do with byte literals. Again a special case of regex usually used in
    lieu of escape sequences.

## What Works

Character sets, the basis for regular expressions, exist and satisfy some basic expectations.

```clj
;; The inverse of the inverse of a character set is the original character set
irregular.char-sets> (char-set-negate (char-set-negate (char-range 50 150)))
{:tag :irregular.char-sets/char-range, :multi-byte true, :upper 150, :lower 50}
;; The union of a set with a subset is the source set
irregular.char-sets> (char-set-union (char-range 50 150) (char-range 50 100))
{:tag :irregular.char-sets/char-range, :multi-byte true, :upper 150, :lower 50}
;; The union of two disjoint sets is a new set
irregular.char-sets> (char-set-union (char-range 50 150) (char-range 0 25))
{:tag :irregular.char-sets/char-set,
 :multi-byte true,
 :ranges
 [{:tag :irregular.char-sets/char-range, :multi-byte true, :upper 150, :lower 50}
  {:tag :irregular.char-sets/char-range, :multi-byte false, :upper 25, :lower 0}]}
;; Negation round-tripping works on character sets not just ranges
irregular.char-sets> (char-set-negate (char-set-negate (char-set-union (char-range 50 150) (char-range 0 25))))
{:tag :irregular.char-sets/char-set,
 :multi-byte true,
 :ranges
 [{:tag :irregular.char-sets/char-range, :multi-byte false, :upper 25, :lower 0}
  {:tag :irregular.char-sets/char-range, :multi-byte true, :upper 150, :lower 50}]}
```

There exists a preliminary regex -> AST analyzer which is at least sufficient for analyzing Java
style regexes. It's definitely sufficient for analyzing POSIX regular expressions, but that isn't
implemented quite yet.

```clj
irregular.jdk-re> (parse "a++[a-z&&[^ac]]*?c?")
{:tag :irregular.combinators/cat,
 :multi-byte false,
 :pattern1 {:tag :irregular.combinators/rep-n+,
            :behavior :irregular.combinators/possessive,
            :multi-byte false, :count 1,
            :pattern {:tag :irregular.char-sets/char-range,
                      :multi-byte false, :upper 97, :lower 97}},
 :pattern2 {:tag :irregular.combinators/cat,
            :multi-byte false,
            :pattern1 {:tag :irregular.combinators/rep-n+,
                       :behavior :irregular.combinators/reluctant,
                       :multi-byte nil,
                       :pattern {:tag :irregular.char-sets/char-set,
                                 :multi-byte false,
                                 :ranges [{:tag :irregular.char-sets/char-range,
                                           :multi-byte false, :upper 122, :lower 100}
                                          {:tag :irregular.char-sets/char-range,
                                           :multi-byte false, :upper 98, :lower 98}]},
                       :count 1},
            :pattern2 {:tag :irregular.combinators/rep-nm,
                       :behavior :irregular.combinators/greedy,
                       :multi-byte false, :min 0, :max 1,
                       :pattern {:tag :irregular.char-sets/char-range,
                                 :multi-byte false,
                                 :upper 99, :lower 99}}}}
```

There is as of yet no regex AST simplification engine. When alternation occurs, we should attempt to
compile the left and right alternatives down to a minimum single pattern or chain of patterns. The
naive implementation of this rewrite is probably pretty trivial - `(fix #(simplify* %) node)` and
worry about being efficient about it later. Basically groups and concatenation are gonna prevent
optimization for the most part, but that's also what they're there for since they largely are state
control constructs.

Another wrinkle is repetition quantifiers - quantifiers with different behavior (eg greedy vs lazy
vs possessive) needn't reduce under concatenation. Detecting stupid patterns without attempting to
compile down to a matching machine is gonna be Hard.

A preliminary compiler from my JDK RE internal representation back to a valid JDK RE string exists
and works well enough to be dangerous.

```clj
irregular.jdk-re> (compile (parse "a++[a-z&&[^ac]]*?c?"))
#"a++[[d-z]b]+?c?"
irregular.jdk-re> (emit (parse "a++[a-z&&[^ac]]*?c?"))
a++[[d-z]b]+?c?
```

BNF dialects totally parse (that wasn't hard). They don't yet however generate a remotely reasonable
AST representation beyond their dialect specific syntax tree.

```clj
blather.rfc5234> (parse (slurp (clojure.java.io/file "etc/rfc5234.txt")))
[:rulelist
 [:addition
  [:rulename "rulelist"]
  [:elements
   [:alternation
    [:concatenation
     [:repetition
      [:repeat [:n-or-more [:DIGIT "1"] "*"]]
      [:element
       [:group
        "("
        [:alternation
         [:concatenation [:repetition [:element [:rulename "rule"]]]]
         [:alternation
          [:concatenation
           [:repetition
            [:element
             [:group
              "("
              [:alternation
               [:concatenation
                [:repetition [:repeat [:zero-or-more "*"]] [:element [:rulename "c-wsp"]]]
                [:repetition [:element [:rulename "c-nl"]]]]]
              ")"]]]]]]
        ")"]]]]]]]
 ...
 ]
```

For instance we can say that repetition constructs fall into `n`, `n or more` or `n to m`. Dialects
may provide notation for special cases, but that's really all there is.

It'd also be nice to be able to talk about the schema (spec/type/whatever) of a dialect in terms of
the set of features or extensions which it uses. This allows us to talk meaningfully about whether a
translator or rendering tool is adequate to the needs of the representation we want to emit. I kinda
got bogged down here tbqh.

## Grammar support

- [ ] BNF 60 - OG BNF from the ALGOL 60 report (1960)
- [X] [RFC 5234](https://tools.ietf.org/html/rfc5234) -  Augmented BNF for Syntax Specifications: ABNF (2008)
- [ ] [Lark](https://github.com/erezsh/lark/blob/master/docs/reference.md) - Lark's dialect of BNF
- [ ] [Instaparse ABNF](https://github.com/Engelberg/instaparse/blob/master/src/instaparse/abnf.cljc) - Instaparse's ABNF dialect
- [ ] [RFC 822](https://tools.ietf.org/html/rfc822) - STANDARD FOR THE FORMAT OF ARPA INTERNET TEXT MESSAGES (1982)
- [ ] [RFC 5511](https://tools.ietf.org/html/rfc5511) - Routing Backus-Naur Form: RBNF (2009)
- [ ] [TBNF](https://dl.acm.org/citation.cfm?id=1147218) - A Translational BNF Grammar Notation: TBNF (2006) (low priority)
- [ ] [WSN](https://dl.acm.org/citation.cfm?doid=359863.359883) - What can we do about the unnecessary diversity of notation for syntactic definitions?: WSN (1977)

## License

Copyright © 2017 Reid Douglas 'arrdem' McKenzie

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
