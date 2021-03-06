# Blather

<img align="right" src="https://github.com/arrdem/blather/raw/master/etc/blatherbot.jpg"/>

> blather
>
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

## Demo

Blather is currently fully capable of translating between some regular expression dialects - a
sub-problem of the BNF translation problem since some BNF dialects support embedding of regular
expressions.

```clj
user> (require '[languages.jdk-re :refer [parse]])
user> (require '[languages.posix-bre :refer [emit]])
user> (emit (parse "(a[bc]\s*d))"))
"\\(a[bc][:space:]{0,}\\)"
```

Full BNF to BNF translation is a work in progress.

## Grammar support

| Name | Parse? | Emit? | Optimize? | More |
|------|--------|-------|-----------|------|
| [JDK Pattern](https://docs.oracle.com/javase/7/docs/api/java/util/regex/Pattern.html) | Yes | Yes | No | |
| [POSIX BRE](http://pubs.opengroup.org/onlinepubs/009695399/basedefs/xbd_chap09.html) | Yes | Yes | No | |
| [POSIX ERE](http://pubs.opengroup.org/onlinepubs/009695399/basedefs/xbd_chap09.html) | No | No | No | |
| [RFC 5234](https://tools.ietf.org/html/rfc5234) | Yes | No | No | Augmented BNF for Syntax Specifications: ABNF (2008) |
| [Lark](https://github.com/erezsh/lark/blob/master/docs/reference.md) | Yes | No | No | Lark's dialect of BNF |
| [Instaparse ABNF](https://github.com/Engelberg/instaparse/blob/master/src/instaparse/abnf.cljc) | No | No | No | Instaparse's ABNF dialect |
| [RFC 822](https://tools.ietf.org/html/rfc822) | No | No | No | STANDARD FOR THE FORMAT OF ARPA INTERNET TEXT MESSAGES (1982) |
| [RFC 5511](https://tools.ietf.org/html/rfc5511) | No | No | No | Routing Backus-Naur Form: RBNF (2009) |
| [WSN](https://dl.acm.org/citation.cfm?doid=359863.359883) | No | No | No | What can we do about the unnecessary diversity of notation for syntactic definitions?: WSN (1977) |

## Implementation

BNF and regular expressions are really just the specialization of some general state machine
concepts to the particular problem of recognizing characters, or sets of characters using finite
state machines.

Blather provides three fundamental groups of structure for representing grammars - sets of
characters, combinators on patterns and grammars on combinators.

### Sets of Characters

See [src/main/clj/irregular/core*](src/main/clj/irregular).

```clj
user=> (require '[irregular.core :as i])
;; Irregular just uses Java characters for singletons
user=> \a
\a

;; Ranges of characters are used where possible.
user=> (i/union \a \b)
{:tag :irregular.core/range, :multi-byte false,
 :lower \a, :upper \b}

;; Irregular also has a general representation for the union
;; of character sets.
user=> (i/union \a \c)
{:tag :irregular.core/union, :terms #{\a \c}}

;; Union behaves as expected given duplicates.
user=> (i/union \a \c \c \d)
{:tag :irregular.core/union, :terms #{\a \c \d}}

;; And it tries really hard to reduce to the minimum
;; equivalent representation!
user=> (i/union (i/union \a \c \c \d) \b)
{:tag :irregular.core/range, :multi-byte false,
 :lower \a, :upper \d}

;; Subtraction can break ranges as you'd expect
user=> (i/subtraction (i/->range (char \u0000))
                      Character/MAX_VALUE)
{:tag :irregular.core/union,
 :terms #{{:tag :irregular.core/range, :multi-byte false,
           :lower \u0000, :upper \`}
          {:tag :irregular.core/range, :multi-byte true,
           :lower \b, :upper \uffff}}}

;; But some things can't be represented as sets of actual
;; characters. For instance the equivalence class on \a
user=> (i/->equiv \a)
{:tag :irregular.core/equivalence, :name \a}

;; Or the "whitespace" character class in a codepage
user=> (i/->class "whitespace")
{:tag :irregular.core/class, :name "whitespace"}

;; Or a collation class
user=> (i/->collation "foos")
{:tag :irregular.core/collation, :name "foos"}

;; Obviously, set operations on these are somewhat difficult.
;; But this makes it possible to represent a number of
;; dialect specific features without global knowledge
;; of them.
user=> (i/union (i/->class "whitespace") \space)
{:tag :irregular.core/union,
 :terms #{\space
          {:tag :irregular.core/class,
           :name "whitespace"}}}
```

### State Machines

A state machine in Blather is a tree of character sets and combinators. Combinators provide tools
for talking about concatenation, alternation, repetition and soforth. There is also a grouping
combinator, which has no effect given a tree structure but represents source level grouping for
disambiguation.

The various repetition operators, `?`, `*`, `+`, `{n}`, `{n,}` and `{n,m}` all can be rewritten as
one of `{n}`, `{n,}` or `{n,m}` where `n` may be zero and `n` and `m` may be equal. `?`, `*` and `+`
are just shorthands in the commonly adopted regular expression notation. Blather only uses the three
required repetition structures (exactly `n`, `n` or more, `n` to `m`) and expects that front-ends /
back-ends will provide the appropriate transformation to use shorthand when available.

By default, combinators are considered to be greedy but cooperative. These are the regular
expression semantics we're used to. Combinators may be transformed to be reluctant or
possessive. These semantics are unusual and not well supported outside of a few more modern
platforms.

Support for these modifiers in grammars will vary. Any back-end which doesn't support the specified
combinator modifiers should error out, rather than produce a grammar which is not operationally
equivalent.

### Grammars

The only difference between a grammar and a state machine is that a grammar is a collection of
labeled states, which allows labels naming states to occur as combinator terminals in addition to
character sets.

Grammars will attempt to communicate the feature set that they expect, so that back-ends can
determine ahead of time whether they're able to emit all the required features.

## Correctness

The goal of this project is to automate the translation of grammars from one notation to another
operationally equivalent notation. Unfortunately, [proving CFG equivalence is
undecidable](https://en.wikipedia.org/wiki/Context-free_grammar#Undecidable_problems). Consequently,
Blather can offer no certainty. Furthermore, because BNF and regular expression dialect features
vary, Blather has to err on the side of correctness over simplicity wherever possible.

This means that while a Blather translated grammar SHOULD be equivalent, it may not be the grammar
you want to use due to translation artifacts and I cannot promise correctness in translation. The
worst part is that many decisions in grammar architecture are driven by the operational semantics of
the engine used to implement the parser.

For instance my grammars tend to prefer right-recursion over inline repetition due to the way that
Instaparse represents the resulting parses and the simplifications that such recursion offers in
processing the resulting tree.

## License

Copyright (C) 2017 Reid Douglas 'arrdem' McKenzie

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
