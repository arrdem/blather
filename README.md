# Blather

<img align="right" src="https://github.com/arrdem/blather/raw/master/etc/blatherbot.jpg"/>

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



## License

Copyright © 2017 Reid Douglas 'arrdem' McKenzie

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
