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

## Grammar support

- [X] [RFC 5234](https://tools.ietf.org/html/rfc5234) -  Augmented BNF for Syntax Specifications: ABNF (2008)
- [ ] [Lark](https://github.com/erezsh/lark/blob/master/docs/reference.md) - Lark's dialect of BNF
- [ ] [Instaparse ABNF](https://github.com/Engelberg/instaparse/blob/master/src/instaparse/abnf.cljc) - Instaparse's ABNF dialect
- [ ] [RFC 822](https://tools.ietf.org/html/rfc822) - STANDARD FOR THE FORMAT OF ARPA INTERNET TEXT MESSAGES (1982)
- [ ] [RFC 5511](https://tools.ietf.org/html/rfc5511) - Routing Backus-Naur Form: RBNF (2009)
- [ ] [TBNF](https://dl.acm.org/citation.cfm?id=1147218) - A Translational BNF Grammar Notation: TBNF (2006) (low priority)

## License

Copyright © 2017 Reid Douglas 'arrdem' McKenzie

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
