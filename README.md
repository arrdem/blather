# Blather

> blath·er
>
> /blaT͟Hər/
>
> verb; To talk long-windedly without making very much sense.

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

## Usage

None yet

## License

Copyright © 2017 Reid Douglas 'arrdem' McKenzie

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
