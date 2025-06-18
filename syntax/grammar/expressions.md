---
tags:
  - grammar
---

# Expressions

```ebnf
expression      ::= logical-or ;

logical-or      ::= logical-and { "||" logical-and } ;
logical-and     ::= equality    { "&&" equality } ;
equality        ::= comparison  { ("==" | "!=") comparison } ;
comparison      ::= term        { ("<" | "<=" | ">" | ">=") term } ;
term            ::= factor      { ("+" | "-") factor } ;
factor          ::= unary       { ("*" | "/" | "%") unary } ;
unary           ::= ("-" | "!") unary | call ;
call            ::= primary { "(" [ arguments ] ")" } ;
arguments       ::= expression { "," expression } ;
primary         ::= literal | identifier | "(" expression ")" ;
```