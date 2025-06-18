---
tags:
  - grammar
---

# Literals

```ebnf
literal         ::= number | string | boolean ;
number          ::= digit { digit } [ "." digit { digit } ] ;
string          ::= "\"" { character } "\"" ;
boolean         ::= "true" | "false" ;
```