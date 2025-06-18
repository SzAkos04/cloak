---
tags:
  - grammar
---

# Function Declaration

```ebnf
function-decl   ::= "fn" identifier "(" [ parameters ] ")" ":" type block ;
parameters      ::= parameter { "," parameter } ;
parameter       ::= identifier ":" type ;
```