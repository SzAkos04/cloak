---
tags:
  - syntax
---

# Expressions

Expressions evaluate to values and can be used in assignments, function returns, conditionals, and other expression contexts.

---

## General Expression Types

### Literals
```cloak
42
3.14
"hello"
true
```
### Identifiers
```cloak
x
some_variable
```
### Unary Expressions
```cloak
-5
!is_ready
```
#### Supported operators:

| Operator | Description |
| -------- | ----------- |
| `-`      | Negatiton   |
| `!`      | Subtraction |

### Binary Expressions
```cloak
a + b
x * (y - 1)
```
#### Supported Operators:

| Operator | Description     |
| -------- | --------------- |
| `+`      | Addition        |
| `-`      | Subtraction     |
| `*`      | Multiplication  |
| `/`      | Division        |
| `%`      | Modulo          |
| `==`     | Equal           |
| `!=`     | Not equal       |
| `<`      | Less than       |
| `<=`     | Less than/equal |
| `>`      | Greater than    |
| `>=`     | Greater/equal   |
| `&&`     | Logical AND     |

---

## Related

- [[syntax/functions]]
- [[syntax/types]]
- [[grammar/expressions]]
- [[grammar/literals]]