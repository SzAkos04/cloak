---
tags:
  - syntax
---

# Let Statement Syntax

## General Form

```cloak
let mut variable_name: Type = expression;
```

---

## Components

| Part           | Description                                   |
| -------------- | --------------------------------------------- |
| `let`          | Keyword indicating a variable declaration     |
| `mut`          | Optional keyword to make the variable mutable |
| `variableName` | Identifier for the variable name              |
| `: Type`       | Explicit type notation for the variable       |
| `= expression` | Optional value assignment for variable        |
| `;`            | Statement terminator                          |

---

## Example

```cloak
let pi: f64 = 3.1415;
let mut x: i32 = 3;
let name: string = "Akos";
```

---

## Type Annotations

- Type annotations (`: Type`) are **necessary**.
- Omitting `= expression` declares a variable without initialization.

---

## Immutability & Mutability

- Variables declared with `let` are **immutable** by default.
- To allow reassignment, use `mut`:
```cloak
let mut score: i32 = 0;
```

---

## Related

- [[syntax/expressions]]
- [[syntax/types]]
- [[grammar/let-statement]]