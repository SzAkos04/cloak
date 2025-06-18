---
tags:
  - syntax
  - todo
---

# Block Syntax

## General Form

```cloak
{
    // statements / expressions
}
```

---

## Components

|Part|Description|
|---|---|
|`{}`|Curly braces enclosing a block of code|
|Statements / Expressions|Code inside the braces executed sequentially|

---

## Example

```cloak
{
    let pi: f64 = 3.1415;
}
```

---

## Notes

- Blocks group multiple statements or expressions into one compound statement.
- **Scopes are not yet implemented**

---

## Related
- [[syntax/statements]]
- [[syntax/expressions]]
- [[grammar/block]]