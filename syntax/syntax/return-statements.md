---
tags:
  - syntax
---

# Return Statement Syntax

## General Form

```cloak
return expression;
```

---

## Components

| Part         | Description                                |
| ------------ | ------------------------------------------ |
| `return`     | Keyword indicating the function exit point |
| `expression` | Optional value or expression to return     |
| `;`          | Statement terminator                       |

---

## Example

```cloak
fn add(a: i32, b: i32): i32 {
    return a + b;
}

fn greet(): void {
    return;
}
```

---

## Notes

- The `return` keyword immediately exits the current function.
- `expression` is optional if the function returns `void`.
- The returned expression must match the function’s declared return type.

---

## Related
- [[syntax/functions]]
- [[syntax/expressions]]
- [[grammar/return-statement]]