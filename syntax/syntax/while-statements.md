---
tags:
  - syntax
---

# While Statement Syntax

## General Form

```cloak
while (condition) {
    // block
}
```

---

## Components

| Part        | Description                                        |
| ----------- | -------------------------------------------------- |
| `while`     | Keyword indicating the start of a loop             |
| `condition` | Boolean expression evaluated before each iteration |
| `{ ... }`   | Block of code to execute repeatedly while true     |

---

## Example

```cloak
while (count < 10) {
    count = count + 1;
}
```

---

## Notes

- The `condition` must evaluate to a boolean value (`true` or `false`).
- The block inside `{}` executes **repeatedly** as long as the condition remains true.
- Curly braces `{}` are **required**, even for a single statement inside the loop.
- Ensure the condition eventually becomes false to avoid infinite loops.

---

## Related

- [[syntax/expressions]]
- [[syntax/block]]
- [[grammar/while-statement]]