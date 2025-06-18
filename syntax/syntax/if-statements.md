---
tags:
  - syntax
---

# If Statement Syntax

## General Form

```cloak
if (condition) {
    // then branch
} else {
    // else branch
}
```

---

## Components

| Part        | Description                                             |
| ----------- | ------------------------------------------------------- |
| `if`        | Keyword starting the conditional statement              |
| `condition` | Boolean expression to evaluate                          |
| `{ ... }`   | Block of code executed if the condition is true         |
| `else`      | Optional, block executed if previous condition is false |

---

## Example

```cloak
if (grade > 50) {
    passed = true;
} else {
    passed = false;
}
```

---

## Notes

- The `condition` must evaluate to a boolean value (`true` or `false`).
- Curly braces `{}` are **required**, even for a single statement inside the loop.
- `else` block is **optional**.

---

## Related

- [[syntax/expressions]]
- [[syntax/block]]
- [[grammar/if-statement]]