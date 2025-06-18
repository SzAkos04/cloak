---
tags:
  - syntax
---

# Function Syntax

## General Form

```cloak
fn function_name(param1: Type1, param2: Type2): ReturnType {
    // body
}
```

---

## Components

| Part            | Description                               |
| --------------- | ----------------------------------------- |
| `fn`            | Keyword indicating a function declaration |
| `function_name` | Identifier for the function name          |
| `param: Type`   | Parameters with explicit type annotations |
| `: ReturnType`  | Optional return type annotation           |
| `{ ... }`       | Function body block                       |

---

## Example

```cloak
fn add(a: i32, b: i32): i32 {
    return a + b;
}
```

---

## Return Types

- Return type is required after the `:` token.
- Use `void` for functions that do not return a value.

## Related

- [[syntax/expressions]]
- [[syntax/types]]
- [[grammar/function-declaration]]