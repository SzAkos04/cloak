---
tags:
  - syntax
---

# Types

This language uses statically declared types. Each variable, function return, and expression has an associated type, either primitive or compound.

---

## Primitive Types

| Keyword  | Description                  |
| -------- | ---------------------------- |
| `bool`   | Boolean (`true`/`false`)     |
| `f32`    | 32-bit floating point number |
| `f64`    | 64-bit floating point number |
| `i8`     | 8-bit signed integer         |
| `i16`    | 16-bit signed integer        |
| `i32`    | 32-bit signed integer        |
| `i64`    | 64-bit signed integer        |
| `string` | UTF-8 encoded text           |
| `void`   | No value returned            |

---

## Examples

### Variable Declarations

```cloak
let is_done: bool = true;
let x: i32 = 42;
let name: string = "Alice";
```
### Function with Type Annotations
```cloak
fn add(a: i32, b: i32): i32 {
    return a + b;
}
```

---

## Type Usage

- All variables must declare a type.
- Function parameters and return types must also be typed.

---

## Related

- [[syntax/let-statements]]
- [[syntax/functions]] 
- [[grammar/types]]