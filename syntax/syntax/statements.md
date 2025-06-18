---
tags:
  - syntax
---

# Statements Syntax

## General Concept

Statements are instructions that perform actions. They include variable declarations, expression evaluations, control flow, and more. Statements do **not** return values (or return the unit type `()`).

---

## Types of Statements

|Statement Type|Description|Example|
|---|---|---|
|Variable Declaration|Declares a new variable|`let x: i32 = 5;`|
|Expression Statement|Executes an expression for side effects|`println!("Hello");`|
|Control Flow|Directs the flow of execution|`if`, `while`, `for` statements|
|Return|Exits a function and optionally returns a value|`return x;`|
|Block|Groups multiple statements|`{ let y = 2; println!("{}", y); }`|

---

## Notes

- Statements typically end with a semicolon `;` except blocks.
- Statements perform actions but don’t produce values usable in expressions.
- Expressions can appear as statements when followed by a semicolon.
- Control flow statements alter the order of execution but are themselves statements.

---

## Example

```cloak
let mut count: i32 = 0;

while (count < 3) {
    count = count + 1;
}

return;
```

---

## Related

- [[syntax/let-statements]]
- [[syntax/expressions]]
- [[syntax/block]]
- [[syntax/return-statements]]