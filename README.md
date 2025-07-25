# Cloak compiler

**Cloak** is a minimalist compiler with a simple command-line interface for compiling `.ck` source files.

## Usage

```bash
cloak [options] <file.ck>
```

## Options

| Option                       | Description                                           |
| ---------------------------- | ----------------------------------------------------- |
| `-o`, `--outfile <file>`     | Set output filename                                   |
| `-O[level]`, `--opt=[level]` | Set optimization level (`0`, `1`, `2`, `3`, `s`, `z`) |
| `--color`                    | Enable color diagnostics                              |
| `--no-color`                 | Disable color diagnostics                             |
| `--silent`                   | Suppress all warnings                                 |
| `--no-silent`                | Enable warnings                                       |
| `--verbose`                  | Show verbose debugging                                |
| `-h`, `--help`               | Show help message                                     |
| `-v`, `--version`            | Show version info                                     |
| `--dump-tokens`              | Print tokens vector to stdout                         |
| `--dump-ast`                 | Print AST to stdout                                   |
| `--dump-ir`                  | Print LLVM IR to stdout                               |

## Example Program

Here is a simple example of a Cloak Language program:

```cloak
fn main(): i32 {
    let x: i32 = 0;

    return x;
}
```

## Compilation Examples

### Compiling with default output name:

```bash
cloak program.ck # create an `a.o` object file
clang a.o # link the object file
./a.out # run executable
```

### Compile specifying the output name:

```bash
cloak program.ck -o program.o # create a `program.o` object file
clang program.o -o program # link the object file
./program # run executable
```

## Requirements

[LLVM](https://llvm.org/docs/GettingStarted.html)
