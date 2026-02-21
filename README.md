# Crafting Interpreters in Common Lisp

This repository contains my journey building the interpreters from Robert Nystrom's [Crafting Interpreters](https://craftinginterpreters.com/) using **Common Lisp**.

The goal of this project is to showcase the elegance, power, and rapid development capabilities of Common Lisp for language design and compiler architecture.

## 🚀 The Three Interpreters

This project builds three distinct runners for the Lox language:

1. **The Tree-walk Interpreter (`lox-tw`)**
   - Built with the **Common Lisp Object System (CLOS)**.
   - Showcases how Lisp's dynamic dispatch completely removes the need for Java's verbose Visitor Pattern.
2. **The Bytecode VM (`lox-vm`)**
   - Built with specialized bit-arrays, macros, and strong typing.
   - Showcases how Lisp can simulate raw, C-like speed and memory control without giving up high-level safety features like garbage collection.
3. **The Lisp-Native Macro Compiler (`lox-lisp`)**
   - The "Cheat Code" version.
   - Showcases Lisp's ultimate superpower: using Reader Macros to natively absorb Lox syntax and compile it instantly to native x86 machine code via SBCL.

## 🛠 Building and Testing

We use `SBCL` and `ASDF` for building, wrapped in a simple `Makefile`.

```bash
# Run the test suite
make test

# Build the tree-walk interpreter
make treewalk
```

## 📖 Milestone Progress

### Setup & Foundation

*Initialized Git, defined the project ASDF systems, and configured the central test runner (`make test`). Lisp's ASDF system allows us to elegantly partition the Tree-walk, VM, and tests into separate loadable definitions within the same physical directory structure.*
