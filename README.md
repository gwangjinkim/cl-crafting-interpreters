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

## 🧠 How It Works: The Execution Pipeline

For those unfamiliar with interpreter design or Common Lisp, here is a high-level overview of how `cl-lox-treewalk` executes a Lox program. The process is a classic interpreter pipeline:

1. **Scanning (Lexical Analysis)**  
   *Input: Raw Source Code String ➡️ Output: List of Tokens*  
   The scanner reads the raw text of the Lox program character by character and groups them into meaningful "words" called **Tokens** (e.g., `VAR`, `IDENTIFIER("a")`, `EQUAL`, `NUMBER(1)`).

2. **Parsing (Syntax Analysis)**  
   *Input: List of Tokens ➡️ Output: Abstract Syntax Tree (AST)*  
   The parser enforces the grammatical rules of the Lox language using a technique called *Recursive Descent*. It takes the linear list of tokens and organizes them into a hierarchical, deeply nested tree built out of Common Lisp Objects (CLOS). For example, `1 + 2 * 3` becomes a `binary-expr` tree where `*` is placed deeper in the tree so it evaluates before `+`.

3. **Evaluation (Tree-Walking)**  
   *Input: Abstract Syntax Tree ➡️ Output: Runtime Values & Side Effects*  
   The evaluator traverses (or "walks") the AST, taking action on each node. In typical object-oriented languages like Java, this step often requires verbose design patterns (like the Visitor Pattern) so the evaluator knows exactly *what* code to run for *which* node.

   In Common Lisp, we use **Generic Functions (`defmethod`)**. We define one `evaluate` function, and Lisp's runtime dynamically dispatches the correct logic based on the AST node's type, making the evaluator extremely clean. Features like environments, variable scoping, and object-oriented `class` instances are implemented by mapping them elegantly to native Lisp hash tables, lists, and closures.

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

### Milestone 1: Lexical Analysis (Scanner)

*Built the lexical analyzer for the Tree-walk Interpreter. Lisp is remarkably well-suited for scanning text. By using local functions (`labels`) for state management (`advance`, `peek`, `match`), we avoided the need for complex object-oriented state tracking or messy global variables. The resulting single-pass scanner is elegant and maps the Lox syntax (as defined in `LEX-SPEC.md`) cleanly into Lisp structs. We also utilized Lisp's dynamic `hash-table` and `string-downcase` to efficiently route reserved keywords natively without relying on verbose string switch-cases common in Java or C.*

### Milestone 2: Abstract Syntax Tree & Parser

*Implemented the AST and the recursive descent parser. In Java, Bob Nystrom has to build a heavily boilerplate tool to generate Java classes and set up the Visitor pattern to emulate double dispatch. In Common Lisp, we simply defined the AST hierarchy using `defclass`. The parser leverages Lisp's dynamic and generic object system (CLOS) to instantly instantiate these types. This avoids thousands of lines of boilerplate and sets the foundation for an elegant evaluator using `defmethod`.*

### Milestone 3: Evaluation (Tree-walking)

*Implemented the evaluation engine. While Java requires double-dispatch Visitor pattern boilerplate, Common Lisp's CLOS handles this natively and transparently. We defined a generic `evaluate` function and provided `defmethod` implementations for each AST node type (`literal-expr`, `binary-expr`, etc.). The Lisp runtime dynamically dispatches to the correct method based on the node's type, making the evaluator extremely clean and extensible.*

### Milestone 4.5: Control Flow

*Implemented traditional 'if', 'while', and 'for' control flow structures. In Java, building the environment stack requires messy scoping objects. Common Lisp's dynamic scoping (e.g. `(let ((*environment* ...)) ...)`) combined with `unwind-protect` creates air-tight scope enforcement with a single line of Lisp, proving its mettle for interpreter design.*

### Milestone 5a: Functions & Closures

*Added first-class functions and closures. By using standard Common Lisp closures (a robust standard since 1984) and dynamic `unwind-protect` for environments, passing Lox function closures is virtually free. Returning early out of deeply nested Lox scopes involves a simple Lisp `(throw 'lox-return value)` matched by a `(catch 'lox-return ...)` in the callable dispatcher—an elegant bypass to building complex Java ReturnExceptions.*

### Milestone 5b: Classes & Object-Oriented Programming

*Implemented Lox's class system, including instance property getters and setters, methods, and lexical `this` binding. In Common Lisp, handling dynamic property hash tables and binding closures to instances inside the environment takes just a few lines of code. Instantiation seamlessly routes through `lox-callable` logic via CLOS `defmethod`, resulting in a powerful OO system mapped over native Lisp hash maps and method pointers.*

### Milestone 6 & 7: Chunks of Bytecode & The VM
*We've officially switched tracks to the Bytecode Virtual Machine! Unlike the Object-Oriented Tree-walk interpreter, the VM is entirely array-driven. We use Common Lisp's `(simple-array (unsigned-byte 8))` to create hyper-dense unboxed memory arrays for our instructions—achieving the same memory profile and layout as C. We then built a core `run` loop that dispatches directly off these opcodes (`+op-add+`, `+op-constant+`) and heavily leverages Lisp's macro capabilities to print gorgeous disassembly traces.*
