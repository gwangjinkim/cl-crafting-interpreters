# LEX-SPEC: The Lox Language Specification

This document strictly defines the **Lox** language (often referred to as LEX in our context) as designed in Robert Nystrom's *Crafting Interpreters*. It is a small, high-level, dynamically typed language that supports procedural, functional, and object-oriented programming paradigms.

## 1. Data Types

Lox dynamically types its values. There are only four base types:

- **Booleans:** `true` and `false`.
- **Numbers:** All numbers are double-precision floating-point numbers (e.g., `1234`, `12.34`).
- **Strings:** Enclosed in double quotes (e.g., `"Hello, world"`.
- **Nil:** Represents the absence of a value (equivalent to `null` or Lisp's `nil`).

## 2. Expressions

Lox features a set of familiar expressions and operators (with C-style precedence):

- **Arithmetic:** `+` (addition and string concatenation), `-` (subtraction/negation), `*` (multiplication), `/` (division).
- **Comparison:** `<`, `<=`, `>`, `>=`, `==`, `!=`. (Matches on value equality, not strictly identity).
- **Logical:** `and`, `or`, `!` (logical NOT).
- **Grouping:** `( ... )` to override precedence.

## 3. Statements & Control Flow

A statement produces an effect but does not return a value.

- **Expression Statements:** Computes an expression and throws away the result. E.g., `"some expression";`
- **Print Statements:** A built-in statement primitive. `print "Hello";`
- **Blocks:** Groups multiple statements to define a new lexical scope. `{ ... }`
- **If / Else:** Conditional execution. `if (condition) { ... } else { ... }`
- **While loops:** `while (condition) { ... }`
- **For loops:** Standard C-style loop syntactic sugar. `for (var i = 0; i < 10; i = i + 1) { ... }`

## 4. Variables & Scoping

- **Declaration:** Uses the `var` keyword. `var beverage = "espresso";`
- If a variable is declared without an initializer, its value defaults to `nil`.
- **Scope:** Lox relies on strict **lexical scoping** (just like Scheme or Common Lisp). Inner blocks can shadow outer variables. Assignments bind to the innermost declared variable.

## 5. Functions & Closures

- **Declaration:** First-class functions declared with the `fun` keyword.

  ```lox
  fun printSum(a, b) {
    print a + b;
  }
  ```

- **Return:** The `return` statement exits a function. If omitted, the function implicitly returns `nil`.
- **Closures:** Functions capture the environment where they were declared. This must work intuitively, preserving local variables long after their outer scope has returned.

## 6. Object-Oriented Programming

Lox supports single-inheritance class-based OOP.

- **Classes:**

  ```lox
  class Breakfast {
    cook() {
      print "Eggs a-fryin'!";
    }
  }
  ```

- **Instantiation:** Calling the class like a function `Breakfast()` invokes the constructor and returns a new instance.
- **Initialization:** If a method named `init()` exists, it is automatically called on instantiation.
- **Properties:** Fields are added and accessed dynamically on instances via the dot operator `.`. E.g., `someObj.field = "value";`
- **The `this` Keyword:** Refers to the current object instance within a method. It acts like a hidden local variable bound at method invocation.
- **Inheritance & `super`:**

  ```lox
  class Brunch < Breakfast {
    drink() { print "Mimosa!"; }
  }
  ```

  The keyword `super` can be used to access shadowed methods on the superclass.

## 7. Grammar Summary (EBNF)

```ebnf
program        → declaration* EOF ;
declaration    → classDecl | funDecl | varDecl | statement ;
classDecl      → "class" IDENTIFIER ( "<" IDENTIFIER )? "{" function* "}" ;
funDecl        → "fun" function ;
varDecl        → "var" IDENTIFIER ( "=" expression )? ";" ;
statement      → exprStmt | forStmt | ifStmt | printStmt | returnStmt | whileStmt | block ;
exprStmt       → expression ";" ;
forStmt        → "for" "(" ( varDecl | exprStmt | ";" ) expression? ";" expression? ")" statement ;
ifStmt         → "if" "(" expression ")" statement ( "else" statement )? ;
printStmt      → "print" expression ";" ;
returnStmt     → "return" expression? ";" ;
whileStmt      → "while" "(" expression ")" statement ;
block          → "{" declaration* "}" ;
expression     → assignment ;
assignment     → ( call "." )? IDENTIFIER "=" assignment | logic_or ;
logic_or       → logic_and ( "or" logic_and )* ;
logic_and      → equality ( "and" equality )* ;
equality       → comparison ( ( "!=" | "==" ) comparison )* ;
comparison     → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
term           → factor ( ( "-" | "+" ) factor )* ;
factor         → unary ( ( "/" | "*" ) unary )* ;
unary          → ( "!" | "-" ) unary | call ;
call           → primary ( "(" arguments? ")" | "." IDENTIFIER )* ;
primary        → "true" | "false" | "nil" | "this" | NUMBER | STRING | IDENTIFIER | "(" expression ")" | "super" "." IDENTIFIER ;
function       → IDENTIFIER "(" parameters? ")" block ;
parameters     → IDENTIFIER ( "," IDENTIFIER )* ;
arguments      → expression ( "," expression )* ;
```
