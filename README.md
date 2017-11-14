`GenTypeLevel`
==============

Generate F# code to evaluate lambda calculus at compile-time.

Running the example
===================

The file `input.txt` contains a non-trivial example of lambda calculus
of Church numerals and list representations.

The script `run_main.sh` compiles this project, and runs the code generator on
`input.txt`. You will see the generated F# code and the output.

To prove that these expressions are evaluated at compile-time, you can open the
generated code `Generated1.fs` in either F# Interactive or an IDE, and check the
types inferred for the four variables in the second-last line:

```
val shouldBe11 : T_succ_1<T_succ_1<T_succ_1<T_succ_1<T_succ_1<T_succ_1<T_succ_1<T_succ_1<T_succ_1<T_succ_1<T_succ_1<T_zero>>>>>>>>>>>
val shouldBeTrue : T_t
val testHead : T_succ_1<T_succ_1<T_succ_1<T_succ_1<T_zero>>>>
val testSum9 : T_succ_1<T_succ_1<T_succ_1<T_succ_1<T_succ_1<T_succ_1<T_succ_1<T_succ_1<T_succ_1<T_zero>>>>>>>>>
```

Instructions
============

The executable is located in
`GenTypeLevel.Main/bin/Debug/GenTypeLevel.Main.exe`. It takes a lambda calculus
program from stdin and outputs the generated F# program.

The syntax of the input is similar to Haskell:

 - Function definition: `f x y z = expr`
 - Lambda abstraction: `\x y z -> expr`
 - Function application: `f x y`
 - Left-associativity: `f x y z` is `((f x) y) z`

In addition, the `var := expr` syntax tells the generated code to evaluate `expr`
into the F# variable `var`.

Current limitations
-------------------

Due to the limitation of the parser, every declaration (`f ... = expr` or
`var := expr`) must be in one line.

Currently, only sequential definitions are supported. That means, a definition
can only depend on previous definition but not itself and definitions below.

How it works
============

Inline functions and statically resolved type parameters
--------------------------------------------------------

In F#, the `inline` keyword indicates that the body of a function must be
expanded at the call site. One important feature of `inline` is
statically resolved type parameters. It allows the compiler to look up a member
(static or instance properties or methods) by name and signature.

A statically-resolved member can be inline as well, allowing further inline
code expansions. We can encode computations such as adding two numbers to run in
compile-time.

Translating lambda abstractions
-------------------------------

Each lambda abstraction `\x -> expr` is translated into an F# type with a static
method `Apply` to facilitate function application. The template is as follows:

```
type {type_name} = {type_name} with
  static member Apply({type_name}, {x}) =
    {Translate(expr)}
```

However, some lambda expressions depend on variables from outside of the
expression (we call them "closures" or "free variables"). For example,
`(\x -> y x)` contains a free variable `y`, and it is often assigned from an
outer lambda expression, such as `\y -> (\x -> y x)`.

To translate closures, we pass the free variables in the constructor.

```
type {type_name}<{free_vars}> = {type_name} of {free_vars} with
  static member Apply({type_name}({free_vars}), {pat}) =
    {Translate(expr)}
```

Each time we need to instantiate a closured lambda expression, we pass the
values of the free variable into the constructor.

Here is the translation for `(\y -> (\x -> y x))`:

```
// (\x -> y x)
type E_1<'y> = E_1 of 'y with
  static member inline Apply(E_1(y), x) =
    (!y <<- !x)

// (\y -> (\x -> y x))
and E = E with
  static member inline Apply(E, y) =
    !E_1(y) // passing the value of y
```

Translating function applications
---------------------------------

To translate the function application `(x y)`, we simply need to invoke
the `Apply` static method using statically resolved type parameters.

We define the `(<|-)` operator as a helper for that, and `(x y)` would
be translated as `{Translate(x)} <|- {Translate(y)}`.

```
let inline (<|-) (x : ^x) (y : ^y) : ^z =
  ( ( ^x or ^y) : (static member Apply : ^x * ^y -> ^z) (x, y))
```

However, due to the strange rules of inline method resolution, not all
translated expression could be compiled properly in F#, especially when one
variable occurred twice in the body of an `Apply` method.

To fix this issue, we instead make the `Apply` method return an expression
tree of the function body. and then we evaluate the expression tree using the
`(!!)` prefix operator.

There are two cases of expression trees (`Expr`s), `E<'a>` and `App<'a, 'b>`.
`E` holds a constant and `App` holds a function application.

The implementations for `(!!)` are:

```
// E
  static member inline (!!) (E a) = a
// App
  static member inline (!!) (App(a, b)) = !!(!!a <|- !!b)
```

To make the generated code more readable, the operators `(!)` and `(<<-)`
are defined as shortcuts to `E` and `App`. For example, `f (g x) y z`
is translated into `((!f <<- (!g <<- !x)) <<- !y) <<- !z`

Other improvements
------------------

Our code generator supports tuples in lambda abstractions. A curried application
like `(\x y z -> expr) x y z` will be rewritten into
`(\(x, y, z) -> expr) (x, y, z)` to save the number of F# types generated.

