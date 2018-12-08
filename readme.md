# Dataflow Compiler

A proof of concept optimizing compiler for a dataflow-based intermediate stream
processing language. The compiler takes is a program that for every element in
the input stream yields zero or more elements. (Think list comprehensions in
Python, `for` comprehensions in Scala, or LINQ in C#). It optimizes the program
by analyzing the data flow, taking advantage of purity, and of the limited
opportunity for control flow in such programs.

My goal is to target a strict language, which is an interesting code generation
problem because the data flow leaves a lot of freedom for scheduling operations.
We need to infer the control flow from the dataflow. On the one hand to avoid
doing useless work, but also for correctness: a conditional division that
verifies that the denominator is nonzero, should not compile to a program that
divides by zero anyway. Targeting a lazy language is simpler in this regard,
because dependencies are tracked dynamically at runtime, rather than statically
at compile time. For example, if a value is only used conditionally, we should
only compute it in the branch where it is used. In a lazy language we could get
away with unconditionally producing a thunk, as it would only be forced inside
the branch.

## Implementation Notes

 * The variable and expression type use GADTs for type safety. An optimization
   pass that would change the type of a value would not typecheck.
 * The use of `PatternSynonyms` and `ViewPatterns` makes for quite readable
   peephole optimization passes.
 * I started out allowing both variables and constants in expressions, but
   allowing only variables (and making constants expressions) make writing
   optimizations more uniform.
 * Having an identity expression is useful to modularize optimization passes.
   One pass would rewrite `$2 = $1 + 0` to `$2 = $1`, and it does not need to
   be cluttered by anything else. Another pass would then rewrite references to
   `$2` with references to `$1`, at which point `$2` becomes dead code.

## Building

    stack build
    stack exec dfc
