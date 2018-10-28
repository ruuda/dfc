# Dataflow

A proof of concept optimizing compiler for a dataflow-based intermediate stream
processing language. The compiler takes is a program that for every element in
the input stream yields zero or more elements. (Think list comprehensions in
Python, `for` comprehensions in Scala, or LINQ in C#). It optimizes the program
by analyzing the data flow, taking advantage of the limited opportunity for
control flow in such programs.

My goal is to target a strict language, which is an interesting code generation
problem because the data flow leaves a lot of freedom for scheduling operations.
We need to infer the control flow from the dataflow. Targeting a lazy language
is simpler in this regard, because there is no need to specify control flow.
For example, if a value is only used conditionally, we should only compute
it in the branch where it is used. In a lazy language we could get away with
unconditionally producing a thunk, as it would only be forced inside the branch.

## Building

    stack setup
    stack build
    stack exec dataflow

