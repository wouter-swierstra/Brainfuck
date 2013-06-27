Brainfuck
=========

A Brainfuck interpreter written in Agda

I get tired of explaining that *totality* is not the same as *Turing
completeness*. To illustrate the difference, here is a Brainfuck
interpreter written in Agda.

The heart of the interpreter is a *run* function that, given a
Brainfuck program and the stream of characters entered via stdin,
produces a (possibly infinite) execution trace.

