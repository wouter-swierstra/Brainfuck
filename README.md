Brainfuck
=========

A Brainfuck interpreter written in Agda

The heart of the interpreter is a `run` function that, given a
Brainfuck program and the stream of characters entered via stdin,
produces a (possibly infinite) execution trace.

This program demonstrates how a total language, such as Agda, can
still be *Turing complete* – the `run` function assigns semantics to
any Brainfuck program, even those that do not terminate. The key
insight (which is not particularly novel) is that (productive)
coinductive programs are total and sufficient to simulate Turing
machines. For a more precise definition of 'total', I'd refer to [David
Turner's work](https://uf-ias-2012.wikispaces.com/file/view/turner.pdf).



