---
title: "Lively typed tables"
tags: [hazel, oopsla]
synopsis: "Alexander Bandukwala's OOPSLA 2026 paper brings static types to exploratory data analysis without giving up the flexibility of data frames."
draft: true
---

Most data analysis happens in dynamically typed tools: Pandas and R data
frames, spreadsheets, notebooks. They are flexible, but the editor knows very
little about your data. Autocompletion and error highlighting, where they exist
at all, come from whatever values happen to be sitting in the notebook kernel,
so the moment you write a reusable function over a table you lose them. And
when something goes wrong, you find out late and far away, in a stack trace
after a long computation.

Statically typed table libraries fix some of this, but they struggle with the
operations analysts reach for every day. Many reshaping operations, like
unstacking a long table into a wide one, produce a table whose columns depend
on the *data*, not the code. No ordinary type checker can tell you whether the
column you access afterwards exists.

In [Interactive Data Analysis with Lively Typed Tables](:2026-typed-tables-oopsla),
which Alexander Bandukwala is presenting at OOPSLA 2026, we take a different
approach in [Hazel](:hazel):

- **Tables are just lists of labeled tuples**, with a few gradually typed
  operations for extending, contracting and projecting across them. When the
  schema is known statically, you get full static typing. When it isn't, the
  type falls back to unknown rather than ruling the program out.
- **Live typing** feeds the types Hazel observes while running your program back
  into the type checker. Because Hazel keeps running programs even when they
  have holes and errors, it can report that a column produced by an unstack
  doesn't exist right where you access it, before you go further.
- **Rich probes** extend Hazel's live probes with table views, so you can sort,
  filter and clean data by direct manipulation, with each action rewriting your
  code rather than mutating a value behind your back.

The paper evaluates the design against the Brown Benchmark for Table Types,
where Hazel Lab expresses every example, and in a lab study with seven
participants doing data cleaning and analysis. You can try tables in Hazel
today (see the Tables page in the documentation at
[hazel.org/build/dev](https://hazel.org/build/dev/)), and there is a
[recording of the talk](https://www.youtube.com/watch?v=aCbrzAnBEbM).
