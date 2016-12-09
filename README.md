[![Build Status](https://travis-ci.org/disco-lang/disco.svg?branch=master)](https://travis-ci.org/disco-lang/disco)

Prototype implementation of a small functional teaching language
for use in a discrete mathematics course.

Design principles:

* Includes those features, and *only* those features, useful in the
  context of a discrete math course. This is *not* intended to be a
  general-purpose language.
* Syntax is as close to standard *mathematical* practice as possible.
* Tooling, error messages, etc. are very important---the language
  needs to be accessible to undergrads with no prior programming
  experience.

Feel free to look around, ask questions, etc.  You can even contribute
some code if you like---collaborators are most welcome.  However, note
that no guarantees are made about anything in particular at the
moment.

Building
--------

First, make sure you have
[the `stack` tool](https://docs.haskellstack.org/en/stable/README/).
Then at a command prompt, execute

```
stack build
```

After this completes, you should be able to

```
stack exec disco
```

to run the Disco command-line REPL.

While developing, you may want to use a command like

```
stack build --fast --file-watch --ghc-options='-Wall'
```

which will turn on warnings, turn off optimizations for a faster
edit-compile-test cycle, and automatically recompile every time a
source file changes.
