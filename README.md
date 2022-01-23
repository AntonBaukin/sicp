## About SICP and the Tasks

Abbreviation SICP means «Structure and Interpretation of Computer Programs». 
This book is old enough, it takes us back to mid 80's, the golden era of 
programming and thinking. Folks used to apply their minds to overcome 
the limitations of the hardware.

Procedural language C plays the major role at that time. SICP teaches Lisp,
the father of functional languages. So, why to learn Lisp? (Not instead of C,
but in parallel.) Modern comeback of functional programming, the new wave
of languages, like Elixir, Erlang, Rust, Scala and Kotlin, hints us that
to become a true master of the Force we may (or event must) start from
the beginning: Lisp and Haskell.

SICP is not a book of text, but a book of tasks. It consists of five chapters,
each having about eighty tasks. Complexity of the tasks raises up to the
end of Chapter 4. When I've started to read the book I thought that solving
them may take a year, but I was wrong. I can't imagine how MIT students were 
able to make them all as a complete programs.

On the Web you may find various implementations of SICP exercises, mostly looking 
the same: as pure texts. You may try to run them, and you be lucky to be
able to. They are academic, just sketches. This way was definitely not mine.


## Scheme Lisp & Gambit

First of all I had to choose the implementation of Lisp. Lisp has tens 
of dialects, and it was risky to take something else than Scheme designed
in MIT and used in the book. Scheme is minimalistic, and Gambit is even more.

Gambit project: [gambitscheme.org]() — is modern implementation of Scheme 
dialect of Lisp aiming fast parallel computations. It's able to compile 
to C and various other platforms.

**Warning:** all the tasks were tested with dynamic interpretation by `gsi` — 
not the compilation. Gambit applies some restrictions on programs able
to compile. As this feature is not required for SICP tasks, it was not 
targeted and definitely will not work!

Gambit is available for various operating systems. I've installed it on Mac
with MacPorts: [https://ports.macports.org/port/gambit-c]().


## Code layout

The code of the tasks is split by the chapter sections, and is located under
`sicp/scheme` in folders like: `3.3.3, 4.3.1, 4.4.4.`

To run a task you need to run console terminal, change directory to the 
folder of interest, and run `gsi` program passing it the task file: 

```gsi 4.4.4-79.scm```

Task files from section 3.3.1 are named as:

```<full section code>-<chapter task index>.scm```

A task may have subtasks labeled like a), b), etc. In this case, when
the code difference between the implementations is spacious, I had to
split it to multiple files named as:

```<full section code>-<chapter task index>-<subtask letter>.scm```


## Shared Utilities

Tasks from Chapter 2 are simple. Folder `2.3.3` bursts with files: here
tree and table utilities are located. These files are widely used within
the following tasks. In some cases even the preceeding tasks do use them
if they were refactored.

Folder `3.3.2` has the assertions used for tests. File `assert.scm` is
the most of the included across the project. It's also the place for queues and
the iteration.

Folder `3.3.2` is prominent as it defines the interfaces for trees, sets,
and implements quick sort and HOF curry.

Then comes `3.3.3` with interface for tables and full implementation of
red-black tree that is used as a dictionary in every following place.

Lisp delayed streams are not available in Gambit, and are implemented from
scratch in folder `3.5.1.` The streams are used further for the queries
evaluator, QEval.


## Logging & Tests

Some tasks are simple enough to print something to console. There is
no shared logging utility, and each file that requires it just places
it at the top: 
```
(define (log . args) (for-each display args) (newline))
```

Logging is also used for the debugging, and there may be some traces
being commented out. For the purpose of task demonstration some
traces of the tasks (not the utilities) are also printed.

Evaluators of Chapter 4 also define special forms like `debug` — to
log inside evaluated programs or to do special tricks, the logging looks so:
```
(debug log "Test «x» = " x)
(debug log-env "—— Env after defining of xyz = 'XYZ ——")
```

Utilities and complex tasks, such as the evaluators, are covered with 
unit tests. Test files commonly have `-test` ending.

Some tests are themself complex. Test file `3.3.3/tree-red-black-test.scm`
has massive random tests for red-black trees. It even prints failed
tree samples with tree print utility from `2.3.3.`

Tests may also log something to console. Some of them are not fully automatic:
namely, tree printing itself is checked manually. 


## Interfaces & IIFs

Lisp interfaces may be of two types. First, a dispatching function that takes
the symbolic name of the method and the following arguments. SICP defines them
and uses in various tasks. Second, is a collection of functions with distinctive
prefix that take a list and return a function bound with it.

Utilities of the project are defined in the second manner. As a sample, 
`2.3.3/tree-interface.scm` has such a function as `(tree-op-get tree-ops).` 
In this case list `tree-ops` is obtained via a tree maker, for red-black
one it's this call: `(make-rb-tree smaller?)` from `3.3.3/tree-red-black.scm.`

Constructor `make-rb-tree` creates a list of utilities that use the given
comparator of the tree values. Each method of the interface takes a node
of the tree instance. A method is bound to the comparator, but not the tree.

On contrary, constructor `(make-index-tree)` from file `2.5.3/index-tree.scm`
creates tree instance, and interface operations as `(index-tree-get itree)`
return functions bound to the instance:
```
(define sample (make-index-tree))
(define get (index-tree-get sample))

(define (log-get-range from to)
 (if (> from to) (newline)
  (begin
   (logs from " := " (get from) " ")
   (log-get-range (+ from 1) to)
  )
 )
)
```

IIFs (immediately invoked functions) are used across the later programs
to incapsulate local variables or to bypass Lisp limitation that local 
definitions must preceed the function operations that are crucial when
building application from multiple modules:
```
(define thunk-aps-off
 (
  (lambda () ;<— immediately invoked function
   (define (not-thunk-aps aps env) aps)
   (set! thunk-aps not-thunk-aps)
   not-thunk-aps ;<— resulting function
  )
 )
)
```

## Modules with Extension Points

SICP tasks have a nasty feature: the same goal is evolved over several tasks,
where each task makes it's own additions, then bangs a next task that asks
to re-implement the work of the previous ten. How to do so and also save
a slim structure without copy-pastes? This was one of the challenges
of the modular design for this project.

Let's look at the evaluator from section `4.1.1`:
```
; 4.1.1/eval.scm:
(define (make-eval-impl global-env includes eval-body-expr-quoted)
 ...
)

; 4.1.1/eval-basic-routine.scm:
(define basic-evaluator
 (make-eval
  basic-evaluator-env
  eval-basic-includes
  (eval-in-nested-env eval-impl exp env)
 )
)
```

Global instance of evaluator is created with tree arguments, where
`eval-basic-includes` has the following definition:
```
; 4.1.1/eval-basic-includes.scm:
(define-value-if-not 'eval-basic-includes
 (list
  "../4.1.1/eval-impl-env.scm"
  "../4.1.1/eval-impl-defs.scm"
  "../4.1.1/eval-impl-apply.scm"
  "../4.1.1/eval-impl-basic.scm"
  "../4.1.1/eval-impl-debug.scm"
  "../4.1.1/eval-impl-primes.scm"
  "../4.1.1/eval-impl-set.scm"
 )
)
```

Each of the files listed contains a part of the evaluator implementation.
You may replace any part with else file preserving the same extension points
being the «public» functions inside that file. 

The thick of this module layout is that Scheme may not include files dynamically:
you may not call `(include some-file-name-var)` in a loop. But when this limitation
is over, the following design is easy to gain.

The next variant of the evaluator with the dispatcher has only two files replaced
and one added:
```
; 4.1.2/eval-disp-includes.scm:
(define-value-if-not 'eval-basic-includes
 (list
  "../4.1.1/eval-impl-env.scm"
  "../4.1.1/eval-impl-defs.scm"
  "../4.1.1/eval-impl-apply.scm"
  "../4.1.2/eval-impl-disp.scm"   ; Replaced
  "../4.1.1/eval-impl-debug.scm"
  "../4.1.1/eval-impl-primes.scm"
  "../4.1.2/eval-impl-forms.scm"  ; Added
  "../4.1.2/eval-impl-set.scm"    ; Replaced
 )
)
```


## Gambit Macroses

Gambit Scheme dialect supports macroses. This sample is from file 
`4.1.1/eval-basic-routine.scm`:
```
(define-macro (eval-basic . script)
 `(basic-evaluator '(,@script))
)
```

Macroses are lightweight versions of special forms. Without them
writing streams in a native manner is not possible:
```
(define-macro (cons-stream item delayed-expr)
 `(cons ,item (delay ,delayed-expr))
)
```

Delayed expression may not be evaluated as a function argument, at
the call time. Special form `delay` is available in Gambit, but the
expression must be passed to it as-is. In Gambit only a macros 
can do this.

Macroses are handy for the evaluators as you may give the symbols
of a program without escaping them:
```
(assert-eq? 5
 (eval-basic
  (define sum (lambda (a b) (+ a b))) ; Start of inlined script,
  (sum 2 3)                           ; resulting expression.
 )
)
```

Regretfully, this special feature of Gambit makes my programs incompatible
over various dialects of Lisp and even, Scheme. Knowing this, I've limited
the usage of macroses, but some files still need to be rewritten for the
target Lisp dialect.


## Chapter 1

Tasks from Chapter 1 are relatively simple and standalone. They are checked
with console logging. The initial commit was on August, 2018.

Note the naming of the early files, like `1.1.7-1.7.scm.` In SICP tasks
are numbered with the chapter prefix. I've preserved this naming, but
then swapped to flat indexing.
