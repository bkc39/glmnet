#lang scribble/manual

@(require scribble/lp-include
          (for-label racket/base
                     glmnet))

@title[#:tag "examples" #:style 'toc]{Examples}

Each example below is a @deftech{literate program}: the prose and the code you
see are the @emph{same source} that lives in the package's
@filepath{glmnet/examples/} directory and is exercised by the test suite under
@filepath{glmnet/examples/test/}. Every example provides a @racket[run-example]
thunk, so you can run any of them directly or read them as documentation that is
guaranteed to stay in sync with the code.

@local-table-of-contents[]

@lp-include["../examples/00-hello-fortran.rkt"]
