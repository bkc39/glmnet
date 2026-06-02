#lang scribble/manual

@(require (for-label racket/base
                     glmnet))

@title[#:tag "reference"]{API reference}

@declare-exporting[glmnet]

The reference documents every public procedure. It grows as each model lands;
model-fitting procedures join the connectivity surface below.

@section[#:tag "ref-connectivity"]{Connectivity and self-checks}

These entry points call directly into the C-ABI shim. They are used by
@secref["ex-hello"] to verify the binding is wired correctly.

@defproc[(glmnet-hello [a real?] [b real?]) real?]{
  Returns @racket[(+ a b)], computed in Fortran. A by-value @tt{double}
  round-trip across the C ABI --- a smoke test that the native library loads and
  marshals correctly.
}

@defproc[(glmnet-default-real-bytes) exact-positive-integer?]{
  The byte width of the Fortran default @tt{real} in the loaded native library.
  This is @racket[8] when the library was compiled with @tt{-fdefault-real-8},
  which the numeric API requires. The package raises an error at load time if it
  is not @racket[8].
}

@defproc[(glmnet-capi-abi-version) exact-positive-integer?]{
  The ABI version of the C-ABI shim. Bumped on any breaking change to a C entry
  point.
}
