#lang scribble/lp2

@(require (for-label racket/base
                     glmnet))

@section[#:tag "ex-hello"]{Hello, Fortran}

This first example is not statistics at all --- it is a toolchain proof. It
exercises the whole spine of the binding: a clean @hyperlink["https://en.wikipedia.org/wiki/Application_binary_interface"]{C ABI}
exported from Fortran (via @tt{iso_c_binding}), loaded through Racket's FFI, with
the shared object resolved from the package's @filepath{native-libs/} directory.
If this runs, every layer below the numeric API is wired correctly.

Three entry points are called. @racket[glmnet-hello] adds two doubles in
Fortran and returns the sum --- a by-value @tt{double} round-trip across the
boundary. @racket[glmnet-default-real-bytes] reports the byte width of the
Fortran default @tt{real}; it @bold{must} be @racket[8], because the vendored
glmnet source declares single-precision arrays that we promote to double with
@tt{-fdefault-real-8}. Get that flag wrong and every later model would return
silent garbage, so we assert it up front. @racket[glmnet-capi-abi-version]
returns the shim's ABI version.

@chunk[<require>
(require glmnet)]

@chunk[<provide>
(provide run-example)]

The example returns the three observed values so the companion test can check
them.

@chunk[<run-example>
(define (run-example)
  (define sum        (glmnet-hello 2.5 4.0))
  (define real-bytes (glmnet-default-real-bytes))
  (define abi        (glmnet-capi-abi-version))
  (list sum real-bytes abi))]

@chunk[<*>
  <require>
  <provide>
  <run-example>]
