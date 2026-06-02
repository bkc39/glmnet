! glmnet_capi.f90 -- C-ABI shim over the vendored glmnet Fortran (vendor/glmnet5.f90).
!
! Every entry point here is `bind(C, name=...)` so it exports a clean, unmangled
! C symbol (no trailing underscore, no module prefix) that Racket's FFI binds to
! directly via `define-ffi-definer ... convention:hyphen->underscore`.
!
! This is modern FREE-FORM Fortran. The vendored glmnet5.f90 is FIXED-FORM; the
! build (../CMakeLists.txt) sets the format per source file.
!
! PRECISION CONTRACT: glmnet5.f90 declares its arrays as single `real`, but the
! whole project is compiled with -fdefault-real-8 so `real` is 8 bytes and the
! elnet ABI is effectively double precision. `glmnet_default_real_bytes` below
! lets callers assert that flag is in force. Keep all `real(c_double)` here.

module glmnet_capi
  use, intrinsic :: iso_c_binding
  implicit none
  private
  public :: glmnet_capi_abi_version, glmnet_hello, glmnet_default_real_bytes

contains

  ! ABI version of this shim. Bump on any breaking change to a C entry point.
  integer(c_int) function glmnet_capi_abi_version() &
       bind(C, name="glmnet_capi_abi_version")
    glmnet_capi_abi_version = 1_c_int
  end function glmnet_capi_abi_version

  ! Hello-world numeric round-trip: proves clean by-value double marshalling
  ! across the FFI boundary. Returns a + b.
  real(c_double) function glmnet_hello(a, b) bind(C, name="glmnet_hello")
    real(c_double), value, intent(in) :: a
    real(c_double), value, intent(in) :: b
    glmnet_hello = a + b
  end function glmnet_hello

  ! Size in bytes of the Fortran default `real`. MUST be 8 -- i.e. the library
  ! was compiled with -fdefault-real-8 -- or the elnet ABI does not match the
  ! double-precision Racket bindings and every numeric result is garbage.
  integer(c_int) function glmnet_default_real_bytes() &
       bind(C, name="glmnet_default_real_bytes")
    real :: probe
    glmnet_default_real_bytes = int(storage_size(probe) / 8, c_int)
  end function glmnet_default_real_bytes

end module glmnet_capi
