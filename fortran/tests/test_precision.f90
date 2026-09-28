! Toolchain + precision proof: links against libglmnetcompat and asserts the
! library was built with -fdefault-real-8 (the single most important invariant in
! the project) and exports the expected C-ABI version. ctest runs this.
program test_precision
  use, intrinsic :: iso_c_binding
  implicit none

  interface
     integer(c_int) function glmnet_default_real_bytes() &
          bind(C, name="glmnet_default_real_bytes")
       import :: c_int
     end function glmnet_default_real_bytes

     integer(c_int) function glmnet_capi_abi_version() &
          bind(C, name="glmnet_capi_abi_version")
       import :: c_int
     end function glmnet_capi_abi_version
  end interface

  integer(c_int) :: nbytes, abi

  ! The single most important invariant in the whole project.
  nbytes = glmnet_default_real_bytes()
  if (nbytes /= 8) then
     print *, "FAIL: default real is", nbytes, "bytes; expected 8 ", &
              "(library not compiled with -fdefault-real-8)"
     error stop 1
  end if

  ! R's glmnet5dpclean.f is DOUBLE PRECISION throughout. -fdefault-real-8 alone
  ! would widen that to 16 bytes; -fdefault-double-8 keeps it at 8. This program
  ! is compiled with the library's flags, so its own kind is the library's.
  if (storage_size(1.0d0) /= 64) then
     print *, "FAIL: double precision is", storage_size(1.0d0) / 8, "bytes; ", &
              "expected 8 (build needs -fdefault-double-8 beside -fdefault-real-8)"
     error stop 1
  end if

  abi = glmnet_capi_abi_version()
  if (abi /= 3) then
     print *, "FAIL: glmnet_capi_abi_version =", abi, "; expected 3"
     error stop 1
  end if

  print *, "OK: default real bytes =", nbytes, "| abi =", abi
end program test_precision
