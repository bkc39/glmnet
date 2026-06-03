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

  abi = glmnet_capi_abi_version()
  if (abi /= 1) then
     print *, "FAIL: glmnet_capi_abi_version =", abi, "; expected 1"
     error stop 1
  end if

  print *, "OK: default real bytes =", nbytes, "| abi =", abi
end program test_precision
