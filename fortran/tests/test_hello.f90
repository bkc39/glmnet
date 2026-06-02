! Phase 0 toolchain proof: links against libglmnetcompat and asserts the
! C-ABI shim works AND that -fdefault-real-8 is in force. ctest runs this.
program test_hello
  use, intrinsic :: iso_c_binding
  implicit none

  interface
     integer(c_int) function glmnet_default_real_bytes() &
          bind(C, name="glmnet_default_real_bytes")
       import :: c_int
     end function glmnet_default_real_bytes

     real(c_double) function glmnet_hello(a, b) bind(C, name="glmnet_hello")
       import :: c_double
       real(c_double), value :: a
       real(c_double), value :: b
     end function glmnet_hello

     integer(c_int) function glmnet_capi_abi_version() &
          bind(C, name="glmnet_capi_abi_version")
       import :: c_int
     end function glmnet_capi_abi_version
  end interface

  integer(c_int)  :: nbytes, abi
  real(c_double)  :: s

  ! The single most important invariant in the whole project.
  nbytes = glmnet_default_real_bytes()
  if (nbytes /= 8) then
     print *, "FAIL: default real is", nbytes, "bytes; expected 8 ", &
              "(library not compiled with -fdefault-real-8)"
     error stop 1
  end if

  s = glmnet_hello(2.5_c_double, 4.0_c_double)
  if (abs(s - 6.5_c_double) > 1.0e-12_c_double) then
     print *, "FAIL: glmnet_hello(2.5, 4.0) =", s, "; expected 6.5"
     error stop 1
  end if

  abi = glmnet_capi_abi_version()
  if (abi /= 1) then
     print *, "FAIL: glmnet_capi_abi_version =", abi, "; expected 1"
     error stop 1
  end if

  print *, "OK: hello fortran | default real bytes =", nbytes, &
           "| hello(2.5,4.0) =", s, "| abi =", abi
end program test_hello
