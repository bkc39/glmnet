! OLS == elastic net at lambda = 0. Noise-free fixture y = 1 + 2*x1 - x2, so the
! fit must recover the intercept and coefficients exactly with R^2 = 1.
program test_ols
  use, intrinsic :: iso_c_binding
  implicit none

  interface
     subroutine glmnet_elnet_solo(alpha, no, ni, x, y, lambda, &
          standardize, intercept, thresh, maxit, &
          intercept_out, beta_out, rsq_out, lambda_out, nlp_out, jerr_out) &
          bind(C, name="glmnet_elnet_solo")
       import :: c_double, c_int
       real(c_double), value :: alpha, lambda, thresh
       integer(c_int), value :: no, ni, standardize, intercept, maxit
       real(c_double) :: x(no, ni), y(no)
       real(c_double) :: intercept_out, beta_out(ni), rsq_out, lambda_out
       integer(c_int) :: nlp_out, jerr_out
     end subroutine glmnet_elnet_solo
  end interface

  integer(c_int), parameter :: no = 5, ni = 2
  real(c_double) :: x(no, ni), y(no)
  real(c_double) :: b0, beta(ni), rsq, lam
  integer(c_int) :: nlp, jerr
  real(c_double), parameter :: tol = 1.0e-3_c_double

  ! column 1 = x1, column 2 = x2 (column-major, as the C ABI expects)
  x(:,1) = [1.0_c_double, 2.0_c_double, 3.0_c_double, 4.0_c_double, 5.0_c_double]
  x(:,2) = [2.0_c_double, 1.0_c_double, 4.0_c_double, 3.0_c_double, 6.0_c_double]
  y      = [1.0_c_double, 4.0_c_double, 3.0_c_double, 6.0_c_double, 5.0_c_double]

  ! alpha is irrelevant at lambda = 0; standardize off for an exact comparison.
  call glmnet_elnet_solo(1.0_c_double, no, ni, x, y, 0.0_c_double, &
       0_c_int, 1_c_int, 1.0e-7_c_double, 100000_c_int, &
       b0, beta, rsq, lam, nlp, jerr)

  if (jerr /= 0) then
     print *, "FAIL: jerr =", jerr
     error stop 1
  end if
  if (abs(b0 - 1.0_c_double) > tol) then
     print *, "FAIL: intercept =", b0, "expected 1.0"
     error stop 1
  end if
  if (abs(beta(1) - 2.0_c_double) > tol) then
     print *, "FAIL: beta(1) =", beta(1), "expected 2.0"
     error stop 1
  end if
  if (abs(beta(2) + 1.0_c_double) > tol) then
     print *, "FAIL: beta(2) =", beta(2), "expected -1.0"
     error stop 1
  end if
  if (abs(rsq - 1.0_c_double) > 1.0e-4_c_double) then
     print *, "FAIL: rsq =", rsq, "expected 1.0"
     error stop 1
  end if

  print *, "OK: OLS recovered intercept =", b0, " beta =", beta, " rsq =", rsq
end program test_ols
