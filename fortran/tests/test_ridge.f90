! Ridge == elastic net at alpha = 0. Fixture: y = 1 + 2*x1 - x2 with an
! irrelevant third predictor x3 = x1^2 (mean(y) = 4.5). Ridge shrinks every
! coefficient smoothly: at a huge lambda all coefficients vanish and the
! intercept collapses to mean(y); at a small lambda the coefficients are all
! nonzero and shrunk relative to OLS.
program test_ridge
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

  integer(c_int), parameter :: no = 6, ni = 3
  real(c_double) :: x(no, ni), y(no)
  real(c_double) :: b0, beta(ni), rsq, lam
  integer(c_int) :: nlp, jerr, j

  x(:,1) = [1.0_c_double, 2.0_c_double, 3.0_c_double, 4.0_c_double, 5.0_c_double, 6.0_c_double]
  x(:,2) = [2.0_c_double, 1.0_c_double, 4.0_c_double, 3.0_c_double, 6.0_c_double, 5.0_c_double]
  x(:,3) = [1.0_c_double, 4.0_c_double, 9.0_c_double, 16.0_c_double, 25.0_c_double, 36.0_c_double]
  y      = [1.0_c_double, 4.0_c_double, 3.0_c_double, 6.0_c_double, 5.0_c_double, 8.0_c_double]

  ! small lambda: coefficients shrunk but all nonzero
  call glmnet_elnet_solo(0.0_c_double, no, ni, x, y, 0.1_c_double, &
       1_c_int, 1_c_int, 1.0e-7_c_double, 100000_c_int, &
       b0, beta, rsq, lam, nlp, jerr)
  if (jerr /= 0) then
     print *, "FAIL: jerr =", jerr
     error stop 1
  end if
  do j = 1, ni
     if (abs(beta(j)) <= 0.0_c_double) then
        print *, "FAIL: ridge zeroed coefficient", j, "=", beta(j)
        error stop 1
     end if
  end do
  if (abs(beta(1)) >= 2.0_c_double) then
     print *, "FAIL: beta(1) not shrunk vs OLS:", beta(1)
     error stop 1
  end if

  ! huge lambda: all coefficients -> 0, intercept -> mean(y) = 4.5
  call glmnet_elnet_solo(0.0_c_double, no, ni, x, y, 1.0e6_c_double, &
       1_c_int, 1_c_int, 1.0e-7_c_double, 100000_c_int, &
       b0, beta, rsq, lam, nlp, jerr)
  if (abs(b0 - 4.5_c_double) > 1.0e-2_c_double) then
     print *, "FAIL: intercept at huge lambda =", b0, "expected 4.5"
     error stop 1
  end if
  do j = 1, ni
     if (abs(beta(j)) > 1.0e-2_c_double) then
        print *, "FAIL: coefficient", j, "not shrunk to ~0:", beta(j)
        error stop 1
     end if
  end do

  print *, "OK: ridge shrinks smoothly; intercept -> mean(y) at huge lambda"
end program test_ridge
