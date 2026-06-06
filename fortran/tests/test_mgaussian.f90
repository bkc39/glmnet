! Multi-response Gaussian ("mgaussian") == one `multelnet` call via
! glmnet_mgaussian_solo. Two noise-free responses that both depend on x1 with
! opposite signs (y1 = 1 + 2*x1, y2 = 10 - x1) plus an uninformative predictor x2.
! The grouped lasso shares one active set across responses, so it must recover a
! positive x1 coefficient for y1, a negative one for y2, and -- because x2 carries
! no signal for either response -- drive the whole x2 row to zero. At a huge
! lambda the model collapses to the per-response means (8 and 6.5).
program test_mgaussian
  use, intrinsic :: iso_c_binding
  implicit none

  interface
     subroutine glmnet_mgaussian_solo(alpha, no, ni, nr, x, y, lambda, &
          standardize, intercept, thresh, maxit, &
          intercept_out, beta_out, rsq_out, lambda_out, nlp_out, jerr_out) &
          bind(C, name="glmnet_mgaussian_solo")
       import :: c_double, c_int
       real(c_double), value :: alpha, lambda, thresh
       integer(c_int), value :: no, ni, nr, standardize, intercept, maxit
       real(c_double) :: x(no, ni), y(no, nr)
       real(c_double) :: intercept_out(nr), beta_out(ni*nr), rsq_out, lambda_out
       integer(c_int) :: nlp_out, jerr_out
     end subroutine glmnet_mgaussian_solo
  end interface

  integer(c_int), parameter :: no = 6, ni = 2, nr = 2
  real(c_double) :: x(no, ni), y(no, nr)
  real(c_double) :: a0(nr), beta(ni*nr), rsq, lam
  integer(c_int) :: nlp, jerr

  x(:,1) = [1.0_c_double, 2.0_c_double, 3.0_c_double, 4.0_c_double, 5.0_c_double, 6.0_c_double]
  x(:,2) = [2.0_c_double, 1.0_c_double, 2.0_c_double, 1.0_c_double, 2.0_c_double, 1.0_c_double]  ! noise
  y(:,1) = [3.0_c_double, 5.0_c_double, 7.0_c_double, 9.0_c_double, 11.0_c_double, 13.0_c_double] ! 1 + 2*x1
  y(:,2) = [9.0_c_double, 8.0_c_double, 7.0_c_double, 6.0_c_double, 5.0_c_double, 4.0_c_double]   ! 10 - x1

  ! --- small lambda: per-response signs, grouped noise removal ---
  call glmnet_mgaussian_solo(1.0_c_double, no, ni, nr, x, y, 0.1_c_double, &
       1_c_int, 1_c_int, 1.0e-7_c_double, 100000_c_int, &
       a0, beta, rsq, lam, nlp, jerr)
  if (jerr /= 0) then
     print *, "FAIL: jerr =", jerr
     error stop 1
  end if
  if (beta(1) <= 0.0_c_double) then     ! x1 -> y1
     print *, "FAIL: x1->y1 coefficient not positive:", beta(1)
     error stop 1
  end if
  if (beta(3) >= 0.0_c_double) then     ! x1 -> y2
     print *, "FAIL: x1->y2 coefficient not negative:", beta(3)
     error stop 1
  end if
  if (beta(2) /= 0.0_c_double .or. beta(4) /= 0.0_c_double) then  ! x2 row
     print *, "FAIL: grouped lasso did not zero the noise row:", beta(2), beta(4)
     error stop 1
  end if
  if (rsq <= 0.0_c_double .or. rsq > 1.0_c_double) then
     print *, "FAIL: rsq out of (0,1]:", rsq
     error stop 1
  end if

  ! --- huge lambda: collapse to the per-response means (8 and 6.5) ---
  call glmnet_mgaussian_solo(1.0_c_double, no, ni, nr, x, y, 1.0e6_c_double, &
       1_c_int, 1_c_int, 1.0e-7_c_double, 100000_c_int, &
       a0, beta, rsq, lam, nlp, jerr)
  if (any(beta /= 0.0_c_double)) then
     print *, "FAIL: coefficients not all zero at huge lambda:", beta
     error stop 1
  end if
  if (abs(a0(1) - 8.0_c_double) > 1.0e-3_c_double) then
     print *, "FAIL: intercept(1) =", a0(1), "expected mean(y1) = 8"
     error stop 1
  end if
  if (abs(a0(2) - 6.5_c_double) > 1.0e-3_c_double) then
     print *, "FAIL: intercept(2) =", a0(2), "expected mean(y2) = 6.5"
     error stop 1
  end if

  print *, "OK: mgaussian recovers per-response signs, grouped selection, means"
end program test_mgaussian
