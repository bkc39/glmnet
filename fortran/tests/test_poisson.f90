! Poisson regression == one `fishnet` call via glmnet_fishnet_solo. Synthetic
! count data with a clean log-linear trend: x1 drives the expected count upward,
! x2 is an uninformative noise predictor, and the counts increase with x1. The
! fit must learn a positive x1 coefficient, explain deviance, drive the noise
! coefficient to zero under the L1 penalty, and at a huge lambda collapse to the
! intercept-only model whose intercept is log(mean(y)) (the Poisson null fit).
program test_poisson
  use, intrinsic :: iso_c_binding
  implicit none

  interface
     subroutine glmnet_fishnet_solo(alpha, no, ni, x, y, lambda, &
          standardize, intercept, thresh, maxit, &
          intercept_out, beta_out, dev_ratio_out, lambda_out, nlp_out, jerr_out) &
          bind(C, name="glmnet_fishnet_solo")
       import :: c_double, c_int
       real(c_double), value :: alpha, lambda, thresh
       integer(c_int), value :: no, ni, standardize, intercept, maxit
       real(c_double) :: x(no, ni), y(no)
       real(c_double) :: intercept_out, beta_out(ni), dev_ratio_out, lambda_out
       integer(c_int) :: nlp_out, jerr_out
     end subroutine glmnet_fishnet_solo
  end interface

  integer(c_int), parameter :: no = 8, ni = 2
  real(c_double) :: x(no, ni), y(no)
  real(c_double) :: b0, beta(ni), devr, lam, meany
  integer(c_int) :: nlp, jerr

  x(:,1) = [1.0_c_double, 2.0_c_double, 3.0_c_double, 4.0_c_double, &
            5.0_c_double, 6.0_c_double, 7.0_c_double, 8.0_c_double]
  ! x2: noise, alternating, uncorrelated with the count trend
  x(:,2) = [2.0_c_double, 1.0_c_double, 2.0_c_double, 1.0_c_double, &
            2.0_c_double, 1.0_c_double, 2.0_c_double, 1.0_c_double]
  ! counts roughly following exp(0.3 * x1) -- increasing with x1
  y      = [1.0_c_double, 2.0_c_double, 2.0_c_double, 3.0_c_double, &
            4.0_c_double, 6.0_c_double, 8.0_c_double, 11.0_c_double]

  ! --- small lambda: positive rate coefficient, good fit ---
  call glmnet_fishnet_solo(1.0_c_double, no, ni, x, y, 0.05_c_double, &
       1_c_int, 1_c_int, 1.0e-7_c_double, 100000_c_int, &
       b0, beta, devr, lam, nlp, jerr)
  if (jerr /= 0) then
     print *, "FAIL: jerr =", jerr
     error stop 1
  end if
  if (beta(1) <= 0.0_c_double) then
     print *, "FAIL: x1 coefficient not positive:", beta(1)
     error stop 1
  end if
  if (devr <= 0.0_c_double .or. devr > 1.0_c_double) then
     print *, "FAIL: dev.ratio out of (0,1]:", devr
     error stop 1
  end if

  ! --- larger lambda: the L1 penalty zeros the noise predictor ---
  call glmnet_fishnet_solo(1.0_c_double, no, ni, x, y, 0.2_c_double, &
       1_c_int, 1_c_int, 1.0e-7_c_double, 100000_c_int, &
       b0, beta, devr, lam, nlp, jerr)
  if (beta(2) /= 0.0_c_double) then
     print *, "FAIL: lasso did not zero the noise coefficient:", beta(2)
     error stop 1
  end if

  ! --- huge lambda: collapse to the intercept-only null model log(mean(y)) ---
  call glmnet_fishnet_solo(1.0_c_double, no, ni, x, y, 1.0e6_c_double, &
       1_c_int, 1_c_int, 1.0e-7_c_double, 100000_c_int, &
       b0, beta, devr, lam, nlp, jerr)
  meany = sum(y) / real(no, c_double)
  if (abs(b0 - log(meany)) > 1.0e-3_c_double) then
     print *, "FAIL: intercept at huge lambda =", b0, "expected log(mean) =", log(meany)
     error stop 1
  end if
  if (any(beta /= 0.0_c_double)) then
     print *, "FAIL: coefficients not all zero at huge lambda:", beta
     error stop 1
  end if
  if (devr > 1.0e-3_c_double) then
     print *, "FAIL: dev.ratio not ~0 at huge lambda:", devr
     error stop 1
  end if

  print *, "OK: poisson recovers the rate effect, selects, collapses to log(mean)"
end program test_poisson
