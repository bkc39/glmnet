! Cox proportional-hazards regression == one `coxnet` call via
! glmnet_coxnet_solo. Synthetic survival data with a clean risk gradient: x1 is a
! risk factor (higher x1 => higher hazard => earlier event), times are distinct
! (no ties), x2 is an uninformative noise predictor, and every subject has an
! observed event. The fit must learn a positive risk coefficient beta1, explain
! deviance, drive the noise coefficient to zero under the L1 penalty, and collapse
! to the null model at a huge lambda. (Cox has no intercept.)
program test_cox
  use, intrinsic :: iso_c_binding
  implicit none

  interface
     subroutine glmnet_coxnet_solo(alpha, no, ni, x, time, status, lambda, &
          standardize, thresh, maxit, &
          beta_out, dev_ratio_out, lambda_out, nlp_out, jerr_out) &
          bind(C, name="glmnet_coxnet_solo")
       import :: c_double, c_int
       real(c_double), value :: alpha, lambda, thresh
       integer(c_int), value :: no, ni, standardize, maxit
       real(c_double) :: x(no, ni), time(no), status(no)
       real(c_double) :: beta_out(ni), dev_ratio_out, lambda_out
       integer(c_int) :: nlp_out, jerr_out
     end subroutine glmnet_coxnet_solo
  end interface

  integer(c_int), parameter :: no = 12, ni = 2
  real(c_double) :: x(no, ni), time(no), status(no)
  real(c_double) :: beta(ni), devr, lam
  integer(c_int) :: nlp, jerr, i

  do i = 1, no
     x(i,1) = real(i, c_double)        ! x1: risk factor, 1..12
     time(i) = 13.0_c_double - real(i, c_double)  ! time = 13 - x1 (high x1 dies first)
  end do
  ! x2: noise, alternating, uncorrelated with the risk ordering
  x(:,2) = [2.0_c_double, 1.0_c_double, 2.0_c_double, 1.0_c_double, &
            2.0_c_double, 1.0_c_double, 2.0_c_double, 1.0_c_double, &
            2.0_c_double, 1.0_c_double, 2.0_c_double, 1.0_c_double]
  status = 1.0_c_double                ! all events observed

  ! --- small lambda: positive risk coefficient, good fit ---
  call glmnet_coxnet_solo(1.0_c_double, no, ni, x, time, status, 0.02_c_double, &
       1_c_int, 1.0e-7_c_double, 100000_c_int, &
       beta, devr, lam, nlp, jerr)
  if (jerr /= 0) then
     print *, "FAIL: jerr =", jerr
     error stop 1
  end if
  if (beta(1) <= 0.0_c_double) then
     print *, "FAIL: risk coefficient not positive:", beta(1)
     error stop 1
  end if
  if (devr <= 0.0_c_double .or. devr > 1.0_c_double) then
     print *, "FAIL: dev.ratio out of (0,1]:", devr
     error stop 1
  end if

  ! --- larger lambda: the L1 penalty zeros the noise predictor ---
  call glmnet_coxnet_solo(1.0_c_double, no, ni, x, time, status, 0.1_c_double, &
       1_c_int, 1.0e-7_c_double, 100000_c_int, &
       beta, devr, lam, nlp, jerr)
  if (beta(2) /= 0.0_c_double) then
     print *, "FAIL: lasso did not zero the noise coefficient:", beta(2)
     error stop 1
  end if

  ! --- huge lambda: collapse to the null model ---
  call glmnet_coxnet_solo(1.0_c_double, no, ni, x, time, status, 1.0e6_c_double, &
       1_c_int, 1.0e-7_c_double, 100000_c_int, &
       beta, devr, lam, nlp, jerr)
  if (any(beta /= 0.0_c_double)) then
     print *, "FAIL: coefficients not all zero at huge lambda:", beta
     error stop 1
  end if
  if (devr > 1.0e-3_c_double) then
     print *, "FAIL: dev.ratio not ~0 at huge lambda:", devr
     error stop 1
  end if

  print *, "OK: cox recovers the risk direction, selects, and collapses"
end program test_cox
