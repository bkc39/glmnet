! Two-class logistic (binomial family) == one `lognet` call via
! glmnet_lognet_solo. Separable synthetic data: class 1 has high x1 / low x2,
! class 0 the reverse, plus an uninformative noise predictor x3 whose values are
! identically distributed in both classes. The fit must learn beta1 > 0,
! beta2 < 0, classify every training point correctly, and -- with the L1 lasso
! penalty -- drive the noise coefficient exactly to zero. At a huge lambda the
! penalized model collapses to the balanced-class null model: all coefficients
! zero, intercept = logit(0.5) = 0, dev.ratio = 0.
program test_binomial
  use, intrinsic :: iso_c_binding
  implicit none

  interface
     subroutine glmnet_lognet_solo(alpha, no, ni, x, y, lambda, &
          standardize, intercept, thresh, maxit, &
          intercept_out, beta_out, dev_ratio_out, lambda_out, nlp_out, jerr_out) &
          bind(C, name="glmnet_lognet_solo")
       import :: c_double, c_int
       real(c_double), value :: alpha, lambda, thresh
       integer(c_int), value :: no, ni, standardize, intercept, maxit
       real(c_double) :: x(no, ni), y(no)
       real(c_double) :: intercept_out, beta_out(ni), dev_ratio_out, lambda_out
       integer(c_int) :: nlp_out, jerr_out
     end subroutine glmnet_lognet_solo
  end interface

  integer(c_int), parameter :: no = 10, ni = 3
  real(c_double) :: x(no, ni), y(no)
  real(c_double) :: b0, beta(ni), devr, lam, eta
  integer(c_int) :: nlp, jerr, i, pred, ncorrect

  ! x1: informative, increases with class 1
  x(:,1) = [1.0_c_double, 2.0_c_double, 1.0_c_double, 2.0_c_double, 3.0_c_double, &
            5.0_c_double, 4.0_c_double, 5.0_c_double, 4.0_c_double, 3.0_c_double]
  ! x2: informative, decreases with class 1
  x(:,2) = [5.0_c_double, 4.0_c_double, 4.0_c_double, 5.0_c_double, 5.0_c_double, &
            1.0_c_double, 2.0_c_double, 2.0_c_double, 1.0_c_double, 1.0_c_double]
  ! x3: noise, same {1,2,1,2,1} pattern within each class -> uncorrelated with y
  x(:,3) = [1.0_c_double, 2.0_c_double, 1.0_c_double, 2.0_c_double, 1.0_c_double, &
            1.0_c_double, 2.0_c_double, 1.0_c_double, 2.0_c_double, 1.0_c_double]
  y      = [0.0_c_double, 0.0_c_double, 0.0_c_double, 0.0_c_double, 0.0_c_double, &
            1.0_c_double, 1.0_c_double, 1.0_c_double, 1.0_c_double, 1.0_c_double]

  ! --- small lambda: good fit, correct signs, 100% training accuracy ---
  call glmnet_lognet_solo(1.0_c_double, no, ni, x, y, 0.02_c_double, &
       1_c_int, 1_c_int, 1.0e-7_c_double, 100000_c_int, &
       b0, beta, devr, lam, nlp, jerr)
  if (jerr /= 0) then
     print *, "FAIL: jerr =", jerr
     error stop 1
  end if
  if (beta(1) <= 0.0_c_double .or. beta(2) >= 0.0_c_double) then
     print *, "FAIL: informative coefficients wrong sign:", beta(1), beta(2)
     error stop 1
  end if
  if (devr <= 0.0_c_double .or. devr > 1.0_c_double) then
     print *, "FAIL: dev.ratio out of (0,1]:", devr
     error stop 1
  end if
  ncorrect = 0
  do i = 1, no
     eta = b0 + x(i,1)*beta(1) + x(i,2)*beta(2) + x(i,3)*beta(3)
     if (eta > 0.0_c_double) then
        pred = 1
     else
        pred = 0
     end if
     if (pred == nint(y(i))) ncorrect = ncorrect + 1
  end do
  if (ncorrect /= no) then
     print *, "FAIL: training accuracy", ncorrect, "/", no
     error stop 1
  end if

  ! --- the L1 penalty drives the uninformative coefficient exactly to zero ---
  call glmnet_lognet_solo(1.0_c_double, no, ni, x, y, 0.05_c_double, &
       1_c_int, 1_c_int, 1.0e-7_c_double, 100000_c_int, &
       b0, beta, devr, lam, nlp, jerr)
  if (beta(3) /= 0.0_c_double) then
     print *, "FAIL: lasso did not zero the noise coefficient:", beta(3)
     error stop 1
  end if

  ! --- huge lambda: collapse to the balanced null model ---
  call glmnet_lognet_solo(1.0_c_double, no, ni, x, y, 1.0e6_c_double, &
       1_c_int, 1_c_int, 1.0e-7_c_double, 100000_c_int, &
       b0, beta, devr, lam, nlp, jerr)
  if (abs(b0) > 1.0e-2_c_double) then
     print *, "FAIL: intercept at huge lambda =", b0, "expected ~0 (balanced)"
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

  print *, "OK: binomial logistic recovers signs, classifies, selects, collapses"
end program test_binomial
