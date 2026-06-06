! K-class multinomial (binomial family with nc>1) == one `lognet` call via
! glmnet_multinomial_solo. Three separable synthetic clusters in 2-D: class 0
! near (1.5,1.5), class 1 near (5.5,1.5), class 2 near (3.5,5.5). The fit must
! converge, explain deviance, and classify every training point correctly under
! argmax of the K linear predictors. At a huge lambda the penalized model
! collapses: all coefficients zero, dev.ratio = 0.
program test_multinomial
  use, intrinsic :: iso_c_binding
  implicit none

  interface
     subroutine glmnet_multinomial_solo(alpha, no, ni, nc, x, y, lambda, &
          standardize, intercept, thresh, maxit, &
          intercept_out, beta_out, dev_ratio_out, lambda_out, nlp_out, jerr_out) &
          bind(C, name="glmnet_multinomial_solo")
       import :: c_double, c_int
       real(c_double), value :: alpha, lambda, thresh
       integer(c_int), value :: no, ni, nc, standardize, intercept, maxit
       real(c_double) :: x(no, ni), y(no)
       real(c_double) :: intercept_out(nc), beta_out(ni*nc), dev_ratio_out, lambda_out
       integer(c_int) :: nlp_out, jerr_out
     end subroutine glmnet_multinomial_solo
  end interface

  integer(c_int), parameter :: no = 12, ni = 2, nc = 3
  real(c_double) :: x(no, ni), y(no)
  real(c_double) :: a0(nc), beta(ni*nc), devr, lam, eta(nc), best
  integer(c_int) :: nlp, jerr, i, k, pred, ncorrect

  ! class 0: x ~ (1-2, 1-2);  class 1: x ~ (5-6, 1-2);  class 2: x ~ (3-4, 5-6)
  x(:,1) = [1.0_c_double, 2.0_c_double, 1.0_c_double, 2.0_c_double, &
            5.0_c_double, 6.0_c_double, 5.0_c_double, 6.0_c_double, &
            3.0_c_double, 4.0_c_double, 3.0_c_double, 4.0_c_double]
  x(:,2) = [1.0_c_double, 1.0_c_double, 2.0_c_double, 2.0_c_double, &
            1.0_c_double, 1.0_c_double, 2.0_c_double, 2.0_c_double, &
            5.0_c_double, 5.0_c_double, 6.0_c_double, 6.0_c_double]
  y      = [0.0_c_double, 0.0_c_double, 0.0_c_double, 0.0_c_double, &
            1.0_c_double, 1.0_c_double, 1.0_c_double, 1.0_c_double, &
            2.0_c_double, 2.0_c_double, 2.0_c_double, 2.0_c_double]

  ! --- small lambda: good fit, 100% argmax training accuracy ---
  call glmnet_multinomial_solo(1.0_c_double, no, ni, nc, x, y, 0.01_c_double, &
       1_c_int, 1_c_int, 1.0e-7_c_double, 100000_c_int, &
       a0, beta, devr, lam, nlp, jerr)
  if (jerr /= 0) then
     print *, "FAIL: jerr =", jerr
     error stop 1
  end if
  if (devr <= 0.0_c_double .or. devr > 1.0_c_double) then
     print *, "FAIL: dev.ratio out of (0,1]:", devr
     error stop 1
  end if
  ncorrect = 0
  do i = 1, no
     do k = 1, nc
        eta(k) = a0(k) + x(i,1)*beta((k-1)*ni+1) + x(i,2)*beta((k-1)*ni+2)
     end do
     pred = 0
     best = eta(1)
     do k = 2, nc
        if (eta(k) > best) then
           best = eta(k)
           pred = k - 1
        end if
     end do
     if (pred == nint(y(i))) ncorrect = ncorrect + 1
  end do
  if (ncorrect /= no) then
     print *, "FAIL: argmax training accuracy", ncorrect, "/", no
     error stop 1
  end if

  ! --- huge lambda: collapse (all coefficients zero, dev.ratio ~ 0) ---
  call glmnet_multinomial_solo(1.0_c_double, no, ni, nc, x, y, 1.0e6_c_double, &
       1_c_int, 1_c_int, 1.0e-7_c_double, 100000_c_int, &
       a0, beta, devr, lam, nlp, jerr)
  if (any(beta /= 0.0_c_double)) then
     print *, "FAIL: coefficients not all zero at huge lambda:", beta
     error stop 1
  end if
  if (devr > 1.0e-3_c_double) then
     print *, "FAIL: dev.ratio not ~0 at huge lambda:", devr
     error stop 1
  end if

  print *, "OK: multinomial classifies 3 separable classes; collapses at huge lambda"
end program test_multinomial
