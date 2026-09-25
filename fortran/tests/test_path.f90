! Regularization path (#10) through glmnet_elnet_path.
!
! Automatic mode (flmin < 1): glmnet chooses the lambdas, reports the first one
! as its `big` sentinel (R's fix.lam replaces it), fits every coefficient to zero
! there (lambda_max), and the sequence then decreases. User mode (flmin >= 1):
! the supplied lambdas are fitted in order, and each point agrees with a single
! fit at that lambda.
program test_path
  use, intrinsic :: iso_c_binding
  implicit none

  interface
     subroutine glmnet_elnet_path(alpha, no, ni, x, y, nlam, flmin, ulam, &
          standardize, intercept, thresh, maxit, &
          lmu_out, intercept_out, beta_out, rsq_out, lambda_out, nlp_out, jerr_out) &
          bind(C, name="glmnet_elnet_path")
       import :: c_double, c_int
       real(c_double), value :: alpha, flmin, thresh
       integer(c_int) :: no, ni, nlam, standardize, intercept, maxit
       real(c_double) :: x(no, ni), y(no), ulam(nlam)
       integer(c_int) :: lmu_out, nlp_out, jerr_out
       real(c_double) :: intercept_out(nlam), beta_out(ni, nlam), rsq_out(nlam), lambda_out(nlam)
     end subroutine glmnet_elnet_path

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

  integer(c_int), parameter :: no = 6, ni = 3, nauto = 20, nuser = 3
  real(c_double), parameter :: big = 9.9e35_c_double
  real(c_double) :: x(no, ni), y(no)
  real(c_double) :: ulam(nauto), a0(nauto), beta(ni, nauto), rsq(nauto), alm(nauto)
  real(c_double) :: ulam3(nuser), a03(nuser), beta3(ni, nuser), rsq3(nuser), alm3(nuser)
  real(c_double) :: b0, bsolo(ni), rsqsolo, lsolo
  integer(c_int) :: lmu, nlp, jerr, m

  x(:,1) = [1.0_c_double, 2.0_c_double, 3.0_c_double, 4.0_c_double, 5.0_c_double, 6.0_c_double]
  x(:,2) = [2.0_c_double, 1.0_c_double, 4.0_c_double, 3.0_c_double, 6.0_c_double, 5.0_c_double]
  x(:,3) = x(:,1)**2
  y      = [1.0_c_double, 4.0_c_double, 3.0_c_double, 6.0_c_double, 5.0_c_double, 8.0_c_double]

  ! --- automatic sequence: 20 lambdas down to 1e-4 * lambda_max ---------------
  ulam = 0.0_c_double
  call glmnet_elnet_path(1.0_c_double, no, ni, x, y, nauto, 1.0e-4_c_double, ulam, &
       1_c_int, 1_c_int, 1.0e-7_c_double, 100000_c_int, &
       lmu, a0, beta, rsq, alm, nlp, jerr)
  call require(jerr == 0, "auto path: jerr /= 0")
  call require(lmu > 2 .and. lmu <= nauto, "auto path: unexpected lmu")
  call require(alm(1) >= big, "auto path: first lambda is not glmnet's big sentinel")
  call require(all(beta(:, 1) == 0.0_c_double), "auto path: coefficients nonzero at lambda_max")
  do m = 3, lmu
     call require(alm(m) < alm(m - 1), "auto path: lambdas not decreasing")
  end do
  call require(rsq(lmu) > rsq(2), "auto path: R^2 did not grow along the path")

  ! --- user sequence: each point agrees with a single fit ---------------------
  ulam3 = [0.5_c_double, 0.1_c_double, 0.01_c_double]
  call glmnet_elnet_path(1.0_c_double, no, ni, x, y, nuser, 1.0_c_double, ulam3, &
       1_c_int, 1_c_int, 1.0e-12_c_double, 100000_c_int, &
       lmu, a03, beta3, rsq3, alm3, nlp, jerr)
  call require(jerr == 0 .and. lmu == nuser, "user path: jerr or lmu")
  call require(all(abs(alm3 - ulam3) < 1.0e-12_c_double), "user path: lambdas not echoed")
  do m = 1, nuser
     call glmnet_elnet_solo(1.0_c_double, no, ni, x, y, ulam3(m), &
          1_c_int, 1_c_int, 1.0e-12_c_double, 100000_c_int, &
          b0, bsolo, rsqsolo, lsolo, nlp, jerr)
     call require(abs(b0 - a03(m)) < 1.0e-6_c_double, "user path: intercept /= single fit")
     call require(maxval(abs(bsolo - beta3(:, m))) < 1.0e-6_c_double, &
          "user path: coefficients /= single fit")
     call require(abs(rsqsolo - rsq3(m)) < 1.0e-9_c_double, "user path: R^2 /= single fit")
  end do

  print *, "OK: auto path of", lmu, "lambdas; user path matches single fits"

contains

  subroutine require(ok, msg)
    logical, intent(in) :: ok
    character(*), intent(in) :: msg
    if (.not. ok) then
       print *, "FAIL: ", msg
       error stop 1
    end if
  end subroutine require

end program test_path
