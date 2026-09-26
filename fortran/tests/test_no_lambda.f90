! A path that fits no lambda (review of #45).
!
! With maxit = 1 every solver gives up at the first lambda: jerr = -1, and it
! returns before it sets lmu. R passes lmu = 0 into the Fortran, so the path
! then has no lambdas. Each glmnet_<family>_path must report lmu_out = 0 too;
! an uninitialised lmu made the shim densify a garbage number of lambdas and
! write past the ends of its outputs. dirty_stack fills the stack with nonzero
! words first, so that an uninitialised lmu is garbage rather than a lucky 0.
program test_no_lambda
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

     subroutine glmnet_lognet_path(alpha, no, ni, x, y, nlam, flmin, ulam, &
          standardize, intercept, thresh, maxit, &
          lmu_out, intercept_out, beta_out, dev_ratio_out, lambda_out, nlp_out, jerr_out) &
          bind(C, name="glmnet_lognet_path")
       import :: c_double, c_int
       real(c_double), value :: alpha, flmin, thresh
       integer(c_int) :: no, ni, nlam, standardize, intercept, maxit
       real(c_double) :: x(no, ni), y(no), ulam(nlam)
       integer(c_int) :: lmu_out, nlp_out, jerr_out
       real(c_double) :: intercept_out(nlam), beta_out(ni, nlam), dev_ratio_out(nlam), lambda_out(nlam)
     end subroutine glmnet_lognet_path

     subroutine glmnet_fishnet_path(alpha, no, ni, x, y, nlam, flmin, ulam, &
          standardize, intercept, thresh, maxit, &
          lmu_out, intercept_out, beta_out, dev_ratio_out, lambda_out, nlp_out, jerr_out) &
          bind(C, name="glmnet_fishnet_path")
       import :: c_double, c_int
       real(c_double), value :: alpha, flmin, thresh
       integer(c_int) :: no, ni, nlam, standardize, intercept, maxit
       real(c_double) :: x(no, ni), y(no), ulam(nlam)
       integer(c_int) :: lmu_out, nlp_out, jerr_out
       real(c_double) :: intercept_out(nlam), beta_out(ni, nlam), dev_ratio_out(nlam), lambda_out(nlam)
     end subroutine glmnet_fishnet_path

     subroutine glmnet_multinomial_path(alpha, no, ni, nc, x, y, nlam, flmin, ulam, &
          standardize, intercept, thresh, maxit, &
          lmu_out, intercept_out, beta_out, dev_ratio_out, lambda_out, nlp_out, jerr_out) &
          bind(C, name="glmnet_multinomial_path")
       import :: c_double, c_int
       real(c_double), value :: alpha, flmin, thresh
       integer(c_int) :: no, ni, nc, nlam, standardize, intercept, maxit
       real(c_double) :: x(no, ni), y(no), ulam(nlam)
       integer(c_int) :: lmu_out, nlp_out, jerr_out
       real(c_double) :: intercept_out(nc, nlam), beta_out(ni, nc, nlam)
       real(c_double) :: dev_ratio_out(nlam), lambda_out(nlam)
     end subroutine glmnet_multinomial_path

     subroutine glmnet_coxnet_path(alpha, no, ni, x, time, status, nlam, flmin, ulam, &
          standardize, thresh, maxit, &
          lmu_out, beta_out, dev_ratio_out, lambda_out, nlp_out, jerr_out) &
          bind(C, name="glmnet_coxnet_path")
       import :: c_double, c_int
       real(c_double), value :: alpha, flmin, thresh
       integer(c_int) :: no, ni, nlam, standardize, maxit
       real(c_double) :: x(no, ni), time(no), status(no), ulam(nlam)
       integer(c_int) :: lmu_out, nlp_out, jerr_out
       real(c_double) :: beta_out(ni, nlam), dev_ratio_out(nlam), lambda_out(nlam)
     end subroutine glmnet_coxnet_path

     subroutine glmnet_mgaussian_path(alpha, no, ni, nr, x, y, nlam, flmin, ulam, &
          standardize, intercept, thresh, maxit, &
          lmu_out, intercept_out, beta_out, rsq_out, lambda_out, nlp_out, jerr_out) &
          bind(C, name="glmnet_mgaussian_path")
       import :: c_double, c_int
       real(c_double), value :: alpha, flmin, thresh
       integer(c_int) :: no, ni, nr, nlam, standardize, intercept, maxit
       real(c_double) :: x(no, ni), y(no, nr), ulam(nlam)
       integer(c_int) :: lmu_out, nlp_out, jerr_out
       real(c_double) :: intercept_out(nr, nlam), beta_out(ni, nr, nlam)
       real(c_double) :: rsq_out(nlam), lambda_out(nlam)
     end subroutine glmnet_mgaussian_path
  end interface

  integer(c_int), parameter :: no = 12, ni = 3, nlam = 1, k = 3
  real(c_double) :: x(no, ni), yg(no), yb(no), yp(no), ym(no), tm(no), st(no), ymg(no, 2)
  real(c_double) :: ulam(nlam), a0(nlam), beta(ni, nlam), dev(nlam), alm(nlam)
  real(c_double) :: a0k(k, nlam), betak(ni, k, nlam), a0r(2, nlam), betar(ni, 2, nlam)
  integer(c_int) :: lmu, nlp, jerr

  x(:,1) = [1, 2, 2, 1, 3, 2, 6, 5, 6, 5, 4, 6]
  x(:,2) = [5, 6, 5, 4, 6, 4, 2, 1, 1, 2, 1, 3]
  x(:,3) = [2, 1, 3, 1, 2, 2, 2, 1, 3, 1, 2, 2]
  yg     = [1.0_c_double, 4.0_c_double, 3.0_c_double, 6.0_c_double, 5.0_c_double, 8.0_c_double, &
            2.0_c_double, 7.0_c_double, 4.0_c_double, 9.0_c_double, 3.0_c_double, 5.0_c_double]
  yb     = [0, 0, 1, 0, 0, 0, 1, 1, 0, 1, 1, 1]
  yp     = [1, 2, 2, 3, 4, 6, 8, 11, 1, 2, 3, 4]
  ym     = [0, 0, 1, 1, 2, 2, 0, 1, 2, 0, 1, 2]
  tm     = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12]
  st     = [1, 1, 1, 0, 1, 1, 1, 0, 1, 1, 1, 1]
  ymg(:,1) = yg
  ymg(:,2) = yp
  ulam   = 0.001_c_double

  call dirty_stack()
  call glmnet_elnet_path(1.0_c_double, no, ni, x, yg, nlam, 1.0_c_double, ulam, &
       1_c_int, 1_c_int, 1.0e-7_c_double, 1_c_int, &
       lmu, a0, beta, dev, alm, nlp, jerr)
  call require_empty("elnet", lmu, jerr)

  call dirty_stack()
  call glmnet_lognet_path(1.0_c_double, no, ni, x, yb, nlam, 1.0_c_double, ulam, &
       1_c_int, 1_c_int, 1.0e-7_c_double, 1_c_int, &
       lmu, a0, beta, dev, alm, nlp, jerr)
  call require_empty("lognet", lmu, jerr)

  call dirty_stack()
  call glmnet_fishnet_path(1.0_c_double, no, ni, x, yp, nlam, 1.0_c_double, ulam, &
       1_c_int, 1_c_int, 1.0e-7_c_double, 1_c_int, &
       lmu, a0, beta, dev, alm, nlp, jerr)
  call require_empty("fishnet", lmu, jerr)

  call dirty_stack()
  call glmnet_multinomial_path(1.0_c_double, no, ni, k, x, ym, nlam, 1.0_c_double, ulam, &
       1_c_int, 1_c_int, 1.0e-7_c_double, 1_c_int, &
       lmu, a0k, betak, dev, alm, nlp, jerr)
  call require_empty("multinomial", lmu, jerr)

  call dirty_stack()
  call glmnet_coxnet_path(1.0_c_double, no, ni, x, tm, st, nlam, 1.0_c_double, ulam, &
       1_c_int, 1.0e-7_c_double, 1_c_int, &
       lmu, beta, dev, alm, nlp, jerr)
  call require_empty("coxnet", lmu, jerr)

  call dirty_stack()
  call glmnet_mgaussian_path(1.0_c_double, no, ni, 2_c_int, x, ymg, nlam, 1.0_c_double, ulam, &
       1_c_int, 1_c_int, 1.0e-7_c_double, 1_c_int, &
       lmu, a0r, betar, dev, alm, nlp, jerr)
  call require_empty("mgaussian", lmu, jerr)

  print *, "OK: every family reports lmu = 0 when it fits no lambda"

contains

  subroutine dirty_stack()
    integer(c_int), volatile :: junk(8192)
    junk = 123456789_c_int
  end subroutine dirty_stack

  subroutine require_empty(family, lmu, jerr)
    character(*), intent(in) :: family
    integer(c_int), intent(in) :: lmu, jerr
    if (jerr /= -1 .or. lmu /= 0) then
       print *, "FAIL: ", family, " with maxit = 1: jerr =", jerr, " lmu =", lmu, &
            " (want jerr = -1, lmu = 0)"
       error stop 1
    end if
  end subroutine require_empty

end program test_no_lambda
