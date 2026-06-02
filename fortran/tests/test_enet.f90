! Elastic net at intermediate alpha sits between ridge and lasso. At a shared
! lambda the number of selected-out (zero) coefficients is monotone in alpha:
! ridge (alpha=0) zeros nothing, lasso (alpha=1) zeros the most, and an
! intermediate alpha lands in between. Same fixture as the ridge/lasso tests.
program test_enet
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
  integer(c_int) :: nz_ridge, nz_enet, nz_lasso

  x(:,1) = [1.0_c_double, 2.0_c_double, 3.0_c_double, 4.0_c_double, 5.0_c_double, 6.0_c_double]
  x(:,2) = [2.0_c_double, 1.0_c_double, 4.0_c_double, 3.0_c_double, 6.0_c_double, 5.0_c_double]
  x(:,3) = [1.0_c_double, 4.0_c_double, 9.0_c_double, 16.0_c_double, 25.0_c_double, 36.0_c_double]
  y      = [1.0_c_double, 4.0_c_double, 3.0_c_double, 6.0_c_double, 5.0_c_double, 8.0_c_double]

  nz_ridge = zeros_at(0.0_c_double, 0.5_c_double)
  nz_enet  = zeros_at(0.5_c_double, 0.5_c_double)
  nz_lasso = zeros_at(1.0_c_double, 0.5_c_double)

  if (nz_ridge /= 0) then
     print *, "FAIL: ridge selected a variable out:", nz_ridge
     error stop 1
  end if
  if (nz_lasso < 1) then
     print *, "FAIL: lasso selected nothing out:", nz_lasso
     error stop 1
  end if
  if (.not. (nz_ridge <= nz_enet .and. nz_enet <= nz_lasso)) then
     print *, "FAIL: elastic-net sparsity not between ridge and lasso:", &
          nz_ridge, nz_enet, nz_lasso
     error stop 1
  end if

  print *, "OK: elastic-net sparsity between ridge and lasso:", &
       nz_ridge, nz_enet, nz_lasso

contains

  ! Number of exactly-zero coefficients for a fit at the given (alpha, lambda).
  integer(c_int) function zeros_at(alpha, lambda) result(nz)
    real(c_double), intent(in) :: alpha, lambda
    real(c_double) :: b0, beta(ni), rsq, lam
    integer(c_int) :: nlp, jerr
    call glmnet_elnet_solo(alpha, no, ni, x, y, lambda, &
         1_c_int, 1_c_int, 1.0e-7_c_double, 100000_c_int, &
         b0, beta, rsq, lam, nlp, jerr)
    if (jerr /= 0) then
       print *, "FAIL: jerr =", jerr, " at alpha=", alpha, " lambda=", lambda
       error stop 1
    end if
    nz = count(beta == 0.0_c_double)
  end function zeros_at

end program test_enet
