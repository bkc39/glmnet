! Gaussian elnet without an intercept (#33). With intr = 0 the response is not
! centered, so the fraction of deviance explained is relative to sum(y^2):
! rsq = 1 - RSS / sum(y^2). The GLMNet.jl snapshot vendored before #21 scaled y
! by its centered norm instead, giving rsq ~ 5.8 here and shifting coefficients.
! Reference values are R glmnet 4.1.8, glmnet(x, y, lambda = ., intercept = FALSE).
program test_no_intercept
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

  x(:,1) = [1.0_c_double, 2.0_c_double, 3.0_c_double, 4.0_c_double, 5.0_c_double]
  x(:,2) = [2.0_c_double, 1.0_c_double, 4.0_c_double, 3.0_c_double, 6.0_c_double]
  y      = [1.0_c_double, 4.0_c_double, 3.0_c_double, 6.0_c_double, 5.0_c_double]

  call check("ols", 0.0_c_double, 1_c_int, 1.0e-10_c_double, &
       [2.2329448_c_double, -0.9622848_c_double], 0.9896292_c_double)
  call check("lasso", 0.1_c_double, 1_c_int, 1.0e-7_c_double, &
       [1.8656870_c_double, -0.6265096_c_double], 0.9832474_c_double)
  call check("lasso-unstandardized", 0.1_c_double, 0_c_int, 1.0e-7_c_double, &
       [1.9955260_c_double, -0.7460684_c_double], 0.9869714_c_double)

  print *, "OK: no-intercept rsq and coefficients match R glmnet"

contains

  subroutine check(label, lambda, standardize, thresh, r_beta, r_rsq)
    character(*),   intent(in) :: label
    real(c_double), intent(in) :: lambda, thresh, r_beta(ni), r_rsq
    integer(c_int), intent(in) :: standardize
    real(c_double) :: b0, beta(ni), rsq, lam, rss
    integer(c_int) :: nlp, jerr
    real(c_double), parameter :: tol = 1.0e-4_c_double

    call glmnet_elnet_solo(1.0_c_double, no, ni, x, y, lambda, &
         standardize, 0_c_int, thresh, 100000_c_int, &
         b0, beta, rsq, lam, nlp, jerr)

    if (jerr /= 0) then
       print *, "FAIL ", label, ": jerr =", jerr
       error stop 1
    end if
    if (b0 /= 0.0_c_double) then
       print *, "FAIL ", label, ": intercept =", b0, "expected 0"
       error stop 1
    end if
    rss = sum((y - matmul(x, beta))**2)
    if (abs(rsq - (1.0_c_double - rss / sum(y**2))) > 1.0e-6_c_double) then
       print *, "FAIL ", label, ": rsq =", rsq, "but 1 - RSS/sum(y^2) =", &
            1.0_c_double - rss / sum(y**2)
       error stop 1
    end if
    if (abs(rsq - r_rsq) > tol) then
       print *, "FAIL ", label, ": rsq =", rsq, "R dev.ratio =", r_rsq
       error stop 1
    end if
    if (maxval(abs(beta - r_beta)) > tol) then
       print *, "FAIL ", label, ": beta =", beta, "R beta =", r_beta
       error stop 1
    end if
  end subroutine check

end program test_no_intercept
