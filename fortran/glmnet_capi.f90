! glmnet_capi.f90 -- C-ABI shim over the vendored glmnet Fortran (vendor/glmnet5.f90).
!
! Every entry point here is `bind(C, name=...)` so it exports a clean, unmangled
! C symbol (no trailing underscore, no module prefix) that Racket's FFI binds to
! directly via `define-ffi-definer ... convention:hyphen->underscore`.
!
! This is modern FREE-FORM Fortran. The vendored glmnet5.f90 is FIXED-FORM; the
! build (../CMakeLists.txt) sets the format per source file.
!
! PRECISION CONTRACT: glmnet5.f90 declares its arrays as single `real`, but the
! whole project is compiled with -fdefault-real-8 so `real` is 8 bytes and the
! elnet ABI is effectively double precision. `glmnet_default_real_bytes` below
! lets callers assert that flag is in force. Keep all `real(c_double)` here.

module glmnet_capi
  use, intrinsic :: iso_c_binding
  implicit none
  private
  public :: glmnet_capi_abi_version, glmnet_default_real_bytes
  public :: glmnet_elnet_solo
  public :: glmnet_lognet_solo

  ! glmnet's "+/- infinity" sentinel for unconstrained coefficient bounds.
  real(c_double), parameter :: big = 9.9e35_c_double

contains

  ! ABI version of this shim. Bump on any breaking change to a C entry point.
  integer(c_int) function glmnet_capi_abi_version() &
       bind(C, name="glmnet_capi_abi_version")
    glmnet_capi_abi_version = 1_c_int
  end function glmnet_capi_abi_version

  ! Size in bytes of the Fortran default `real`. MUST be 8 -- i.e. the library
  ! was compiled with -fdefault-real-8 -- or the elnet ABI does not match the
  ! double-precision Racket bindings and every numeric result is garbage.
  integer(c_int) function glmnet_default_real_bytes() &
       bind(C, name="glmnet_default_real_bytes")
    real :: probe
    glmnet_default_real_bytes = int(storage_size(probe) / 8, c_int)
  end function glmnet_default_real_bytes

  ! Fit a single dense Gaussian elastic-net model (one alpha, one lambda) and
  ! return a DENSE coefficient vector. This is the workhorse behind the OLS /
  ! ridge / lasso / elastic-net "solo" fits -- they differ only in alpha and
  ! lambda. Internally it is one call to the vendored `elnet` with nlam = 1.
  !
  !   alpha          : elastic-net mixing in [0,1] (0 = ridge, 1 = lasso)
  !   no, ni         : observations, predictors
  !   x(no,ni)       : column-major predictor matrix (NOT modified -- copied)
  !   y(no)          : response (NOT modified -- copied)
  !   lambda         : the single penalty value to fit
  !   standardize    : 1 => standardize predictors (glmnet default), 0 => no
  !   intercept      : 1 => fit an intercept, 0 => no
  !   thresh, maxit  : convergence threshold and max passes
  !
  !   intercept_out  : fitted intercept
  !   beta_out(ni)   : DENSE coefficients on the original predictor scale
  !   rsq_out        : fraction of null deviance explained (R^2)
  !   lambda_out     : the lambda actually used
  !   nlp_out        : number of passes over the data
  !   jerr_out       : 0 ok; >0 fatal (no output); <0 non-fatal partial
  !                    (see vendor/glmnet5.f90 header for the codes)
  subroutine glmnet_elnet_solo(alpha, no, ni, x, y, lambda, &
       standardize, intercept, thresh, maxit, &
       intercept_out, beta_out, rsq_out, lambda_out, nlp_out, jerr_out) &
       bind(C, name="glmnet_elnet_solo")
    real(c_double),    value, intent(in)  :: alpha, lambda, thresh
    integer(c_int),    value, intent(in)  :: no, ni, standardize, intercept, maxit
    real(c_double),           intent(in)  :: x(no, ni)
    real(c_double),           intent(in)  :: y(no)
    real(c_double),           intent(out) :: intercept_out
    real(c_double),           intent(out) :: beta_out(ni)
    real(c_double),           intent(out) :: rsq_out, lambda_out
    integer(c_int),           intent(out) :: nlp_out, jerr_out

    ! elnet is an external (non-module) subroutine from vendor/glmnet5.f90.
    external :: elnet

    ! Work copies (elnet overwrites x, y, w) and elnet scratch/output arrays.
    real(c_double), allocatable :: xw(:,:), yw(:), ww(:), vp(:), cl(:,:)
    real(c_double), allocatable :: ulam(:), a0(:), ca(:,:), alm(:), rsq(:)
    integer(c_int), allocatable :: jd(:), ia(:), nin(:)
    integer(c_int) :: ka, ne, nx, nlam, isd, intr, lmu, nlp, jerr, k

    ka   = 1            ! covariance-updating algorithm (good for ni not huge)
    nlam = 1            ! single lambda -> "solo" fit
    isd  = standardize
    intr = intercept
    ne   = ni + 1       ! allow every variable to enter (dfmax)
    nx   = ni           ! room for every variable's coefficient (pmax)

    allocate(xw(no, ni), yw(no), ww(no), vp(ni), cl(2, ni))
    allocate(ulam(nlam), a0(nlam), ca(nx, nlam), alm(nlam), rsq(nlam))
    allocate(jd(1), ia(nx), nin(nlam))

    xw      = x                 ! copy: elnet destroys its x, y, w arguments
    yw      = y
    ww      = 1.0_c_double       ! equal observation weights
    vp      = 1.0_c_double       ! equal per-predictor penalty factors
    cl(1,:) = -big               ! no lower bound on coefficients
    cl(2,:) =  big               ! no upper bound
    jd(1)   = 0                  ! use all variables
    ulam(1) = lambda             ! flmin >= 1 => use this supplied lambda

    call elnet(ka, alpha, no, ni, xw, yw, ww, jd, vp, cl, ne, nx, nlam, &
         1.0_c_double, ulam, thresh, isd, intr, maxit, &
         lmu, a0, ca, ia, nin, rsq, alm, nlp, jerr)

    jerr_out      = jerr
    nlp_out       = nlp
    intercept_out = 0.0_c_double
    beta_out      = 0.0_c_double
    rsq_out       = 0.0_c_double
    lambda_out    = 0.0_c_double

    ! jerr > 0 is fatal (no output). Otherwise densify the first (only) solution.
    if (jerr <= 0 .and. lmu >= 1) then
       intercept_out = a0(1)
       rsq_out       = rsq(1)
       lambda_out    = alm(1)
       do k = 1, nin(1)
          beta_out(ia(k)) = ca(k, 1)
       end do
    end if

    deallocate(xw, yw, ww, vp, cl, ulam, a0, ca, alm, rsq, jd, ia, nin)
  end subroutine glmnet_elnet_solo

  ! Fit a single dense two-class logistic (binomial) elastic-net model -- one
  ! alpha, one lambda -- and return a DENSE coefficient vector plus the fitted
  ! intercept. This is the binomial-family analogue of glmnet_elnet_solo: one
  ! call to the vendored `lognet` with nc = 1 and nlam = 1.
  !
  ! The response y is a 0/1 class label per observation. glmnet's two-class
  ! `lognet` wants an no-by-2 indicator matrix and models the probability of the
  ! FIRST column (internally it fits `lognet2n` on `y(:,1)`). To model class 1 we
  ! therefore put the class-1 indicator in column 1 and class 0 in column 2; the
  ! fit then gives the log-odds of class 1, so the predicted probability is
  !   P(y = 1 | x) = 1 / (1 + exp(-(intercept + x . beta))).
  !
  !   alpha          : elastic-net mixing in [0,1] (0 = ridge, 1 = lasso)
  !   no, ni         : observations, predictors
  !   x(no,ni)       : column-major predictor matrix (NOT modified -- copied)
  !   y(no)          : 0/1 class labels (NOT modified -- copied)
  !   lambda         : the single penalty value to fit
  !   standardize    : 1 => standardize predictors (glmnet default), 0 => no
  !   intercept      : 1 => fit an intercept, 0 => no
  !   thresh, maxit  : convergence threshold and max passes
  !
  !   intercept_out  : fitted intercept (log-odds scale)
  !   beta_out(ni)   : DENSE coefficients on the original predictor scale
  !   dev_ratio_out  : fraction of null deviance explained (logistic "R^2")
  !   lambda_out     : the lambda actually used
  !   nlp_out        : number of passes over the data
  !   jerr_out       : 0 ok; >0 fatal (no output); <0 non-fatal partial
  !                    (see vendor/glmnet5.f90 header for the codes)
  subroutine glmnet_lognet_solo(alpha, no, ni, x, y, lambda, &
       standardize, intercept, thresh, maxit, &
       intercept_out, beta_out, dev_ratio_out, lambda_out, nlp_out, jerr_out) &
       bind(C, name="glmnet_lognet_solo")
    real(c_double),    value, intent(in)  :: alpha, lambda, thresh
    integer(c_int),    value, intent(in)  :: no, ni, standardize, intercept, maxit
    real(c_double),           intent(in)  :: x(no, ni)
    real(c_double),           intent(in)  :: y(no)
    real(c_double),           intent(out) :: intercept_out
    real(c_double),           intent(out) :: beta_out(ni)
    real(c_double),           intent(out) :: dev_ratio_out, lambda_out
    integer(c_int),           intent(out) :: nlp_out, jerr_out

    ! lognet is an external (non-module) subroutine from vendor/glmnet5.f90.
    external :: lognet

    ! Work copies (lognet standardizes x and normalizes y in place) plus the
    ! lognet scratch/output arrays. For the two-class case nc = 1, so a0/ca/dev
    ! carry a single class column.
    real(c_double), allocatable :: xw(:,:), yw(:,:), gw(:,:), vp(:), cl(:,:)
    real(c_double), allocatable :: ulam(:), a0(:,:), ca(:,:,:), alm(:), dev(:)
    integer(c_int), allocatable :: jd(:), ia(:), nin(:)
    integer(c_int) :: nc, ne, nx, nlam, isd, intr, kopt, lmu, nlp, jerr, k
    real(c_double) :: dev0

    nc   = 1            ! two-class binomial -> single coefficient column
    nlam = 1            ! single lambda -> "solo" fit
    isd  = standardize
    intr = intercept
    kopt = 0            ! 0 = exact Newton-Raphson Hessian
    ne   = ni + 1       ! allow every variable to enter (dfmax)
    nx   = ni           ! room for every variable's coefficient (pmax)

    allocate(xw(no, ni), yw(no, 2), gw(no, nc), vp(ni), cl(2, ni))
    allocate(ulam(nlam), a0(nc, nlam), ca(nx, nc, nlam), alm(nlam), dev(nlam))
    allocate(jd(1), ia(nx), nin(nlam))

    xw      = x                  ! copy: lognet destroys (standardizes) its x
    yw(:,1) = y                   ! class-1 indicator (the modelled column)
    yw(:,2) = 1.0_c_double - y    ! class-0 indicator
    gw      = 0.0_c_double        ! no offset
    vp      = 1.0_c_double        ! equal per-predictor penalty factors
    cl(1,:) = -big                ! no lower bound on coefficients
    cl(2,:) =  big                ! no upper bound
    jd(1)   = 0                   ! use all variables
    ulam(1) = lambda              ! flmin >= 1 => use this supplied lambda

    call lognet(alpha, no, ni, nc, xw, yw, gw, jd, vp, cl, ne, nx, nlam, &
         1.0_c_double, ulam, thresh, isd, intr, maxit, kopt, &
         lmu, a0, ca, ia, nin, dev0, dev, alm, nlp, jerr)

    jerr_out      = jerr
    nlp_out       = nlp
    intercept_out = 0.0_c_double
    beta_out      = 0.0_c_double
    dev_ratio_out = 0.0_c_double
    lambda_out    = 0.0_c_double

    ! jerr > 0 is fatal (no output). Otherwise densify the first (only) solution.
    if (jerr <= 0 .and. lmu >= 1) then
       intercept_out = a0(1, 1)
       dev_ratio_out = dev(1)
       lambda_out    = alm(1)
       do k = 1, nin(1)
          beta_out(ia(k)) = ca(k, 1, 1)
       end do
    end if

    deallocate(xw, yw, gw, vp, cl, ulam, a0, ca, alm, dev, jd, ia, nin)
  end subroutine glmnet_lognet_solo

end module glmnet_capi
