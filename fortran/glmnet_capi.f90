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
  public :: glmnet_capi_abi_version, glmnet_hello, glmnet_default_real_bytes
  public :: glmnet_elnet_solo

  ! glmnet's "+/- infinity" sentinel for unconstrained coefficient bounds.
  real(c_double), parameter :: big = 9.9e35_c_double

contains

  ! ABI version of this shim. Bump on any breaking change to a C entry point.
  integer(c_int) function glmnet_capi_abi_version() &
       bind(C, name="glmnet_capi_abi_version")
    glmnet_capi_abi_version = 1_c_int
  end function glmnet_capi_abi_version

  ! Hello-world numeric round-trip: proves clean by-value double marshalling
  ! across the FFI boundary. Returns a + b.
  real(c_double) function glmnet_hello(a, b) bind(C, name="glmnet_hello")
    real(c_double), value, intent(in) :: a
    real(c_double), value, intent(in) :: b
    glmnet_hello = a + b
  end function glmnet_hello

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

end module glmnet_capi
