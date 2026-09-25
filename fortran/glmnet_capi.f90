! glmnet_capi.f90 -- C-ABI shim over R glmnet 4.1's Fortran (vendor/glmnet5dpclean.f).
!
! Every entry point here is `bind(C, name=...)` so it exports a clean, unmangled
! C symbol (no trailing underscore, no module prefix) that Racket's FFI binds to
! directly via `define-ffi-definer ... convention:hyphen->underscore`.
!
! This is modern FREE-FORM Fortran. The vendored R Fortran is FIXED-FORM; the
! build (../CMakeLists.txt) sets the format per source file.
!
! PRECISION CONTRACT: the vendored R Fortran is DOUBLE PRECISION, and the build
! (-fdefault-real-8 -fdefault-double-8) keeps both default real and double
! precision at 8 bytes. `glmnet_default_real_bytes` below lets callers assert
! it. Keep all `real(c_double)` here.
!
! Each family has a PATH entry point (glmnet_<family>_path) that fits a whole
! sequence of lambdas and a SOLO entry point (glmnet_<family>_solo) for a single
! lambda, which is the path entry with nlam = 1. A path takes
!
!   nlam           : number of lambdas to fit (the length of ulam and of every
!                    per-lambda output)
!   flmin          : >= 1 => fit exactly the supplied ulam(1..nlam), which must
!                    be decreasing; < 1 => glmnet's automatic sequence of nlam
!                    lambdas from lambda_max down to flmin * lambda_max (ulam is
!                    then ignored)
!
! Path entry points take their INTEGER scalars BY REFERENCE, unlike the solo ones.
! A path call has more than eight integer and pointer arguments, so some spill
! onto the stack, and Apple's arm64 ABI packs 4-byte ints there while standard
! AAPCS64 gives each an 8-byte slot. With by-value ints on the stack the caller
! and gfortran disagreed on macOS and outputs landed in the wrong place; as
! pointers, every stack argument is 8 bytes under both conventions.
!
! A path returns lmu, the number of lambdas actually fitted: glmnet stops early
! once the deviance ratio stops improving, so lmu <= nlam and only the first lmu
! entries of each per-lambda output are meaningful. In automatic mode glmnet
! reports the first lambda as `big`; R replaces it (fix.lam), and so does the
! Racket side. Coefficients come back DENSE, one column per lambda.

module glmnet_capi
  use, intrinsic :: iso_c_binding
  implicit none
  private
  public :: glmnet_capi_abi_version, glmnet_default_real_bytes
  public :: glmnet_elnet_solo, glmnet_elnet_path
  public :: glmnet_lognet_solo, glmnet_lognet_path
  public :: glmnet_multinomial_solo, glmnet_multinomial_path
  public :: glmnet_coxnet_solo, glmnet_coxnet_path
  public :: glmnet_fishnet_solo, glmnet_fishnet_path
  public :: glmnet_mgaussian_solo, glmnet_mgaussian_path

  ! glmnet's "+/- infinity" sentinel for unconstrained coefficient bounds.
  real(c_double), parameter :: big = 9.9e35_c_double

contains

  ! ABI version of this shim: the set of bind(C) entry points it exports. Bump on
  ! any change to that set (a new family symbol, a signature change) so the
  ! load-time guard in foreign.rkt rejects a stale prebuilt native candidate.
  !   1 -> elnet only (initial)
  !   2 -> + lognet, multinomial, coxnet, fishnet, mgaussian (all six families)
  !   3 -> + a *_path entry point per family (regularization paths, #10)
  integer(c_int) function glmnet_capi_abi_version() &
       bind(C, name="glmnet_capi_abi_version")
    glmnet_capi_abi_version = 3_c_int
  end function glmnet_capi_abi_version

  ! Size in bytes of the Fortran default `real`. MUST be 8 -- i.e. the library
  ! was compiled with the project's precision flags -- or the build does not
  ! match the double-precision Racket bindings.
  integer(c_int) function glmnet_default_real_bytes() &
       bind(C, name="glmnet_default_real_bytes")
    real :: probe
    glmnet_default_real_bytes = int(storage_size(probe) / 8, c_int)
  end function glmnet_default_real_bytes

  ! --- Gaussian (elnet) --------------------------------------------------------
  !
  ! Dense Gaussian elastic net over a lambda path: one call to the vendored
  ! `elnet`. OLS, ridge, lasso and elastic net differ only in alpha and lambda.
  !
  !   alpha          : elastic-net mixing in [0,1] (0 = ridge, 1 = lasso)
  !   no, ni         : observations, predictors
  !   x(no,ni)       : column-major predictor matrix (NOT modified -- copied)
  !   y(no)          : response (NOT modified -- copied)
  !   nlam, flmin, ulam(nlam) : the lambda path (see the module header)
  !   standardize    : 1 => standardize predictors (glmnet default), 0 => no
  !   intercept      : 1 => fit an intercept, 0 => no
  !   thresh, maxit  : convergence threshold and max passes
  !
  !   lmu_out              : lambdas actually fitted (<= nlam)
  !   intercept_out(nlam)  : fitted intercept per lambda
  !   beta_out(ni,nlam)    : DENSE coefficients per lambda, original scale
  !   rsq_out(nlam)        : fraction of null deviance explained (R^2)
  !   lambda_out(nlam)     : the lambdas used
  !   nlp_out              : number of passes over the data
  !   jerr_out             : 0 ok; >0 fatal (no output); <0 non-fatal partial
  !                          (codes: R glmnet's R/jerr.R)
  subroutine glmnet_elnet_path(alpha, no, ni, x, y, nlam, flmin, ulam, &
       standardize, intercept, thresh, maxit, &
       lmu_out, intercept_out, beta_out, rsq_out, lambda_out, nlp_out, jerr_out) &
       bind(C, name="glmnet_elnet_path")
    real(c_double),    value, intent(in)  :: alpha, flmin, thresh
    integer(c_int),           intent(in)  :: no, ni, nlam, standardize, intercept, maxit
    real(c_double),           intent(in)  :: x(no, ni), y(no), ulam(nlam)
    integer(c_int),           intent(out) :: lmu_out, nlp_out, jerr_out
    real(c_double),           intent(out) :: intercept_out(nlam), beta_out(ni, nlam)
    real(c_double),           intent(out) :: rsq_out(nlam), lambda_out(nlam)

    external :: elnet

    ! Work copies (elnet overwrites x, y, w) and elnet scratch/output arrays.
    real(c_double), allocatable :: xw(:,:), yw(:), ww(:), vp(:), cl(:,:)
    real(c_double), allocatable :: ulamw(:), a0(:), ca(:,:), alm(:), rsq(:)
    integer(c_int), allocatable :: jd(:), ia(:), nin(:)
    integer(c_int) :: lmu, nlp, jerr, k, m

    allocate(xw(no, ni), yw(no), ww(no), vp(ni), cl(2, ni))
    allocate(ulamw(nlam), a0(nlam), ca(ni, nlam), alm(nlam), rsq(nlam))
    allocate(jd(1), ia(ni), nin(nlam))

    xw      = x                 ! copy: elnet destroys its x, y, w arguments
    yw      = y
    ww      = 1.0_c_double       ! equal observation weights
    vp      = 1.0_c_double       ! equal per-predictor penalty factors
    cl(1,:) = -big               ! no lower bound on coefficients
    cl(2,:) =  big               ! no upper bound
    jd(1)   = 0                  ! use all variables
    ulamw   = ulam

    ! ka = 1: covariance updating (good for ni not huge). ne = ni + 1 lets every
    ! variable enter (dfmax); nx = ni leaves room for all of them (pmax).
    call elnet(1, alpha, no, ni, xw, yw, ww, jd, vp, cl, ni + 1, ni, nlam, &
         flmin, ulamw, thresh, standardize, intercept, maxit, &
         lmu, a0, ca, ia, nin, rsq, alm, nlp, jerr)

    jerr_out      = jerr
    nlp_out       = nlp
    lmu_out       = 0
    intercept_out = 0.0_c_double
    beta_out      = 0.0_c_double
    rsq_out       = 0.0_c_double
    lambda_out    = 0.0_c_double

    ! jerr > 0 is fatal (no output). Otherwise densify every fitted lambda.
    if (jerr <= 0) then
       lmu_out = lmu
       do m = 1, lmu
          intercept_out(m) = a0(m)
          rsq_out(m)       = rsq(m)
          lambda_out(m)    = alm(m)
          do k = 1, nin(m)
             beta_out(ia(k), m) = ca(k, m)
          end do
       end do
    end if

    deallocate(xw, yw, ww, vp, cl, ulamw, a0, ca, alm, rsq, jd, ia, nin)
  end subroutine glmnet_elnet_path

  ! A single Gaussian fit: glmnet_elnet_path with nlam = 1 at the supplied lambda.
  subroutine glmnet_elnet_solo(alpha, no, ni, x, y, lambda, &
       standardize, intercept, thresh, maxit, &
       intercept_out, beta_out, rsq_out, lambda_out, nlp_out, jerr_out) &
       bind(C, name="glmnet_elnet_solo")
    real(c_double),    value, intent(in)  :: alpha, lambda, thresh
    integer(c_int),    value, intent(in)  :: no, ni, standardize, intercept, maxit
    real(c_double),           intent(in)  :: x(no, ni), y(no)
    real(c_double),           intent(out) :: intercept_out, beta_out(ni)
    real(c_double),           intent(out) :: rsq_out, lambda_out
    integer(c_int),           intent(out) :: nlp_out, jerr_out

    real(c_double) :: ulam(1), a0(1), rsq(1), alm(1)
    real(c_double), allocatable :: beta(:,:)
    integer(c_int) :: lmu

    allocate(beta(ni, 1))
    ulam(1) = lambda
    call glmnet_elnet_path(alpha, no, ni, x, y, 1, 1.0_c_double, ulam, &
         standardize, intercept, thresh, maxit, &
         lmu, a0, beta, rsq, alm, nlp_out, jerr_out)
    intercept_out = a0(1)
    beta_out      = beta(:, 1)
    rsq_out       = rsq(1)
    lambda_out    = alm(1)
  end subroutine glmnet_elnet_solo

  ! --- Binomial (lognet, two classes) ------------------------------------------
  !
  ! Dense two-class logistic elastic net over a lambda path: the vendored `lognet`
  ! with nc = 1. glmnet's two-class `lognet` wants an no-by-2 indicator matrix and
  ! models the probability of its FIRST column, so the class-1 indicator goes in
  ! column 1: the fit gives the log-odds of class 1, and
  !   P(y = 1 | x) = 1 / (1 + exp(-(intercept + x . beta))).
  !
  !   y(no)                : 0/1 class labels (NOT modified -- copied)
  !   dev_ratio_out(nlam)  : fraction of null deviance explained
  !   (the other arguments are as for glmnet_elnet_path)
  subroutine glmnet_lognet_path(alpha, no, ni, x, y, nlam, flmin, ulam, &
       standardize, intercept, thresh, maxit, &
       lmu_out, intercept_out, beta_out, dev_ratio_out, lambda_out, nlp_out, jerr_out) &
       bind(C, name="glmnet_lognet_path")
    real(c_double),    value, intent(in)  :: alpha, flmin, thresh
    integer(c_int),           intent(in)  :: no, ni, nlam, standardize, intercept, maxit
    real(c_double),           intent(in)  :: x(no, ni), y(no), ulam(nlam)
    integer(c_int),           intent(out) :: lmu_out, nlp_out, jerr_out
    real(c_double),           intent(out) :: intercept_out(nlam), beta_out(ni, nlam)
    real(c_double),           intent(out) :: dev_ratio_out(nlam), lambda_out(nlam)

    external :: lognet

    ! Work copies (lognet standardizes x and normalizes y in place) plus the
    ! lognet scratch/output arrays. For two classes nc = 1, so a0/ca/dev carry a
    ! single class column.
    real(c_double), allocatable :: xw(:,:), yw(:,:), gw(:,:), vp(:), cl(:,:)
    real(c_double), allocatable :: ulamw(:), a0(:,:), ca(:,:,:), alm(:), dev(:)
    integer(c_int), allocatable :: jd(:), ia(:), nin(:)
    integer(c_int) :: lmu, nlp, jerr, k, m
    real(c_double) :: dev0

    allocate(xw(no, ni), yw(no, 2), gw(no, 1), vp(ni), cl(2, ni))
    allocate(ulamw(nlam), a0(1, nlam), ca(ni, 1, nlam), alm(nlam), dev(nlam))
    allocate(jd(1), ia(ni), nin(nlam))

    xw      = x                  ! copy: lognet destroys (standardizes) its x
    yw(:,1) = y                   ! class-1 indicator (the modelled column)
    yw(:,2) = 1.0_c_double - y    ! class-0 indicator
    gw      = 0.0_c_double        ! no offset
    vp      = 1.0_c_double        ! equal per-predictor penalty factors
    cl(1,:) = -big                ! no lower bound on coefficients
    cl(2,:) =  big                ! no upper bound
    jd(1)   = 0                   ! use all variables
    ulamw   = ulam

    ! nc = 1 (two classes); kopt = 0 is the exact Newton-Raphson Hessian.
    call lognet(alpha, no, ni, 1, xw, yw, gw, jd, vp, cl, ni + 1, ni, nlam, &
         flmin, ulamw, thresh, standardize, intercept, maxit, 0, &
         lmu, a0, ca, ia, nin, dev0, dev, alm, nlp, jerr)

    jerr_out      = jerr
    nlp_out       = nlp
    lmu_out       = 0
    intercept_out = 0.0_c_double
    beta_out      = 0.0_c_double
    dev_ratio_out = 0.0_c_double
    lambda_out    = 0.0_c_double

    if (jerr <= 0) then
       lmu_out = lmu
       do m = 1, lmu
          intercept_out(m) = a0(1, m)
          dev_ratio_out(m) = dev(m)
          lambda_out(m)    = alm(m)
          do k = 1, nin(m)
             beta_out(ia(k), m) = ca(k, 1, m)
          end do
       end do
    end if

    deallocate(xw, yw, gw, vp, cl, ulamw, a0, ca, alm, dev, jd, ia, nin)
  end subroutine glmnet_lognet_path

  ! A single binomial fit: glmnet_lognet_path with nlam = 1.
  subroutine glmnet_lognet_solo(alpha, no, ni, x, y, lambda, &
       standardize, intercept, thresh, maxit, &
       intercept_out, beta_out, dev_ratio_out, lambda_out, nlp_out, jerr_out) &
       bind(C, name="glmnet_lognet_solo")
    real(c_double),    value, intent(in)  :: alpha, lambda, thresh
    integer(c_int),    value, intent(in)  :: no, ni, standardize, intercept, maxit
    real(c_double),           intent(in)  :: x(no, ni), y(no)
    real(c_double),           intent(out) :: intercept_out, beta_out(ni)
    real(c_double),           intent(out) :: dev_ratio_out, lambda_out
    integer(c_int),           intent(out) :: nlp_out, jerr_out

    real(c_double) :: ulam(1), a0(1), dev(1), alm(1)
    real(c_double), allocatable :: beta(:,:)
    integer(c_int) :: lmu

    allocate(beta(ni, 1))
    ulam(1) = lambda
    call glmnet_lognet_path(alpha, no, ni, x, y, 1, 1.0_c_double, ulam, &
         standardize, intercept, thresh, maxit, &
         lmu, a0, beta, dev, alm, nlp_out, jerr_out)
    intercept_out = a0(1)
    beta_out      = beta(:, 1)
    dev_ratio_out = dev(1)
    lambda_out    = alm(1)
  end subroutine glmnet_lognet_solo

  ! --- Multinomial (lognet, K classes) -----------------------------------------
  !
  ! Dense K-class multinomial elastic net over a lambda path: the vendored
  ! `lognet` with nc = K (>= 2), in its symmetric parameterization (K intercepts
  ! and K coefficient columns; probabilities are the softmax of a0_k + x . beta_k).
  !
  !   nc                   : number of classes K
  !   y(no)                : integer class labels 0..K-1 (NOT modified -- copied)
  !   intercept_out(nc,nlam)  : the K intercepts per lambda
  !   beta_out(ni,nc,nlam)    : DENSE coefficients per class per lambda
  !   dev_ratio_out(nlam)     : fraction of null deviance explained
  !   (the other arguments are as for glmnet_elnet_path)
  subroutine glmnet_multinomial_path(alpha, no, ni, nc, x, y, nlam, flmin, ulam, &
       standardize, intercept, thresh, maxit, &
       lmu_out, intercept_out, beta_out, dev_ratio_out, lambda_out, nlp_out, jerr_out) &
       bind(C, name="glmnet_multinomial_path")
    real(c_double),    value, intent(in)  :: alpha, flmin, thresh
    integer(c_int),           intent(in)  :: no, ni, nc, nlam, standardize, intercept, maxit
    real(c_double),           intent(in)  :: x(no, ni), y(no), ulam(nlam)
    integer(c_int),           intent(out) :: lmu_out, nlp_out, jerr_out
    real(c_double),           intent(out) :: intercept_out(nc, nlam), beta_out(ni, nc, nlam)
    real(c_double),           intent(out) :: dev_ratio_out(nlam), lambda_out(nlam)

    external :: lognet

    ! The active set (ia/nin) is SHARED across classes.
    real(c_double), allocatable :: xw(:,:), yw(:,:), gw(:,:), vp(:), cl(:,:)
    real(c_double), allocatable :: ulamw(:), a0(:,:), ca(:,:,:), alm(:), dev(:)
    integer(c_int), allocatable :: jd(:), ia(:), nin(:)
    integer(c_int) :: lmu, nlp, jerr, k, m, ic, l, lbl
    real(c_double) :: dev0

    allocate(xw(no, ni), yw(no, nc), gw(no, nc), vp(ni), cl(2, ni))
    allocate(ulamw(nlam), a0(nc, nlam), ca(ni, nc, nlam), alm(nlam), dev(nlam))
    allocate(jd(1), ia(ni), nin(nlam))

    xw = x                          ! copy: lognet destroys (standardizes) its x
    yw = 0.0_c_double               ! one-hot the integer labels 0..nc-1
    do l = 1, no
       lbl = nint(y(l)) + 1
       yw(l, lbl) = 1.0_c_double
    end do
    gw      = 0.0_c_double          ! no offset
    vp      = 1.0_c_double          ! equal per-predictor penalty factors
    cl(1,:) = -big                  ! no lower bound on coefficients
    cl(2,:) =  big                  ! no upper bound
    jd(1)   = 0                     ! use all variables
    ulamw   = ulam

    ! kopt = 0: exact Newton, ungrouped (R type.multinomial = "ungrouped").
    call lognet(alpha, no, ni, nc, xw, yw, gw, jd, vp, cl, ni + 1, ni, nlam, &
         flmin, ulamw, thresh, standardize, intercept, maxit, 0, &
         lmu, a0, ca, ia, nin, dev0, dev, alm, nlp, jerr)

    jerr_out      = jerr
    nlp_out       = nlp
    lmu_out       = 0
    intercept_out = 0.0_c_double
    beta_out      = 0.0_c_double
    dev_ratio_out = 0.0_c_double
    lambda_out    = 0.0_c_double

    if (jerr <= 0) then
       lmu_out = lmu
       do m = 1, lmu
          intercept_out(:, m) = a0(:, m)
          dev_ratio_out(m)    = dev(m)
          lambda_out(m)       = alm(m)
          do k = 1, nin(m)
             do ic = 1, nc
                beta_out(ia(k), ic, m) = ca(k, ic, m)
             end do
          end do
       end do
    end if

    deallocate(xw, yw, gw, vp, cl, ulamw, a0, ca, alm, dev, jd, ia, nin)
  end subroutine glmnet_multinomial_path

  ! A single multinomial fit: glmnet_multinomial_path with nlam = 1.
  !   beta_out(ni*nc) is CLASS-MAJOR: class ic (1..nc), predictor j (1..ni) lives
  !   at (ic-1)*ni + j.
  subroutine glmnet_multinomial_solo(alpha, no, ni, nc, x, y, lambda, &
       standardize, intercept, thresh, maxit, &
       intercept_out, beta_out, dev_ratio_out, lambda_out, nlp_out, jerr_out) &
       bind(C, name="glmnet_multinomial_solo")
    real(c_double),    value, intent(in)  :: alpha, lambda, thresh
    integer(c_int),    value, intent(in)  :: no, ni, nc, standardize, intercept, maxit
    real(c_double),           intent(in)  :: x(no, ni), y(no)
    real(c_double),           intent(out) :: intercept_out(nc), beta_out(ni*nc)
    real(c_double),           intent(out) :: dev_ratio_out, lambda_out
    integer(c_int),           intent(out) :: nlp_out, jerr_out

    real(c_double) :: ulam(1), dev(1), alm(1)
    real(c_double), allocatable :: a0(:,:), beta(:,:,:)
    integer(c_int) :: lmu

    allocate(a0(nc, 1), beta(ni, nc, 1))
    ulam(1) = lambda
    call glmnet_multinomial_path(alpha, no, ni, nc, x, y, 1, 1.0_c_double, ulam, &
         standardize, intercept, thresh, maxit, &
         lmu, a0, beta, dev, alm, nlp_out, jerr_out)
    intercept_out = a0(:, 1)
    beta_out      = reshape(beta(:, :, 1), [ni * nc])
    dev_ratio_out = dev(1)
    lambda_out    = alm(1)
  end subroutine glmnet_multinomial_solo

  ! --- Cox (coxnet) ------------------------------------------------------------
  !
  ! Dense Cox proportional-hazards elastic net over a lambda path: the vendored
  ! `coxnet`. There is NO intercept (the baseline hazard absorbs it); the fit
  ! models the log relative hazard x . beta.
  !
  !   time(no)             : follow-up time (> 0; NOT modified -- copied)
  !   status(no)           : 1 = event observed, 0 = right-censored
  !   beta_out(ni,nlam)    : DENSE coefficients per lambda
  !   dev_ratio_out(nlam)  : fraction of null partial-likelihood deviance explained
  !   (the other arguments are as for glmnet_elnet_path; there is no intercept)
  subroutine glmnet_coxnet_path(alpha, no, ni, x, time, status, nlam, flmin, ulam, &
       standardize, thresh, maxit, &
       lmu_out, beta_out, dev_ratio_out, lambda_out, nlp_out, jerr_out) &
       bind(C, name="glmnet_coxnet_path")
    real(c_double),    value, intent(in)  :: alpha, flmin, thresh
    integer(c_int),           intent(in)  :: no, ni, nlam, standardize, maxit
    real(c_double),           intent(in)  :: x(no, ni), time(no), status(no), ulam(nlam)
    integer(c_int),           intent(out) :: lmu_out, nlp_out, jerr_out
    real(c_double),           intent(out) :: beta_out(ni, nlam)
    real(c_double),           intent(out) :: dev_ratio_out(nlam), lambda_out(nlam)

    external :: coxnet

    real(c_double), allocatable :: xw(:,:), yw(:), dw(:), gw(:), ww(:), vp(:), cl(:,:)
    real(c_double), allocatable :: ulamw(:), ca(:,:), alm(:), dev(:)
    integer(c_int), allocatable :: jd(:), ia(:), nin(:)
    integer(c_int) :: lmu, nlp, jerr, k, m
    real(c_double) :: dev0

    allocate(xw(no, ni), yw(no), dw(no), gw(no), ww(no), vp(ni), cl(2, ni))
    allocate(ulamw(nlam), ca(ni, nlam), alm(nlam), dev(nlam))
    allocate(jd(1), ia(ni), nin(nlam))

    xw      = x                  ! copy: coxnet standardizes its x in place
    ! R's coxnet wrapper nudges censored times up by 100 machine epsilons, so a
    ! subject censored at an event time stays in that event's risk set. Match
    ! it exactly; without it, tied event/censoring times give R-divergent fits.
    yw      = time + (1.0_c_double - status) * 100.0_c_double * epsilon(1.0_c_double)
    dw      = status             ! 1 = event, 0 = censored
    gw      = 0.0_c_double       ! no offset
    ww      = 1.0_c_double       ! equal observation weights
    vp      = 1.0_c_double       ! equal per-predictor penalty factors
    cl(1,:) = -big               ! no lower bound on coefficients
    cl(2,:) =  big               ! no upper bound
    jd(1)   = 0                  ! use all variables
    ulamw   = ulam

    ! NOTE coxnet's argument order is thr, maxit, isd (no intercept argument).
    call coxnet(alpha, no, ni, xw, yw, dw, gw, ww, jd, vp, cl, ni + 1, ni, nlam, &
         flmin, ulamw, thresh, maxit, standardize, &
         lmu, ca, ia, nin, dev0, dev, alm, nlp, jerr)

    jerr_out      = jerr
    nlp_out       = nlp
    lmu_out       = 0
    beta_out      = 0.0_c_double
    dev_ratio_out = 0.0_c_double
    lambda_out    = 0.0_c_double

    if (jerr <= 0) then
       lmu_out = lmu
       do m = 1, lmu
          dev_ratio_out(m) = dev(m)
          lambda_out(m)    = alm(m)
          do k = 1, nin(m)
             beta_out(ia(k), m) = ca(k, m)
          end do
       end do
    end if

    deallocate(xw, yw, dw, gw, ww, vp, cl, ulamw, ca, alm, dev, jd, ia, nin)
  end subroutine glmnet_coxnet_path

  ! A single Cox fit: glmnet_coxnet_path with nlam = 1.
  subroutine glmnet_coxnet_solo(alpha, no, ni, x, time, status, lambda, &
       standardize, thresh, maxit, &
       beta_out, dev_ratio_out, lambda_out, nlp_out, jerr_out) &
       bind(C, name="glmnet_coxnet_solo")
    real(c_double),    value, intent(in)  :: alpha, lambda, thresh
    integer(c_int),    value, intent(in)  :: no, ni, standardize, maxit
    real(c_double),           intent(in)  :: x(no, ni), time(no), status(no)
    real(c_double),           intent(out) :: beta_out(ni), dev_ratio_out, lambda_out
    integer(c_int),           intent(out) :: nlp_out, jerr_out

    real(c_double) :: ulam(1), dev(1), alm(1)
    real(c_double), allocatable :: beta(:,:)
    integer(c_int) :: lmu

    allocate(beta(ni, 1))
    ulam(1) = lambda
    call glmnet_coxnet_path(alpha, no, ni, x, time, status, 1, 1.0_c_double, ulam, &
         standardize, thresh, maxit, &
         lmu, beta, dev, alm, nlp_out, jerr_out)
    beta_out      = beta(:, 1)
    dev_ratio_out = dev(1)
    lambda_out    = alm(1)
  end subroutine glmnet_coxnet_solo

  ! --- Poisson (fishnet) -------------------------------------------------------
  !
  ! Dense Poisson elastic net over a lambda path: the vendored `fishnet`. Log
  ! link, so the fitted mean is mu = exp(intercept + x . beta).
  !
  !   y(no)                : non-negative counts (NOT modified -- copied)
  !   dev_ratio_out(nlam)  : fraction of null deviance explained
  !   (the other arguments are as for glmnet_elnet_path)
  subroutine glmnet_fishnet_path(alpha, no, ni, x, y, nlam, flmin, ulam, &
       standardize, intercept, thresh, maxit, &
       lmu_out, intercept_out, beta_out, dev_ratio_out, lambda_out, nlp_out, jerr_out) &
       bind(C, name="glmnet_fishnet_path")
    real(c_double),    value, intent(in)  :: alpha, flmin, thresh
    integer(c_int),           intent(in)  :: no, ni, nlam, standardize, intercept, maxit
    real(c_double),           intent(in)  :: x(no, ni), y(no), ulam(nlam)
    integer(c_int),           intent(out) :: lmu_out, nlp_out, jerr_out
    real(c_double),           intent(out) :: intercept_out(nlam), beta_out(ni, nlam)
    real(c_double),           intent(out) :: dev_ratio_out(nlam), lambda_out(nlam)

    external :: fishnet

    real(c_double), allocatable :: xw(:,:), yw(:), gw(:), ww(:), vp(:), cl(:,:)
    real(c_double), allocatable :: ulamw(:), a0(:), ca(:,:), alm(:), dev(:)
    integer(c_int), allocatable :: jd(:), ia(:), nin(:)
    integer(c_int) :: lmu, nlp, jerr, k, m
    real(c_double) :: dev0

    allocate(xw(no, ni), yw(no), gw(no), ww(no), vp(ni), cl(2, ni))
    allocate(ulamw(nlam), a0(nlam), ca(ni, nlam), alm(nlam), dev(nlam))
    allocate(jd(1), ia(ni), nin(nlam))

    xw      = x                  ! copy: fishnet standardizes its x in place
    yw      = y
    gw      = 0.0_c_double       ! no offset
    ww      = 1.0_c_double       ! equal observation weights
    vp      = 1.0_c_double       ! equal per-predictor penalty factors
    cl(1,:) = -big               ! no lower bound on coefficients
    cl(2,:) =  big               ! no upper bound
    jd(1)   = 0                  ! use all variables
    ulamw   = ulam

    call fishnet(alpha, no, ni, xw, yw, gw, ww, jd, vp, cl, ni + 1, ni, nlam, &
         flmin, ulamw, thresh, standardize, intercept, maxit, &
         lmu, a0, ca, ia, nin, dev0, dev, alm, nlp, jerr)

    jerr_out      = jerr
    nlp_out       = nlp
    lmu_out       = 0
    intercept_out = 0.0_c_double
    beta_out      = 0.0_c_double
    dev_ratio_out = 0.0_c_double
    lambda_out    = 0.0_c_double

    if (jerr <= 0) then
       lmu_out = lmu
       do m = 1, lmu
          intercept_out(m) = a0(m)
          dev_ratio_out(m) = dev(m)
          lambda_out(m)    = alm(m)
          do k = 1, nin(m)
             beta_out(ia(k), m) = ca(k, m)
          end do
       end do
    end if

    deallocate(xw, yw, gw, ww, vp, cl, ulamw, a0, ca, alm, dev, jd, ia, nin)
  end subroutine glmnet_fishnet_path

  ! A single Poisson fit: glmnet_fishnet_path with nlam = 1.
  subroutine glmnet_fishnet_solo(alpha, no, ni, x, y, lambda, &
       standardize, intercept, thresh, maxit, &
       intercept_out, beta_out, dev_ratio_out, lambda_out, nlp_out, jerr_out) &
       bind(C, name="glmnet_fishnet_solo")
    real(c_double),    value, intent(in)  :: alpha, lambda, thresh
    integer(c_int),    value, intent(in)  :: no, ni, standardize, intercept, maxit
    real(c_double),           intent(in)  :: x(no, ni), y(no)
    real(c_double),           intent(out) :: intercept_out, beta_out(ni)
    real(c_double),           intent(out) :: dev_ratio_out, lambda_out
    integer(c_int),           intent(out) :: nlp_out, jerr_out

    real(c_double) :: ulam(1), a0(1), dev(1), alm(1)
    real(c_double), allocatable :: beta(:,:)
    integer(c_int) :: lmu

    allocate(beta(ni, 1))
    ulam(1) = lambda
    call glmnet_fishnet_path(alpha, no, ni, x, y, 1, 1.0_c_double, ulam, &
         standardize, intercept, thresh, maxit, &
         lmu, a0, beta, dev, alm, nlp_out, jerr_out)
    intercept_out = a0(1)
    beta_out      = beta(:, 1)
    dev_ratio_out = dev(1)
    lambda_out    = alm(1)
  end subroutine glmnet_fishnet_solo

  ! --- Multi-response Gaussian (multelnet) -------------------------------------
  !
  ! Dense multi-response Gaussian elastic net over a lambda path: the vendored
  ! `multelnet`, a GROUPED lasso across the nr responses (a predictor enters or
  ! leaves for all of them), with one intercept per response.
  !
  !   nr                   : number of responses
  !   y(no,nr)             : column-major response matrix (NOT modified -- copied)
  !   intercept_out(nr,nlam)  : the nr intercepts per lambda
  !   beta_out(ni,nr,nlam)    : DENSE coefficients per response per lambda
  !   rsq_out(nlam)           : fraction of (multi-response) variance explained
  !   (the other arguments are as for glmnet_elnet_path)
  subroutine glmnet_mgaussian_path(alpha, no, ni, nr, x, y, nlam, flmin, ulam, &
       standardize, intercept, thresh, maxit, &
       lmu_out, intercept_out, beta_out, rsq_out, lambda_out, nlp_out, jerr_out) &
       bind(C, name="glmnet_mgaussian_path")
    real(c_double),    value, intent(in)  :: alpha, flmin, thresh
    integer(c_int),           intent(in)  :: no, ni, nr, nlam, standardize, intercept, maxit
    real(c_double),           intent(in)  :: x(no, ni), y(no, nr), ulam(nlam)
    integer(c_int),           intent(out) :: lmu_out, nlp_out, jerr_out
    real(c_double),           intent(out) :: intercept_out(nr, nlam), beta_out(ni, nr, nlam)
    real(c_double),           intent(out) :: rsq_out(nlam), lambda_out(nlam)

    external :: multelnet

    ! The active set (ia/nin) is SHARED across responses.
    real(c_double), allocatable :: xw(:,:), yw(:,:), ww(:), vp(:), cl(:,:)
    real(c_double), allocatable :: ulamw(:), a0(:,:), ca(:,:,:), alm(:), rsq(:)
    integer(c_int), allocatable :: jd(:), ia(:), nin(:)
    integer(c_int) :: lmu, nlp, jerr, k, m, r

    allocate(xw(no, ni), yw(no, nr), ww(no), vp(ni), cl(2, ni))
    allocate(ulamw(nlam), a0(nr, nlam), ca(ni, nr, nlam), alm(nlam), rsq(nlam))
    allocate(jd(1), ia(ni), nin(nlam))

    xw      = x                  ! copy: multelnet standardizes x in place
    yw      = y                  ! copy: multelnet standardizes y in place
    ww      = 1.0_c_double       ! equal observation weights
    vp      = 1.0_c_double       ! equal per-predictor penalty factors
    cl(1,:) = -big               ! no lower bound on coefficients
    cl(2,:) =  big               ! no upper bound
    jd(1)   = 0                  ! use all variables
    ulamw   = ulam

    ! jsd = 0: do NOT standardize the responses (R standardize.response = FALSE).
    call multelnet(alpha, no, ni, nr, xw, yw, ww, jd, vp, cl, ni + 1, ni, nlam, &
         flmin, ulamw, thresh, standardize, 0, intercept, maxit, &
         lmu, a0, ca, ia, nin, rsq, alm, nlp, jerr)

    jerr_out      = jerr
    nlp_out       = nlp
    lmu_out       = 0
    intercept_out = 0.0_c_double
    beta_out      = 0.0_c_double
    rsq_out       = 0.0_c_double
    lambda_out    = 0.0_c_double

    if (jerr <= 0) then
       lmu_out = lmu
       do m = 1, lmu
          intercept_out(:, m) = a0(:, m)
          rsq_out(m)          = rsq(m)
          lambda_out(m)       = alm(m)
          do k = 1, nin(m)
             do r = 1, nr
                beta_out(ia(k), r, m) = ca(k, r, m)
             end do
          end do
       end do
    end if

    deallocate(xw, yw, ww, vp, cl, ulamw, a0, ca, alm, rsq, jd, ia, nin)
  end subroutine glmnet_mgaussian_path

  ! A single multi-response fit: glmnet_mgaussian_path with nlam = 1.
  !   beta_out(ni*nr) is RESPONSE-MAJOR: response r (1..nr), predictor j (1..ni)
  !   lives at (r-1)*ni + j.
  subroutine glmnet_mgaussian_solo(alpha, no, ni, nr, x, y, lambda, &
       standardize, intercept, thresh, maxit, &
       intercept_out, beta_out, rsq_out, lambda_out, nlp_out, jerr_out) &
       bind(C, name="glmnet_mgaussian_solo")
    real(c_double),    value, intent(in)  :: alpha, lambda, thresh
    integer(c_int),    value, intent(in)  :: no, ni, nr, standardize, intercept, maxit
    real(c_double),           intent(in)  :: x(no, ni), y(no, nr)
    real(c_double),           intent(out) :: intercept_out(nr), beta_out(ni*nr)
    real(c_double),           intent(out) :: rsq_out, lambda_out
    integer(c_int),           intent(out) :: nlp_out, jerr_out

    real(c_double) :: ulam(1), rsq(1), alm(1)
    real(c_double), allocatable :: a0(:,:), beta(:,:,:)
    integer(c_int) :: lmu

    allocate(a0(nr, 1), beta(ni, nr, 1))
    ulam(1) = lambda
    call glmnet_mgaussian_path(alpha, no, ni, nr, x, y, 1, 1.0_c_double, ulam, &
         standardize, intercept, thresh, maxit, &
         lmu, a0, beta, rsq, alm, nlp_out, jerr_out)
    intercept_out = a0(:, 1)
    beta_out      = reshape(beta(:, :, 1), [ni * nr])
    rsq_out       = rsq(1)
    lambda_out    = alm(1)
  end subroutine glmnet_mgaussian_solo

end module glmnet_capi
