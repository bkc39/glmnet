! glmnet_capi.f90 -- C-ABI shim over the vendored R glmnet Fortran (vendor/*.f).
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

module glmnet_capi
  use, intrinsic :: iso_c_binding
  implicit none
  private
  public :: glmnet_capi_abi_version, glmnet_default_real_bytes
  public :: glmnet_elnet_solo
  public :: glmnet_lognet_solo
  public :: glmnet_multinomial_solo
  public :: glmnet_coxnet_solo
  public :: glmnet_fishnet_solo
  public :: glmnet_mgaussian_solo

  ! glmnet's "+/- infinity" sentinel for unconstrained coefficient bounds.
  real(c_double), parameter :: big = 9.9e35_c_double

contains

  ! ABI version of this shim: the set of bind(C) entry points it exports. Bump on
  ! any change to that set (a new family symbol, a signature change) so the
  ! load-time guard in foreign.rkt rejects a stale prebuilt native candidate.
  !   1 -> elnet only (initial)
  !   2 -> + lognet, multinomial, coxnet, fishnet, mgaussian (all six families)
  integer(c_int) function glmnet_capi_abi_version() &
       bind(C, name="glmnet_capi_abi_version")
    glmnet_capi_abi_version = 2_c_int
  end function glmnet_capi_abi_version

  ! Size in bytes of the Fortran default `real`. MUST be 8 -- i.e. the library
  ! was compiled with the project's precision flags -- or the build does not
  ! match the double-precision Racket bindings.
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
  !                    (codes: R glmnet's R/jerr.R)
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

    ! elnet is an external (non-module) subroutine from vendor/.
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
  !                    (codes: R glmnet's R/jerr.R)
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

    ! lognet is an external (non-module) subroutine from vendor/.
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

  ! Fit a single dense K-class multinomial elastic-net model -- one alpha, one
  ! lambda -- and return DENSE per-class coefficient vectors plus per-class
  ! intercepts. This is the multiclass extension of glmnet_lognet_solo: the same
  ! vendored `lognet`, but with nc = K (>= 2), which routes internally to the
  ! multiclass `lognetn` path.
  !
  ! The response y is an integer class label in 0..K-1 per observation. glmnet
  ! wants an no-by-K indicator matrix; we build it as a one-hot of y. The fit is
  ! the symmetric multinomial parameterization, so there are K intercepts and K
  ! coefficient columns; the predicted probabilities are the softmax over the K
  ! linear predictors  eta_k = a0_k + x . beta_k.
  !
  !   alpha          : elastic-net mixing in [0,1] (0 = ridge, 1 = lasso)
  !   no, ni, nc     : observations, predictors, classes (nc = K >= 2)
  !   x(no,ni)       : column-major predictor matrix (NOT modified -- copied)
  !   y(no)          : integer class labels 0..K-1 (NOT modified -- copied)
  !   lambda         : the single penalty value to fit
  !   standardize    : 1 => standardize predictors (glmnet default), 0 => no
  !   intercept      : 1 => fit intercepts, 0 => no
  !   thresh, maxit  : convergence threshold and max passes
  !
  !   intercept_out(nc)  : the K fitted intercepts (log-odds scale)
  !   beta_out(ni*nc)    : DENSE coefficients, CLASS-MAJOR -- class ic (1..nc)
  !                        predictor j (1..ni) lives at (ic-1)*ni + j
  !   dev_ratio_out      : fraction of null deviance explained (multinomial "R^2")
  !   lambda_out         : the lambda actually used
  !   nlp_out            : number of passes over the data
  !   jerr_out           : 0 ok; >0 fatal (no output); <0 non-fatal partial
  !                        (codes: R glmnet's R/jerr.R)
  subroutine glmnet_multinomial_solo(alpha, no, ni, nc, x, y, lambda, &
       standardize, intercept, thresh, maxit, &
       intercept_out, beta_out, dev_ratio_out, lambda_out, nlp_out, jerr_out) &
       bind(C, name="glmnet_multinomial_solo")
    real(c_double),    value, intent(in)  :: alpha, lambda, thresh
    integer(c_int),    value, intent(in)  :: no, ni, nc, standardize, intercept, maxit
    real(c_double),           intent(in)  :: x(no, ni)
    real(c_double),           intent(in)  :: y(no)
    real(c_double),           intent(out) :: intercept_out(nc)
    real(c_double),           intent(out) :: beta_out(ni*nc)
    real(c_double),           intent(out) :: dev_ratio_out, lambda_out
    integer(c_int),           intent(out) :: nlp_out, jerr_out

    ! lognet is an external (non-module) subroutine from vendor/.
    external :: lognet

    ! Work copies (lognet standardizes x and normalizes y in place) plus the
    ! lognet scratch/output arrays. For the multiclass case a0/ca/dev carry nc
    ! class columns and the active set (ia/nin) is SHARED across classes.
    real(c_double), allocatable :: xw(:,:), yw(:,:), gw(:,:), vp(:), cl(:,:)
    real(c_double), allocatable :: ulam(:), a0(:,:), ca(:,:,:), alm(:), dev(:)
    integer(c_int), allocatable :: jd(:), ia(:), nin(:)
    integer(c_int) :: nlam, isd, intr, kopt, lmu, nlp, jerr, l, ic, lbl
    real(c_double) :: dev0

    nlam = 1            ! single lambda -> "solo" fit
    isd  = standardize
    intr = intercept
    kopt = 0            ! 0 = exact Newton, ungrouped (R type.multinomial="ungrouped")

    allocate(xw(no, ni), yw(no, nc), gw(no, nc), vp(ni), cl(2, ni))
    allocate(ulam(nlam), a0(nc, nlam), ca(ni, nc, nlam), alm(nlam), dev(nlam))
    allocate(jd(1), ia(ni), nin(nlam))

    xw = x                          ! copy: lognet destroys (standardizes) its x
    yw = 0.0_c_double               ! one-hot the integer labels 0..nc-1
    do l = 1, no
       lbl = nint(y(l)) + 1         ! label 0..nc-1 -> column 1..nc
       yw(l, lbl) = 1.0_c_double
    end do
    gw      = 0.0_c_double          ! no offset
    vp      = 1.0_c_double          ! equal per-predictor penalty factors
    cl(1,:) = -big                  ! no lower bound on coefficients
    cl(2,:) =  big                  ! no upper bound
    jd(1)   = 0                     ! use all variables
    ulam(1) = lambda                ! flmin >= 1 => use this supplied lambda

    call lognet(alpha, no, ni, nc, xw, yw, gw, jd, vp, cl, ni + 1, ni, nlam, &
         1.0_c_double, ulam, thresh, isd, intr, maxit, kopt, &
         lmu, a0, ca, ia, nin, dev0, dev, alm, nlp, jerr)

    jerr_out      = jerr
    nlp_out       = nlp
    intercept_out = 0.0_c_double
    beta_out      = 0.0_c_double
    dev_ratio_out = 0.0_c_double
    lambda_out    = 0.0_c_double

    ! jerr > 0 is fatal (no output). Otherwise densify the first (only) solution;
    ! ia(1..nin) is the active set shared by all classes, ca(l,ic,1) its weight.
    if (jerr <= 0 .and. lmu >= 1) then
       intercept_out(1:nc) = a0(1:nc, 1)
       dev_ratio_out       = dev(1)
       lambda_out          = alm(1)
       do l = 1, nin(1)
          do ic = 1, nc
             beta_out((ic - 1) * ni + ia(l)) = ca(l, ic, 1)
          end do
       end do
    end if

    deallocate(xw, yw, gw, vp, cl, ulam, a0, ca, alm, dev, jd, ia, nin)
  end subroutine glmnet_multinomial_solo

  ! Fit a single dense Cox proportional-hazards elastic-net model -- one alpha,
  ! one lambda -- and return a DENSE coefficient vector. One call to the vendored
  ! `coxnet` with nlam = 1.
  !
  ! Cox is a survival model: there is NO intercept (the baseline hazard is left
  ! unspecified and absorbs it), so this returns coefficients only. The response
  ! is a follow-up time plus a 0/1 event indicator. The fit models the log relative
  ! hazard  x . beta  (a positive coefficient => higher hazard / shorter survival).
  !
  !   alpha          : elastic-net mixing in [0,1] (0 = ridge, 1 = lasso)
  !   no, ni         : observations, predictors
  !   x(no,ni)       : column-major predictor matrix (NOT modified -- copied)
  !   time(no)       : follow-up / survival time (> 0; NOT modified -- copied)
  !   status(no)     : 1 = event observed, 0 = right-censored (NOT modified -- copied)
  !   lambda         : the single penalty value to fit
  !   standardize    : 1 => standardize predictors (glmnet default), 0 => no
  !   thresh, maxit  : convergence threshold and max passes
  !
  !   beta_out(ni)   : DENSE coefficients on the original predictor scale
  !   dev_ratio_out  : fraction of null deviance explained (Cox partial-likelihood)
  !   lambda_out     : the lambda actually used
  !   nlp_out        : number of passes over the data
  !   jerr_out       : 0 ok; >0 fatal (no output); <0 non-fatal partial
  !                    (codes: R glmnet's R/jerr.R)
  subroutine glmnet_coxnet_solo(alpha, no, ni, x, time, status, lambda, &
       standardize, thresh, maxit, &
       beta_out, dev_ratio_out, lambda_out, nlp_out, jerr_out) &
       bind(C, name="glmnet_coxnet_solo")
    real(c_double),    value, intent(in)  :: alpha, lambda, thresh
    integer(c_int),    value, intent(in)  :: no, ni, standardize, maxit
    real(c_double),           intent(in)  :: x(no, ni)
    real(c_double),           intent(in)  :: time(no)
    real(c_double),           intent(in)  :: status(no)
    real(c_double),           intent(out) :: beta_out(ni)
    real(c_double),           intent(out) :: dev_ratio_out, lambda_out
    integer(c_int),           intent(out) :: nlp_out, jerr_out

    ! coxnet is an external (non-module) subroutine from vendor/.
    external :: coxnet

    ! Work copies (coxnet standardizes x in place) plus the coxnet scratch/output
    ! arrays. There is no a0 -- Cox has no intercept -- so ca is 2-D (nx, nlam).
    real(c_double), allocatable :: xw(:,:), yw(:), dw(:), gw(:), ww(:), vp(:), cl(:,:)
    real(c_double), allocatable :: ulam(:), ca(:,:), alm(:), dev(:)
    integer(c_int), allocatable :: jd(:), ia(:), nin(:)
    integer(c_int) :: nlam, isd, lmu, nlp, jerr, l
    real(c_double) :: dev0

    nlam = 1            ! single lambda -> "solo" fit
    isd  = standardize

    allocate(xw(no, ni), yw(no), dw(no), gw(no), ww(no), vp(ni), cl(2, ni))
    allocate(ulam(nlam), ca(ni, nlam), alm(nlam), dev(nlam))
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
    ulam(1) = lambda             ! flmin >= 1 => use this supplied lambda

    ! NOTE coxnet's argument order is thr, maxit, isd (no intercept argument).
    call coxnet(alpha, no, ni, xw, yw, dw, gw, ww, jd, vp, cl, ni + 1, ni, nlam, &
         1.0_c_double, ulam, thresh, maxit, isd, &
         lmu, ca, ia, nin, dev0, dev, alm, nlp, jerr)

    jerr_out      = jerr
    nlp_out       = nlp
    beta_out      = 0.0_c_double
    dev_ratio_out = 0.0_c_double
    lambda_out    = 0.0_c_double

    ! jerr > 0 is fatal (no output). Otherwise densify the first (only) solution.
    if (jerr <= 0 .and. lmu >= 1) then
       dev_ratio_out = dev(1)
       lambda_out    = alm(1)
       do l = 1, nin(1)
          beta_out(ia(l)) = ca(l, 1)
       end do
    end if

    deallocate(xw, yw, dw, gw, ww, vp, cl, ulam, ca, alm, dev, jd, ia, nin)
  end subroutine glmnet_coxnet_solo

  ! Fit a single dense Poisson elastic-net model -- one alpha, one lambda -- and
  ! return a DENSE coefficient vector plus the fitted intercept. One call to the
  ! vendored `fishnet` with nlam = 1.
  !
  ! The response y is a non-negative count per observation; the model uses a log
  ! link, so the fitted mean is  mu = exp(intercept + x . beta). A positive
  ! coefficient raises the expected count.
  !
  !   alpha          : elastic-net mixing in [0,1] (0 = ridge, 1 = lasso)
  !   no, ni         : observations, predictors
  !   x(no,ni)       : column-major predictor matrix (NOT modified -- copied)
  !   y(no)          : non-negative counts (NOT modified -- copied)
  !   lambda         : the single penalty value to fit
  !   standardize    : 1 => standardize predictors (glmnet default), 0 => no
  !   intercept      : 1 => fit an intercept, 0 => no
  !   thresh, maxit  : convergence threshold and max passes
  !
  !   intercept_out  : fitted intercept (log-mean scale)
  !   beta_out(ni)   : DENSE coefficients on the original predictor scale
  !   dev_ratio_out  : fraction of null deviance explained (Poisson "R^2")
  !   lambda_out     : the lambda actually used
  !   nlp_out        : number of passes over the data
  !   jerr_out       : 0 ok; >0 fatal (no output); <0 non-fatal partial
  !                    (codes: R glmnet's R/jerr.R)
  subroutine glmnet_fishnet_solo(alpha, no, ni, x, y, lambda, &
       standardize, intercept, thresh, maxit, &
       intercept_out, beta_out, dev_ratio_out, lambda_out, nlp_out, jerr_out) &
       bind(C, name="glmnet_fishnet_solo")
    real(c_double),    value, intent(in)  :: alpha, lambda, thresh
    integer(c_int),    value, intent(in)  :: no, ni, standardize, intercept, maxit
    real(c_double),           intent(in)  :: x(no, ni)
    real(c_double),           intent(in)  :: y(no)
    real(c_double),           intent(out) :: intercept_out
    real(c_double),           intent(out) :: beta_out(ni)
    real(c_double),           intent(out) :: dev_ratio_out, lambda_out
    integer(c_int),           intent(out) :: nlp_out, jerr_out

    ! fishnet is an external (non-module) subroutine from vendor/.
    external :: fishnet

    ! Work copies (fishnet standardizes x in place) plus the fishnet scratch/output
    ! arrays. Like elnet, a0 is a scalar intercept per lambda and ca is 2-D.
    real(c_double), allocatable :: xw(:,:), yw(:), gw(:), ww(:), vp(:), cl(:,:)
    real(c_double), allocatable :: ulam(:), a0(:), ca(:,:), alm(:), dev(:)
    integer(c_int), allocatable :: jd(:), ia(:), nin(:)
    integer(c_int) :: nlam, isd, intr, lmu, nlp, jerr, l
    real(c_double) :: dev0

    nlam = 1            ! single lambda -> "solo" fit
    isd  = standardize
    intr = intercept

    allocate(xw(no, ni), yw(no), gw(no), ww(no), vp(ni), cl(2, ni))
    allocate(ulam(nlam), a0(nlam), ca(ni, nlam), alm(nlam), dev(nlam))
    allocate(jd(1), ia(ni), nin(nlam))

    xw      = x                  ! copy: fishnet standardizes its x in place
    yw      = y                  ! non-negative counts
    gw      = 0.0_c_double       ! no offset
    ww      = 1.0_c_double       ! equal observation weights
    vp      = 1.0_c_double       ! equal per-predictor penalty factors
    cl(1,:) = -big               ! no lower bound on coefficients
    cl(2,:) =  big               ! no upper bound
    jd(1)   = 0                  ! use all variables
    ulam(1) = lambda             ! flmin >= 1 => use this supplied lambda

    call fishnet(alpha, no, ni, xw, yw, gw, ww, jd, vp, cl, ni + 1, ni, nlam, &
         1.0_c_double, ulam, thresh, isd, intr, maxit, &
         lmu, a0, ca, ia, nin, dev0, dev, alm, nlp, jerr)

    jerr_out      = jerr
    nlp_out       = nlp
    intercept_out = 0.0_c_double
    beta_out      = 0.0_c_double
    dev_ratio_out = 0.0_c_double
    lambda_out    = 0.0_c_double

    ! jerr > 0 is fatal (no output). Otherwise densify the first (only) solution.
    if (jerr <= 0 .and. lmu >= 1) then
       intercept_out = a0(1)
       dev_ratio_out = dev(1)
       lambda_out    = alm(1)
       do l = 1, nin(1)
          beta_out(ia(l)) = ca(l, 1)
       end do
    end if

    deallocate(xw, yw, gw, ww, vp, cl, ulam, a0, ca, alm, dev, jd, ia, nin)
  end subroutine glmnet_fishnet_solo

  ! Fit a single dense multi-response Gaussian ("mgaussian") elastic-net model --
  ! one alpha, one lambda -- and return DENSE per-response coefficient vectors plus
  ! per-response intercepts. One call to the vendored `multelnet` with nlam = 1.
  !
  ! The response is an no-by-nr matrix (nr response columns). multelnet applies a
  ! GROUPED lasso across the responses: a predictor enters or leaves for all nr
  ! responses together, so the active set (ia/nin) is shared and the coefficient
  ! matrix has shared row support. Each response gets its own intercept; this is a
  ! plain (identity-link) Gaussian fit, so prediction is  y_r = a0_r + x . beta_r.
  !
  !   alpha          : elastic-net mixing in [0,1] (0 = ridge, 1 = grouped lasso)
  !   no, ni, nr     : observations, predictors, responses
  !   x(no,ni)       : column-major predictor matrix (NOT modified -- copied)
  !   y(no,nr)       : column-major response matrix (NOT modified -- copied)
  !   lambda         : the single penalty value to fit
  !   standardize    : 1 => standardize predictors (glmnet default), 0 => no
  !   intercept      : 1 => fit intercepts, 0 => no
  !   thresh, maxit  : convergence threshold and max passes
  !
  !   intercept_out(nr)  : the nr fitted intercepts
  !   beta_out(ni*nr)    : DENSE coefficients, RESPONSE-MAJOR -- response r (1..nr)
  !                        predictor j (1..ni) lives at (r-1)*ni + j
  !   rsq_out            : fraction of (multi-response) variance explained (R^2)
  !   lambda_out         : the lambda actually used
  !   nlp_out            : number of passes over the data
  !   jerr_out           : 0 ok; >0 fatal (no output); <0 non-fatal partial
  !                        (codes: R glmnet's R/jerr.R)
  subroutine glmnet_mgaussian_solo(alpha, no, ni, nr, x, y, lambda, &
       standardize, intercept, thresh, maxit, &
       intercept_out, beta_out, rsq_out, lambda_out, nlp_out, jerr_out) &
       bind(C, name="glmnet_mgaussian_solo")
    real(c_double),    value, intent(in)  :: alpha, lambda, thresh
    integer(c_int),    value, intent(in)  :: no, ni, nr, standardize, intercept, maxit
    real(c_double),           intent(in)  :: x(no, ni)
    real(c_double),           intent(in)  :: y(no, nr)
    real(c_double),           intent(out) :: intercept_out(nr)
    real(c_double),           intent(out) :: beta_out(ni*nr)
    real(c_double),           intent(out) :: rsq_out, lambda_out
    integer(c_int),           intent(out) :: nlp_out, jerr_out

    ! multelnet is an external (non-module) subroutine from vendor/.
    external :: multelnet

    ! Work copies (multelnet standardizes x and y in place) plus the multelnet
    ! scratch/output arrays. a0/ca carry nr response columns; the active set
    ! (ia/nin) is SHARED across responses.
    real(c_double), allocatable :: xw(:,:), yw(:,:), ww(:), vp(:), cl(:,:)
    real(c_double), allocatable :: ulam(:), a0(:,:), ca(:,:,:), alm(:), rsq(:)
    integer(c_int), allocatable :: jd(:), ia(:), nin(:)
    integer(c_int) :: nlam, isd, jsd, intr, lmu, nlp, jerr, l, r

    nlam = 1            ! single lambda -> "solo" fit
    isd  = standardize
    jsd  = 0            ! do NOT standardize the responses (R standardize.response=FALSE)
    intr = intercept

    allocate(xw(no, ni), yw(no, nr), ww(no), vp(ni), cl(2, ni))
    allocate(ulam(nlam), a0(nr, nlam), ca(ni, nr, nlam), alm(nlam), rsq(nlam))
    allocate(jd(1), ia(ni), nin(nlam))

    xw      = x                  ! copy: multelnet standardizes x in place
    yw      = y                  ! copy: multelnet standardizes y in place
    ww      = 1.0_c_double       ! equal observation weights
    vp      = 1.0_c_double       ! equal per-predictor penalty factors
    cl(1,:) = -big               ! no lower bound on coefficients
    cl(2,:) =  big               ! no upper bound
    jd(1)   = 0                  ! use all variables
    ulam(1) = lambda             ! flmin >= 1 => use this supplied lambda

    call multelnet(alpha, no, ni, nr, xw, yw, ww, jd, vp, cl, ni + 1, ni, nlam, &
         1.0_c_double, ulam, thresh, isd, jsd, intr, maxit, &
         lmu, a0, ca, ia, nin, rsq, alm, nlp, jerr)

    jerr_out      = jerr
    nlp_out       = nlp
    intercept_out = 0.0_c_double
    beta_out      = 0.0_c_double
    rsq_out       = 0.0_c_double
    lambda_out    = 0.0_c_double

    ! jerr > 0 is fatal (no output). Otherwise densify the first (only) solution;
    ! ia(1..nin) is the active set shared by all responses, ca(l,r,1) its weight.
    if (jerr <= 0 .and. lmu >= 1) then
       intercept_out(1:nr) = a0(1:nr, 1)
       rsq_out             = rsq(1)
       lambda_out          = alm(1)
       do l = 1, nin(1)
          do r = 1, nr
             beta_out((r - 1) * ni + ia(l)) = ca(l, r, 1)
          end do
       end do
    end if

    deallocate(xw, yw, ww, vp, cl, ulam, a0, ca, alm, rsq, jd, ia, nin)
  end subroutine glmnet_mgaussian_solo

end module glmnet_capi
