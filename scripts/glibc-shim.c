/* libglmnetshim.so — companion library bundled with the glmnet native lib on
 * Linux.
 *
 * Provides definitions for glibc symbols that polyfill-glibc cannot rewrite
 * down to the glibc 2.17 baseline.  By redirecting the bundled libraries to
 * resolve these symbols here (via --rename-dynamic-symbols, see
 * scripts/glibc-renames.txt), the package loads cleanly on hosts whose system
 * libc is older than the Nix toolchain's (e.g. pkg-build.racket-lang.org and
 * Ubuntu 22.04, glibc 2.35; the baseline below covers far older still).
 *
 * The only library that needs help is libgfortran.so.5: nixpkgs' gfortran is
 * built against glibc >= 2.26 and imports glibc's _Float128 (quad-precision)
 * math functions, which have no pre-2.26 equivalent for polyfill-glibc to
 * redirect.  glmnet is compiled with -fdefault-real-8, i.e. it solves in
 * 8-byte doubles, so libgfortran's quad paths are never reached; these stubs
 * abort() if ever called, which is behaviourally safe for our use and turns a
 * silent load-time failure on old glibc into an explicit one should that
 * assumption ever break.
 *
 * Built against only the glibc baseline, so the shim itself depends on nothing
 * newer than GLIBC_2.2.5.
 */

#include <stdlib.h>

/* Bind-by-name stub: the dynamic linker resolves the reference by symbol name
 * only, so a no-argument function is ABI-safe here — it never returns. */
#define GLMNET_F128_STUB(name) \
    __attribute__((visibility("default"))) void name(void) { abort(); }

GLMNET_F128_STUB(acosf128)
GLMNET_F128_STUB(acoshf128)
GLMNET_F128_STUB(asinf128)
GLMNET_F128_STUB(asinhf128)
GLMNET_F128_STUB(atan2f128)
GLMNET_F128_STUB(atanf128)
GLMNET_F128_STUB(atanhf128)
GLMNET_F128_STUB(cabsf128)
GLMNET_F128_STUB(ccosf128)
GLMNET_F128_STUB(cexpf128)
GLMNET_F128_STUB(clogf128)
GLMNET_F128_STUB(copysignf128)
GLMNET_F128_STUB(cosf128)
GLMNET_F128_STUB(coshf128)
GLMNET_F128_STUB(csinf128)
GLMNET_F128_STUB(csqrtf128)
GLMNET_F128_STUB(erfcf128)
GLMNET_F128_STUB(expf128)
GLMNET_F128_STUB(fmaf128)
GLMNET_F128_STUB(fmodf128)
GLMNET_F128_STUB(jnf128)
GLMNET_F128_STUB(log10f128)
GLMNET_F128_STUB(logf128)
GLMNET_F128_STUB(lroundf128)
GLMNET_F128_STUB(roundf128)
GLMNET_F128_STUB(sinf128)
GLMNET_F128_STUB(sinhf128)
GLMNET_F128_STUB(sqrtf128)
GLMNET_F128_STUB(strfromf128)
GLMNET_F128_STUB(strtof128)
GLMNET_F128_STUB(tanf128)
GLMNET_F128_STUB(tanhf128)
GLMNET_F128_STUB(truncf128)
GLMNET_F128_STUB(ynf128)
