cmake_minimum_required(VERSION 3.18)
project(glmnetcompat VERSION 0.1.0 LANGUAGES Fortran)

# --- Precision contract ---------------------------------------------------
# glmnet5.f90 declares all arrays as single-precision `real`. Promote default
# real to 8 bytes so the elnet ABI is double precision -- this is what R's
# glmnet and Julia's glmnet_jll do, and what the Racket bindings assume. The
# `glmnet_default_real_bytes` probe asserts this flag took effect.
add_compile_options(-fdefault-real-8 -O2 -fPIC)

# --- Source forms ---------------------------------------------------------
# The vendored glmnet5.f90 is FIXED-FORM (col-1 `c` comments, `*` continuation
# in col 6, sequence numbers in cols 73-80) despite the .f90 name. Force fixed
# form on it; gfortran's default 72-col width ignores the sequence numbers.
# Our shim is modern free-form.
set_source_files_properties(vendor/glmnet5.f90 PROPERTIES Fortran_FORMAT FIXED)
set_source_files_properties(glmnet_capi.f90    PROPERTIES Fortran_FORMAT FREE)

add_library(glmnetcompat SHARED
  vendor/glmnet5.f90
  glmnet_capi.f90)
set_target_properties(glmnetcompat PROPERTIES POSITION_INDEPENDENT_CODE ON)

include(GNUInstallDirs)
install(TARGETS glmnetcompat LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR})

# --- Tests ----------------------------------------------------------------
# Dependency-free standalone Fortran drivers registered with ctest. Each asserts
# numeric results within tolerance and `error stop 1`s on failure.
if(BUILD_TESTING)
  enable_testing()

  add_executable(test_hello tests/test_hello.f90)
  set_source_files_properties(tests/test_hello.f90 PROPERTIES Fortran_FORMAT FREE)
  target_link_libraries(test_hello glmnetcompat)
  add_test(NAME hello COMMAND test_hello)

  add_executable(test_ols tests/test_ols.f90)
  set_source_files_properties(tests/test_ols.f90 PROPERTIES Fortran_FORMAT FREE)
  target_link_libraries(test_ols glmnetcompat)
  add_test(NAME ols COMMAND test_ols)
endif()
