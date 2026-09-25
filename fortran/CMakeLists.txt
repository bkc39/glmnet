cmake_minimum_required(VERSION 3.18)
project(glmnetcompat VERSION 0.1.0 LANGUAGES Fortran)

# --- Precision contract ---------------------------------------------------
# R glmnet's Fortran (vendor/*.f) is explicitly DOUBLE PRECISION. Default real
# is promoted to 8 bytes too, and -fdefault-double-8 keeps DOUBLE PRECISION at 8
# bytes, which -fdefault-real-8 alone would widen to 16. The
# `glmnet_default_real_bytes` probe and tests/test_precision.f90 assert both.
add_compile_options(-fdefault-real-8 -fdefault-double-8 -O2 -fPIC)

# --- Source forms ---------------------------------------------------------
# The vendored R Fortran is FIXED-FORM (col-1 `c` comments, `*` continuation in
# col 6, sequence numbers in cols 73-80); gfortran's default 72-col width ignores
# the sequence numbers. Our shim and the setpb stub are modern free-form.
set_source_files_properties(vendor/glmnet5dpclean.f vendor/coxnet5dpclean.f
                            PROPERTIES Fortran_FORMAT FIXED)
set_source_files_properties(r_stubs.f90 PROPERTIES Fortran_FORMAT FREE)
set_source_files_properties(glmnet_capi.f90    PROPERTIES Fortran_FORMAT FREE)

add_library(glmnetcompat SHARED
  vendor/glmnet5dpclean.f
  vendor/coxnet5dpclean.f
  r_stubs.f90
  glmnet_capi.f90)
set_target_properties(glmnetcompat PROPERTIES POSITION_INDEPENDENT_CODE ON)

include(GNUInstallDirs)
install(TARGETS glmnetcompat LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR})

# --- Tests ----------------------------------------------------------------
# Dependency-free standalone Fortran drivers registered with ctest. Each asserts
# numeric results within tolerance and `error stop 1`s on failure.
if(BUILD_TESTING)
  enable_testing()

  add_executable(test_precision tests/test_precision.f90)
  set_source_files_properties(tests/test_precision.f90 PROPERTIES Fortran_FORMAT FREE)
  target_link_libraries(test_precision glmnetcompat)
  add_test(NAME precision COMMAND test_precision)

  add_executable(test_ols tests/test_ols.f90)
  set_source_files_properties(tests/test_ols.f90 PROPERTIES Fortran_FORMAT FREE)
  target_link_libraries(test_ols glmnetcompat)
  add_test(NAME ols COMMAND test_ols)

  add_executable(test_ridge tests/test_ridge.f90)
  set_source_files_properties(tests/test_ridge.f90 PROPERTIES Fortran_FORMAT FREE)
  target_link_libraries(test_ridge glmnetcompat)
  add_test(NAME ridge COMMAND test_ridge)

  add_executable(test_lasso tests/test_lasso.f90)
  set_source_files_properties(tests/test_lasso.f90 PROPERTIES Fortran_FORMAT FREE)
  target_link_libraries(test_lasso glmnetcompat)
  add_test(NAME lasso COMMAND test_lasso)

  add_executable(test_enet tests/test_enet.f90)
  set_source_files_properties(tests/test_enet.f90 PROPERTIES Fortran_FORMAT FREE)
  target_link_libraries(test_enet glmnetcompat)
  add_test(NAME enet COMMAND test_enet)

  add_executable(test_binomial tests/test_binomial.f90)
  set_source_files_properties(tests/test_binomial.f90 PROPERTIES Fortran_FORMAT FREE)
  target_link_libraries(test_binomial glmnetcompat)
  add_test(NAME binomial COMMAND test_binomial)

  add_executable(test_multinomial tests/test_multinomial.f90)
  set_source_files_properties(tests/test_multinomial.f90 PROPERTIES Fortran_FORMAT FREE)
  target_link_libraries(test_multinomial glmnetcompat)
  add_test(NAME multinomial COMMAND test_multinomial)

  add_executable(test_cox tests/test_cox.f90)
  set_source_files_properties(tests/test_cox.f90 PROPERTIES Fortran_FORMAT FREE)
  target_link_libraries(test_cox glmnetcompat)
  add_test(NAME cox COMMAND test_cox)

  add_executable(test_poisson tests/test_poisson.f90)
  set_source_files_properties(tests/test_poisson.f90 PROPERTIES Fortran_FORMAT FREE)
  target_link_libraries(test_poisson glmnetcompat)
  add_test(NAME poisson COMMAND test_poisson)

  add_executable(test_mgaussian tests/test_mgaussian.f90)
  set_source_files_properties(tests/test_mgaussian.f90 PROPERTIES Fortran_FORMAT FREE)
  target_link_libraries(test_mgaussian glmnetcompat)
  add_test(NAME mgaussian COMMAND test_mgaussian)

  add_executable(test_no_intercept tests/test_no_intercept.f90)
  set_source_files_properties(tests/test_no_intercept.f90 PROPERTIES Fortran_FORMAT FREE)
  target_link_libraries(test_no_intercept glmnetcompat)
  add_test(NAME no_intercept COMMAND test_no_intercept)
endif()
