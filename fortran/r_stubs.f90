! R glmnet's Fortran reports progress through setpb, which R defines in C. It is
! only called when itrace is nonzero (the default is 0), so outside R it is a
! no-op that exists to satisfy the linker.
subroutine setpb(k)
  implicit none
  integer, intent(in) :: k
end subroutine setpb
