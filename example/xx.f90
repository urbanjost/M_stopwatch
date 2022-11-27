program testit
use,intrinsic :: iso_fortran_env, only : int8, int16, int32, int64
use M_stopwatch, only : tic, toc
integer(kind=int64) :: clicks(2)
a=0.0
      call tic()
      call system_clock(clicks(1))
      do i=1,10000
         a=a+sqrt(real(i))
      enddo
      call system_clock(clicks(2))
      call toc()
      write(*,*)'clicks:',clicks(2)-clicks(1)
      write(*,*)a
end program testit
