
! This program illustrates using tic(3f) and toc(3f) when you 
! want just one quick clock

program simple
use M_stopwatch,only : tic,toc  
implicit none
integer          :: i
real             :: value

call tic() ! create and start default clock

! code to be timed would be located here
   value=0.0
   do i=1,1000000
      value=sqrt(real(i)+value)
      write(10,*)value
   enddo
   flush(10)
   write(*,*)'average sqrt value=',value/10000.0

call toc() ! stop, print and delete default clock

stop
end program simple
