! This program illustrates some of the advanced features of StopWatch.

! In this program, there are two phases that are performed inside a loop.
! (An application of this form is a partial differential equation solver
! with adaptive refinement, which alternates between refining the grid and
! solving the PDE.)  In this example, the two phases just perform a different
! number of multiplications.

! The following times are measured: each phase each time through the loop;
! total time for each phase; the total time.  There will be printed output
! that should not be included in the measured time.

module globals_1
use M_stopwatch
implicit none
private
public :: watchgroup
type (watchgroup), public :: g3 ! the group with all the watches; global var
end module globals_1

module workers_1
implicit none
! The routines being measured

public :: phases
private :: mult
contains

subroutine phases(n)
use M_stopwatch
use globals_1

! This subroutine performs n multiplications, just to give us something
! to time.  The multiplications are performed in another subroutine so that
! they won't get optimized away.  The watch group g is the group of all
! watches.  This is needed in order to put a pause_watch around the print
! statement (so that printing is not included in the timing measurements)
! because we don't know which watches are running when inside this routine.

integer, intent(in) :: n

integer :: i
real :: a,b,c
a=2.0
b=real(n)

do i=1,n
   call mult(a,b,c)
end do
a=c ! just to fool elf90 into believing that c is used
call pause_watch(g3)
write(unit=*,fmt=*) "Performed ",n," multiplications"
call end_pause_watch(g3)

end subroutine phases

subroutine mult(x,y,z)
real, intent(in) :: x,y
real, intent(out) :: z
z=x*y
end subroutine mult

end module workers_1

program advanced
use M_stopwatch
use globals_1
use workers_1
implicit none

! The watches are: w(1) time for phase 1 this time through the loop
!                  w(2) time for phase 2 this time through the loop
!                  w(3) total time for phase 1
!                  w(4) total time for phase 2
!                  w(5) total time
type (watchtype), dimension(5) :: w

! The watch groups are: g1 phase 1 times w(1) and w(3)
!                       g2 phase 2 times w(2) and w(4)
!                       g3 all of them (declared in module globals)
type (watchgroup) :: g1, g2

! loop counter, number of multiplies to do, flag for cpu clock
integer :: i, nmult
logical :: cpu_is_there

! Measure only cpu and wall time

call option_stopwatch(default_clock=(/"cpu ","wall"/))

! create the watches

call create_watch(w,name=(/ "phase 1      ", &
                            "phase 2      ", &
                            "total phase 1", &
                            "total phase 2", &
                            "Total        " /) )

! create the groups

call create_watchgroup(w(1),g1)
call join_watchgroup(w(3),g1)
call create_watchgroup(w(2:4:2),g2) ! a shorter way
call create_watchgroup(w,g3)

! start the total time

call start_watch(w(5))

! loop 3 times

nmult = 200000
i=0
! using simulated while instead of do i=1,3 because of a compiler bug
do
   if (i >= 3) then
      exit
   end if
   i = i+1

! reset the watches that measure the time for this loop

   call reset_watch(w(1:2))

! start the phase 1 watches, do phase 1, and stop the phase 1 watches

   call start_watch(g1)
   nmult = 5*nmult
   call phases(nmult)
   call stop_watch(g1)

! same for phase 2

   call start_watch(g2)
   nmult = 2*nmult
   call phases(nmult)
   call stop_watch(g2)

! pause the cpu clock of the total time watch while printing the current times,
! if the cpu clock is available on this implementation, but leave the wall
! clock running.  The call to inquiry_stopwatch should be outside the loop, but
! this should make a clearer illustration.

   call inquiry_stopwatch(cpu_avail=cpu_is_there)
   if (cpu_is_there) then
      call pause_watch(w(5),"cpu")
   end if

   call print_watch(w(1:2),title="Times for this loop")
   call print_watch(w(3:4),title="Total times so far")

   if (cpu_is_there) then
      call end_pause_watch(w(5),"cpu")
   end if

! end of loop
end do

! print the total times

call print_watch((/w(3),w(4),w(5)/),title="Final total times")

write(unit=*,fmt=*)"Note: the difference between the sum of the first two wall clocks"
write(unit=*,fmt=*)"      and the Total wall clock is due to not pausing the wall clock"
write(unit=*,fmt=*)"      on the Total watch while printing."

! destroy the watches

call destroy_watch(w)

stop
end program advanced

