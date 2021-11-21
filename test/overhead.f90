module nothing
implicit none

public :: do_nothing, do_nothing_a, do_nothing_g

contains

subroutine do_nothing()
return
end subroutine do_nothing

subroutine do_nothing_a()
return
end subroutine do_nothing_a

subroutine do_nothing_g()
return
end subroutine do_nothing_g

end module nothing

program overhead

!----------------------------------------------------
! This program measures the execution time of the StopWatch subroutines.
! This can be used to determine if the process of measuring the time will
! seriously affect the execution time.  As long as the execution time of
! the StopWatch subroutines is significantly less than the segments of time
! being measured (which must be larger than the clock precision to be
! reliable), there is no interference.
!
! Set the parameter "detailed" to .false. to get just the time of each
! routine (probably what you want to do), or to .true. to get a detailed
! report of the timing (may be useful if something goes wrong).
!----------------------------------------------------

use M_stopwatch
use nothing
implicit none

logical, parameter :: detailed=.false.   ! <--- SET DETAILED HERE
real :: cpu,wall
character(len=4) :: clock
logical :: cpu_avail,wall_avail
type (watchtype) :: measure, & ! used to measure the time of the calls
                    dummy      ! used to pass to the routines being called
type (watchtype), dimension(10) :: &
                    dum        ! used to pass array and group to routines
                               ! being called
type (watchgroup) :: agroup
integer :: ncalls,i
real :: t1,t2,t3

!             Print clock precisions

write(unit=*,fmt=*)
write(unit=*,fmt=*) " ------------------ Clock precisions -----------------"
write(unit=*,fmt=*)

call inquiry_stopwatch(cpu_prec=cpu, wall_prec=wall)

write(unit=*,fmt=*) " This is the smallest time interval that the cpu and wall clocks can"
write(unit=*,fmt=*) " measure, in seconds.  A value of 0. indicates that that clock is not"
write(unit=*,fmt=*) " available."
write(unit=*,fmt=*)
write(unit=*,fmt=*) " CPU  clock precision = ",cpu
write(unit=*,fmt=*) " wall clock precision = ",wall
write(unit=*,fmt=*)

!              Timing measurements             

write(unit=*,fmt=*)
write(unit=*,fmt=*)
write(unit=*,fmt=*) " ------------------ Timing measurements -----------------"
write(unit=*,fmt=*)

! Use the cpu clock if it's available, and the wall clock otherwise.

call inquiry_stopwatch(cpu_avail=cpu_avail,wall_avail=wall_avail)
if (cpu_avail) then
   clock = "cpu "
else if (wall_avail) then
   clock = "wall"
else
   write(unit=*,fmt=*) " Neither cpu nor wall clock available.  Quitting."
   stop
end if
call option_stopwatch(default_clock=clock)

write(unit=*,fmt=*) " All times are ",clock," clock in seconds."
write(unit=*,fmt=*)
write(unit=*,fmt=*) " The following calls to StopWatch routines are with a single watch"
write(unit=*,fmt=*) " and with default clock, which is the single clock '",clock,"'."
write(unit=*,fmt=*)

!----------------------------------------------------
! measure the time to create and destroy watches

if (detailed) then
  write(unit=*,fmt=*)
  write(unit=*,fmt=*) "Measure time for create_watch and destroy_watch"
end if
call create_watch(measure)

! find a number of calls that take a significant amount of time
ncalls = 100
do
   call reset_watch(measure)
   call start_watch(measure)
   do i=1,ncalls
      call create_watch(dummy)
      call destroy_watch(dummy)
   end do
   call stop_watch(measure)
   call read_watch(t1,measure,clock)
   if (t1>1.0) then
     exit
   end if
   if (detailed) then
     write(unit=*,fmt=*) "the time for ",ncalls," calls to create and destroy",t1
   end if
   ncalls=10*ncalls
end do

! determine the overhead of the loop and calls (hope this doesn't
! get optimized away)

call create_watch(dummy)
call reset_watch(measure)
call start_watch(measure)
do i=1,ncalls
   call do_nothing()
end do
call stop_watch(measure)
call read_watch(t2,measure,clock)

if (detailed) then
  write(unit=*,fmt=*) "the time for ",ncalls," calls to create and destroy is ",t1
  write(unit=*,fmt=*) "the time for ",ncalls," calls to do_nothing is     ",t2
end if
write(unit=*,fmt=*) "average time for a call to create_watch or destroy_watch is ",(t1-t2)/(2*ncalls)

!----------------------------------------------------
! measure the time to start/stop watches

if (detailed) then
  write(unit=*,fmt=*)
  write(unit=*,fmt=*) "Measure time for start_watch and stop_watch"
end if

! find a number of calls that take a significant amount of time
ncalls = 100
do
   call reset_watch(measure)
   call start_watch(measure)
   do i=1,ncalls
      call start_watch(dummy)
      call stop_watch(dummy)
   end do
   call stop_watch(measure)
   call read_watch(t1,measure,clock)
   if (t1>1.0) then
     exit
   end if
   if (detailed) then
     write(unit=*,fmt=*) "the time for ",ncalls," calls to start and stop is ",t1
   end if
   ncalls=10*ncalls
end do

! determine the overhead of the loop and calls (hope this doesn't
! get optimized away)

call reset_watch(measure)
call start_watch(measure)
do i=1,ncalls
   call do_nothing()
   call do_nothing()
end do
call stop_watch(measure)
call read_watch(t2,measure,clock)

if (detailed) then
   write(unit=*,fmt=*) "the time for ",ncalls," calls to start and stop is ",t1
   write(unit=*,fmt=*) "the time for ",ncalls," calls to do_nothing is     ",t2
end if
write(unit=*,fmt=*) "average time for a call to start_watch or stop_watch is ",&
         (t1-t2)/(2*ncalls)

!----------------------------------------------------
! measure the time to reset watches

if (detailed) then
  write(unit=*,fmt=*)
  write(unit=*,fmt=*) "Measure time for reset_watch"
end if

! find a number of calls that take a significant amount of time
ncalls = 100
do
   call reset_watch(measure)
   call start_watch(measure)
   do i=1,ncalls
      call reset_watch(dummy)
   end do
   call stop_watch(measure)
   call read_watch(t1,measure,clock)
   if (t1>1.0) then
     exit
   end if
   if (detailed) then
      write(unit=*,fmt=*) "the time for ",ncalls," calls to reset is ",t1
   end if
   ncalls=10*ncalls
end do

! determine the overhead of the loop and calls (hope this doesn't
! get optimized away)

call reset_watch(measure)
call start_watch(measure)
do i=1,ncalls
   call do_nothing()
end do
call stop_watch(measure)
call read_watch(t2,measure,clock)

if (detailed) then
  write(unit=*,fmt=*) "the time for ",ncalls," calls to reset is      ",t1
  write(unit=*,fmt=*) "the time for ",ncalls," calls to do_nothing is ",t2
end if
write(unit=*,fmt=*) "average time for a call to reset_watch is ",(t1-t2)/ncalls

!----------------------------------------------------
! measure the time to pause/end_pause watches

if (detailed) then
  write(unit=*,fmt=*)
  write(unit=*,fmt=*) "Measure time for pause_watch and end_pause_watch"
end if

call start_watch(dummy)

! find a number of calls that take a significant amount of time
ncalls = 100
do
   call reset_watch(measure)
   call start_watch(measure)
   do i=1,ncalls
      call pause_watch(dummy)
      call end_pause_watch(dummy)
   end do
   call stop_watch(measure)
   call read_watch(t1,measure,clock)
   if (t1>1.0) then
     exit
   end if
   if (detailed) then
      write(unit=*,fmt=*) "the time for ",ncalls," calls to pause and end_pause is ",t1
   end if
   ncalls=10*ncalls
end do
call stop_watch(dummy)

! determine the overhead of the loop and calls (hope this doesn't
! get optimized away)

call reset_watch(measure)
call start_watch(measure)
do i=1,ncalls
   call do_nothing()
   call do_nothing()
end do
call stop_watch(measure)
call read_watch(t2,measure,clock)

if (detailed) then
  write(unit=*,fmt=*) "the time for ",ncalls," calls to pause and end_pause is ",t1
  write(unit=*,fmt=*) "the time for ",ncalls," calls to do_nothing is     ",t2
end if
write(unit=*,fmt=*) "average time for a call to pause_watch or end_pause_watch is ",&
        (t1-t2)/(2*ncalls)

!----------------------------------------------------
! measure the time to read watches

if (detailed) then
  write(unit=*,fmt=*)
  write(unit=*,fmt=*) "Measure time for read_watch"
end if

! find a number of calls that take a significant amount of time
ncalls = 100
do
   call reset_watch(measure)
   call start_watch(measure)
   do i=1,ncalls
      call read_watch(t3,dummy,clock)
   end do
   t1=t3 ! just to fool elf90 into thinking that t3 is used
   call stop_watch(measure)
   call read_watch(t1,measure,clock)
   if (t1>1.0) then
     exit
   end if
   if (detailed) then
      write(unit=*,fmt=*) "the time for ",ncalls," calls to read is ",t1
   end if
   ncalls=10*ncalls
end do

! determine the overhead of the loop and calls (hope this doesn't
! get optimized away)

call reset_watch(measure)
call start_watch(measure)
do i=1,ncalls
   call do_nothing()
end do
call stop_watch(measure)
call read_watch(t2,measure,clock)

if (detailed) then
   write(unit=*,fmt=*) "the time for ",ncalls," calls to read is       ",t1
   write(unit=*,fmt=*) "the time for ",ncalls," calls to do_nothing is ",t2
end if
write(unit=*,fmt=*) "average time for a call to read_watch is ",(t1-t2)/ncalls

!----------------------------------------------------
! measure the time for arrays of 10 watches; should be less than
! 10 times the time for individual watches.

write(unit=*,fmt=*)
write(unit=*,fmt=*) " The following calls to StopWatch routines are with an array of 10 watches"
write(unit=*,fmt=*) " with default clock, which is the single clock '",clock,"'."
write(unit=*,fmt=*)

!----------------------------------------------------
! measure the time to start/stop an array of 10 watches

if (detailed) then
  write(unit=*,fmt=*)
  write(unit=*,fmt=*) "Measure time for start_watch and stop_watch"
end if

call create_watch(dum)
! find a number of calls that take a significant amount of time
ncalls = 100
do
   call reset_watch(measure)
   call start_watch(measure)
   do i=1,ncalls
      call start_watch(dum)
      call stop_watch(dum)
   end do
   call stop_watch(measure)
   call read_watch(t1,measure,clock)
   if (t1>1.0) then
     exit
   end if
   if (detailed) then
     write(unit=*,fmt=*) "the time for ",ncalls," calls to start and stop is ",t1
   end if
   ncalls=10*ncalls
end do

! determine the overhead of the loop and calls (hope this doesn't
! get optimized away)

call reset_watch(measure)
call start_watch(measure)
do i=1,ncalls
   call do_nothing_a()
   call do_nothing_a()
end do
call stop_watch(measure)
call read_watch(t2,measure,clock)

if (detailed) then
  write(unit=*,fmt=*) "the time for ",ncalls," calls to start and stop is ",t1
  write(unit=*,fmt=*) "the time for ",ncalls," calls to do_nothing is     ",t2
end if
write(unit=*,fmt=*) "average time for a call to start_watch or stop_watch is ",&
         (t1-t2)/(2*ncalls)

!----------------------------------------------------
! measure the time for groups of 10 watches; should be less than
! 10 times the time for individual watches.

write(unit=*,fmt=*)
write(unit=*,fmt=*) " The following calls to StopWatch routines are with a group of 10 watches"
write(unit=*,fmt=*) " with default clock, which is the single clock '",clock,"'."
write(unit=*,fmt=*)

!----------------------------------------------------
! measure the time for creating a group

if (detailed) then
  write(unit=*,fmt=*)
  write(unit=*,fmt=*) "Measure time for create_watchgroup and destroy_watchgroup"
end if

call create_watch(dum)
! find a number of calls that take a significant amount of time
ncalls = 100
do
   call reset_watch(measure)
   call start_watch(measure)
   do i=1,ncalls
      call create_watchgroup(dum,agroup)
      call destroy_watchgroup(agroup)
   end do
   call stop_watch(measure)
   call read_watch(t1,measure,clock)
   if (t1>1.0) then
     exit
   end if
   if (detailed) then
     write(unit=*,fmt=*) "the time for ",ncalls," calls to create and destroy is ",t1
   end if
   ncalls=10*ncalls
end do

! determine the overhead of the loop and calls (hope this doesn't
! get optimized away)

call reset_watch(measure)
call start_watch(measure)
do i=1,ncalls
   call do_nothing_a()
   call do_nothing_g()
end do
call stop_watch(measure)
call read_watch(t2,measure,clock)

if (detailed) then
  write(unit=*,fmt=*) "the time for ",ncalls," calls to create and destroy is ",t1
  write(unit=*,fmt=*) "the time for ",ncalls," calls to do_nothing is     ",t2
end if
write(unit=*,fmt=*) "average time for a call to create_watchgroup or destroy_watchgroup is ",&
         (t1-t2)/(2*ncalls)

!----------------------------------------------------
! measure the time for adding and removing a single watch from a  group

if (detailed) then
  write(unit=*,fmt=*)
  write(unit=*,fmt=*) "Measure time for join_watchgroup and leave_watchgroup; single watch"
end if

call create_watch(dummy)
call create_watchgroup(handle=agroup)

! find a number of calls that take a significant amount of time
ncalls = 100
do
   call reset_watch(measure)
   call start_watch(measure)
   do i=1,ncalls
      call join_watchgroup(dummy,agroup)
      call leave_watchgroup(dummy,agroup)
   end do
   call stop_watch(measure)
   call read_watch(t1,measure,clock)
   if (t1>1.0) then
     exit
   end if
   if (detailed) then
     write(unit=*,fmt=*) "the time for ",ncalls," calls to join and leave is ",t1
   end if
   ncalls=10*ncalls
end do

call destroy_watchgroup(agroup)

! determine the overhead of the loop and calls (hope this doesn't
! get optimized away)

call reset_watch(measure)
call start_watch(measure)
do i=1,ncalls
   call do_nothing()
   call do_nothing()
end do
call stop_watch(measure)
call read_watch(t2,measure,clock)

if (detailed) then
  write(unit=*,fmt=*) "the time for ",ncalls," calls to join and leave is ",t1
  write(unit=*,fmt=*) "the time for ",ncalls," calls to do_nothing is     ",t2
end if
write(unit=*,fmt=*) "average time for a call to join_watchgroup or leave_watchgroup (single watch) is ",&
         (t1-t2)/(2*ncalls)

!----------------------------------------------------
! measure the time for adding and removing an array of 10 watches from a  group

if (detailed) then
  write(unit=*,fmt=*)
  write(unit=*,fmt=*) "Measure time for join_watchgroup and leave_watchgroup; array of 10 watches"
end if

call create_watch(dum)
call create_watchgroup(handle=agroup)

! find a number of calls that take a significant amount of time
ncalls = 100
do
   call reset_watch(measure)
   call start_watch(measure)
   do i=1,ncalls
      call join_watchgroup(dum,agroup)
      call leave_watchgroup(dum,agroup)
   end do
   call stop_watch(measure)
   call read_watch(t1,measure,clock)
   if (t1>1.0) then
     exit
   end if
   if (detailed) then
     write(unit=*,fmt=*) "the time for ",ncalls," calls to join and leave is ",t1
   end if
   ncalls=10*ncalls
end do

call destroy_watchgroup(agroup)

! determine the overhead of the loop and calls (hope this doesn't
! get optimized away)

call reset_watch(measure)
call start_watch(measure)
do i=1,ncalls
   call do_nothing_a()
   call do_nothing_a()
end do
call stop_watch(measure)
call read_watch(t2,measure,clock)

if (detailed) then
  write(unit=*,fmt=*) "the time for ",ncalls," calls to join and leave is ",t1
  write(unit=*,fmt=*) "the time for ",ncalls," calls to do_nothing is     ",t2
end if
write(unit=*,fmt=*) "average time for a call to join_watchgroup or leave_watchgroup (array of 10 watches) is ",&
         (t1-t2)/(2*ncalls)

!----------------------------------------------------
! measure the time to start/stop watches with a group of 10 watches

if (detailed) then
  write(unit=*,fmt=*)
  write(unit=*,fmt=*) "Measure time for start_watch and stop_watch with a group of 10"
end if

call create_watchgroup(dum,agroup)

! find a number of calls that take a significant amount of time
ncalls = 100
do
   call reset_watch(measure)
   call start_watch(measure)
   do i=1,ncalls
      call start_watch(agroup)
      call stop_watch(agroup)
   end do
   call stop_watch(measure)
   call read_watch(t1,measure,clock)
   if (t1>1.0) then
     exit
   end if
   if (detailed) then
     write(unit=*,fmt=*) "the time for ",ncalls," calls to start and stop is ",t1
   end if
   ncalls=10*ncalls
end do

! determine the overhead of the loop and calls (hope this doesn't
! get optimized away)

call reset_watch(measure)
call start_watch(measure)
do i=1,ncalls
   call do_nothing_g()
   call do_nothing_g()
end do
call stop_watch(measure)
call read_watch(t2,measure,clock)

if (detailed) then
  write(unit=*,fmt=*) "the time for ",ncalls," calls to start and stop is ",t1
  write(unit=*,fmt=*) "the time for ",ncalls," calls to do_nothing is     ",t2
end if
write(unit=*,fmt=*) "average time for a call to start_watch or stop_watch (group of 10) is ",&
         (t1-t2)/(2*ncalls)

!----------------------------------------------------

stop
end program overhead
