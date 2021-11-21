! This program tests the StopWatch module.

program testsw
use M_stopwatch
implicit none

type (watchtype) :: w1, w2, w3, w4
type (watchtype), dimension(3) :: aw1
type (watchgroup) :: g1,g2,g3

character(len=5), dimension(4) :: defclocks
integer :: iopr,ioerr
logical :: availc,availu,avails,availw,prerr,aberr
real :: prec_c,prec_w,r11,readval
real, pointer, dimension(:) :: r12,r21
real, pointer, dimension(:,:) ::r22
character(len=16) :: vers

integer :: err

! ---------- Test inquiry_stopwatch

write(unit=*,fmt=*)"Testing inquiry_stopwatch..."

call inquiry_stopwatch(version=vers,err=err)
if (err /= 0) then
   write(unit=*,fmt=*)"Returned error code ",err
end if
write(unit=*,fmt=*)"This is StopWatch Version ",vers

call inquiry_stopwatch(defclocks,err=err)
if (err /= 0) then
   write(unit=*,fmt=*)"Returned error code ",err
end if
write(unit=*,fmt=*)"Default clocks are ",defclocks

call inquiry_stopwatch(defclocks,iopr,ioerr,prerr,aberr,err=err)
   if (err /= 0) then
      write(unit=*,fmt=*)"Returned error code ",err
   end if
   write(unit=*,fmt=*)"I/O units for printing and errors: ",iopr,ioerr

   if (prerr) then
      write(unit=*,fmt=*)"Error messages will be printed"
   else
      write(unit=*,fmt=*)"Error messages will not be printed"
   end if

   if (aberr) then
      write(unit=*,fmt=*)"Program will abort on errors"
   else
      write(unit=*,fmt=*)"Program will not abort on errors"
   end if

call inquiry_stopwatch(cpu_avail=availc,user_avail=availu,sys_avail=avails, &
                   wall_avail=availw,cpu_prec=prec_c,wall_prec=prec_w,err=err)

   if (err /= 0) then
      write(unit=*,fmt=*)"Returned error code ",err
   end if

   if (availc) then
      write(unit=*,fmt=*)"CPU clock is available"
   else
      write(unit=*,fmt=*)"CPU clock is not available"
   end if
   
   if (availu) then
      write(unit=*,fmt=*)"User clock is available"
   else
      write(unit=*,fmt=*)"User clock is not available"
   end if
   
   if (avails) then
      write(unit=*,fmt=*)"Sys clock is available"
   else
      write(unit=*,fmt=*)"Sys clock is not available"
   end if
   
   if (availw) then
      write(unit=*,fmt=*)"Wall clock is available"
   else
      write(unit=*,fmt=*)"Wall clock is not available"
   end if

   write(unit=*,fmt=*)"cpu, user and sys clock precision is ",prec_c
   write(unit=*,fmt=*)"wall clock precision is ",prec_w

   if (.not. (availc .and. availu .and. avails .and. availw)) then
      write(unit=*,fmt=*)
      write(unit=*,fmt=*) "NOTE: some of the clocks are not available.  There will be some"
      write(unit=*,fmt=*) "      error messages about missing clocks, and the list of clocks"
      write(unit=*,fmt=*) "      on a watch will not always be as expected."
      write(unit=*,fmt=*)
   end if

! ---------- Test create_watch

write(unit=*,fmt=*)
write(unit=*,fmt=*)"Testing create_watch..."

call create_watch(w1,err=err)

if (err /= 0) then
   write(unit=*,fmt=*)"Returned error code ",err
end if

call create_watch(w2,"wall",err=err)
if (err /= 0) then
   write(unit=*,fmt=*)"Returned error code ",err
end if

call create_watch(w3,(/"cpu ","wall"/),err=err)
if (err /= 0) then
   write(unit=*,fmt=*)"Returned error code ",err
end if

call create_watch(w4,name="scalar watch #4",err=err)
if (err /= 0) then
   write(unit=*,fmt=*)"Returned error code ",err
end if

call create_watch(aw1,name=(/"array 1, #1","array 1, #2","array 1, #3"/),err=err)
if (err /= 0) then
   write(unit=*,fmt=*)"Returned error code ",err
end if

write(unit=*,fmt=*)"Printing created watches; the first three watches should have the"
write(unit=*,fmt=*)"  name 'unnamed watch', the others should be named.  Subject to"
write(unit=*,fmt=*)"  the availability of the clocks, the second one should have just the"
write(unit=*,fmt=*)"  wall clock, the third should have cpu and wall, and the others"
write(unit=*,fmt=*)"  should have all."

call print_watch((/w1,w2,w3,w4/),err=err)
if (err /= 0) then
   write(unit=*,fmt=*)"Returned error code ",err
end if
call print_watch(aw1,err=err)
if (err /= 0) then
   write(unit=*,fmt=*)"Returned error code ",err
end if

call start_watch( (/w1, w2/) )
! ---------- Test destroy_watch

write(unit=*,fmt=*)
write(unit=*,fmt=*)"Testing destroy_watch..."

call destroy_watch(w1,"cpu",err=err)
if (err /= 0) then
   write(unit=*,fmt=*)"Returned error code ",err
end if
call destroy_watch(w4,(/"user","cpu ","sys "/),err=err)
if (err /= 0) then
   write(unit=*,fmt=*)"Returned error code ",err
end if
write(unit=*,fmt=*)"Printing watches for which only some clocks were destroyed."
write(unit=*,fmt=*)"  Should have user, sys and wall on the first, and wall on the second"
call print_watch((/w1,w4/),err=err)
if (err /= 0) then
   write(unit=*,fmt=*)"Returned error code ",err
end if
call destroy_watch(w1,err=err)
if (err /= 0) then
   write(unit=*,fmt=*)"Returned error code ",err
end if
call destroy_watch(w4,"wall",err=err)
if (err /= 0) then
   write(unit=*,fmt=*)"Returned error code ",err
end if
call destroy_watch(w2,err=err)
if (err /= 0) then
   write(unit=*,fmt=*)"Returned error code ",err
end if
call destroy_watch(w3,err=err)
if (err /= 0) then
   write(unit=*,fmt=*)"Returned error code ",err
end if
call destroy_watch(aw1,err=err)
if (err /= 0) then
   write(unit=*,fmt=*)"Returned error code ",err
end if

! ---------- Test create_watchgroup

write(unit=*,fmt=*)
write(unit=*,fmt=*)"Testing create_watchgroup..."
call create_watch(w1,name="scalar watch #1",err=err)
if (err /= 0) then
   write(unit=*,fmt=*)"Returned error code ",err
end if
call create_watch(w2,name="scalar watch #2",err=err)
if (err /= 0) then
   write(unit=*,fmt=*)"Returned error code ",err
end if
call create_watch(w3,name="scalar watch #3",err=err)
if (err /= 0) then
   write(unit=*,fmt=*)"Returned error code ",err
end if
call create_watch(w4,name="scalar watch #4",err=err)
if (err /= 0) then
   write(unit=*,fmt=*)"Returned error code ",err
end if
call create_watch(aw1,name=(/"array 1, #1","array 1, #2","array 1, #3"/),err=err)
if (err /= 0) then
   write(unit=*,fmt=*)"Returned error code ",err
end if
call option_stopwatch(default_clock="wall",err=err)
if (err /= 0) then
   write(unit=*,fmt=*)"Returned error code ",err
end if

call create_watchgroup(handle=g1,err=err)
if (err /= 0) then
   write(unit=*,fmt=*)"Returned error code ",err
end if
write(unit=*,fmt=*)"This group should be empty:"
call print_watch(g1,title="Oops, this should not have been printed",err=err)
if (err /= 0) then
   write(unit=*,fmt=*)"Returned error code ",err
end if
call create_watchgroup(w1,g2,err)
if (err /= 0) then
   write(unit=*,fmt=*)"Returned error code ",err
end if
call print_watch(g2,title="This group should have scalar watch 1:",err=err)
if (err /= 0) then
   write(unit=*,fmt=*)"Returned error code ",err
end if
call create_watchgroup(aw1,g3,err)
if (err /= 0) then
   write(unit=*,fmt=*)"Returned error code ",err
end if
call print_watch(g3,title="This group should have three array watches:",err=err)
if (err /= 0) then
   write(unit=*,fmt=*)"Returned error code ",err
end if

! ---------- Test join_watchgroup

write(unit=*,fmt=*)
write(unit=*,fmt=*)"Testing join_watchgroup..."
call join_watchgroup(w3,g2,err)
if (err /= 0) then
   write(unit=*,fmt=*)"Returned error code ",err
end if
call print_watch(g2,title="This group should have scalar watches 1 and 3:",err=err)
if (err /= 0) then
   write(unit=*,fmt=*)"Returned error code ",err
end if
call join_watchgroup(aw1,g2,err)
if (err /= 0) then
   write(unit=*,fmt=*)"Returned error code ",err
end if
call print_watch(g2,title="This group as above, but also the 3 array watches:",err=err)
if (err /= 0) then
   write(unit=*,fmt=*)"Returned error code ",err
end if
call join_watchgroup(w4,g3,err)
if (err /= 0) then
   write(unit=*,fmt=*)"Returned error code ",err
end if
call print_watch(g3,title="This group should have scalar watch 4 and 3 array watches:",err=err)
if (err /= 0) then
   write(unit=*,fmt=*)"Returned error code ",err
end if
call join_watchgroup(w1,g1,err)
if (err /= 0) then
   write(unit=*,fmt=*)"Returned error code ",err
end if
call print_watch(g1,title="This group should have scalar watch 1:",err=err)
if (err /= 0) then
   write(unit=*,fmt=*)"Returned error code ",err
end if
call join_watchgroup(aw1,g1,err)
if (err /= 0) then
   write(unit=*,fmt=*)"Returned error code ",err
end if
call print_watch(g1,title="and now also the 3 array watches:",err=err)
if (err /= 0) then
   write(unit=*,fmt=*)"Returned error code ",err
end if

!----------- Test leave_watchgroup

write(unit=*,fmt=*)
write(unit=*,fmt=*)"Testing leave_watchgroup..."
call leave_watchgroup(aw1(2),g1,err)
if (err /= 0) then
   write(unit=*,fmt=*)"Returned error code ",err
end if
call print_watch(g1,title="This should have array 1&3 and scalar 1:",err=err)
if (err /= 0) then
   write(unit=*,fmt=*)"Returned error code ",err
end if
call leave_watchgroup(aw1(3),g1,err)
if (err /= 0) then
   write(unit=*,fmt=*)"Returned error code ",err
end if
call print_watch(g1,title="This should have array 1 and scalar 1:",err=err)
if (err /= 0) then
   write(unit=*,fmt=*)"Returned error code ",err
end if
call leave_watchgroup(w1,g1,err)
if (err /= 0) then
   write(unit=*,fmt=*)"Returned error code ",err
end if
call print_watch(g1,title="This should have array watch 1:",err=err)
if (err /= 0) then
   write(unit=*,fmt=*)"Returned error code ",err
end if
call leave_watchgroup(aw1(1),g1,err)
if (err /= 0) then
   write(unit=*,fmt=*)"Returned error code ",err
end if
write(unit=*,fmt=*)"This group should be empty:"
call print_watch(g1,title="Oops, this should not have been printed",err=err)
if (err /= 0) then
   write(unit=*,fmt=*)"Returned error code ",err
end if
call leave_watchgroup(aw1,g2,err)
if (err /= 0) then
   write(unit=*,fmt=*)"Returned error code ",err
end if
call print_watch(g2,title="This should have scalar watches 1 & 3:",err=err)
if (err /= 0) then
   write(unit=*,fmt=*)"Returned error code ",err
end if

!----------- Test destroy_watchgroup

write(unit=*,fmt=*)
write(unit=*,fmt=*)"Testing destroy_watchgroup..."
call destroy_watchgroup(g1,err=err)
if (err /= 0) then
   write(unit=*,fmt=*)"Returned error code ",err
end if
call destroy_watchgroup(g2,err=err)
if (err /= 0) then
   write(unit=*,fmt=*)"Returned error code ",err
end if
call destroy_watchgroup(g3,err=err)
if (err /= 0) then
   write(unit=*,fmt=*)"Returned error code ",err
end if

!----------- Test start_watch

write(unit=*,fmt=*)
write(unit=*,fmt=*)"Testing start_watch..."
call option_stopwatch((/"cpu ","user","sys ","wall"/))
call create_watchgroup(w3,g1,err=err)
if (err /= 0) then
   write(unit=*,fmt=*)"Returned error code ",err
end if
call join_watchgroup(w4,g1,err=err)
if (err /= 0) then
   write(unit=*,fmt=*)"Returned error code ",err
end if
call start_watch(w1,err=err)
if (err /= 0) then
   write(unit=*,fmt=*)"Returned error code ",err
end if
call start_watch(w2,"cpu",err=err)
if (err /= 0) then
   write(unit=*,fmt=*)"Returned error code ",err
end if
call start_watch(aw1,(/"cpu ","wall"/),err=err)
if (err /= 0) then
   write(unit=*,fmt=*)"Returned error code ",err
end if
call start_watch(g1,err=err)
if (err /= 0) then
   write(unit=*,fmt=*)"Returned error code ",err
end if

!----------- Test stop_watch

write(unit=*,fmt=*)
write(unit=*,fmt=*)"Testing stop_watch..."
call stop_watch(w1,err=err)
if (err /= 0) then
   write(unit=*,fmt=*)"Returned error code ",err
end if
call stop_watch(w2,"cpu",err=err)
if (err /= 0) then
   write(unit=*,fmt=*)"Returned error code ",err
end if
call stop_watch(aw1,(/"cpu ","wall"/),err=err)
if (err /= 0) then
   write(unit=*,fmt=*)"Returned error code ",err
end if
call stop_watch(g1,err=err)
if (err /= 0) then
   write(unit=*,fmt=*)"Returned error code ",err
end if

!----------- Test reset_watch

write(unit=*,fmt=*)
write(unit=*,fmt=*)"Testing reset_watch..."
call reset_watch(w1,err=err)
if (err /= 0) then
   write(unit=*,fmt=*)"Returned error code ",err
end if
call reset_watch(w2,"cpu",err=err)
if (err /= 0) then
   write(unit=*,fmt=*)"Returned error code ",err
end if
call reset_watch(aw1,(/"cpu ","wall"/),err=err)
if (err /= 0) then
   write(unit=*,fmt=*)"Returned error code ",err
end if
call reset_watch(g1,err=err)
if (err /= 0) then
   write(unit=*,fmt=*)"Returned error code ",err
end if

!----------- Test passing a constructed array argument

write(unit=*,fmt=*)
write(unit=*,fmt=*)"Testing constructed array argument..."
call start_watch(w1,"wall",err=err)
if (err /= 0) then
   write(unit=*,fmt=*)"Returned error code ",err
end if
do ! wait for a nonzero value
  call read_watch(readval,w1,"wall",err=err)
  if (readval > 0.02) then
     exit
  end if
end do
if (err /= 0) then
   write(unit=*,fmt=*)"Returned error code ",err
end if
call start_watch(w2,"wall",err=err)
if (err /= 0) then
   write(unit=*,fmt=*)"Returned error code ",err
end if
do ! wait for a nonzero value
  call read_watch(readval,w2,"wall",err=err)
  if (readval > 0.02) then
     exit
  end if
end do
if (err /= 0) then
   write(unit=*,fmt=*)"Returned error code ",err
end if
call stop_watch((/w1,w2/),"wall",err=err)
if (err /= 0) then
   write(unit=*,fmt=*)"Returned error code ",err
end if
write(unit=*,fmt=*)"These two watches will probably contain different times:"
call print_watch((/w1,w2/),"wall",err=err)
if (err /= 0) then
   write(unit=*,fmt=*)"Returned error code ",err
end if
call reset_watch((/w1,w2/),err=err) ! this is the big test
if (err /= 0) then
   write(unit=*,fmt=*)"Returned error code ",err
end if
write(unit=*,fmt=*)"Now they should both be 0"
call print_watch((/w1,w2/), "wall",err=err)
if (err /= 0) then
   write(unit=*,fmt=*)"Returned error code ",err
end if

!----------- Test pause_watch

write(unit=*,fmt=*)
write(unit=*,fmt=*)"Testing pause_watch..."
call start_watch(w3,err=err)
if (err /= 0) then
   write(unit=*,fmt=*)"Returned error code ",err
end if
call pause_watch(g1,err=err)
if (err /= 0) then
   write(unit=*,fmt=*)"Returned error code ",err
end if

!---------- Test end_pause_watch

write(unit=*,fmt=*)
write(unit=*,fmt=*)"Testing end_pause_watch..."
call end_pause_watch(g1,err=err)
if (err /= 0) then
   write(unit=*,fmt=*)"Returned error code ",err
end if

!----------- Test print_watch

write(unit=*,fmt=*)
write(unit=*,fmt=*)"Testing print_watch..."
call print_watch(w3,err=err)
if (err /= 0) then
   write(unit=*,fmt=*)"Returned error code ",err
end if
call pause_watch(w3,err=err)
if (err /= 0) then
   write(unit=*,fmt=*)"Returned error code ",err
end if
call print_watch(w3,title="giving just cpu clock",clock="cpu")
if (err /= 0) then
   write(unit=*,fmt=*)"Returned error code ",err
end if
call end_pause_watch(w3,err=err)
if (err /= 0) then
   write(unit=*,fmt=*)"Returned error code ",err
end if
call print_watch(w3,title="using hh:mm:ss format ",form="hh:mm:ss",err=err)
if (err /= 0) then
   write(unit=*,fmt=*)"Returned error code ",err
end if
call start_watch(aw1,"wall")
write(unit=*,fmt=*)"Waiting for the watch to exceed 1 minute for next test ..."
do
  call read_watch(readval,w3,"wall")
  if (readval > 62.0) then
     exit
  end if
end do
call print_watch(w3,title="using [[hh:]mm:]ss format ",clock="wall", &
                 form="[[hh:]mm:]ss",err=err)
if (err /= 0) then
   write(unit=*,fmt=*)"Returned error code ",err
end if

!----------- Test read_watch

call stop_watch(w3)
call stop_watch(aw1,"wall")
write(unit=*,fmt=*)
write(unit=*,fmt=*)"Testing read_watch..."
call read_watch(r11,w1,"wall",err=err)
if (err /= 0) then
   write(unit=*,fmt=*)"Returned error code ",err
end if
write(unit=*,fmt=*)"returned from read: ",r11
call print_watch(w1,title="",clock="wall")
call read_watch(r12,w3,err=err)
if (err /= 0) then
   write(unit=*,fmt=*)"Returned error code ",err
end if
write(unit=*,fmt=*)"returned from read: ",r12
call print_watch(w3,title="")
call read_watch(r21,(/w1,w2,w3,w4/),"wall",err=err)
if (err /= 0) then
   write(unit=*,fmt=*)"Returned error code ",err
end if
write(unit=*,fmt=*)"returned from read: ",r21
call print_watch((/w1,w2,w3,w4/),"wall")
call read_watch(r22,aw1,(/"cpu ","wall"/),err=err)
if (err /= 0) then
   write(unit=*,fmt=*)"Returned error code ",err
end if
write(unit=*,fmt=*)"returned from read: ",r22
call print_watch(aw1,title="",clock=(/"cpu ","wall"/))

!----------- Test printing an error message

write(unit=*,fmt=*)
write(unit=*,fmt=*)"Testing printing an error message..."
write(unit=*,fmt=*)"This error message should say I'm trying to stop a watch that is not running"

call stop_watch(w1,clock="wall",err=err)
write(unit=*,fmt=*)"The error code returned from that faulty call is ",err

!----------- Test option_stopwatch

write(unit=*,fmt=*)
write(unit=*,fmt=*)"Testing option_stopwatch..."
call destroy_watch(aw1(2),err=err)
if (err /= 0) then
   write(unit=*,fmt=*)"Returned error code ",err
end if
call option_stopwatch("wall",err=err)
if (err /= 0) then
   write(unit=*,fmt=*)"Returned error code ",err
end if
call create_watch(aw1(2),name="array watch #2",err=err)
if (err /= 0) then
   write(unit=*,fmt=*)"Returned error code ",err
end if
call option_stopwatch((/"cpu ","wall"/),err=err)
if (err /= 0) then
   write(unit=*,fmt=*)"Returned error code ",err
end if
write(unit=*,fmt=*)"Test setting default clocks"
write(unit=*,fmt=*)"The next print is with default clocks cpu and wall,"
write(unit=*,fmt=*)"and with watch #2 created when default clocks was wall"
call print_watch(aw1,err=err)
if (err /= 0) then
   write(unit=*,fmt=*)"Returned error code ",err
end if
open(unit=24,file="testsw.output",action="WRITE",status="new")
open(unit=25,file="testsw.errout",action="WRITE",status="new")
write(unit=*,fmt=*)"Test changing output files.  Look in the files testsw.output and testsw.errout"
call option_stopwatch(io_unit_print=24,io_unit_error=25,err=err)
if (err /= 0) then
   write(unit=*,fmt=*)"Returned error code ",err
end if
call print_watch(aw1,err=err)
if (err /= 0) then
   write(unit=*,fmt=*)"Returned error code ",err
end if
call stop_watch(w1,err=err)
close(unit=24)
close(unit=25)
call option_stopwatch(io_unit_print=6,io_unit_error=6,err=err)
if (err /= 0) then
   write(unit=*,fmt=*)"Returned error code ",err
end if
write(unit=*,fmt=*)"Test turning off printing of errors.  First make sure they're printing now."
call stop_watch(w1,clock="cpu",err=err)
if (err /= 0) then
   write(unit=*,fmt=*)"Returned error code ",err
end if
call option_stopwatch(print_errors=.false.,err=err)
if (err /= 0) then
   write(unit=*,fmt=*)"Returned error code ",err
end if
write(unit=*,fmt=*)"And the next thing printed should be the error code."
call stop_watch(w1,clock="cpu",err=err)
if (err /= 0) then
   write(unit=*,fmt=*)"Returned error code ",err
end if
write(unit=*,fmt=*)"The final test (by necessity): test aborting on errors.  This should"
write(unit=*,fmt=*)"print the usual error message, and then say it is aborting, and quit."
call option_stopwatch(print_errors=.true.,abort_errors=.true.,err=err)
if (err /= 0) then
   write(unit=*,fmt=*)"Returned error code ",err
end if
call stop_watch(w1,clock="cpu")
write(unit=*,fmt=*)"Oops -- it returned to the main program when it was supposed to abort."

stop
end program testsw
