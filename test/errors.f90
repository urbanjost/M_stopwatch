! This program tests the error messages of StopWatch

program errors
use M_stopwatch
implicit none

type (watchtype) :: w1
type (watchtype), dimension(2) :: w2
type (watchgroup) :: g1
logical :: sys_there
real :: x

write(unit=*,fmt=*) "This test is for a watch name that is too long."
write(unit=*,fmt=*) "It should print the error message and then print the watch with"
write(unit=*,fmt=*) "the truncated name, which should end with 312"
write(unit=*,fmt=*)
call create_watch(w1,name="1234567891123456789212345678931234567894"// &
      "123456789512345678961234567897123456789812345678991234567890"// &
      "1234567891123456789212345678931234567894")
call print_watch(w1)
call destroy_watch(w1)

write(unit=*,fmt=*)
write(unit=*,fmt=*) "This test is for using a watch that has been destroyed."
write(unit=*,fmt=*)
call start_watch(w1)

write(unit=*,fmt=*)
write(unit=*,fmt=*) "This test is for giving the wrong number of names to create_watch"
write(unit=*,fmt=*)
call create_watch(w2,name=(/"name1","name2","name3"/))

call create_watch(w1,name="w1")
call create_watch(w2,name=(/"w2(1)","w2(2)"/))

write(unit=*,fmt=*)
write(unit=*,fmt=*) "This test is for a wrong operation, specifically starting a running watch"
write(unit=*,fmt=*)
call start_watch(w1)
call start_watch(w1)
call stop_watch(w1)

write(unit=*,fmt=*)
write(unit=*,fmt=*) "This test is for giving too many clocks.  The fifth clock is"
write(unit=*,fmt=*) "five (invalid name) followed by four instances of wall (too many clocks)."
write(unit=*,fmt=*) "There might also be error messages for invalid clock types if"
write(unit=*,fmt=*) "your system does not support all four clocks.  The print_watch"
write(unit=*,fmt=*) "after the error message should have the same set of clocks as above."
write(unit=*,fmt=*)
call option_stopwatch((/"user","sys ","cpu ","wall","five","wall","wall","wall","wall"/))
call print_watch(w1)

write(unit=*,fmt=*)
write(unit=*,fmt=*) "This test is for giving an invalid clock name."
write(unit=*,fmt=*)
call read_watch(x,w1,"not_a_clock")
if (x < 0.0) then ! just so that elf90 thinks I'm using x
   write(unit=*,fmt=*)
end if
call inquiry_stopwatch(sys_avail=sys_there)
if (.not. sys_there) then
  write(unit=*,fmt=*)
  write(unit=*,fmt=*) "This test is for specifying a clock that is not on this system"
  write(unit=*,fmt=*)
  call start_watch(w1,"sys")
end if

write(unit=*,fmt=*)
write(unit=*,fmt=*) "This test removes a watch from a group when it doesn't belong to it"
write(unit=*,fmt=*)
call create_watchgroup(w2,g1)
call leave_watchgroup(w1,g1)

write(unit=*,fmt=*)
write(unit=*,fmt=*) "This test assigns the I/O unit to a file not opened.  It is followed"
write(unit=*,fmt=*) "by a print_watch which should go to the usual place."
write(unit=*,fmt=*)
call option_stopwatch(io_unit_print=11)
call print_watch(w1)

write(unit=*,fmt=*)
write(unit=*,fmt=*) "This test assigns I/O to a file, prints to that file (see errors.output),"
write(unit=*,fmt=*) "closes the file and tries to print to it, and then sets the I/O unit to 6"
open(unit=12,file="errors.output",action="WRITE",status="new")
call option_stopwatch(io_unit_print=12)
call print_watch(w1,title="This should print into errors.output")
close(unit=12)
call print_watch(w1,title="This should be an error")
call option_stopwatch(io_unit_print=6)
call print_watch(w1,title="This print should be back in the usual place")

write(unit=*,fmt=*)
write(unit=*,fmt=*) "This test redirects error messages to errors.errout, generates an"
write(unit=*,fmt=*) "error message (stopping a watch that is not running) in that file,"
write(unit=*,fmt=*) "closes the file, and generates another error message (bad clock name)"
open(unit=13,file="errors.errout",action="WRITE",status="new")
call option_stopwatch(io_unit_error=13)
call stop_watch(w1)
close(unit=13)
call reset_watch(w2,"not-a-clock")

stop
end program errors
