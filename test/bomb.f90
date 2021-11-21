program crash1
use M_stopwatch
implicit none

! This program demonstrates the error that occurs if you try to use a
! watch that has not been created, i.e., not passed through create_watch.
! If the program doesn't actually crash, try some similar tests.

type (watchtype) :: w
type (watchgroup) :: g, g2
integer :: err

write(unit=*,fmt=*) "Attempt to start a watch that has not been created.  This should"
write(unit=*,fmt=*) "crash the program with an error that says a pointer with undefined"
write(unit=*,fmt=*) "association status was passed to the intrinsic function associated"
write(unit=*,fmt=*)

call option_stopwatch(default_clock="wall")
call start_watch(w,err=err)

write(unit=*,fmt=*) "Returned to the main program."
write(unit=*,fmt=*) "The StopWatch error code is ",err
write(unit=*,fmt=*) "Programs that do not create a watch before using it will not"
write(unit=*,fmt=*) "necessarily crash, but you can not count on it."

! As long as it"s still running, try crashing it by passing a noncreated
! watch group to a routine

err=0
call create_watch(w)

write(unit=*,fmt=*)
write(unit=*,fmt=*) "Attempt to start a watch group that has not been created.  This should"
write(unit=*,fmt=*) "have the same behaviour as using a watch that has not been created."
write(unit=*,fmt=*)

call start_watch(g,err=err)

write(unit=*,fmt=*) "Returned to the main program."
write(unit=*,fmt=*) "The StopWatch error code is ",err

! If it's still going, try adding a watch to a non-created group

err=0

write(unit=*,fmt=*)
write(unit=*,fmt=*) "Attempt to add a watch (join_watchgroup) to a group that has not been"
write(unit=*,fmt=*) "created."
write(unit=*,fmt=*)

call join_watchgroup(w,g2,err=err)

write(unit=*,fmt=*) "Returned to the main program."
write(unit=*,fmt=*) "The StopWatch error code is ",err

write(unit=*,fmt=*)
write(unit=*,fmt=*) "Call print_watch with the group that the watch was added to.  If this"
write(unit=*,fmt=*) "print contains a watch, then the process of adding the watch to the"
write(unit=*,fmt=*) "group created the group, but you can not count on this behaviour."
write(unit=*,fmt=*)

call print_watch(g2,err=err)

write(unit=*,fmt=*) "Returned to the main program."
write(unit=*,fmt=*) "The StopWatch error code is ",err

stop
end program crash1
