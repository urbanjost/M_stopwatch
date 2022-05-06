![string](docs/images/alphabet.gif)
# M_stopwatch.f90 and associated files

## NAME
   M_stopwatch - package for measuring cpu and wall clock execution time (LICENSE:PD) based on [StopWatch](https://math.nist.gov/StopWatch)

## DESCRIPTION

   M_StopWatch is a Fortran 90 module for measuring execution time of
   program segments. M_StopWatch is designed to be a portable, easy-to-use 
   means of measuring execution time. It supports the wall clock,
   CPU clock, and a breakdown of the CPU clock into user and system
   times. It returns all times in seconds. It provides a simple means
   of determining which clocks are available, and the precision of
   those clocks.

   M_StopWatch is used by instrumenting your code with subroutine calls
   that mimic the operation of a stop watch. The primary routines are
   start_watch, stop_watch, reset_watch, read_watch, and print_watch.
   M_StopWatch supports multiple watches, and provides the concept
   of watch groups to allow functions to operate on multiple watches
   simultaneously.

## DOWNLOAD and BUILD with make(1) ![gmake](docs/images/gnu.gif)
Just download the github repository, enter the src/ directory and run make:

     git clone https://github.com/urbanjost/M_stopwatch.git
     cd M_stopwatch/src
     # change Makefile if not using one of the listed compilers

     # for gfortran
     make clean
     make F90=gfortran gfortran

     # for ifort
     make clean
     make F90=ifort ifort

     # for nvfortran
     make clean
     make F90=nvfortran nvfortran

     # optionally
     make test # run the unit tests
     make run  # run all the demo programs from the man-pages
     make help # see other developer options

This will compile the M_stopwatch(3f) module and optionally build all the
example programs from the document pages in the example/ sub-directory
and run the unit tests.

## DOWNLOAD AND BUILD WITH FPM(1) ![fpm](docs/images/fpm_logo.gif)

Alternatively, download the github repository and build it with
fpm ( as described at [Fortran Package Manager](https://github.com/fortran-lang/fpm) )

```bash
     git clone https://github.com/urbanjost/M_stopwatch.git
     cd M_stopwatch
     fpm build
     fpm test  # run unit tests
```

or just list it as a dependency in your fpm.toml project file.

```toml
     [dependencies]
     M_stopwatch        = { git = "https://github.com/urbanjost/M_stopwatch.git" ,tag="v1.0.1"}
```

Note that M_stopwatch.f90 is registered at the [fpm(1) registry](https://github.com/fortran-lang/fpm-registry)


## DOCUMENTATION   ![docs](docs/images/docs.gif)
### USER

[user guide](https://urbanjost.github.io/M_stopwatch/user_guide.html).

<!--
   There are HTML documents for each
   subprogram in the style of man-pages:

 - An [index](https://urbanjost.github.io/M_stopwatch/man3.html) to the HTML versions
   of the man-pages.

 - A single page (that uses javascript) combining all the HTML descriptions of the man-pages
   for easy searching and printing:
   [BOOK_M_stopwatch](https://urbanjost.github.io/M_stopwatch/BOOK_M_stopwatch.html).
-->

 - Literal man-pages for use on GNU/Linux, Unix and CygWin platforms:
![man-pages](docs/images/manpages.gif)
<!--
    + [manpages.zip](https://urbanjost.github.io/M_stopwatch/manpages.zip)
-->
    + [manpages.tgz](https://urbanjost.github.io/M_stopwatch/manpages.tgz)
    + [CHANGELOG](docs/CHANGELOG.md) provides a history of significant changes

### DEVELOPER (__experimental__)

   - The code was run through [ford(1)](https://politicalphysicist.github.io/ford-fortran-documentation.html)
     to produce a [developers' document](https://urbanjost.github.io/M_stopwatch/fpm-ford/index.html).
   - [github action status](docs/STATUS.md)

## DEMO PROGRAMS![demos](docs/images/demo.gif)

Each man-page includes a working example program. These and additional
examples are included in the example/ directory.
