module module_mpi_universe

!cjb
!  "INCLUDE mpif.h" is deprecated in future MPI releases
!   and "USE mpi" is recommended
!  "USE mpi_f08" is a (recommended) Fortran 2008 extension of "USE mpi"
!   (however, certain implementations of mpi_f08 do not support MPI_Recv)
!   if you already have an MPI library with precompiled mpi.mod
!   we recommend to uncomment version "USE mpi_f08" below 
!   and comment out version 'mpif.h'
!   depending on your environment
!   it may be necessary to add $(MPI_INC) = -I/path  in Makefile
!   with 'path' pointing to module 'mpi.mod'
!cjb
!   otherwise uncomment 'mpif.h' or "USE mpi" 
!   do not uncomment both
!   observe reversed order :: USE/implicit vs implicit/INCLUDE

! begin version USE mpi_f08
! uncomment the next 2 lines
      USE mpi_f08
      implicit none
! end   version USE mpi

! begin version USE mpi
! uncomment the next 2 lines
!     USE mpi
!     implicit none
! end   version USE mpi

! begin version INCLUDE 'mpif.h'
! uncomment the next 2 lines
!     implicit none
!     INCLUDE 'mpif.h'
! end   version INCLUDE 'mpif.h'

!     INCLUDE 'fenv.h'
!     character*(*) host
!     character (LEN=32) :: host

!-----------------------------------------------------------------------
! https://www.open-mpi.org/doc/v3.0/man3/MPI_Recv.3.php
! Fortran 2008 Syntax
! USE mpi_f08
! MPI_Recv(buf, count, datatype, source, tag, comm, status, ierror)
!    TYPE(*), DIMENSION(..) :: buf
!    INTEGER, INTENT(IN) :: count, source, tag
!    TYPE(MPI_Datatype), INTENT(IN) :: datatype
!    TYPE(MPI_Comm), INTENT(IN) :: comm
!    TYPE(MPI_Status) :: status
!    INTEGER, OPTIONAL, INTENT(OUT) :: ierror
!-----------------------------------------------------------------------

! MPI ----------------------------------------------------

      logical, PARAMETER :: MultiProcessorRun = .true.
!     logical, PARAMETER :: MultiProcessorRun = .false.

      character*(MPI_MAX_PROCESSOR_NAME) hostname
      INTEGER myid, nprocs, lenhost
      INTEGER ierr, ierrtotal
      TYPE (MPI_Status) :: istat
!     INTEGER istat(MPI_STATUS_SIZE)

! OpenMP  ----------------------------------------------------

      INTEGER number_of_threads
! number_of_threads = read from input or determine from environment
! nthreads = used in subroutines

end module module_mpi_universe

