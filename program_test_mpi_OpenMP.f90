!***********************************************************************
! mpitest
!                                               Jacek Bieron 2007-17-06
! 2010-07-09 mpd.hosts 
! 2011-15-02 loopthread
!            modules
!            threads
! 2013-06-13 module_mpi_universe
! 2020-12-31 gfortran
! 2022-04-16 use mpi 
!            gfortran-10 -fallow-argument-mismatch -> c_loc
! 2022-05-04 use mpi_f08
!
!***********************************************************************

      PROGRAM test_mpi_OpenMP

!     use /opt/intel/Compiler/11.1/064/include/intel64/omp_lib.mod
!     include '/opt/intel/Compiler/11.1/064/include/omp_lib.h'
!     INCLUDE 'mpif.h'
      use module_mpi_universe
      use module_io_numbers
      use omp_lib
      use module_matrixAXY

      implicit none

!-------------------------------------------------------------------------
      interface
         subroutine matrixDotVector (dimenA)
      integer, intent(in) :: dimenA
         end subroutine matrixDotVector
      end interface
!-------------------------------------------------------------------------

      integer :: dimenA, i, j, allocerror

      character(LEN=4), PARAMETER :: HOSTcmd = 'HOST'

      logical, PARAMETER :: trim_host_name = .true.

!     integer :: number_of_threads ! moved to module_mpi_universe
      integer :: nthreads
! number_of_threads :: in test_mpi_OpenMP
! nthreads :: in subroutines

      real :: CPUtimeStart, CPUtimeStop, CPUtimeUsed
      real :: CPUtimeStartFull, CPUtimeStopFull, CPUtimeUsedFull

        character sdate*8,stime*10,szone*5
        character edate*8,etime*10,ezone*5
        character rdate*8,rtime*10,rzone*5
        integer :: svalues(8), evalues(8), rvalues(8)

        integer :: count_number1,count_rate1,count_max1
        integer :: count_number2,count_rate2,count_max2
        real :: wall_time_used

      logical :: input_threadsExists
      logical :: input_matrixExists
!     logical :: xinputexists

!-------------------------------------------------------------------------
! mpi setup

      ierrtotal = 0

      if (MultiProcessorRun) then
        call MPI_Init (ierr)
        ierrtotal = ierrtotal + ierr
        call MPI_Comm_rank (MPI_COMM_WORLD, myid, ierr)
        ierrtotal = ierrtotal + ierr
        call MPI_Comm_size (MPI_COMM_WORLD, nprocs, ierr)
        ierrtotal = ierrtotal + ierr
        call MPI_Get_processor_name (hostname, lenhost, ierr)
        ierrtotal = ierrtotal + ierr
      else
        myid = 0
        nprocs = 1
       call get_environment_variable &
           (HOSTcmd,hostname,lenhost,ierr,trim_host_name)
        print *, ' myid =', myid, ' hostname = ', hostname(1:lenhost)
        ierrtotal = ierrtotal + ierr
      endif
 
      if (MultiProcessorRun) then
!-------------------------------------------------------------------------
! MultiProcessorRun debug

      print *, ' MultiProcessorRun test output = initialized MPI_Comm values: '
      if (ierrtotal .ne. MPI_SUCCESS .and. ierrtotal .ne. 0) then
         print *, ' myid =', myid, ' MPI_Init ierrtotal = ', ierrtotal
         stop
      else
        print *, ' myid =', myid, ' ierrtotal = ', ierrtotal
        print *, ' myid =', myid, ' MPI_COMM_WORLD = ', MPI_COMM_WORLD
        print *, ' myid =', myid, ' hostname = ', hostname(1:lenhost)
        print *, ' myid =', myid, ' lenhost = ', lenhost
        print *, ' myid =', myid, ' nprocs = ', nprocs
      endif

      else
!-------------------------------------------------------------------------
! SingleProcessorRun debug

      print *, ' SingleProcessorRun test output = initialized MPI_Comm values: '
      if (ierrtotal .ne. 0) then
         print *, ' myid =', myid, ' ierrtotal = ', ierrtotal
         stop
      else
        print *, ' myid =', myid, ' ierrtotal = ', ierrtotal
        print *, ' myid =', myid, ' hostname = ', hostname(1:lenhost)
        print *, ' myid =', myid, ' lenhost = ', lenhost
        print *, ' myid =', myid, ' nprocs = ', nprocs
      endif

!-------------------------------------------------------------------------
      endif

!-------------------------------------------------------------------------
! multi- or single-processor-run

      if (myid .eq. 0) then
        if ( nprocs == 1 ) then
          print *, ' single-processor-run '
        else
          print *, ' MPI multi-processor-run '
        endif
      endif

!-------------------------------------------------------------------------
! timing

      if (myid .eq. 0) then
        call system_clock(count_number1,count_rate1,count_max1)
          print *, ' program: system_clock: ' &
                 , ' count_number1,count_rate1,count_max1 = ' &
                 ,   count_number1,count_rate1,count_max1

        call date_and_time(sdate,stime,szone,svalues)
          WRITE(istdo,*) ' program start time = ', stime
         print *, ' svalues = ', svalues
        call cpu_time (CPUtimeStartFull)
      endif

!-------------------------------------------------------------------------
! call slaves = determine machine-dependent parameters in MPI mode
!
      call cpu_time (CPUtimeStart)
      call slaves 
      call cpu_time (CPUtimeStop)
      CPUtimeUsed = CPUtimeStop - CPUtimeStart

      print *, ' timing in main program for slaves ' &
             , ' CPUtime used = ', CPUtimeUsed, ' seconds ' &
             , ' on node = ', myid

    if (MultiProcessorRun) then
      call MPI_Barrier (MPI_COMM_WORLD,ierr)
    endif

!-------------------------------------------------------------------------
! determine number_of_threads
!
      if (myid .eq. 0) then

        number_of_threads = 0
! read number_of_threads from input_threads
        INQUIRE(file = 'input_threads', exist=input_threadsExists)
        if(input_threadsExists) then
          OPEN (5, FILE ='input_threads', STATUS = 'OLD', IOSTAT = ierr)
          read (5,*) number_of_threads
          write(istdo,*) ' number_of_threads from file input_threads = ', &
                           number_of_threads
        else
! otherwise
! read number_of_threads from console
          print *,       ' file  input_threads  does not exist '
          write(istde,*) ' '
          write(istde,*) ' type number_of_threads '
          write(istde,*) ' '
          flush(istde)
          read (*,*) number_of_threads
          write(istde,*) ' number_of_threads from console = ', &
                           number_of_threads
          write(istdo,*) ' number_of_threads from console = ', &
                           number_of_threads
        endif

! otherwise
! read number_of_threads from environment
        if (number_of_threads == 0) then
          number_of_threads = OMP_GET_NUM_THREADS()
          write(istdo,*) ' number_of_threads from OMP_GET_NUM_THREADS = ', &
                           number_of_threads
        endif

! otherwise
! set default number_of_threads = 1
        if (number_of_threads == 0) then
          number_of_threads = 1
          write(istdo,*) ' number_of_threads default = ', &
                           number_of_threads
        endif
      endif

!-------------------------------------------------------------------------
! set environment variable OMP_NUM_THREADS = number_of_threads 
!
    if (MultiProcessorRun) then
      CALL MPI_Bcast (number_of_threads,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)
    endif

! test environment variable OMP_NUM_THREADS 
      call OMP_SET_NUM_THREADS(number_of_threads)
   nthreads = OMP_GET_NUM_THREADS()
   if (nthreads == 0) then
     print *, ' on myid ', myid &
            , ' error in program: nthreads = 0 '
     stop     ' error in program: nthreads = 0 '
   else
     print *, ' program: myid ', myid, ' OMP_GET_NUM_THREADS = ', nthreads
   endif

!-------------------------------------------------------------------------
! call loopthread = each node
!
      call cpu_time (CPUtimeStart)
      call loopthread
      call cpu_time (CPUtimeStop)
      CPUtimeUsed = CPUtimeStop - CPUtimeStart

      print *, ' timing in main program for loopthread ' &
             , ' CPU time used = ', CPUtimeUsed, ' seconds ' &
             , ' with threads = ', number_of_threads &
             , ' on node = ', myid

    if (MultiProcessorRun) then
      call MPI_Barrier (MPI_COMM_WORLD,ierr)
    endif

!-------------------------------------------------------------------------
! MatrixDotVector (Y = A x X) = MPI to all nodes + OpenMP threads within nodes
!
      if (myid .eq. 0) then

        INQUIRE(file = 'input_matrix', exist=input_matrixExists)
        if(input_matrixExists) then
          OPEN (5, FILE ='input_matrix', STATUS = 'OLD', IOSTAT = ierr)
          read (5,*) dimenA
        else
          print *,       ' file  input_matrix  does not exist '
          write(istde,*) ' '
          write(istde,*) ' type matrix size dimenA '
          write(istde,*) ' '
          flush(istde)
          read (*,*) dimenA
          write(istde,*) ' dimenA = ', &
                           dimenA
          write(istdo,*) ' dimenA = ', &
                           dimenA
        endif
      endif

    if (MultiProcessorRun) then
      CALL MPI_Bcast (dimenA,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)
    endif

      allocate ( MatrixA(1:dimenA,1:dimenA), stat=allocerror )
    if ( allocerror /= 0 ) stop 'error in memory allocation for MatrixA'
      allocate ( VectorX(1:dimenA), stat=allocerror )
    if ( allocerror /= 0 ) stop 'error in memory allocation for VectorX'
      allocate ( VectorY(1:dimenA), stat=allocerror )
    if ( allocerror /= 0 ) stop 'error in memory allocation for VectorY'

!  input: MatrixA = 1 ; VectorX = 1
! output: VectorY = 0 initially

      MatrixA(1:dimenA,1:dimenA) = 1.0

      do i = 1, dimenA
        VectorX(i) = 1.0
        VectorY(i) = 0.0
      end do

      if (myid .eq. 0) then
        print *, ' '
        print *, ' in test_mpi_OpenMP '
        print *, ' VectorX(1) = ', VectorX(1)
        print *, ' VectorX(2) = ', VectorX(2)
        print *, ' VectorX(3) = ', VectorX(3)
        print *, ' VectorX(4) = ', VectorX(4)
        print *, ' VectorX(dimenA) = ', VectorX(dimenA)
        print *, ' VectorX(dimenA-1) = ', VectorX(dimenA-1)
        print *, ' '
      endif

!-------------------------------------------------------------------------
      call cpu_time (CPUtimeStart)
      call MatrixDotVector (dimenA)
      call cpu_time (CPUtimeStop)
      CPUtimeUsed = CPUtimeStop - CPUtimeStart

      if (myid .eq. 0) then
        print *, ' '
        print *, ' in test_mpi_OpenMP '
        print *, ' VectorY(1) = ', VectorY(1)
        print *, ' VectorY(2) = ', VectorY(2)
        print *, ' VectorY(3) = ', VectorY(3)
        print *, ' VectorY(4) = ', VectorY(4)
        print *, ' VectorY(dimenA) = ', VectorY(dimenA)
        print *, ' VectorY(dimenA-1) = ', VectorY(dimenA-1)
        print *, ' '
      endif

      print *, ' timing in main program for MatrixDotVector ' &
             , ' CPUtime used = ', CPUtimeUsed, ' seconds ' &
             , ' for size = ', dimenA, ' x ', dimenA &
             , ' with threads = ', number_of_threads &
             , ' on node = ', myid


!-------------------------------------------------------------------------
! dealloc and stop

      if ( allocated(MatrixA) )  deallocate(MatrixA,stat=allocerror)
       if ( allocerror /= 0 ) stop 'error in memory deallocation for MatrixA'
      if ( allocated(VectorX) )  deallocate(VectorX,stat=allocerror)
       if ( allocerror /= 0 ) stop 'error in memory deallocation for VectorX'
      if ( allocated(VectorY) )  deallocate(VectorY,stat=allocerror)
       if ( allocerror /= 0 ) stop 'error in memory deallocation for VectorY'

      if (myid .eq. 0) then
         print *, '===================================================='
         print *, '       : Execution Finished ...'
         print *, '===================================================='
      endif

    if (MultiProcessorRun) then
      call MPI_Barrier (MPI_COMM_WORLD,ierr)
      print *, 'mpi stopped by node', myid
      call MPI_Barrier (MPI_COMM_WORLD,ierr)
!     call MPI_Finalize (ierr)

      call MPI_Finalize (ierr)
      if (ierr .ne. MPI_SUCCESS) then
         print *, ' myid =', myid, ' MPI_Finalize ierr = ', ierr
      endif
    endif

      if (myid .eq. 0) then
        call cpu_time (CPUtimeStopFull)
        CPUtimeUsedFull = CPUtimeStopFull - CPUtimeStartFull
        print *, ' '
        print *, ' full timing in main '
        print *, ' full CPUtime used = ', CPUtimeUsedFull , ' seconds '
        print *, ' '

        call date_and_time(edate,etime,ezone,evalues)
          WRITE(istdo,*) ' program end time = ', etime
         print *, ' evalues = ', evalues

        call system_clock(count_number2,count_rate2,count_max2)
          print *, ' program: system_clock: ' &
                 , ' count_number2,count_rate2,count_max2 = ' &
                 ,   count_number2,count_rate2,count_max2

        if ( count_number2 > count_max2 .or. &
             count_number1 > count_max2 .or. &
             (count_number2 - count_number1) < 0 .or. &
             count_rate2 /= count_rate1 .or. &
             count_rate2 == 0 .or. &
             count_max2 /= count_max1 .or. &
             count_max2 == 0 ) then
          WRITE(istdo,*) '---------------------------------- '
          WRITE(istdo,*) ' myid =', myid, ' program: wall_clock error '
          WRITE(istdo,*) '---------------------------------- '
        else
          wall_time_used = &
            real((count_number2 - count_number1)) / real(count_rate2)
          WRITE(istdo,*) '---------------------------------- '
          WRITE(istdo,*) ' myid =', myid, &
                         ' program: wall_time_used (sec) = ', &
                          wall_time_used
          WRITE(istdo,*) '---------------------------------- '
        endif
      endif
      stop
      end

