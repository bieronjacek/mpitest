
      subroutine matrixDotVector (dimenA)

!     INCLUDE 'mpif.h'
      use module_mpi_universe
      use module_io_numbers
      use omp_lib
      use module_matrixAXY

      implicit none

      integer, intent(in) :: dimenA

      integer :: i, j, k
      integer :: allocerror

        real :: CPUtimeStart, CPUtimeStop, CPUtimeUsed

        character sdate*8, stime*10, szone*5
        character edate*8, etime*10, ezone*5
        character tdate*8, ttime*10, tzone*5
        integer :: svalues(8), evalues(8), tvalues(8)

        integer :: count_number1,count_rate1,count_max1
        integer :: count_number2,count_rate2,count_max2
        real :: system_time_counter

   integer :: idthread, nthreads
!  integer :: number_of_threads ! moved to module_mpi_universe

        logical :: FlagPrinted1000
        logical :: FlagPrinted100
        logical :: FlagPrinted10
        logical :: FlagPrinted1
        logical :: FlagPrinted25
        logical :: FlagPrinted50
        logical :: FlagPrinted75
        logical :: FlagPrinted90

!----------------------------------------------------------------------

   nthreads = 0
! failed read nthreads from environment :  nthreads = OMP_GET_NUM_THREADS()
! instead: set nthreads from number_of_threads
   nthreads = number_of_threads
   if (nthreads == 0) then
     print *, ' on myid ', myid &
            , ' error in matrixDotVector: nthreads = 0 '
     stop     ' error in matrixDotVector: nthreads = 0 '
   else
     print *, ' matrix: myid ', myid, ' nthreads = ', nthreads
   endif

!$omp parallel private(idthread)&
!$OMP NUM_THREADS(nthreads)
   idthread = omp_get_thread_num()
   write (istdo,*) ' in matrix: Hello World from thread', idthread, &
                   ' on myid ', myid
!$omp barrier
   if ( idthread == 0 ) then
     nthreads = omp_get_num_threads()
     write (istdo,*) 'There are', nthreads, 'threads on myid ', myid
   end if
!$omp end parallel

!----------------------------------------------------------------------
      call cpu_time (CPUtimeStart)
      if (myid .eq. 0) then
        call system_clock(count_number1,count_rate1,count_max1)
        call date_and_time(sdate,stime,szone,svalues)
        WRITE(istdo,*) '----------------------------------- '
        WRITE(istdo,*) ' begin wall time = ', stime
        WRITE(istdo,*) '----------------------------------- '
      endif

!--------------------------------
! progress printing
!
        if ( dimenA .lt. 1000 ) then
! supress progress printing
          FlagPrinted1000 = .true.
          FlagPrinted100 = .true.
          FlagPrinted10 = .true.
          FlagPrinted25 = .true.
          FlagPrinted50 = .true.
          FlagPrinted75 = .true.
          FlagPrinted90 = .true.
        else
! allow progress printing
          FlagPrinted1000 = .false.
          FlagPrinted100 = .false.
          FlagPrinted10 = .false.
          FlagPrinted1 = .false.
          FlagPrinted25 = .false.
          FlagPrinted50 = .false.
          FlagPrinted75 = .false.
          FlagPrinted90 = .false.
        endif

!----------------------------------------------------------------------
! outer MPI loop
!
      do i =  myid+1, dimenA, nprocs
!
!--------------------------------
! progress printing
! 
        if ( i == dimenA * 0.0001 .and. FlagPrinted1000 .eqv. .false. ) then
          call date_and_time(tdate,ttime,tzone,tvalues)
           write (istdo,*) ' matrix: progress = ', i, ' of ', dimenA &
                          ,' on node = ', myid, ' at wall time = ', ttime
          FlagPrinted1000 = .true.

        else if ( i == dimenA * 0.001 .and. FlagPrinted100 .eqv. .false. ) then
          call date_and_time(tdate,ttime,tzone,tvalues)
           write (istdo,*) ' matrix: progress = ', i, ' of ', dimenA &
                          ,' on node = ', myid, ' at wall time = ', ttime
          FlagPrinted100 = .true.

        else if ( i == dimenA * 0.01 .and. FlagPrinted10 .eqv. .false. ) then
          call date_and_time(tdate,ttime,tzone,tvalues)
           write (istdo,*) ' matrix: progress = ', i, ' of ', dimenA &
                          ,' on node = ', myid, ' at wall time = ', ttime
          FlagPrinted10 = .true.

        else if ( i == dimenA * 0.1 .and. FlagPrinted1 .eqv. .false. ) then
          call date_and_time(tdate,ttime,tzone,tvalues)
           write (istdo,*) ' matrix: progress = ', i, ' of ', dimenA &
                          ,' on node = ', myid, ' at wall time = ', ttime
          FlagPrinted1 = .true.

        else if ( i == dimenA * 0.25 .and. FlagPrinted25 .eqv. .false. ) then
          call date_and_time(tdate,ttime,tzone,tvalues)
           write (istdo,*) ' matrix: progress = ', i, ' of ', dimenA &
                          ,' on node = ', myid, ' at wall time = ', ttime
          FlagPrinted25 = .true.

        else if ( i == dimenA * 0.50 .and. FlagPrinted50 .eqv. .false. ) then
          call date_and_time(tdate,ttime,tzone,tvalues)
           write (istdo,*) ' matrix: progress = ', i, ' of ', dimenA &
                          ,' on node = ', myid, ' at wall time = ', ttime
          FlagPrinted50 = .true.

        else if ( i == dimenA * 0.75 .and. FlagPrinted75 .eqv. .false. ) then
          call date_and_time(tdate,ttime,tzone,tvalues)
           write (istdo,*) ' matrix: progress = ', i, ' of ', dimenA &
                          ,' on node = ', myid, ' at wall time = ', ttime
          FlagPrinted75 = .true.

        else if ( i == dimenA * 0.90 .and. FlagPrinted90 .eqv. .false. ) then
          call date_and_time(tdate,ttime,tzone,tvalues)
           write (istdo,*) ' matrix: progress = ', i, ' of ', dimenA &
                          ,' on node = ', myid, ' at wall time = ', ttime
          FlagPrinted90 = .true.
        endif

!--------------------------------
! inner OpenMP loop

        if ( i <= nprocs ) then
!         nthreads = OMP_GET_NUM_THREADS()
           write (istdo,*) ' matrix: in outer MPI loop nthreads = ', nthreads &
                          ,' on node = ', myid
        endif

!$OMP PARALLEL DO SCHEDULE(RUNTIME) DEFAULT(NONE)&
!$OMP SHARED(istdo,myid,dimenA,MatrixA,VectorX,VectorY,i)&
!$OMP PRIVATE(j,k,idthread)&
!$OMP NUM_THREADS(nthreads)

        do j = 1, dimenA
         do k = 1, dimenA
           MatrixA(i,j) = MatrixA(i,k) * MatrixA(k,j)
           idthread = omp_get_thread_num()
           if (i==myid+1 .and. j==1 .and. k==1) then
             WRITE(istdo,&
             '(" matrix: myid, idthread, i,j,k = ", I5,I5,I5,I5,I5)') &
                         myid, idthread, i,j,k
!           print *, ' matrix: myid, idthread, i,j,k = ' &
!                            , myid, idthread, i,j,k
           endif
           if (i==myid+1 .and. j==dimenA .and. k==dimenA) then
             WRITE(istdo,&
             '(" matrix: myid, idthread, i,j,k = ", I5,I5,I5,I5,I5)') &
                         myid, idthread, i,j,k
!           print *, ' matrix: myid, idthread, i,j,k = ' &
!                            , myid, idthread, i,j,k
           endif
         end do
          VectorY(i) = VectorY(i) + MatrixA(i,j) * VectorX(j)
        end do

!--------------------------------
!$OMP END PARALLEL DO

! inner OpenMP loop end

!----------------------------------------------------------------------
!     if (myid .eq. 0) then
!       write(istdo,*) ' loop progress = ', i, ' of ', dimenA
!       flush(istdo)
!       if ( ( (i-1)/(nprocs*100) )*nprocs*100 .eq. (i-1) ) then
!          write(istdo,*) ' loop progress = ', i, ' of ', dimenA
!          flush(istdo)
!        endif
!      endif
!----------------------------------------------------------------------

      end do
! outer MPI loop end
!----------------------------------------------------------------------

      if (myid .eq. 0) then
        call date_and_time(edate,etime,ezone,evalues)
        WRITE(istdo,*) '---------------------------------- '
        WRITE(istdo,*) ' | end wall time = ', etime
        WRITE(istdo,*) '---------------------------------- '
        call system_clock(count_number2,count_rate2,count_max2)
        if ( (count_number2 - count_number1) > count_max2 .or. &
             (count_max2 /= count_max1) .or. &
             (count_rate2 /= count_rate1) .or. &
             (count_max2 == 0) ) then
          WRITE(istdo,*) '---------------------------------- '
          WRITE(istdo,*) ' | system_clock error '
          WRITE(istdo,*) '---------------------------------- '
        else
         system_time_counter = &
           real((count_number2 - count_number1)) / real(count_rate2)
          WRITE(istdo,*) '---------------------------------- '
          WRITE(istdo,*) ' | system_time_counter (sec) = ', system_time_counter
          WRITE(istdo,*) '---------------------------------- '
        endif
      endif

      call cpu_time (CPUtimeStop)
      CPUtimeUsed = CPUtimeStop - CPUtimeStart
      print *, ' CPUtimeUsed in matrixDotVector = ', CPUtimeUsed, ' seconds '
      print *, ' on node = ', myid

      if (myid .eq. 0) then
        print *, ' '
        print *, ' in subroutine matrixDotVector '
        print *, ' VectorY(1) = ', VectorY(1)
        print *, ' VectorY(2) = ', VectorY(2)
        print *, ' VectorY(3) = ', VectorY(3)
        print *, ' VectorY(4) = ', VectorY(4)
        print *, ' VectorY(dimenA) = ', VectorY(dimenA)
        print *, ' VectorY(dimenA-1) = ', VectorY(dimenA-1)
        print *, ' '
      endif

!----------------------------------------------------------------------
! MPI_Allreduce VectorY
!
!     if ( allocated(MatrixA) )  deallocate(MatrixA,stat=allocerror)
!   if ( allocerror /= 0 ) stop 'error in memory deallocation for MatrixA'

      do i =  myid+1, dimenA, nprocs
        VectorX(i) = VectorY(i)
      end do

      if (MultiProcessorRun) then
   call MPI_Allreduce (VectorX, VectorY, dimenA, MPI_REAL, &
                       MPI_SUM, MPI_COMM_WORLD, ierr)
      endif

      return
      end

