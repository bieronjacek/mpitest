
subroutine loopthread
      use module_mpi_universe
      use omp_lib
      use module_io_numbers

        IMPLICIT NONE
        INTEGER I,J
!       INTEGER, PARAMETER:: DIM1=30000, DIM2=40000  ! 3.5 min
!       INTEGER, PARAMETER:: DIM1=20000, DIM2=20000  ! 1 min
        INTEGER, PARAMETER:: DIM1=10000, DIM2=10000  ! 15 sec
      real, allocatable :: A(:), B(:,:), C(:,:)
      integer :: allocerror

        character sdate*8,stime*10,szone*5
        character edate*8,etime*10,ezone*5
        character rdate*8,rtime*10,rzone*5
        integer :: svalues(8), evalues(8), rvalues(8)

        integer :: count_number1,count_rate1,count_max1
        integer :: count_number2,count_rate2,count_max2
        real :: system_time_used

        REAL before, after, elapsed, S
        INTEGER nthreads

      allocate ( A(1:dim1), stat=allocerror )
    if ( allocerror /= 0 ) stop 'error in memory allocation for A'
      allocate ( B(1:dim2,1:dim1), stat=allocerror )
    if ( allocerror /= 0 ) stop 'error in memory allocation for A'
      allocate ( C(1:dim2,1:dim1), stat=allocerror )
    if ( allocerror /= 0 ) stop 'error in memory allocation for A'

!       CALL RANDOM_NUMBER(A)
!       A(1:DIM1) = 1.0
      do i = 1, dim1
        A(i) = i
      end do

        call cpu_time(before)
        call system_clock(count_number1,count_rate1,count_max1)
        call date_and_time(sdate,stime,szone,svalues)
        print *, ' enter loopthread on myid =', myid, ' begin time = ', stime
!       WRITE(istdo,*) ' myid =', myid, ' begin time = ', stime

!$OMP PARALLEL DO SCHEDULE(RUNTIME) PRIVATE(I,J) SHARED (A,B,C,nthreads)
        DO J=1,DIM2
           if ( J == 1 ) then
                nthreads = OMP_GET_NUM_THREADS()
           endif
          DO I=2, DIM1
                B(J,I) = ( (A(I)+A(I-1))/2.0 ) / SQRT(A(I))
                C(J,I) = SQRT( A(I)*2 ) / ( A(I)-(A(I)/2.0) )
                B(J,I) = C(J,I) * ( B(J,I)**2 ) * SIN(A(I))
          END DO
        END DO

!$OMP END PARALLEL DO

        call date_and_time(edate,etime,ezone,evalues)
         WRITE(istdo,*) ' myid =', myid, ' loopthread  end time = ', etime
        call system_clock(count_number2,count_rate2,count_max2)
        if ( (count_number2 - count_number1) > count_max2 .or. &
             (count_max2 /= count_max1) .or. & 
             (count_rate2 /= count_rate1) .or. &
             (count_max2 == 0) ) then
          WRITE(istdo,*) '---------------------------------- '
          WRITE(istdo,*) ' myid =', myid, ' loopthread system_clock error '
          WRITE(istdo,*) '---------------------------------- '
        else
         system_time_used = &
           real((count_number2 - count_number1)) / real(count_rate2)
          WRITE(istdo,*) '---------------------------------- '
       WRITE(istdo,*) ' myid =', myid, &
                      ' loopthread system_time_used (sec) = ', &
                        system_time_used
          WRITE(istdo,*) '---------------------------------- '
        endif
        call cpu_time(after)
        !Find elapsed time; convert to seconds from ms
        elapsed = after-before

        S=MAXVAL(B)

        WRITE(istdo,&
     '("loopthread maximum of B = ",1pe9.2," found in ", 1pe9.2, &
                " seconds using ", I0," threads ", " on myid ", I0)') &
                 S,elapsed,nthreads, myid

!-------------------------------------------------------------------------
! dealloc

      if ( allocated(A) )  deallocate(A,stat=allocerror)
       if ( allocerror /= 0 ) stop 'error in memory deallocation for Matrix A'
      if ( allocated(B) )  deallocate(B,stat=allocerror)
       if ( allocerror /= 0 ) stop 'error in memory deallocation for Vector X'
      if ( allocated(C) )  deallocate(C,stat=allocerror)
       if ( allocerror /= 0 ) stop 'error in memory deallocation for Vector Y'

        call date_and_time(rdate,rtime,rzone,rvalues)
      print *, ' leave loopthread on myid =', myid, ' time = ', rtime

return
END subroutine loopthread

