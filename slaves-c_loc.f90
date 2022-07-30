      subroutine slaves

!     INCLUDE 'mpif.h'
      use module_mpi_universe
      use module_io_numbers
      use module_gauss_summa_interface
! -fallow-argument-mismatch
      use, intrinsic :: ISO_C_BINDING, only: c_loc

      implicit none

!     character msg*32
      character, TARGET :: msg*32

       INTEGER M_M_P_N

      INTEGER chiffre,                             inID
      INTEGER(4), TARGET :: nodexchiffre, msgLength
      INTEGER gauss_sum

      character input_chiffre*13
      LOGICAL FOUND

!******************************************************************************

       if (myid .eq. 0) then
          M_M_P_N = MPI_MAX_PROCESSOR_NAME
         print *, ' MPI_MAX_PROCESSOR_NAME = ', M_M_P_N
       endif

      msg = hostname
      msgLength = len_trim (msg)
!        print *, ' msg = ', msg
!        print *, ' msgLength = ', msgLength

!------------------------------------------------------------------------------
    if (MultiProcessorRun) then
!     call MPI_Barrier (MPI_COMM_WORLD,ierr)

! slaves send hostnames
      if (myid .ne. 0) then
         call MPI_Send (c_loc(msgLength), 1, MPI_INTEGER,   0, myid, &
                        MPI_COMM_WORLD, ierr)   ! Send msgLength
       call MPI_Send (c_loc(msg), msgLength, MPI_CHARACTER, 0, myid+nprocs, &
                        MPI_COMM_WORLD, ierr)   ! Send msg
      else
! master receives hostnames
         print *
         print *, ' ----- host list begin ------------------- '
         print *, ' ', msg(1:msgLength)
         do inID = 1, nprocs - 1
           call MPI_Recv (c_loc(msgLength), 1, MPI_INTEGER,  inID,inID, &
                          MPI_COMM_WORLD, istat, ierr)
           call MPI_Recv (c_loc(msg),msgLength,MPI_CHARACTER,inID,inID+nprocs,&
                           MPI_COMM_WORLD, istat, ierr)
! master node prints hostnames
            print *, ' ', msg(1:msgLength)
         enddo
         print *, ' ----- host list end --------------------- '
         print *
      endif

!     call MPI_Barrier (MPI_COMM_WORLD,ierr)
    endif ! MultiProcessorRun 
!------------------------------------------------------------------------------

! master node reads input and broadcasts
      if (myid .EQ. 0) then

      input_chiffre = 'input_chiffre'
      INQUIRE (FILE = input_chiffre, EXIST = FOUND)
      IF (.NOT. FOUND) THEN
         print *, ' ERROR: file  input_chiffre  does not exist'
         goto 911
      ENDIF

      OPEN (iwork, FILE = input_chiffre, STATUS = 'OLD', IOSTAT = ierr)
         if (ierr .ne. 0) then
           print *, ' iwork open ERROR, ierr = ', ierr, ' myid = ', myid
           goto 911
         endif
         print *, ' ---------------------------------------- '
         print *, 'myid = ', myid, ' file  input_chiffre  opened'
         print *, ' ---------------------------------------- '
      endif

 111  continue

      IF (myid .EQ. 0) THEN
!        print *, 'myid = ', myid
!        print *,
!        print *, ' Give node ',myid,' an integer '
!        print *,
!        READ *, chiffre
!cdebug
!        print *, ' debug1111: chiffre = ', chiffre
!cd
         read  (iwork,*,end=911) chiffre
      ENDIF

    if (MultiProcessorRun) then
      CALL MPI_Bcast (chiffre,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)
    endif

! all nodes exit if chiffre = 0
         IF (chiffre .eq. 0) then
           goto 999
         ENDIF

!cdebug
!        print *, ' debug1112: chiffre = ', chiffre
!cd
!        CALL alloc (pntriq, nnnwp  *ncf, 4)

! all nodes call function gauss_summa

      gauss_sum = gauss_summa (nprocs-1)
      if (myid .eq. 0) then
!       print *, ' gauss_sum = ', gauss_sum
        write (istdo, '(A,I8)') &
               ' Gauss sum = 0 + 1 + ... + (nprocs-1) = ', gauss_sum
      endif

! all nodes compute machine-dependent parameters
      call dlam0 
      nodexchiffre = chiffre + myid

!           print *, ' msg, msgLength, myid, nodexchiffre = ', &
!                 msg(1:msgLength), msgLength, myid, nodexchiffre

    if (MultiProcessorRun) then
! slaves send
      if (myid .ne. 0) then
            print *, ' node',myid,' sends result = ', nodexchiffre
        call MPI_Send (c_loc(msgLength), 1, MPI_INTEGER, 0, myid+2*nprocs, &
                        MPI_COMM_WORLD, ierr)   ! Send msgLength
        call MPI_Send (c_loc(msg),msgLength,MPI_CHARACTER,0,myid+3*nprocs, &
                        MPI_COMM_WORLD, ierr)   ! Send msg
        call MPI_Send (c_loc(nodexchiffre),1,MPI_INTEGER,0,myid+4*nprocs, &
                        MPI_COMM_WORLD, ierr)   ! Send result
      else
! master receives
         do inID = 1, nprocs - 1
          call MPI_Recv (c_loc(msgLength),1,MPI_INTEGER,inID,inID+2*nprocs,&
                           MPI_COMM_WORLD, istat, ierr)
          call MPI_Recv (c_loc(msg),msgLength,MPI_CHARACTER,inID,inID+3*nprocs,&
                           MPI_COMM_WORLD, istat, ierr)
          call MPI_Recv (c_loc(nodexchiffre),1,MPI_INTEGER,inID,inID+4*nprocs,&
                           MPI_COMM_WORLD, istat, ierr)
         enddo
      endif
    endif ! MultiProcessorRun 

      if (myid .eq. 0) then
! master node prints
!
!          print *, ' from: hostID + chiffre = ', &
!                msg(1:msgLength), ' : ', inID, '+', chiffre, '=', &
!                nodexchiffre
!
           if (inID .gt. 9999 .or. &
              chiffre .gt. 9999 .or. &
         nodexchiffre .gt. 9999 ) then
             print *, ' change format for write in subro '
             stop
           endif
           write (istdo,'(a,a,a,I4,a,I4,a,I4)') &
               ' from: hostID + chiffre = ', msg(1:msgLength), &
               ' : ', inID, ' + ', chiffre, ' = ', nodexchiffre
           flush istdo
      endif

      goto 111

 911  continue
      print *, ' error read input '
 999  continue

      return
      end

