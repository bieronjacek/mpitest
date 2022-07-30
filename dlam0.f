************************************************************************
*
      subroutine dlam0 
*
      use module_mpi_universe

      IMPLICIT DOUBLE PRECISION          (A-H, O-Z)
c     INCLUDE 'mpif.h'

c     CHARACTER          CMACH
c     INTEGER EMIN, EMAX, RND, T

*   Set the machine-dependent parameters:
*
        if (myid .eq. 0) then
      print *, ' eps   = relative machine precision'
      print *, ' sfmin = safe minimum, such that 1/sfmin',
     :                 ' does not overflow'
      print *, ' base  = base of the machine'
*     print *, ' prec  = eps*base'
      print *, ' bits  = number of (base) digits in the',
     :                 ' mantissa'
      print *, ' rnd   = 1.0 when rounding occurs in addition,',
     :                 ' 0.0  otherwise'
      print *, ' emin  = minimum exponent before (gradual)',
     :                 ' underflow'
      print *, ' rmin  = underflow threshold - base**(emin-1)'
      print *, ' emax  = largest exponent before overflow'
      print *, ' rmax  = overflow threshold',
     :                 ' - (base**emax)*(1-eps)'
        endif

      if (MultiProcessorRun) then 
      call MPI_Barrier (MPI_COMM_WORLD,ierr)
      endif
      eps   = DLAMCH( 'E') 
      print *, ' on node = ', myid, ' eps =   ', eps 
      if (MultiProcessorRun) then 
      call MPI_Barrier (MPI_COMM_WORLD,ierr)
      endif
      sfmin = DLAMCH( 'S') 
      print *, ' on node = ', myid, ' sfmin = ', sfmin
      if (MultiProcessorRun) then 
      call MPI_Barrier (MPI_COMM_WORLD,ierr)
      endif
      base  = DLAMCH( 'B') 
      print *, ' on node = ', myid, ' base =  ', base
*     call MPI_Barrier (MPI_COMM_WORLD,ierr)
*     ebase = DLAMCH( 'P') 
*     print *, ' on node = ', myid, ' ebase = ', ebase
      if (MultiProcessorRun) then 
      call MPI_Barrier (MPI_COMM_WORLD,ierr)
      endif
      bits     = DLAMCH( 'N') 
      print *, ' on node = ', myid, ' bits =     ', bits
      if (MultiProcessorRun) then 
      call MPI_Barrier (MPI_COMM_WORLD,ierr)
      endif
      rnd   = DLAMCH( 'R') 
      print *, ' on node = ', myid, ' rnd =   ', rnd
      if (MultiProcessorRun) then 
      call MPI_Barrier (MPI_COMM_WORLD,ierr)
      endif
      emin  = DLAMCH( 'M') 
      print *, ' on node = ', myid, ' emin =  ', emin
      if (MultiProcessorRun) then 
      call MPI_Barrier (MPI_COMM_WORLD,ierr)
      endif
      rmin  = DLAMCH( 'U') 
      print *, ' on node = ', myid, ' rmin =  ', rmin
      if (MultiProcessorRun) then 
      call MPI_Barrier (MPI_COMM_WORLD,ierr)
      endif
      emax  = DLAMCH( 'L') 
      print *, ' on node = ', myid, ' emax =  ', emax
      if (MultiProcessorRun) then 
      call MPI_Barrier (MPI_COMM_WORLD,ierr)
      endif
      rmax  = DLAMCH( 'O') 
      print *, ' on node = ', myid, ' rmax =  ', rmax
      if (MultiProcessorRun) then 
      call MPI_Barrier (MPI_COMM_WORLD,ierr)
      endif
*
      return
      end

