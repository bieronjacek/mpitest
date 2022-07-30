************************************************************************
*
      program dlam
*
      IMPLICIT DOUBLE PRECISION          (A-H, O-Z)

c     CHARACTER          CMACH
c     INTEGER EMIN, EMAX, RND, T

*   Set the machine-dependent parameters:
*
      eps   = DLAMCH( 'E') 
      sfmin = DLAMCH( 'S') 
      base  = DLAMCH( 'B') 
      ebase = DLAMCH( 'P') 
      t     = DLAMCH( 'N') 
      rnd   = DLAMCH( 'R') 
      emin  = DLAMCH( 'M') 
      rmin  = DLAMCH( 'U') 
      emax  = DLAMCH( 'L') 
      rmax  = DLAMCH( 'O') 

      print *, eps 
      print *, sfmin
      print *, base
      print *, ebase
      print *, t
      print *, rnd
      print *, emin
      print *, rmin
      print *, emax
      print *, rmax
*
      stop
      end


