!     module gauss_summa_function

! computes sum = 0 + 1 + ... + (nprocs-1) 
! Gauss_sum_G via MPI_Allreduce
! Gauss_sum_C via explicit summation in loop
! Gauss_sum_N from formula N * (N+1), where N := numberek = dummy argument

function gauss_summa (numberek)
!use module_gauss_summa_interface
use module_mpi_universe

implicit none
integer gauss_summa
integer Gauss_number, inGs
integer Gauss_sum_N, Gauss_sum_G, Gauss_sum_C
integer, intent(in) :: numberek

! fake initialisation
Gauss_sum_C = 7
Gauss_sum_G = 8
Gauss_sum_N = 9

      if (MultiProcessorRun) then
call MPI_Barrier (MPI_COMM_WORLD,ierr)
! all nodes do MPI_Allreduce with MPI_SUM for Gauss_sum_G
Gauss_number = myid
call MPI_Allreduce (Gauss_number, Gauss_sum_G, 1, MPI_INTEGER, &
                       MPI_SUM, MPI_COMM_WORLD, ierr) 
! master broadcasts Gauss_sum_G
CALL MPI_Bcast (Gauss_sum_G,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)
      else
Gauss_sum_G = numberek * ( numberek + 1 ) / 2
      endif

! all nodes compute Gauss_sum_C
Gauss_sum_C = 0
do inGs = 0, nprocs - 1
  Gauss_sum_C = Gauss_sum_C + inGs
enddo

! all nodes compute Gauss_sum_N
Gauss_sum_N = numberek * ( numberek + 1 ) / 2

if ( Gauss_sum_C .eq. Gauss_sum_G .and. Gauss_sum_G .eq. Gauss_sum_N ) then
  gauss_summa = Gauss_sum_C
  return
else
  print *, ' Gauss_sum_C ', Gauss_sum_C
  print *, ' Gauss_sum_G ', Gauss_sum_G
  print *, ' Gauss_sum_N ', Gauss_sum_N
  stop
  endif
end

