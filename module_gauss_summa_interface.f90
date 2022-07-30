module module_gauss_summa_interface
  interface gauss_summa_interface
    function gauss_summa (numberek)
      use module_mpi_universe
implicit none
      integer gauss_summa
      integer Gauss_number, inGs
      integer Gauss_sum_N, Gauss_sum_G, Gauss_sum_C
      integer, intent(in) :: numberek
    end function gauss_summa
  end interface gauss_summa_interface
end module module_gauss_summa_interface

