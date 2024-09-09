module qft_module
  implicit none

  type :: qft_result
    integer :: n
    complex(kind=8), allocatable :: data(:)
  end type qft_result

  contains
    function qft(qubits) result(res)
      implicit none
      integer, intent(in) :: qubits
      type(qft_result) :: res
      integer :: n, j, k, l
      real(kind=8) :: theta
      complex(kind=8) :: w

      ! Initialize the result variable
      n = 2**qubits
      res%n = n
      allocate(res%data(n))
      res%data = (1.0d0, 0.0d0)  ! Initialize array to 1.0d0 (complex)

      do j = 1, n
        do k = 1, qubits
          l = 2**(k-1)
          theta = 2.0d0 * atan(1.0d0) * 4.0d0 * j / l
          w = cmplx(cos(theta), sin(theta), kind=8)
          res%data(j) = res%data(j) * w
        enddo
      enddo

    end function qft
end module qft_module
