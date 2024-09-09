program main
  use qft_module
  implicit none

  integer :: num_qubits, i
  type(qft_result) :: result

  write(*,*) "Enter the number of qubits:"
  read(*,*) num_qubits

  result = qft(num_qubits)

  write(*,*) "Quantum Fourier Transform result:"
  do i = 1, 2**num_qubits
    write(*,*) result%data(i)
  enddo

end program main
