Program prgm_01_03
!     This is Exercise 3 of HW 1 (CHEM225, Spring 2025)
!
!
!     Alyx Mitkov, 2025
!     This program was created as a homework assignment #1 for Chemistry 225
!     University of California, Merced
!     Merced, California, USA
!
!
!     This program reads two 3x3 matrices from user-provided input files. After the
!     files are opened and read, they are closed, printed, and their product is printed.
!
!
      implicit none
      integer,parameter::inFileUnitA=10, inFileUnitB = 11
      integer::errorFlag,i
      real,dimension(3,3)::matrixInA, matrixInB, matrixProduct
      character(len=128)::fileNameA, fileNameB
!
!
      write(*,*)' What is the name of the first input data file you would like to use?'
      read(*,*) fileNameA
!
      write(*,*)' What is the name of the second input data file you would like to use?'
      read(*,*) fileNameB
!
!     Open the data file and read matrixInA from that file.
!
      open(unit=inFileUnitA,file=TRIM(fileNameA),status='old',  &
        iostat=errorFlag)
      if(errorFlag.ne.0) then
        write(*,*)' There was a problem opening the input file.'
        goto 999
      endIf
      do i = 1,3
        read(inFileUnitA,*) matrixInA(1,i),matrixInA(2,i),matrixInA(3,i)
      endDo
      close(inFileUnitA)
!
      open(unit=inFileUnitB,file=TRIM(fileNameB),status='old',  &
        iostat=errorFlag)
      if(errorFlag.ne.0) then
        write(*,*)' There was a problem opening the input file.'
        goto 999
      endIf
      do i = 1,3
        read(inFileUnitB,*) matrixInB(1,i),matrixInB(2,i),matrixInB(3,i)
      endDo
      close(inFileUnitB)
!
!     Call the subroutine PrintMatrix to print matrixInA.
!
      call PrintMatrix3x3(matrixInA)
!
!     Call the subroutine PrintMatrix to print matrixInB.
!
      call PrintMatrix3x3(matrixInB)
!
!
!     Use MatMul to print the matrix product.
!
!
      matrixProduct = MatMul(matrixInA, matrixInB)
      call PrintMatrix3x3(matrixProduct)
!
!
  999 continue
      End Program prgm_01_03


      Subroutine PrintMatrix3x3(matrix)
!
!     This subroutine prints a 3x3 real matrix. The output is written to StdOut.
!
      implicit none
      real,dimension(3,3),intent(in)::matrix
      integer::i
!
!     Format statements.
!
 1000 format(3(2x,f5.1))
!
!     Do the printing job.
!
      write(*,*)' Printing Matrix'
!
      do i = 1, 3
	write(*,1000) matrix(i,1),matrix(i,2),matrix(i,3)
      end do
!
!
      return
      End Subroutine PrintMatrix3x3
