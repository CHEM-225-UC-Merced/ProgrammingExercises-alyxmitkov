Program prgm_02_03
  Implicit None
  Integer :: IIn = 10, IError, NDim, i, j
  Real, Dimension(:), Allocatable :: Array_Input, EVals, Temp_Vector
  Real, Dimension(:,:), Allocatable :: Matrix, EVecs, Temp_Matrix
  Character(Len=256) :: FileName

  ! Read the input file name from the command line, open the file, and read the input array.
  Call Get_Command_Argument(1, FileName)
  Open(Unit=IIn, File=TRIM(FileName), Status='OLD', IOStat=IError)
  If (IError /= 0) Then
    Write(*,*) 'Error opening input file.'
    STOP
  End If

  Read(IIn,*) NDim
  Allocate(Array_Input((NDim*(NDim+1))/2), Matrix(NDim,NDim))
  Allocate(EVals(NDim), EVecs(NDim,NDim), Temp_Vector(3*NDim))
  Allocate(Temp_Matrix(NDim,NDim))

  ! Read the array elements from the input file.
  Do i = 1, (NDim*(NDim+1))/2
    Read(IIn,*) Array_Input(i)
  End Do
  Close(Unit=IIn)

  ! Convert Array_Input to Matrix and print the matrix.
  Write(*,*) 'The matrix loaded (column) lower-triangle packed:'
  Call SymmetricPacked2Matrix_LowerPac(NDim, Array_Input, Matrix)
  Call Print_Matrix_Full_Real(Matrix, NDim, NDim)

  ! Diagonalize the matrix using SSPEV.
  Call SSPEV('V', 'L', NDim, Array_Input, EVals, EVecs, NDim, &
             Temp_Vector, IError)
  If (IError /= 0) Then
    Write(*,*) 'Failure in DSPEV.'
    STOP
  End If

  Write(*,*) 'EVals:'
  Call Print_Matrix_Full_Real(RESHAPE(EVals, (/1, NDim/)), 1, NDim)
  Write(*,*) 'EVecs:'
  Call Print_Matrix_Full_Real(EVecs, NDim, NDim)

End Program prgm_02_03

Subroutine SymmetricPacked2Matrix_LowerPac(NDim, Packed, FullMatrix)
  Implicit None
  Integer, Intent(In) :: NDim
  Real, Intent(In) :: Packed((NDim*(NDim+1))/2)
  Real, Intent(Out) :: FullMatrix(NDim, NDim)
  Integer :: i, j, k

  FullMatrix = 0.0
  k = 1
  Do j = 1, NDim
    Do i = j, NDim
      FullMatrix(i, j) = Packed(k)
      FullMatrix(j, i) = Packed(k)
      k = k + 1
    End Do
  End Do

End Subroutine SymmetricPacked2Matrix_LowerPac

Subroutine Print_Matrix_Full_Real(AMat, M, N)
  Implicit None
  Integer, Intent(In) :: M, N
  Real, Dimension(M, N), Intent(In) :: AMat
  Integer, Parameter :: IOut = 6, NColumns = 5
  Integer :: i, j, IFirst, ILast

  1000 Format(1x, A)
  2000 Format(5x, 5(7x, I7))
  2010 Format(1x, I7, 5F14.6)

  Do IFirst = 1, N, NColumns
    ILast = Min(IFirst + NColumns - 1, N)
    Write(IOut, 2000) (i, i = IFirst, ILast)
    Do i = 1, M
      Write(IOut, 2010) i, (AMat(i, j), j = IFirst, ILast)
    End Do
  End Do

End Subroutine Print_Matrix_Full_Real

