! Steganography Utility - Bitmap Parser
! Liam Bowen [bowenl2@cs.rpi.edu]

MODULE BMP_PARSER
  IMPLICIT NONE
CONTAINS
  SUBROUTINE BMP_PARSE(BMP_U,X_SIZE,Y_SIZE,CAPACITY,SIZE,OFFSET,BMP_HEADER)
    ! Parameters
    INTEGER, INTENT(IN) :: BMP_U
    INTEGER, INTENT(OUT) :: X_SIZE
    INTEGER, INTENT(OUT) :: Y_SIZE
    INTEGER, INTENT(OUT) :: SIZE
    INTEGER, INTENT(OUT), OPTIONAL :: CAPACITY
    INTEGER, INTENT(OUT) :: OFFSET
    CHARACTER(LEN=1), DIMENSION(Z'36'), INTENT(OUT) :: BMP_HEADER
    

    ! Locals
    ! Entire header
    ! Bitmap Lengths
    INTEGER :: BMP_LEN

    ! Return Code
    INTEGER :: RC

    ! Data Lengths
    INTEGER :: INFO(13),  ROW_LEN

    INTEGER :: BMP_COMPRESS_T ! Carrier compression method


    ! Read carrier bitmap header
    READ(BMP_U, POS=1) BMP_HEADER

    ! Verify the carrier file size
    RC=FSTAT(BMP_U, INFO)
    SIZE=INFO(8)
    BMP_LEN = TRANSFER(BMP_HEADER(3:),BMP_LEN)
    IF (BMP_LEN /= SIZE) THEN
       PRINT ('(A)'), 'Fatal error: operating system disagrees with carrier bitmap about its size (bitmap damaged?)'
       STOP
    END IF

    ! Magic Number = "BM"
    IF (BMP_HEADER(1).NE.'B'.OR.BMP_HEADER(2).NE.'M') THEN
       PRINT('(A)'), 'Invalid bitmap [bad magic]'
       STOP
    END IF

    ! Determine pixel offset
    OFFSET = TRANSFER(BMP_HEADER(Z'B':), OFFSET)
    PRINT ('(9X,A,Z0)'), 'Pixel Address = 0x', OFFSET

    ! Dimensions of the image
    X_SIZE = TRANSFER(BMP_HEADER(Z'13':), X_SIZE)
    Y_SIZE = TRANSFER(BMP_HEADER(Z'17':), Y_SIZE)
    PRINT ('(9X,I0,A,I0,A)'), X_SIZE, ' px X ', Y_SIZE, ' px'

    ROW_LEN = X_SIZE*3 ! (R, G, B) X 1 BYTE
    ! PRINT ('(9X,A,I0,A)'), 'Row length: ', CARRIER_ROW_LEN, ' bytes'

    ! Make sure it isn't compressed
    BMP_COMPRESS_T = TRANSFER(BMP_HEADER(Z'1F':), BMP_COMPRESS_T)
    IF (BMP_COMPRESS_T.NE.0) THEN
       PRINT ('(A)'), 'Fatal error: cannot encode data in compressed carrier bitmap'
       STOP
    END IF


    CAPACITY=((3*Y_SIZE*X_SIZE)/8)-4
    PRINT ('(9X,A,I0,A)'), 'Carrier capacity: ', CAPACITY, ' bytes'
  END SUBROUTINE BMP_PARSE
END MODULE BMP_PARSER
