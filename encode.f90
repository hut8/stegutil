! Steganography Utility - Encoding Module
! Liam Bowen [bowenl2@cs.rpi.edu]

MODULE ENCODE
  IMPLICIT NONE
  INTEGER, PARAMETER :: PAYLOAD_UNIT=50, CARRIER_UNIT=51, &
       PACKAGE_UNIT=52, PAD_UNIT=53
CONTAINS
  SUBROUTINE ENCODE_C_BIT(CARRIER_BUFF, PAYLOAD_BUFF, BITMASK)
    ! Encodes (BITMASK & PAYLOAD_BUFF) into LSB of CARRIER_BUFF
    ! then advances BITMASK circularly to encode next bit
    CHARACTER(LEN=1), INTENT(INOUT) :: CARRIER_BUFF
    CHARACTER(LEN=1), INTENT(IN) :: PAYLOAD_BUFF
    CHARACTER(LEN=1), INTENT(INOUT) :: BITMASK

    IF (IAND(ICHAR(PAYLOAD_BUFF), ICHAR(BITMASK)).EQ.0) THEN
       ! Make LSB = 0
       CARRIER_BUFF = CHAR(IBCLR(ICHAR(CARRIER_BUFF), 0))
    ELSE
       ! Make LSB = 1
       CARRIER_BUFF = CHAR(IBSET(ICHAR(CARRIER_BUFF), 0))
    END IF

    IF (BITMASK /= CHAR(B'10000000')) THEN
       BITMASK = CHAR(LSHIFT(ICHAR(BITMASK),1))
    ELSE
       BITMASK = CHAR(0)
    END IF
  END SUBROUTINE ENCODE_C_BIT

  SUBROUTINE ENCODE_I_BIT(CARRIER_BUFF, VAL, BITMASK)
    ! Encodes (BITMASK & PAYLOAD_BUFF) into LSB of CARRIER_BUFF
    ! then advances BITMASK circularly to encode next bit
    CHARACTER(LEN=1), INTENT(INOUT) :: CARRIER_BUFF
    INTEGER, INTENT(IN) :: VAL
    INTEGER, INTENT(INOUT) :: BITMASK

    IF (IAND(VAL, BITMASK).EQ.0) THEN
       ! Make LSB = 0
       CARRIER_BUFF = CHAR(IBCLR(ICHAR(CARRIER_BUFF), 0))
    ELSE
       ! Make LSB = 1
       CARRIER_BUFF = CHAR(IBSET(ICHAR(CARRIER_BUFF), 0))
    END IF

    IF (BITMASK /= B'10000000000000000000000000000000') THEN
       BITMASK = LSHIFT(BITMASK,1)
    ELSE
       BITMASK = 0
    END IF
  END SUBROUTINE ENCODE_I_BIT

  SUBROUTINE STEGO_ENCODE(PAYLOAD_FN,CARRIER_FN,PAD_FN,PACKAGE_FN)
    USE UTIL
    USE BMP_PARSER
    ! Return code from system calls (used repeatedly)
    INTEGER :: RC
    LOGICAL :: PKG_EXIST
    ! Subroutine Parameters
    ! Payload, Carrier and Package filenames are mandatory and IN only
    CHARACTER(LEN=255), INTENT(IN) :: PAYLOAD_FN, CARRIER_FN, PACKAGE_FN
    ! Pad filename is optional
    CHARACTER(LEN=255), INTENT(IN), OPTIONAL :: PAD_FN

    ! Payload data
    CHARACTER, DIMENSION(:), ALLOCATABLE :: PAYLOAD
    INTEGER :: PAYLOAD_I

    ! Entire bitmap image less header
    CHARACTER, DIMENSION(:), ALLOCATABLE :: BITMAP
    INTEGER :: BITMAP_ADDRESS

    ! Entire padding data
    CHARACTER, DIMENSION(:), ALLOCATABLE :: PAD

    ! Which bit we're encoding (1-8)
    CHARACTER :: DATABUFF_BITMASK
    ! What bit in length we're encoding (1-32)
    INTEGER :: LENGTH_BITMASK

    LOGICAL PADDING
    ! Data Lengths
    INTEGER :: INFO(13), PAYLOAD_LEN, PAD_LEN, &
         CARRIER_BMP_LEN, CARRIER_X_LEN, CARRIER_Y_LEN,&
         & MAX_PAYLOAD_LEN

    INTEGER :: BMP_PIX_OFFSET ! Offset of pixels from header

    CHARACTER(LEN=1), DIMENSION(Z'36') :: BMP_HEADER

    PADDING=PRESENT(PAD_FN)

    ! Size of payload
    RC=STAT(PAYLOAD_FN,INFO)
    IF (RC.NE.0) THEN
       WRITE(*,'(A,A,A)') 'Fatal Error: Payload ', PAYLOAD_FN, ' not found'
       STOP
    END IF
    PAYLOAD_LEN=INFO(8)
    PRINT ('(A,A,A1,A1,I0,A3)'), 'Payload: ', TRIM(PAYLOAD_FN), ACHAR(9), &
         '[',PAYLOAD_LEN,' B]'

    ! Open data files
    OPEN(UNIT=PAYLOAD_UNIT,FILE=PAYLOAD_FN, STATUS='OLD', FORM='UNFORMATTED', &
         ACTION="READ", ACCESS="STREAM")
    OPEN(UNIT=CARRIER_UNIT,FILE=CARRIER_FN, STATUS="OLD", FORM='UNFORMATTED', &
         ACTION="READ", ACCESS="STREAM")


    PRINT ('(A,A)'), 'Carrier: ', TRIM(CARRIER_FN)
    ! Parse bitmap stuff
    CALL BMP_PARSE(BMP_U=CARRIER_UNIT,X_SIZE=CARRIER_X_LEN,Y_SIZE=CARRIER_Y_LEN, &
         SIZE=CARRIER_BMP_LEN, CAPACITY=MAX_PAYLOAD_LEN, OFFSET=BMP_PIX_OFFSET, &
         BMP_HEADER=BMP_HEADER)
    
    ! Maximum size for payload
    ! 3 = bits per pixel, 4 = size of payload_len
    IF(PAYLOAD_LEN > MAX_PAYLOAD_LEN) THEN
       PRINT ('(A,I0,A)'), 'Fatal error: carrier is too small [payload is ', &
            (PAYLOAD_LEN-(MAX_PAYLOAD_LEN/8)), ' bytes larger than capacity]'
       STOP
    END IF

    ! Deal with padding
    IF (PADDING) THEN
       OPEN(UNIT=PAD_UNIT,FILE=PAD_FN, STATUS='OLD', FORM='UNFORMATTED', &
            ACTION="READ", ACCESS="STREAM")
       ! Padding length (must be at least as large as plaintext)
       RC=FSTAT(PAD_UNIT,INFO)
       PAD_LEN=INFO(8)
       PRINT('(A,A,A1,A1,I0,A3)'), 'Pad: ', TRIM(PAD_FN), ACHAR(9), '[', PAD_LEN, ' B]'
       IF (PAD_LEN < PAYLOAD_LEN+4) THEN
          PRINT ('(A)'), 'Fatal error: a pad, if given, must &
               &be at least as large as (payload+4) bytes'
          STOP
       END IF
       ALLOCATE(PAD(PAD_LEN))
       READ(PAD_UNIT, POS=1, IOSTAT=RC) PAD
       IF (RC<0) THEN
          PRINT('(A)'), 'Fatal error: could not read entire pad'
          STOP
       END IF
    END IF

    ! Package file
    INQUIRE(FILE=PACKAGE_FN, EXIST=PKG_EXIST)
    IF (PKG_EXIST .EQV. .TRUE.) THEN
       IF (CONFIRM(PROMPT='Package bitmap exists.  Overwrite?') .EQV. .FALSE.) THEN
          PRINT ('(A)'), 'Fatal error: no package destination'
          STOP
       END IF
    END IF
    OPEN(UNIT=PACKAGE_UNIT,FILE=PACKAGE_FN, STATUS='REPLACE', &
         FORM='UNFORMATTED', ACTION="WRITE", ACCESS="STREAM")

    ! Copy header
    WRITE(PACKAGE_UNIT, POS=1, IOSTAT=RC) BMP_HEADER
    IF (RC<0) THEN
       PRINT('(A)'), 'Fatal error: could not write header to package'
       STOP
    END IF

    ! Allocate bitmap memory
    PRINT('(A,I0,A)'), 'Allocating ', CARRIER_BMP_LEN-BMP_PIX_OFFSET, ' B for bitmap buffer'
    ALLOCATE(BITMAP(CARRIER_BMP_LEN-BMP_PIX_OFFSET))

    ! Allocate payload memory
    PRINT('(A,I0,A)'), 'Allocating ', PAYLOAD_LEN, ' B for payload buffer'
    ALLOCATE(PAYLOAD(PAYLOAD_LEN))

    ! Read payload from beginning
    READ(PAYLOAD_UNIT, POS=1, IOSTAT=RC) PAYLOAD
    IF (RC < 0) THEN
       PRINT ('(A)'), 'Fatal error: unexpected end of file encountered while reading &
            &payload'
       STOP
    END IF
    CLOSE(PAYLOAD_UNIT)
    ! Done with payload file

    ! Read carrier into bitmap buffer
    READ(CARRIER_UNIT, POS=BMP_PIX_OFFSET+1, IOSTAT=RC) BITMAP
    IF (RC < 0) THEN
       PRINT ('(A)'), 'Fatal error: unexpected end of file encountered while reading &
            &carrier bitmap'
       STOP
    END IF
    CLOSE(CARRIER_UNIT)
    ! Done with carrier
    
    ! Encoding subroutine
    BITMAP_ADDRESS = 1

    ! Encode the length of the data (least significant bit first)
    LENGTH_BITMASK = 1 ! Bitmask will go 2**0 -> 2**32
    PRINT ('(A,Z8.8BZ,A)'), 'Encoding length [0x', PAYLOAD_LEN, ']'
    DO
       IF (PADDING) THEN
          ! Use first four bytes of pad to encode the length
          PAYLOAD_LEN = IEOR(PAYLOAD_LEN, TRANSFER(PAD(1:4), PAYLOAD_LEN))
       END IF
       CALL ENCODE_I_BIT(CARRIER_BUFF=BITMAP(BITMAP_ADDRESS), &
            VAL=PAYLOAD_LEN, BITMASK=LENGTH_BITMASK)
       BITMAP_ADDRESS = BITMAP_ADDRESS + 1
       IF (LENGTH_BITMASK == 0) EXIT ! Set by ENCODE_I_BIT
    END DO

    PRINT ('(A)'), 'Encoding data...'
    PAYLOAD_I = 1
    ! Loop over all the data
    DO
       IF (PAYLOAD_I > PAYLOAD_LEN) EXIT
       IF (PADDING) THEN
          PAYLOAD(PAYLOAD_I) = &
               CHAR(IEOR( &
               ICHAR(PAYLOAD(PAYLOAD_I)),ICHAR(PAD(PAYLOAD_I+4)))) ! 4 offset due to padding length
       END IF
       DATABUFF_BITMASK = CHAR(B'00000001')
       ! Break data up into 8 bits
       DO
          CALL ENCODE_C_BIT(CARRIER_BUFF=BITMAP(BITMAP_ADDRESS), &
               PAYLOAD_BUFF=PAYLOAD(PAYLOAD_I), BITMASK=DATABUFF_BITMASK)
          BITMAP_ADDRESS = BITMAP_ADDRESS+1
          IF (DATABUFF_BITMASK == CHAR(0)) EXIT
       END DO
       PAYLOAD_I = PAYLOAD_I + 1
    END DO
    ! Finished encoding.
    PRINT ('(A)'), "Finished encoding payload into bitmap buffer.  Deallocating payload buffer."
    DEALLOCATE(PAYLOAD)

    PRINT ('(A)'), 'Commiting bitmap buffer to package'
    ! Commit buffer to package
    ! Apply padding to the rest if needed
    WRITE(PACKAGE_UNIT, POS=BMP_PIX_OFFSET+1, IOSTAT=RC) BITMAP
    IF (RC < 0) THEN
       PRINT ('(A)'), 'Fatal error while writing to package!'
       STOP
    END IF
    CLOSE(PACKAGE_UNIT)
    PRINT ('(A)'), 'Encoding successful.'
  END SUBROUTINE STEGO_ENCODE  
END MODULE ENCODE
