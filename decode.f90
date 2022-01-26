! Steganography Utility - Decoding Module
! Liam Bowen [bowenl2@cs.rpi.edu]

MODULE DECODE
  IMPLICIT NONE
  INTEGER, PARAMETER :: PAYLOAD_UNIT=50, PACKAGE_UNIT=51, PAD_UNIT=52
CONTAINS
  SUBROUTINE DECODE_I_BIT(PACKAGE_BUFF, VAL, BITMASK)
    ! Takes LSB of PACKAGE_BUFF and sticks it in VAL
    ! at BITMASK.  Sets BITMASK = 0 when done
    CHARACTER(LEN=1), INTENT(IN) :: PACKAGE_BUFF
    INTEGER, INTENT(INOUT) :: VAL
    INTEGER, INTENT(INOUT) :: BITMASK

    IF (IAND(ICHAR(PACKAGE_BUFF), B'00000001') /= 0) THEN
       VAL = IOR(VAL, BITMASK)
    END IF

    IF (BITMASK /= B'10000000000000000000000000000000') THEN
       ! Advance bitmask
       BITMASK = LSHIFT(BITMASK,1)
    ELSE
       ! Done encoding integer
       BITMASK = 0
    END IF
  END SUBROUTINE DECODE_I_BIT

  SUBROUTINE STEGO_DECODE(PACKAGE_FN,PAYLOAD_FN,PAD_FN)
    USE UTIL
    USE BMP_PARSER
    ! Return code from system calls (used repeatedly)
    INTEGER :: RC
    LOGICAL :: PAYLOAD_EXIST
    ! Subroutine Parameters
    ! Payload and Package filenames are mandatory and IN only
    CHARACTER(LEN=255), INTENT(IN) :: PAYLOAD_FN, PACKAGE_FN
    ! Pad filename is optional
    CHARACTER(LEN=255), INTENT(IN), OPTIONAL :: PAD_FN
    ! Single character buffer of decoded data
    CHARACTER :: DATABUFF
    CHARACTER :: PAYLOAD_BUFF ! Reconstructed payload from LSB of package
    CHARACTER :: PAYLOAD_BITMASK

    ! Whether or not we have padding at all
    LOGICAL PADDING
    ! Data Lengths
    INTEGER :: INFO(13), PAYLOAD_LEN, PAYLOAD_POS, PAD_LEN, &
         PACKAGE_BMP_LEN, PACKAGE_STAT_LEN, MAX_PAYLOAD_LEN, &
         PACKAGE_X_LEN, PACKAGE_Y_LEN, PACKAGE_ROW_LEN
    INTEGER :: LENGTH_BITMASK ! Where we are while deriving PAYLOAD_LEN
    INTEGER :: BMP_PIX_OFFSET ! Offset of pixels from header

    CHARACTER(LEN=1), DIMENSION(Z'36') :: BMP_HEADER

    PADDING=PRESENT(PAD_FN)

    OPEN(UNIT=PACKAGE_UNIT,FILE=PACKAGE_FN, STATUS='OLD', FORM='UNFORMATTED', &
         ACTION="READ", ACCESS="STREAM")

    ! Parse bitmap stuff
    CALL BMP_PARSE(BMP_U=PACKAGE_UNIT,X_SIZE=PACKAGE_X_LEN,Y_SIZE=PACKAGE_Y_LEN, &
         SIZE=PACKAGE_BMP_LEN, OFFSET=BMP_PIX_OFFSET, &
         BMP_HEADER=BMP_HEADER)

    PRINT ('(A,A,A1,A1,I0,A3)'), 'Package: ', TRIM(PACKAGE_FN), ACHAR(7), &
         '[', PACKAGE_STAT_LEN, ' B]'

    ! Determine pixel offset
    READ(UNIT=PACKAGE_UNIT,POS=Z'B') BMP_PIX_OFFSET
    PRINT ('(9X,A,Z0)'), 'Pixel Address = 0x', BMP_PIX_OFFSET

    ! Dimensions of the image
    READ(UNIT=PACKAGE_UNIT,POS=Z'13') PACKAGE_X_LEN
    READ(UNIT=PACKAGE_UNIT,POS=Z'17') PACKAGE_Y_LEN
    PRINT ('(9X,I0,A6,I0,A3)'), PACKAGE_X_LEN, ' px X ', &
         PACKAGE_Y_LEN, ' px'

    PACKAGE_ROW_LEN = PACKAGE_X_LEN*3 ! (R, G, B) X 1 BYTE

    IF (PADDING) THEN
       OPEN(UNIT=PAD_UNIT,FILE=PAD_FN, STATUS='OLD', FORM='UNFORMATTED', &
            ACTION="READ", ACCESS="STREAM")
       RC=FSTAT(PAD_UNIT,INFO)
       PAD_LEN=INFO(8)
       WRITE(6,'(A,I11,A)') 'Pad Size: ', PAD_LEN, 'B'
       IF (PAD_LEN.LT.PAYLOAD_LEN) THEN
          PRINT *, 'Fatal error: a pad, if given, must &
               &be at least as long as data in package'
          STOP
       END IF
    END IF

    ! Position package at pixel data for streaming
    READ(PACKAGE_UNIT, POS=BMP_PIX_OFFSET+1)

    ! Determine length of payload from package
    PAYLOAD_LEN = 0
    LENGTH_BITMASK = 1
    DO
       READ (PACKAGE_UNIT, IOSTAT=RC) DATABUFF
       IF (RC < 0) THEN
          PRINT ('(A)'), 'Fatal error: could not read length from package'
          STOP
       END IF
       CALL DECODE_I_BIT(PACKAGE_BUFF=DATABUFF, VAL=PAYLOAD_LEN, &
            BITMASK=LENGTH_BITMASK)
       IF (LENGTH_BITMASK == 0) EXIT
    END DO

    ! Check for impossible payload length
    MAX_PAYLOAD_LEN = ((3*PACKAGE_Y_LEN*PACKAGE_X_LEN)/8)-4
    IF (PAYLOAD_LEN > MAX_PAYLOAD_LEN) THEN
       PRINT ('(A,Z8,A,A,A,Z8,A)'), &
            'Fatal error: package indicated that it contains &
            &payload of 0x', PAYLOAD_LEN, ' bytes, yet ', TRIM(PACKAGE_FN), &
            ' can only hold 0x', MAX_PAYLOAD_LEN, ' bytes' 
       STOP
    END IF

    PRINT ('(A,Z0,A)'), 'Package contains 0x', PAYLOAD_LEN, ' bytes'

    ! Open the payload (output) and prompt for overwrite
    INQUIRE(FILE=PAYLOAD_FN, EXIST=PAYLOAD_EXIST)
    IF (PAYLOAD_EXIST .EQV. .TRUE.) THEN
       IF (CONFIRM(PROMPT='Payload file exists.  Overwrite?').EQV..FALSE.) THEN
          PRINT *, 'Fatal error: no payload target'
          STOP
       END IF
    END IF

    OPEN(UNIT=PAYLOAD_UNIT,FILE=PAYLOAD_FN, STATUS='REPLACE', &
         FORM='UNFORMATTED', ACTION="WRITE", ACCESS="STREAM")
    WRITE(PAYLOAD_UNIT, POS=1) ! Rewind file just in case

    PAYLOAD_POS = 0
    PAYLOAD_BUFF = CHAR(0)
    PAYLOAD_BITMASK = CHAR(B'00000001')

    ! Loop over all data
    DO
       READ(PACKAGE_UNIT, IOSTAT=RC) DATABUFF
       IF (RC < 0) THEN
          ! End of package
          PRINT ('(A)'), 'Fatal error: reached end of package before &
               & encountering expected length of payload'
          STOP
       END IF
       !PRINT ('(A,Z2,A,B8.8BZ,A,L1)'), 'Decoding package color value ', &
       !     DATABUFF, ' = [', DATABUFF, '] LSB: ', &
       !     (AND(ICHAR(DATABUFF), B'00000001') /= 0)
       IF (AND(ICHAR(DATABUFF), B'00000001') /= 0) THEN
        !  PRINT ('(5X,A,B8.8BZ,A)'), 'Assigning value with mask [', &
        !       PAYLOAD_BITMASK, ']'
          ! If the LSB is set in the color, set it in the
          ! correct position in PAYLOAD_BUFF
          PAYLOAD_BUFF = CHAR(IOR(ICHAR(PAYLOAD_BUFF), ICHAR(PAYLOAD_BITMASK)))
       END IF
       ! PRINT ('(7X,A,B8.8BZ,A)'), 'Payload byte now: [', PAYLOAD_BUFF, ']'
       IF (PAYLOAD_BITMASK == CHAR(B'10000000')) THEN
          ! Constructed one byte
          !PRINT ('(A,B8.8BZ,4A)'), 'Got byte: [', PAYLOAD_BUFF, '] [', &
          !     PAYLOAD_BUFF,']'
          WRITE (PAYLOAD_UNIT, IOSTAT=RC) PAYLOAD_BUFF
          IF (RC < 0) THEN
             PRINT ('(A)'), 'Fatal error: could not write decoded byte to &
                  & payload file'
             STOP
          END IF
          PAYLOAD_BITMASK = CHAR(B'00000001')
          PAYLOAD_BUFF = CHAR(0)
          PAYLOAD_POS = PAYLOAD_POS + 1
          IF (PAYLOAD_POS == PAYLOAD_LEN) EXIT
       ELSE
          ! Advance the bitmask!
          PAYLOAD_BITMASK = CHAR(LSHIFT(ICHAR(PAYLOAD_BITMASK), 1))
       END IF
    END DO
  END SUBROUTINE STEGO_DECODE
END MODULE DECODE
