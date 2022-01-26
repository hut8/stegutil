! Steganography Utility
! Liam Bowen [bowenl2@cs.rpi.edu]
PROGRAM STEGUTIL
  USE ENCODE
  USE DECODE
  USE STATIC
  IMPLICIT NONE
  INTEGER :: ARGC
  CHARACTER(LEN=255) :: PAYLOAD_FN, CARRIER_FN
  CHARACTER(LEN=255) :: PAD_FN, PACKAGE_FN
  CHARACTER(LEN=255) :: HELP_TOPIC
  CHARACTER(LEN=6) :: OPERATION_MODE_TEXT

  PRINT ('(A)'), LOGO
  !===========================+
  !Parse the input parameters |
  !===========================+
  ARGC=COMMAND_ARGUMENT_COUNT()
  IF (ARGC.LT.1) THEN
     CALL PRINT_USAGE()
     STOP
  END IF

  ! Determine mode from first argument
  CALL GETARG(1, OPERATION_MODE_TEXT)

  ! === ENCODE ===
  IF (OPERATION_MODE_TEXT.EQ.'encode') THEN
     ! encode <payload> <carrier.bmp> [<padfile>] <package.bmp>
     IF (ARGC.EQ.4.OR.ARGC.EQ.5) THEN
        CALL GETARG(2, PAYLOAD_FN)
        CALL GETARG(3, CARRIER_FN)
        IF (ARGC.EQ.4) THEN
           CALL GETARG(4, PACKAGE_FN)
           CALL STEGO_ENCODE(PAYLOAD_FN=PAYLOAD_FN, &
                CARRIER_FN=CARRIER_FN, &
                PACKAGE_FN=PACKAGE_FN)
        ELSE
           CALL GETARG(4, PAD_FN)               
           CALL GETARG(5, PACKAGE_FN)
           CALL STEGO_ENCODE(PAYLOAD_FN=PAYLOAD_FN, &
                CARRIER_FN=CARRIER_FN, &
                PAD_FN=PAD_FN,PACKAGE_FN=PACKAGE_FN)
        END IF
     ELSE
        PRINT('(A)'), 'Incorrect number of parameters for encode'
        CALL PRINT_USAGE()
        STOP
     END IF
  ! === DECODE ===
  ELSE IF (OPERATION_MODE_TEXT.EQ.'decode') THEN
     ! decode <package.bmp> [<padfile>] <payload>
     IF (ARGC.EQ.3.OR.ARGC.EQ.4) THEN
        CALL GETARG(2, PACKAGE_FN)
        IF (ARGC.EQ.3) THEN
           CALL GETARG(3, PAYLOAD_FN)
           CALL STEGO_DECODE(PACKAGE_FN=PACKAGE_FN,PAYLOAD_FN=PAYLOAD_FN)
        ELSE
           CALL GETARG(3, PAD_FN)
           CALL GETARG(4, PAYLOAD_FN)
           CALL STEGO_DECODE(PACKAGE_FN=PACKAGE_FN,PAYLOAD_FN=PAYLOAD_FN,&
                PAD_FN=PAD_FN)
        END IF
     ELSE
        PRINT('(A)'), 'Incorrect number of parameters for decode'
        CALL PRINT_USAGE()
        STOP
     END IF
  ELSE IF (OPERATION_MODE_TEXT == 'help') THEN
     ! help <operation>
     IF (ARGC == 2) THEN
        CALL GETARG(2, HELP_TOPIC)
        CALL SHOWHELP(HELP_TOPIC)
     ELSE
        PRINT ('(A,/,4X,A,/)'), 'Help usage: stegutil help <topic>', &
             'topics: "encode", "decode", "chaff", "considerations"'
     END IF
  ELSE
     WRITE(*,*) "Error: Invalid operation passed"
     WRITE(*,*) "I only know 'encode' and 'decode'"
     CALL PRINT_USAGE()
     STOP
  END IF
  STOP
END PROGRAM STEGUTIL


SUBROUTINE PRINT_USAGE
  USE STATIC
  PRINT ('(A)'), SU_DESC
  PRINT ('(A,/,3X,A)'), '== Usage: stegutil <operation> <arguments>', &
       'For help, use "stegutil help"'
END SUBROUTINE PRINT_USAGE
