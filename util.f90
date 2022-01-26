MODULE UTIL
  IMPLICIT NONE
CONTAINS
  ! Gfortran lacks QUERY() apparently.
  LOGICAL FUNCTION CONFIRM(PROMPT)
    ! PROMPT = One standard 80x25 console line long (max)
    CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: PROMPT    
    CHARACTER(LEN=1) :: CHOICE
    DO
       IF (PRESENT(PROMPT) .EQV. .TRUE.) THEN
          WRITE(*, '(A,A)', ADVANCE="NO") PROMPT, ' [y/n]: '
       ELSE
          WRITE(*,'(A)',ADVANCE="NO") 'Confirm [y/n]: '
       END IF
       READ(*, '(A1)') CHOICE
       IF (CHOICE == 'y' .OR. CHOICE == 'Y') THEN
          CONFIRM = .TRUE.
          RETURN
       ELSE IF (CHOICE == 'n' .OR. CHOICE == 'N') THEN
          CONFIRM = .FALSE.
          RETURN
       END IF
    END DO
    RETURN
  END FUNCTION CONFIRM

  SUBROUTINE HEXDUMP(BUFFER, LENGTH)
    CHARACTER(LEN=1), DIMENSION(*), INTENT(IN) :: BUFFER
    INTEGER, INTENT(IN) :: LENGTH
    INTEGER :: OFFSET
    OFFSET = 0
    DO
       ! Print offset as 7 hex characters
       WRITE (*, '(Z7.7BZ,2X)', ADVANCE='NO'), OFFSET
       ! Get 16 bytes
       DO
          OFFSET = OFFSET + 1
          WRITE (*, '(Z2.2,2X)', ADVANCE='NO') BUFFER(OFFSET)
          IF (MOD(OFFSET, 16) == 0 .OR. OFFSET > LENGTH) THEN
          !IF (OFFSET > LENGTH) THEN
             EXIT
          END IF
       END DO       
       ! Newline
       WRITE (*, '(/$)')
       IF (OFFSET > LENGTH) EXIT
    END DO
  END SUBROUTINE HEXDUMP

END MODULE UTIL
