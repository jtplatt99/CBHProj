!---------------------------------------------
! This program takes in data from a state file
! and sorts and loads it into a file in a
! directory "cbhprojDB".
!
! Written by Jonathan Platt Section 1 11/20/17
!---------------------------------------------

PROGRAM ldState
  IMPLICIT NONE
  INTEGER::code,gCount=0,bCount=0,rc
  CHARACTER::data*22
  OPEN(10,FILE='cbhdataf17/state.data',ACTION='READ')
  OPEN(11,FILE='cbhprojDB/state.db',FORM='FORMATTED', ACCESS='DIRECT',RECL=22)
  DO
    READ(10,'(I2,A22)',IOSTAT=rc) code,data
    IF(rc/=0) EXIT
    IF(code<1 .OR. code>51) THEN
      bCount=bCount+1
      CYCLE
    END IF
    WRITE(11,'(A22)',REC=code+1) data
    gCount=gCount+1
  END DO
  WRITE(*,*) "The state loading sequence has finished."
  WRITE(*,'(I2,A)') gCount," states were loaded."
  WRITE(*,'(I2,A)') bCount," invalid data items were detected and ignored."
  WRITE(11,'(I2)',REC=1) gCount
  CLOSE(10)
  CLOSE(11)
END PROGRAM ldState
