!---------------------------------------------
! This program takes in data from a county file
! and sorts and loads it into a file in a
! directory "cbhprojDB".
!
! Written by Jonathan Platt Section 1 11/20/17
!---------------------------------------------

PROGRAM ldCounty
  IMPLICIT NONE
  INTEGER::code,gCount=0,bCount=0,rc
  CHARACTER::data*12
  OPEN(10,FILE='cbhdataf17/county.data',ACTION='READ')
  OPEN(11,FILE='cbhprojDB/county.db',FORM='FORMATTED', ACCESS='DIRECT',RECL=12)
  DO
    READ(10,'(I2,A12)',IOSTAT=rc) code,data
    IF(rc/=0) EXIT
    IF(code<0 .OR. code>68) THEN
      bCount=bCount+1
      CYCLE
    END IF
    WRITE(11,'(A12)',REC=code+2) data
    gCount=gCount+1
  END DO
  WRITE(*,*) "The county loading sequence has finished."
  WRITE(*,'(I2,A)') gCount," counties were loaded."
  WRITE(*,'(I2,A)') bCount," invalid data items were detected and ignored."
  WRITE(11,'(I2)',REC=1) gCount
  CLOSE(10)
  CLOSE(11)
END PROGRAM ldCounty
