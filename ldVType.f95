!---------------------------------------------
! This program takes in data from a vehical 
! type data file and sorts and loads it into a 
! file in a directory "cbhprojDB".
!
! Written by Jonathan Platt Section 1 11/20/17
!---------------------------------------------

PROGRAM ldVType
  IMPLICIT NONE
  INTEGER::code,gCount=0,bCount=0,rc
  CHARACTER::data*15
  OPEN(10,FILE='cbhdataf17/vtype.data',ACTION='READ')
  OPEN(11,FILE='cbhprojDB/vtype.db',FORM='FORMATTED', ACCESS='DIRECT',RECL=15)
  DO
    READ(10,'(I2,A15)',IOSTAT=rc) code,data
    IF(rc/=0) EXIT
    IF(code<1 .OR. code>15) THEN
      bCount=bCount+1
      CYCLE
    END IF
    WRITE(11,'(A15)',REC=code+1) data
    gCount=gCount+1
  END DO
  WRITE(*,*) "The vehicle type loading sequence has finished."
  WRITE(*,'(I2,A)') gCount," vehicle types were loaded."
  WRITE(*,'(I2,A)') bCount," invalid data items were detected and ignored."
  WRITE(11,'(I2)',REC=1) gCount
  CLOSE(10)
  CLOSE(11)
END PROGRAM ldVType
