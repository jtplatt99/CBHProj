!---------------------------------------------
! This program takes in data from a data file
! and sorts and loads it into a file in a
! directory "cbhprojDB".
!
! Written by Jonathan Platt Section 1 11/20/17
!---------------------------------------------

SUBROUTINE load(dataType,recordLength,maxRecord)
  IMPLICIT NONE
  CHARACTER,INTENT(IN)::dataType*6
  INTEGER,INTENT(IN)::recordLength,maxRecord

  INTEGER::code,gCount,bCount,rc,minRecord
  CHARACTER::data*25,recordLengthChar*2
  gCount=0
  bCount=0
  minRecord=1
  IF(dataType=="county") minRecord=0
  WRITE(recordLengthChar,'(I2)') recordLength

  OPEN(10,FILE='cbhdataf17/'//trim(dataType)//'.data',ACTION='READ')
  OPEN(11,FILE='cbhprojDB/'//trim(dataType)//'.db',FORM='FORMATTED', ACCESS='DIRECT',RECL=recordLength)
  DO
    READ(10,'(I2,A'//recordlengthChar//')',IOSTAT=rc) code,data
    IF(rc/=0) EXIT
    IF(code<minRecord .OR. code>maxRecord) THEN
      bCount=bCount+1
      CYCLE
    END IF
    WRITE(11,'(A'//recordLengthChar//')',REC=code+2-minRecord) data
    gCount=gCount+1
  END DO
  WRITE(*,'(2X,A,3X,I2,A,3X,I1,A)') "'"//dataType//"' load finished with",gCount," good records and",bCount," bad records."
  WRITE(11,'(I2)',REC=1) gCount
  CLOSE(10)
  CLOSE(11)
END SUBROUTINE load
