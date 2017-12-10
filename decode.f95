!------------------------------------
! This function takes a data type and
! a data code. It uses the data code
! as the record number in that data
! type to decode the number. The
! function returns that number.
!
! Written by Jonathan Platt Section 1
!-----------------------------------
CHARACTER(LEN=25) FUNCTION decode(dataType,recordLength,dataCode)
  IMPLICIT NONE
  CHARACTER,INTENT(IN)::dataType*6
  INTEGER,INTENT(IN)::recordLength,dataCode
  INTEGER::minRecord,maxRecord
  CHARACTER::recordLengthChar*2
  WRITE(recordLengthChar,'(I2)') recordLength
  minRecord=1
  IF(dataType=='county') minRecord=0
  
  OPEN(10,FILE='cbhprojDB/'//trim(dataType)//'.db',FORM='formatted',ACCESS='direct',RECL=recordLength)
  READ(10,'(I2)',REC=1) maxRecord
  IF(dataCode>maxRecord .OR. dataCode<minRecord) THEN
    decode="Invalid "//trim(dataType)//" code"
  ELSE
    READ(10,'(A'//recordLengthChar//')',REC=dataCode+2-minRecord) decode 
  END IF
  CLOSE(10)
END FUNCTION decode
