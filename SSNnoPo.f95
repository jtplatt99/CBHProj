!-----------------------------------------
! This simple subroutine will return the
! recird number for which a given SSN is
! found on.
!
! Written by Jonathan Platt
!-----------------------------------------
INTEGER FUNCTION SSNnoPo(searchSSN)
  IMPLICIT NONE
  CHARACTER::searchSSN*9,SSN*9
  INTEGER::nRecs,first,middle,last
  READ(11,'(I2)',REC=1) nRecs
  first=2
  last=nRecs+1
  SSNnoPo=0
  DO
    IF(first>last) RETURN
    middle=(first+last)/2
    READ(11,100,REC=middle) SSN
100 FORMAT(A9)
    IF(searchSSN<SSN) last=middle-1
    IF(searchSSN>SSN) first=middle+1
    IF(searchSSN==SSN) THEN
      SSNnoPo=middle
      RETURN
    END IF
  END DO
END FUNCTION SSNnoPo
