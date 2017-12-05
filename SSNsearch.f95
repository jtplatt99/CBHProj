!-----------------------------------------
! This simple subroutine will return the
! recird number for which a given SSN is
! found on.
!
! Written by Jonathan Platt
!-----------------------------------------
INTEGER FUNCTION SSNsearch(searchSSN)
  USE police
  IMPLICIT NONE
  CHARACTER::searchSSN*9
  INTEGER::nRecs,first,middle,last
  READ(11,'(I2)',REC=1) nRecs
  first=2
  last=nRecs+1
  SSNsearch=0
  DO
    IF(first>last) RETURN
    middle=(first+last)/2
    READ(11,100,REC=middle) SSN,name,street,city,zip,istcode,ictycode,ivtcode,itccode,ivmcode,ibccode,tag
100 FORMAT(A9,A20,A30,A19,A9,6I2,A7)
    IF(searchSSN<SSN) last=middle-1
    IF(searchSSN>SSN) first=middle+1
    IF(searchSSN==SSN) THEN
      SSNsearch=middle
      RETURN
    END IF
  END DO
END FUNCTION SSNsearch
