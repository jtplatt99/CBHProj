!--------------------------------------
! This is a subroutine which sorts the
! police master database numberically by
! SSN. It uses the bubble sort algorithm
!
! Written by Jonathan Platt Section 1 12/1/17
!--------------------------------------
SUBROUTINE sort
  IMPLICIT NONE
  CHARACTER(106)::str1,str2
  INTEGER::nRecs,pass,first
  LOGICAL::sorted

  pass=1
  READ(11,'(I2)',rec=1) nRecs
  sorted=.FALSE.
  
  DO
    IF(sorted) EXIT
    sorted=.TRUE.
    DO first=2,nRecs-pass+1
100   FORMAT(A106)
      READ(11,100,rec=first) str1
      READ(11,100,rec=first+1) str2
      IF(str1(1:9)<=str2(1:9)) CYCLE
      WRITE(11,100,rec=first) str2
      WRITE(11,100,rec=first+1) str1
      sorted=.FALSE.
    END DO
    pass=pass+1
  END DO
  WRITE(*,'(1X,I2,A)') nRecs," records in master file sorted."
END SUBROUTINE sort
