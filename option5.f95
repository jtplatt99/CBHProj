!------------------------
! This program finds a person
! by their SSN and asks confirmation 
! before deleting a record.
!
! Written by Jonathan Platt Section 1 11/17/2017
! Updated 12/8/17
!------------------------
SUBROUTINE option5
  IMPLICIT NONE
  INTEGER::rec,SSNsearch,nRecs
  CHARACTER::readSSN*11,SSN*9,userIn*1,pData*106
  
  OPEN(11,FILE='cbhprojDB/master.db',FORM='formatted',ACCESS='direct',RECL=106)
  DO
    CALL SYSTEM("clear")
    WRITE(*,*) "* * * Police Information System [Delete] * * *"
    WRITE(*,*) 
    WRITE(*,*) "Enter a SSN in the format XXX-XX-XXXX or (q)uit"
    WRITE(*,'(1X,A)',ADVANCE='no') "SSN: "
    READ(*,*) readSSN
    IF(readSSN(1:1)=="q" .OR. readSSN(1:1)=="Q") EXIT
    IF(readSSN(1:1)>'9' .OR. readSSN(1:1)<'0') THEN
      WRITE(*,*)
      WRITE(*,*) "ERROR: Invalid SSN: ",readSSN
      WRITE(*,*) "Only integers 1-9 and '-' are valid entries"
      WRITE(*,*) "Press enter to continue..."
      READ(*,*)
      CYCLE
    ELSEIF(len(trim(readSSN))/=9 .AND. len(trim(readSSN))/=11) THEN
      WRITE(*,*)
      WRITE(*,*) "ERROR: Invalid SSN: ",readSSN
      WRITE(*,*) "A SSN must be 9 (or 11 with hyphens) digits long"
      WRITE(*,'(1X,A,I2,A)') "You entered: ",len(trim(readSSN))," digits"
      WRITE(*,*) "Press enter to continue..."
      READ(*,*)
      CYCLE
    END IF
    IF(readSSN(4:4)=='-' .OR. readSSN(4:4)==":") THEN
      SSN=readSSN(1:3)//readSSN(5:6)//readSSN(8:11)
    ELSE
      SSN=readSSN(1:9)
    END IF
    rec=SSNsearch(SSN)
    WRITE(*,*)
    IF(rec==0) THEN
      WRITE(*,*) "Record with SSN: ",SSN(1:3)//'-'//SSN(4:5)//'-'//SSN(6:9)," not found"
      WRITE(*,*)
      WRITE(*,*) "Press enter to continue..."
      READ(*,*)
      CYCLE
    ELSE
      CALL pDisplay
      WRITE(*,*)
!* Begin delete record process
      WRITE(*,'(A)',ADVANCE='no') " Are you sure you want to delete this record (y/n): "
      READ(*,'(A1)') userIn
      WRITE(*,*)
      IF(userIn=="y" .OR. userIn=="Y") THEN
        READ(11,'(I2)',rec=1) nRecs
        READ(11,'(A106)',rec=nRecs+1) pData
        WRITE(11,'(A106)',rec=rec) pData
        WRITE(11,'(I2)',rec=1) nRecs-1
        CALL sort
        WRITE(*,*) "Record successfully deleted. Press enter to continue..."
      ELSE
        WRITE(*,*) "Record NOT deleted. Press enter to continue..."
      END IF
      READ*
    END IF
  END DO
  CLOSE(11)
END SUBROUTINE option5
