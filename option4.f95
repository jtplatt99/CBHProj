!------------------------
! This program allow the user
! to input and SSN and if
! it does not already exist
! allows them to create a
! new person with that SSN
! and also prompts for their
! other data
!
! Written by Jonathan Platt Section 1 11/17/2017
! Updated 12/9/17
!------------------------
SUBROUTINE option4
  USE police
  IMPLICIT NONE
  INTEGER::rec,SSNsearch,nRecs
  CHARACTER::readSSN*11,userIn*1,newSSN*9,tempZip*10
  
  OPEN(11,FILE='cbhprojDB/master.db',FORM='formatted',ACCESS='direct',RECL=106)
  READ(11,'(I2)',rec=1) nRecs
  DO
    CALL SYSTEM("clear")
    WRITE(*,*) "* * * Police Information System [Add] * * *"
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
      newSSN=readSSN(1:3)//readSSN(5:6)//readSSN(8:11)
    ELSE
      newSSN=readSSN(1:9)
    END IF
    rec=SSNsearch(newSSN)
    WRITE(*,*)
    IF(rec==0) THEN
      WRITE(*,*) "Creating record with SSN: ",newSSN(1:3)//'-'//newSSN(4:5)//'-'//newSSN(6:9)
      WRITE(*,*)
      WRITE(*,'(A)',ADVANCE='no') " Please enter the name for this record (20 characters max): "
      READ(*,'(A20)') name
      PRINT*
      WRITE(*,'(A)',ADVANCE='no') " Please enter the street address for this record (30 characters max): "
      READ(*,'(A30)') street
      PRINT*
      WRITE(*,'(A)',ADVANCE='no') " Please enter the city for this record (19 characters max): "
      READ(*,'(A19)') city
      PRINT*
      WRITE(*,'(A)',ADVANCE='no') " Please enter the zip code for this record (XXXXX-XXXX): "
      READ(*,'(A10)') tempZip
       zip=tempZip(1:5)//tempZip(7:10)
      PRINT*
      DO
        WRITE(*,'(A)',ADVANCE='no') " Please enter the state code for this record (Number 01-51): "
        READ(*,'(I2)') istcode
        IF(istcode<=51 .AND. istcode>=1) EXIT
        WRITE(*,'(A,I2.2,A)') " ERROR: Invalid code: ",istcode,". Please enter a code between 01 and 51"
      END DO
      PRINT*

      DO
        WRITE(*,'(A)',ADVANCE='no') " Please enter the county code for this record (Number 00-67): "
        READ(*,'(I2)') ictycode
        IF(ictycode<=67 .AND. ictycode>=0) EXIT
        WRITE(*,'(A,I2.2,A)') " ERROR: Invalid code: ",ictycode,". Please enter a code between 00 and 67"
      END DO
      PRINT*
      
      DO
        WRITE(*,'(A)',ADVANCE='no') " Please enter the vehicle type code for this record (Number 01-15): "
        READ(*,'(I2)') ivtcode
        IF(ivtcode<=15 .AND. ivtcode>=1) EXIT
        WRITE(*,'(A,I2.2,A)') " ERROR: Invalid code: ",ivtcode,". Please enter a code between 01 and 15"
      END DO
      PRINT*
     
      DO
        WRITE(*,'(A)',ADVANCE='no') " Please enter the vehicle make code for this record (Number 01-51): "
        READ(*,'(I2)') ivmcode
        IF(ivmcode<=51 .AND. ivmcode>=1) EXIT
        WRITE(*,'(A,I2.2,A)') " ERROR: Invalid code: ",ivmcode,". Please enter a code between 01 and 51"
      END DO
      PRINT*
      
      DO
        WRITE(*,'(A)',ADVANCE='no') " Please enter the vehicle top color code for this record (Number 01-31): "
        READ(*,'(I2)') itccode
        IF(itccode<=31 .AND. itccode>=1) EXIT
        WRITE(*,'(A,I2.2,A)') " ERROR: Invalid code: ",itccode,". Please enter a code between 01 and 31"
      END DO
      PRINT*
    
      DO
        WRITE(*,'(A)',ADVANCE='no') " Please enter the vehicle bottom color code for this record (Number 01-31): "
        READ(*,'(I2)') ibccode
        IF(ibccode<=31 .AND. ibccode>=1) EXIT
        WRITE(*,'(A,I2.2,A)') " ERROR: Invalid code: ",ibccode,". Please enter a code between 01 and 51"
      END DO
      PRINT*
      
      WRITE(*,'(A)',ADVANCE='no') " Please enter the license plate tag for this record (7 characters max): "
      READ(*,'(A7)') tag
      PRINT*
      WRITE(*,*) "Press enter to see the record you entered..."
      READ*
      SSN=newSSN
      CALL pDisplay
      WRITE(*,*)
      WRITE(*,'(A)',ADVANCE='no') "Save this record? (y/n): "
      READ(*,'(A1)') userIn
      WRITE(*,*)
      IF(userIn=='Y' .OR. userIn=='y') THEN
        WRITE(11,100,rec=nRecs+2) newSSN,name,street,city,zip,istcode,ictycode,ivtcode,itccode,ivmcode,ibccode,tag
100      FORMAT(A9,A20,A30,A19,A9,6I2,A7)
        WRITE(11,'(I2)',rec=1) nRecs+1
        CALL SORT
        WRITE(*,*) "Record saved. Press enter to continue..."
      ELSE
        WRITE(*,*) "Record NOT saved. Press enter to continue..."
      END IF
      READ*
      CYCLE
    ELSE
      WRITE(*,*) "Record with SSN: ",SSN(1:3)//'-'//SSN(4:5)//'-'//SSN(6:9)," already exists"
      WRITE(*,*) 
      WRITE(*,*) "Press enter to continue..."
      READ*
      CYCLE
    END IF
  END DO
  CLOSE(11)
END SUBROUTINE option4
