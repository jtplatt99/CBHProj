!------------------------
! This program allow the user
! to input and SSN and if
! it exists allows them to
! modify it
!
! Written by Jonathan Platt Section 1 11/17/2017
! Updated 12/9/17
!------------------------
SUBROUTINE option6
  USE police
  IMPLICIT NONE
  INTEGER::rec,SSNsearch,nRecs
  CHARACTER::readSSN*11,userIn*1,modSSN*9,newIn*30,decode*25
  
  OPEN(11,FILE='cbhprojDB/master.db',FORM='formatted',ACCESS='direct',RECL=106)
  READ(11,'(I2)',rec=1) nRecs
  DO
    CALL SYSTEM("clear")
    WRITE(*,*) "* * * Police Information System [Modify] * * *"
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
      modSSN=readSSN(1:3)//readSSN(5:6)//readSSN(8:11)
    ELSE
      modSSN=readSSN(1:9)
    END IF
    rec=SSNsearch(modSSN)
    WRITE(*,*)
    IF(rec==0) THEN
      WRITE(*,*) "Record with SSN: ",modSSN(1:3)//'-'//modSSN(4:5)//'-'//modSSN(6:9)," not found."
      WRITE(*,*)
      WRITE(*,*) "Press enter to continue..."
      CYCLE
    ELSE
      SSN=modSSN
      CALL pDisplay
      PRINT*
      WRITE(*,'(A)',ADVANCE='no') "Is this the record you wish to modify? (y/n): "
      READ(*,'(A1)') userIn
      IF(userIn=='n' .OR. userIn=='N') CYCLE
      WRITE(*,*)

    DO
      WRITE(*,'(A,A9)') " Press enter to keep current SSN: ",modSSN
      WRITE(*,'(A)',ADVANCE='no') "  Please enter the modified SSN (11 digits w/ hyphens): "
      READ(*,'(A11)') newIn
      IF(newIn(1:1)/=' ') THEN

        IF(newIn(1:1)>'9' .OR. newIn(1:1)<'0') THEN
          WRITE(*,*)
          WRITE(*,*) "ERROR: Invalid SSN: ",newIn
          WRITE(*,*) "Only integers 1-9 and '-' are valid entries"
          CYCLE
        ELSEIF(len(trim(newIn))/=9 .AND. len(trim(newIn))/=11) THEN
          WRITE(*,*)
          WRITE(*,*) "ERROR: Invalid SSN: ",newIn
          WRITE(*,*) "A SSN must be 9 (or 11 with hyphens) digits long"
          WRITE(*,'(1X,A,I2,A)') "You entered: ",len(trim(newIn))," digits"
          CYCLE
        END IF
        IF(newIn(4:4)=='-' .OR. newIn(4:4)==":") newIn=newIn(1:3)//newIn(5:6)//newIn(8:11) 
        IF(SSNsearch(newIn(1:9))==0 .OR. newIn(1:9)==modSSN) THEN
          rec=SSNsearch(modSSN)
          WRITE(modSSN,'(A9)') newIn(1:9)
          EXIT
        ELSE
          WRITE(*,'(A,A9,A)') " SSN: ",newIn(1:9)," already exists."
          CYCLE
        END IF
      END IF
      WRITE(*,*) "Keeping unmodified SSN"
      EXIT
    END DO  
      PRINT*

      WRITE(*,'(A,A20)') " Press enter to keep current name: ",name 
      WRITE(*,'(A)',ADVANCE='no') "  Please enter the modified name (20 characters max): "
      READ(*,'(A20)') newIn
      IF(newIn(1:1)/=' ') WRITE(name,'(A20)') newIn(1:20)
      IF(newIn(1:1)==' ') WRITE(*,'(A,A3,A)') "Keeping unmodified"
      PRINT*

      WRITE(*,'(A,A30)') " Press enter to keep current street: ",street 
      WRITE(*,'(A)',ADVANCE='no') "  Please enter the modified street (30 characters max): "
      READ(*,'(A30)') newIn
      IF(newIn(1:1)/=' ') WRITE(street,'(A30)') newIn(1:30)
      IF(newIn(1:1)==' ') WRITE(*,'(A,A3,A)') "Keeping unmodified"
      PRINT*

      WRITE(*,'(A,A19)') " Press enter to keep current city: ",city 
      WRITE(*,'(A)',ADVANCE='no') "  Please enter the modified city (19 characters max): "
      READ(*,'(A19)') newIn
      IF(newIn(1:1)/=' ') WRITE(city,'(A19)') newIn(1:19)
      IF(newIn(1:1)==' ') WRITE(*,'(A,A3,A)') "Keeping unmodified"
      PRINT*

      WRITE(*,'(A,A5,A,A4)') " Press enter to keep current zip: ",zip(1:5),"-",zip(6:9)
      WRITE(*,'(A)',ADVANCE='no') "  Please enter the modified zip (XXXXX-XXXX): "
      READ(*,'(A10)') newIn
      IF(newIn(1:1)/=' ') WRITE(zip,'(A5,A4)') newIn(1:5),newIn(7:10)
      IF(newIn(1:1)==' ') WRITE(*,'(A,A3,A)') "Keeping unmodified"
      PRINT*

    DO
      WRITE(*,'(A,I2.2,A,A2)') " Press enter to keep current state: (",istcode,") ",decode("state ",22,istcode)
      WRITE(*,'(A)',ADVANCE='no') "  Please enter the modified state code (Number 01-51): "
      READ(*,'(A2)') newIn
      IF(newIn(1:1)/=' ' .AND. (newIn>='01' .AND. newIn<='51')) THEN
        READ(newIn(1:2),'(I2)') istcode
        EXIT
      ELSEIF(newIn(1:1)==' ') THEN
        WRITE(*,*) "Keeping code unmodified"
        EXIT
      END IF
      WRITE(*,'(A,A3,A)') "ERROR: Invalid code: ",newIn(1:3),&
            ". Please enter a code between 01 and 51 (single digits must include leading 0)"
    END DO
      PRINT*

    DO
      WRITE(*,'(A,I2.2,A,A12)') " Press enter to keep current county: (",ictycode,") ",decode("county",12,ictycode)
      WRITE(*,'(A)',ADVANCE='no') "  Please enter modified county code (Number 00-67): "
      READ(*,'(A2)') newIn
      IF(newIn(1:1)/=' ' .AND. (newIn>='00' .AND. newIn<='67')) THEN
        READ(newIn(1:2),'(I2)') ictycode
        EXIT
      ELSEIF(newIn(1:1)==' ') THEN
        WRITE(*,*) "Keeping code unmodified"
        EXIT
      END IF
      WRITE(*,'(A,A3,A)') "ERROR: Invalid code: ",newIn(1:3),&
            ". Please enter a code between 00 and 67 (single digits must include leading 0)"
    END DO
      PRINT*

    DO
      WRITE(*,'(A,I2.2,A,A15)') " Press enter to keep current vehicle type: (",ivtcode,") ",decode("vtype ",15,ivtcode)
      WRITE(*,'(A)',ADVANCE='no') "  Please enter the modified vehicle type code (Number 01-15): "
      READ(*,'(A2)') newIn
      IF(newIn(1:1)/=' ' .AND. (newIn>='01' .AND. newIn<='15')) THEN
        READ(newIn(1:2),'(I2)') ivtcode
        EXIT
      ELSEIF(newIn(1:1)==' ') THEN
        WRITE(*,*) "Keeping code unmodified"
        EXIT
      END IF
      WRITE(*,'(A,A3,A)') "ERROR: Invalid code: ",newIn(1:3),&
            ". Please enter a code between 01 and 15 (single digits must include leading 0)"
    END DO
      PRINT*

    DO
      WRITE(*,'(A,I2.2,A,A11)') " Press enter to keep current vehicle make: (",ivmcode,") ",decode("vmake ",11,ivmcode)
      WRITE(*,'(A)',ADVANCE='no') "  Please enter the modified vehicle make code (Number 01-51): "
      READ(*,'(A2)') newIn
      IF(newIn(1:1)/=' ' .AND. (newIn>='01' .AND. newIn<='51')) THEN
        READ(newIn(1:2),'(I2)') ivmcode
        EXIT
      ELSEIF(newIn(1:1)==' ') THEN
        WRITE(*,*) "Keeping code unmodified"
        EXIT
      END IF
      WRITE(*,'(A,A3,A)') "ERROR: Invalid code: ",newIn(1:3),&
            ". Please enter a code between 01 and 51 (single digits must include leading 0)"
    END DO
      PRINT*

    DO
      WRITE(*,'(A,I2.2,A,A3)') " Press enter to keep current top color: (",itccode,") ",decode("color ",25,itccode)
      WRITE(*,'(A)',ADVANCE='no') "  Please enter the modified top color code (Number 01-31): "
      READ(*,'(A2)') newIn
      IF(newIn(1:1)/=' ' .AND. (newIn>='01' .AND. newIn<='31')) THEN
        READ(newIn(1:2),'(I2)') itccode
        EXIT
      ELSEIF(newIn(1:1)==' ') THEN
        WRITE(*,*) "Keeping code unmodified"
        EXIT
      END IF
      WRITE(*,'(A,A3,A)') "ERROR: Invalid code: ",newIn(1:3),&
            ". Please enter a code between 01 and 31 (single digits must include leading 0)"
    END DO
      PRINT*

    DO
      WRITE(*,'(A,I2.2,A,A3)') " Press enter to keep current bottom color: (",ibccode,") ",decode("color ",25,ibccode)
      WRITE(*,'(A)',ADVANCE='no') "  Please enter the motified bottom color code (Number 01-31): "
      READ(*,'(A2)') newIn
      IF(newIn(1:1)/=' ' .AND. (newIn>='01' .AND. newIn<='31')) THEN
        READ(newIn(1:2),'(I2)') ibccode
        EXIT
      ELSEIF(newIn(1:1)==' ') THEN
        WRITE(*,*) "Keeping code unmodified"
        EXIT
      END IF
      WRITE(*,'(A,A3,A)') "ERROR: Invalid code: ",newIn(1:3),&
            ". Please enter a code between 01 and 31 (single digits must include leading 0)"
    END DO
      PRINT*

      WRITE(*,'(A,A7)') " Press enter to keep current tag: ",tag
      WRITE(*,'(A)',ADVANCE='no') "  Please enter the modified license plate tag (7 characters max): "
      READ(*,'(A7)') newIn
      IF(newIn(1:1)/=' ') WRITE(tag,'(A7)') newIn(1:7)
      IF(newIn(1:1)==' ') WRITE(*,'(A,A3,A)') "Keeping unmodified"
      PRINT*

      WRITE(*,*) "Press enter to see the record you entered..."
      READ*
      SSN=modSSN
      CALL pDisplay
      WRITE(*,*)
      WRITE(*,'(A)',ADVANCE='no') "Save this record? (y/n): "
      READ(*,'(A1)') userIn
      WRITE(*,*)
      IF(userIn=='Y' .OR. userIn=='y') THEN
        WRITE(11,100,rec=rec) modSSN,name,street,city,zip,istcode,ictycode,ivtcode,itccode,ivmcode,ibccode,tag
100      FORMAT(A9,A20,A30,A19,A9,6I2,A7)
        CALL SORT
        WRITE(*,*) "Modifications saved. Press enter to continue..."
      ELSE
        WRITE(*,*) "Modifications NOT saved. Press enter to continue..."
      END IF
      READ*
      CYCLE
    END IF
  END DO
  CLOSE(11)
END SUBROUTINE option6
