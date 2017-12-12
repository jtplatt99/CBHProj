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
  INTEGER::rec,rec2,SSNnoPo,nRecs,SSNsearch
  CHARACTER::readSSN*11,userIn*2,modSSN*9,newIn*30,decode*25
  
  OPEN(11,FILE='cbhprojDB/master.db',FORM='formatted',ACCESS='direct',RECL=106)
  READ(11,'(I2)',rec=1) nRecs
mainDo:  DO
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
      READ*
      CYCLE
    ELSE


    DO
      WRITE(*,*) "* * * Police Information System [Modify] * * *"
      PRINT*
      CALL pDisplay
      PRINT*
      WRITE(*,*) "1. SSN             4. City            7. County Code          10. Top Color Code"
      WRITE(*,*) "2. Name            5. Zip Code        8. Vehicle Type Code    11. Bottom Color Code"
      WRITE(*,*) "3. Street Address  6. State Code      9. Vehicle Model Code   12. Vehicle Tag"
      PRINT*
      WRITE(*,'(A)',ADVANCE='no') " Enter the # you would like to modify, S to save or Q to quit without saving: "
      READ(*,'(A2)') userIn
      PRINT*

      SELECT CASE(userIn)
        CASE('s','S')
          WRITE(11,100,rec=rec) modSSN,name,street,city,zip,istcode,ictycode,ivtcode,itccode,ivmcode,ibccode,tag
100        FORMAT(A9,A20,A30,A19,A9,6I2,A7)
          CALL SORT
          WRITE(*,*) "Modifications saved. Press enter to continue..."
          READ*
          CYCLE mainDo
        CASE('q','Q')
          WRITE(*,*) "Modifications NOT saved. Press enter to continue..."
          READ*
          CYCLE mainDo
        CASE('1')
        DO
          WRITE(*,'(A)',ADVANCE='no') "  Please enter the modified SSN (11 digits w/ hyphens): "
          READ(*,'(A11)') newIn
          IF(newIn(1:1)==' ') EXIT
          IF(newIn(1:1)>'9' .OR. newIn(1:1)<'0') THEN
            WRITE(*,*)
            WRITE(*,*) "ERROR: Invalid SSN: ",newIn
            WRITE(*,*) "Only integers 1-9 and '-' are valid entries"
            PRINT*
            WRITE(*,*) "Press enter to continue..."
            READ*
            CYCLE
          ELSEIF(len(trim(newIn))/=9 .AND. len(trim(newIn))/=11) THEN
            WRITE(*,*)
            WRITE(*,*) "ERROR: Invalid SSN: ",newIn
            WRITE(*,*) "A SSN must be 9 (or 11 with hyphens) digits long"
            WRITE(*,'(1X,A,I2,A)') "You entered: ",len(trim(newIn))," digits"
            PRINT*
            WRITE(*,*) "Press enter to continue..."
            READ*
            CYCLE
          END IF
          IF(newIn(4:4)=='-' .OR. newIn(4:4)==":") newIn=newIn(1:3)//newIn(5:6)//newIn(8:11) 
          IF(SSNnoPo(newIn(1:9))==0 .OR. newIn(1:9)==modSSN) THEN
            WRITE(modSSN,'(A9)') newIn(1:9)
            WRITE(SSN,'(A9)') newIn(1:9)
            EXIT
          ELSE 
            WRITE(*,'(A,A9,A)') " SSN: ",newIn(1:9)," already exists."
            PRINT*
            WRITE(*,*) "Press enter to continue..."
            READ*
            CYCLE
          END IF
        END DO
        CASE('2')
          WRITE(*,'(A)',ADVANCE='no') "  Please enter the name: LAST, FIRST (20 characters max): "
          READ(*,'(A20)') newIn
          IF(newIn(1:1)/=' ') name=newIn(1:20)
        CASE('3')
          WRITE(*,'(A)',ADVANCE='no') "  Please enter the modified street (30 characters max): "
          READ(*,'(A30)') newIn
          IF(newIn(1:1)/=' ') street=newIn(1:30)
        CASE('4')
          WRITE(*,'(A)',ADVANCE='no') "  Please enter the modified city (19 characters max): "
          READ(*,'(A19)') newIn
          IF(newIn(1:1)/=' ') city=newIn(1:19)
        CASE('5')
        DO     
          WRITE(*,'(A)',ADVANCE='no') "  Please enter the modified zip (XXXXX-XXXX): "
          READ(*,'(A10)') newIn
          IF(newIn(1:1)==' ') EXIT
          IF(newIn(1:1)<'0' .OR. newIn(1:1)>'9' .OR. (len(trim(newIn))/=9 .AND. len(trim(newIn))/=10)) THEN
            WRITE(*,*) "Please enter only digits between 0 and 9 in the form XXXXX-XXXX"
            CYCLE
          END IF
          IF(len(trim(newIn))==10) zip=newIn(1:5)//newIn(7:10)
          IF(len(trim(newIn))==9) zip=newIn(1:9)
          EXIT
        END DO
        CASE('6')
        DO
          CALL SYSTEM('clear')
          CALL dDisplay("state ",22,2,20)
          PRINT*
          WRITE(*,'(A)',ADVANCE='no') "  Please enter the modified state code (Number 01-51): "
          READ(*,'(A2)') newIn
          IF(newIn(1:1)==' ') EXIT
          IF(newIn>='01' .AND. newIn<='51') THEN
            READ(newIn(1:2),'(I2)') istcode
            EXIT
          END IF
          WRITE(*,'(A,A3,A)') "ERROR: Invalid code: ",newIn(1:3),&
            ". Please enter a code between 01 and 51 (single digits must include leading 0)"
          PRINT*
          WRITE(*,*) "Press enter to continue..."
          READ*
        END DO
        CASE('7')
        DO
          CALL SYSTEM('clear')
          CALL dDisplay("county",12,0,20)
          PRINT*
          WRITE(*,'(A)',ADVANCE='no') "  Please enter modified county code (Number 00-67): "
          READ(*,'(A2)') newIn
          IF(newIn(1:1)==' ') EXIT
          IF(newIn>='00' .AND. newIn<='67') THEN
            READ(newIn(1:2),'(I2)') ictycode
            EXIT
          END IF
          WRITE(*,'(A,A3,A)') "ERROR: Invalid code: ",newIn(1:3),&
            ". Please enter a code between 00 and 67 (single digits must include leading 0)"
          PRINT*
          WRITE(*,*) "Press enter to continue..."
          READ*
        END DO
        CASE('8')
        DO
          CALL SYSTEM('clear')
          CALL dDisplay("vtype ",15,0,6)
          WRITE(*,'(A)',ADVANCE='no') "  Please enter the modified vehicle type code (Number 01-15): "
          READ(*,'(A2)') newIn
          IF(newIn(1:1)==' ') EXIT
          IF(newIn>='01' .AND. newIn<='15') THEN
            READ(newIn(1:2),'(I2)') ivtcode
            EXIT
          END IF
          WRITE(*,'(A,A3,A)') "ERROR: Invalid code: ",newIn(1:3),&
            ". Please enter a code between 01 and 15 (single digits must include leading 0)"
          PRINT*
          WRITE(*,*) "Press enter to continue..."
        END DO
        CASE('9')
        DO
          CALL SYSTEM('clear')
          CALL dDisplay("vmake ",11,0,20)
          WRITE(*,'(A)',ADVANCE='no') "  Please enter the modified vehicle make code (Number 01-51): "
          READ(*,'(A2)') newIn
          IF(newIn(1:1)==' ') EXIT
          IF(newIn>='01' .AND. newIn<='51') THEN
            READ(newIn(1:2),'(I2)') ivmcode
            EXIT
          END IF
          WRITE(*,'(A,A3,A)') "ERROR: Invalid code: ",newIn(1:3),&
            ". Please enter a code between 01 and 51 (single digits must include leading 0)"
          PRINT*
          WRITE(*,*) "Press enter to continue..."
        END DO
        CASE('10')
        DO
          CALL SYSTEM('clear')
          CALL dDisplay("color ",25,3,12)
          WRITE(*,'(A)',ADVANCE='no') "  Please enter the modified top color code (Number 01-31): "
          READ(*,'(A2)') newIn
          IF(newIn(1:1)==' ') EXIT
          IF(newIn>='01' .AND. newIn<='31') THEN
            READ(newIn(1:2),'(I2)') itccode
            EXIT
          END IF
          WRITE(*,'(A,A3,A)') "ERROR: Invalid code: ",newIn(1:3),&
            ". Please enter a code between 01 and 31 (single digits must include leading 0)"
          PRINT*
          WRITE(*,*) "Press enter to continue..."
        END DO
        CASE('11')
        DO
          CALL SYSTEM('clear')
          CALL dDisplay("color ",25,3,12)
          WRITE(*,'(A)',ADVANCE='no') "  Please enter the motified bottom color code (Number 01-31): "
          READ(*,'(A2)') newIn
          IF(newIn(1:1)==' ') EXIT
          IF(newIn>='01' .AND. newIn<='31') THEN
            READ(newIn(1:2),'(I2)') ibccode
            EXIT
          END IF
          WRITE(*,'(A,A3,A)') "ERROR: Invalid code: ",newIn(1:3),&
            ". Please enter a code between 01 and 31 (single digits must include leading 0)"
          PRINT*
          WRITE(*,*) "Press enter to continue..."
        END DO
        CASE('12')
          WRITE(*,'(A)',ADVANCE='no') "  Please enter the modified license plate tag (7 characters max): "
          READ(*,'(A7)') newIn
          IF(newIn(1:1)/=' ') tag=newIn(1:7)
        CASE DEFAULT
          WRITE(*,'(A,A2,A)') " ERROR: Invalid Entry: ",userIn,". Please enter numbers 1-12, 'S' or 'Q'"
          PRINT*
          WRITE(*,*) "Press enter to continue"
          READ*
        END SELECT
      END DO
    END IF
  END DO mainDo
  CLOSE(11)
END SUBROUTINE option6
