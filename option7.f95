!------------------------
! This subroutine prints
! out the masterfile in a
! more or less human
! readable format using
! the 'print' function 
!
! Written by Jonathan Platt Section 1 11/17/2017
! Updated 12/8/17
!------------------------
SUBROUTINE option7()
  USE police
  IMPLICIT NONE

  CHARACTER::quit*1
  INTEGER::count,nRecs,I
  OPEN(11,FILE='cbhprojDB/master.db',FORM='formatted',ACCESS='direct',RECL=106)
  READ(11,'(I2)',rec=1) nRecs
  count=0
  CALL SYSTEM('clear')
  WRITE(*,95)
95 FORMAT("SSN",T13,"Name",T35,"Address (Street)",T64,"(City)",T85,"ST, Zip",/,&
   T12,"Vehicle: Type",T35,"Make",T53,"Top Color",T85,"Bottom Color",/,&
   T13,"Tag",T35,"State",T65,"County",/,&
      "=================================================================================================================")
  DO I=2,nRecs+1
    READ(11,100,REC=I) SSN,name,street,city,zip,istcode,ictycode,ivtcode,itccode,ivmcode,ibccode,tag
100  FORMAT(A9,A20,A30,A19,A9,6I2,A7)
    CALL print
    count=count+1
    IF(count==6) THEN
      WRITE(*,'(A)',ADVANCE='no') " Press enter to continue or (q)uit: "
      READ(*,'(A1)') quit
      IF(quit=='q' .OR. quit=='Q') EXIT
      CALL SYSTEM('clear')
      WRITE(*,95)
      count=0
    END IF
  END DO
  CLOSE(11)
  WRITE(*,*)
  WRITE(*,'(1X,I2.2,A,I2.2,A)') I-2," out of ",nRecs," records printed"
  WRITE(*,*) "Press enter to continue..."
  READ*
END SUBROUTINE option7
