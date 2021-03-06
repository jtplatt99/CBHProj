!------------------------------------------------------------------------------------
! This program is the main program for the CBH final project. I may or may not know
! exactly what is does, but it is my understading that is has something to do with
! a police database management system.
!
! This program specifically allows the user to decide which program mode the user
! seeks to enter. It they redirects the user to the apporpriate subroutine(s) for the
! mode they entered.
!
! Written by Jonathan Platt Section 1 11/17/17
! Updated 11/27/17 
!------------------------------------------------------------------------------------
PROGRAM cbhproj
  IMPLICIT NONE
  INTEGER::rc,dummy
  CHARACTER::userIn*1

  OPEN(15,FILE='cbhprojDB/master.db',FORM='formatted',ACCESS='direct',RECL=106)
  READ(15,'(I2)',REC=1,IOSTAT=rc) dummy
  CLOSE(15)

  DO
    CALL SYSTEM("clear")
    WRITE(*,*) "* * * Police Information System Main Menu * * *"
    WRITE(*,*)
    WRITE(*,*) "1 - Initial Data Load"
    WRITE(*,*) "2 - Display Auxiliary Files"
    WRITE(*,*) "3 - Display a Record (by SSN)"
    WRITE(*,*) "4 - Add a Record"
    WRITE(*,*) "5 - Delete a Record"
    WRITE(*,*) "6 - Modify a Record"
    WRITE(*,*) "7 - List Master File"
    WRITE(*,*) "8 - Exit"
    WRITE(*,*)
    IF(rc/=0) WRITE(*,*) "Loaded data not found. Please run Option 1"
    WRITE(*,'(1X,A)',ADVANCE='no') "Enter your desired mode: "
    READ*,userIn
    SELECT CASE(userIn)
      CASE("0")
        PRINT*,"143466098  173946264  534675763  987690532"
        READ*
      CASE("1")
        CALL option1
        rc=0
      CASE("2")
        CALL option2
      CASE("3")
        CALL option3
      CASE("4")
        CALL option4
      CASE("5")
        CALL option5
      CASE("6")
        CALL option6
      CASE("7")
        CALL option7
      CASE("8","e","E","q","Q","-")
        EXIT
      CASE DEFAULT
        WRITE(*,*) "INVALID CODE ENTERED: ",userIn
        WRITE(*,*) "Please enter a number 1 - 8 or (q)uit"
        WRITE(*,*)
        WRITE(*,*) "Press enter to continue..."
        READ*,
    END SELECT
  END DO
  WRITE(*,*) "Thank you for using the Police Information System"
  WRITE(*,*) "END cbhproj"
END PROGRAM cbhproj
