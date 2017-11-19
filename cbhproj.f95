!------------------------------------------------------------------------------------
! This prohram is the main program for the CBH final project. I may or may not know
! exactly what is does, but it is my understading that is has something to do with
! a police database management system.
!
! This program specifically allows the user to decide which program mode the user
! seeks to enter. It they redirects the user to the apporpriate subroutine(s) for the
! mode they entered.
!
! Written by Jonathan Platt Section 1 11/17/17
! Updated ------
!------------------------------------------------------------------------------------
PROGRAM cbhproj
  IMPLICIT NONE
  CHARACTER::userIn*1

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
    WRITE(*,*) "Enter your desired mode: "
    READ*,userIn
    SELECT CASE(userIn)
      CASE("1")
        CALL option1
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
      CASE("8","e","E","q","Q")
        WRITE(*,*) "Please confirm you would like to exit (y/n): "
        READ*,userIn
        IF (userIn=='y' .OR. userIn=='Y') EXIT
        CYCLE
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
