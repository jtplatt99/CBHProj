!------------------------
! Subroutine for Option 2
!
! Written by Jonathan Platt Section 1 11/17/2017
! Updated 11/27/17
!------------------------
SUBROUTINE option2()
  IMPLICIT NONE
  CHARACTER::userIn*1

  DO
    CALL SYSTEM("clear")
    WRITE(*,*) "* * * Police Information System Option 2 Menu * * *"
    WRITE(*,*)
    WRITE(*,*) "1 - Display State Data"
    WRITE(*,*) "2 - Display County Data"
    WRITE(*,*) "3 - Display Vehicle Makes"
    WRITE(*,*) "4 - Display Vehicle Types"
    WRITE(*,*) "5 - Display Vehicle Colors"
    WRITE(*,*) "6 - Return to Main Menu"
    WRITE(*,*)
    WRITE(*,*) "Enter your desired mode: "
    READ*,userIn
    SELECT CASE(userIn)
      CASE("1")
        CALL display("state ",22,2,20)
      CASE("2")
        CALL display("county",12,0,20)
      CASE("3")
        CALL display("vmake ",11,0,20)
      CASE("4")
        CALL display("vtype ",15,0,6)
      CASE("5")
        CALL display("color ",25,3,12)
      CASE("6")
        EXIT
      CASE DEFAULT
        WRITE(*,*) "INVALID CODE ENTERED: ",userIn
        WRITE(*,*) "Please enter a number 1 - 6"
        WRITE(*,*)
        WRITE(*,*) "Press enter to continue..."
        READ*,
    END SELECT
  END DO
END SUBROUTINE option2
