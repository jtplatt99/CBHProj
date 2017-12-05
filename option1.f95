!------------------------
! Subroutine for Option 1
! It loads everything
!
! Written by Jonathan Platt Section 1 11/17/2017
!------------------------
SUBROUTINE option1()
  CALL SYSTEM("clear")
  WRITE(*,*) "* * * Police Infortmation System Data Load Sequence * * *"
  WRITE(*,*)
  CALL load("state ",22,51)
  CALL load("county",12,68)
  CALL load("vmake ",11,52)
  CALL load("vtype ",15,16)
  CALL load("color ",25,32)
  CALL ldMaster
  WRITE(*,*)
  WRITE(*,*) "Press enter to continue..."
  READ*,
END SUBROUTINE option1
