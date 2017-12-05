!------------------------
! Subroutine for Option 7
!
! Written by Jonathan Platt Section 1 11/17/2017
!------------------------
SUBROUTINE option7()
  CALL SYSTEM("clear")
  WRITE(*,*) "* * * Police Information System Master File * * *"
  CALL SYSTEM("./chkmaster")
  WRITE(*,*) "Press enter to continue..."
  READ*,
END SUBROUTINE option7
