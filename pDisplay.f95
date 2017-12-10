!--------------------------------------
! This simple subroutine will be passed
! a record number and will return
! nothing but will display the details
! of the person located at that record
! number.
!
! Written by Jonathan Platt
!--------------------------------------
SUBROUTINE pDisplay
  USE police
  IMPLICIT NONE
  CHARACTER::decode*25,st*22,cty*12,vm*11,vt*15,tc*25,bc*25

  st=decode("state ",22,istcode)
  cty=decode("county",12,ictycode)
  vm=decode("vmake ",11,ivmcode)
  vt=decode("vtype ",15,ivtcode)
  tc=decode("color ",25,itccode)
  bc=decode("color ",25,ibccode)

  CALL SYSTEM("clear")  

  WRITE(*,*) "* * * Police Information System Person Display * * *"
  WRITE(*,*)
  WRITE(*,95) SSN(1:3),SSN(4:5),SSN(6:9)
95  FORMAT(T18,"SSN: ",A3,'-',A2,'-',A4)
  WRITE(*,'(T17,"Name: ",A20)') name
  WRITE(*,'(T14,"Address: ",A30)') street
  WRITE(*,100) city,st(1:2),zip(1:5),zip(6:9) 
100 FORMAT(T23,A19,", ",A2,1X,A5,"-",A4)
  WRITE(*,*)
  WRITE(*,105) istcode,st(3:22),ictycode,cty
105 FORMAT(T16,"State: (",I2.2,") ",A20,/,T15,"County: (",I2.2,") ",A12) 
  WRITE(*,*)
  WRITE(*,'(T9,"Vehicle Information:")')
  WRITE(*,110) tag,st(1:2)
110 FORMAT(T18,"Tag: ",A7,2X,"(",A2,")")
  WRITE(*,'(T17,"Make: (",I2.2,") ",A11)') ivmcode,vm
  WRITE(*,'(T17,"Type: (",I2.2,") ",A15)') ivtcode,vt
  WRITE(*,130) itccode,tc(1:3),tc(4:25)
130 FORMAT(T12,"Top Color: (",I2.2,") ",A3,1X,A22)
  WRITE(*,140) ibccode,bc(1:3),bc(4:25)
140 FORMAT(T9,"Bottom Color: (",I2.2,") ",A3,1X,A22)
END SUBROUTINE pDisplay
