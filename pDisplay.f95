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

  WRITE(*,*) "Name: ",name
  WRITE(*,100) " Address:",street,city,st(1:2),st(3:22),zip(1:5),zip(6:9) 
100 FORMAT(A,1X,A30,1X,A19,",",1X,A2,1X,A20,1X,A5,"-",A4)
  WRITE(*,*) "County: ",cty
  WRITE(*,*) "Vehicle Information:"
  WRITE(*,110) st(3:22),tag
110 FORMAT("  Tag: ",A20,1X,A7)
  WRITE(*,120) vm,vt
120 FORMAT("  Make and Type: ",A11,1X,A15)
  WRITE(*,130) "  Top Color: ",tc(1:3),tc(4:25)
130 FORMAT(A,A3,1X,A22)
  WRITE(*,130) "  Bottom Color: ",bc(1:3),bc(4:25)
END SUBROUTINE pDisplay
