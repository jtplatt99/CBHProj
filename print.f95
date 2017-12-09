!--------------------------------------
! This simple subroutine will be passed
! a record number and will return
! nothing but will display the details
! of the person located at that record
! number.
!
! Written by Jonathan Platt
!--------------------------------------
SUBROUTINE print
  USE police
  IMPLICIT NONE
  CHARACTER::decode*25,st*22,cty*12,vm*11,vt*15,tc*25,bc*25

  st=decode("state ",22,istcode)
  cty=decode("county",12,ictycode)
  vm=decode("vmake ",11,ivmcode)
  vt=decode("vtype ",15,ivtcode)
  tc=decode("color ",25,itccode)
  bc=decode("color ",25,ibccode)

  WRITE(*,100) SSN(1:3),SSN(4:5),SSN(6:9),name,street,city,st(1:2),zip(1:5),zip(6:9)
100 FORMAT(A3,'-',A2,'-',A4,T13,A20,T35,A30,A19,1X,A2,', ',A5,'-',A4) 

  WRITE(*,200) ivtcode,vt,ivmcode,vm,itccode,tc(1:3),tc(4:25),ibccode,bc(1:3),bc(4:25)
200 FORMAT(T12,'(',I2.2,')',A15,T34,'(',I2.2,')',A11,T52,'(',I2.2,')',A3,1X,A23,T84,'(',I2.2,')',A3,1X,A23)

  WRITE(*,300) tag,istcode,st(3:22),ictycode,cty
300 FORMAT(T13,A7,T34,'(',I2.2,')',A20,T64,'(',I2.2,')',A12)
  WRITE(*,*)
END SUBROUTINE print
