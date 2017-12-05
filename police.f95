!--------------------------------
! This is a module which contains
! the relavent person data for
! for the Police Management
! System. It is used in both the
!'SSNsearch' and 'pDisplay'
! subprograms
!
! Written by Jonathan Platt
!--------------------------------
MODULE police
  IMPLICIT NONE
  SAVE
  CHARACTER::SSN*9,name*20,street*30,city*19,zip*9,tag*7
  INTEGER::istcode,ictycode,ivtcode,itccode,ivmcode,ibccode
END MODULE police
