!--------------------------------------------------
! This program loads both of the master data files
! into a random access file (master.db) for reference
! in a different part of the program
!
! Written by Jonathan Platt 11/30/17 Section 1
!--------------------------------------------------
PROGRAM ldMaster
  IMPLICIT NONE
  CHARACTER::P1*3,P2*2,P3*78,P4*22
  INTEGER::count=0,rc,i=1
  OPEN(11,FILE='cbhprojDB/master.db',FORM='formatted',ACCESS='direct',RECL=105)
  OPEN(10,FILE='cbhdataf17/police1.data')
  DO
    DO
      READ(10,100,IOSTAT=rc) P1,P2,P3,P4
100   FORMAT(A3,1X,A2,1X,A78,1X,A22)
      IF(rc /= 0) EXIT
      count=count+1
      WRITE(11,200,REC=count+1) P1,P2,P3,P4
200   FORMAT(A3,A2,A78,A22)
    END DO
    i=i+1
    CLOSE(10)
    SELECT CASE(i)
      CASE(2)
        OPEN(10,FILE='cbhdataf17/police2.data')
        CYCLE
      CASE DEFAULT
        EXIT
    END SELECT 
  END DO
  WRITE(11,'(I2)',REC=1) count
  WRITE(*,'(1X,I2,A)') count," records found and loaded."
  CLOSE(11)
END PROGRAM ldMaster
