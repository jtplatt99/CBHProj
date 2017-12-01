!--------------------------------
! This program will display data
! stored in a database file in a
! human readable format (columns
! of data incrementing downward
! then rightward)
! This program handles states specifically
!
! Created by Jonathan Platt Section 1 11/28/2017
!--------------------------------
PROGRAM dispState
  IMPLICIT NONE

!* columnWidth = recordLength+6     numberInRow ~ nRecs / 3
  INTEGER,PARAMETER::columnWidth=28,numberInRow=20,recordLength=22

  CHARACTER::line(numberInRow)*100,data*(recordLength)
! 'Column' represents the current column being printed in (1-3) while 'characterPos' is the position of the text within the 'line' variable
  INTEGER::column=1, rowPos=1,characterPos,nRecs,I
!* For other programs change the file location
  OPEN(10,FILE='cbhprojDB/state.db',FORM='FORMATTED',ACCESS='DIRECT',RECL=recordLength,ACTION='READ')
  READ(10,'(I2)',REC=1) nRecs

! The following code blank fills the line strings in preparation for data filling later on
  DO I=1,numberInRow
    line(I)=' '
  END DO

  DO I=1,nRecs
    characterPos=(column-1)*columnWidth
!*   For other program replace 25 with recordLength
    READ(10,'(A22)',REC=I+1) data

!   The following code handles filling the line strings with the relavent data starting with color number, color code, and ending with the full color name
!*   Take note: Depending on the lngth of the code, you may have to alter the offsets written below
    WRITE(line(rowPos)(characterPos+1:characterPos+2),'(I2)') I
    line(rowPos)(characterPos+4:characterPos+5)=data(1:2)
    line(rowPos)(characterPos+7:characterPos+columnWidth-2)=data(3:recordLength)

    rowPos=rowPos+1
    IF(rowPos>numberInRow) THEN
      rowPos=1
      column=column+1
    END IF
  END DO

  CALL SYSTEM('clear')
!* For other programs replace color
  WRITE(*,*) "* * * Police Information System State Database * * *"
  WRITE(*,*)

! The following code prints out each line string in order
  DO I=1,numberInRow
    WRITE(*,*) line(I)
  END DO

  WRITE(*,*)
  WRITE(*,'(A,I2,A)') " End of data. Wrote ",nRecs," data items"
  WRITE(*,*)
END PROGRAM dispState
