!--------------------------------
! This program will display data
! stored in a database file in a
! human readable format (columns
! of data incrementing downward
! then rightward)
! This program handles takes in
! the data type, length, code
! length, etc...
!
! Created by Jonathan Platt Section 1 11/30/2017
!--------------------------------
SUBROUTINE dDisplay(dataType,recordLength,codeLength,numberInRow)
  IMPLICIT NONE

  INTEGER,INTENT(IN)::numberInRow,recordLength,codeLength
  CHARACTER,INTENT(IN)::dataType*6

  INTEGER::columnWidth

! 'Column' represents the current column being printed in (1-3) while 'characterPos' is the position of the text within the 'line' variable
  INTEGER::column,rowPos,characterPos,nRecs,I
  CHARACTER::line(numberInRow)*100,data*(recordLength),recordLengthChar*2

  column=1
  rowPos=1
  WRITE(recordLengthChar,'(I2)') recordLength

  columnWidth=recordLength+6
  OPEN(10,FILE='cbhprojDB/'//trim(dataType)//'.db',FORM='FORMATTED',ACCESS='DIRECT',RECL=recordLength,ACTION='READ')
  READ(10,'(I2)',REC=1) nRecs

! The following code blank fills the line strings in preparation for data filling later on
  DO I=1,numberInRow
    line(I)=' '
  END DO

  DO I=1,nRecs
    characterPos=(column-1)*columnWidth
    READ(10,'(A'//recordLengthChar//')',REC=I+1) data

!   The following code handles filling the line strings with the relavent data starting with number, code, and ending with the full name
    IF (dataType=="county") THEN
      WRITE(line(rowPos)(characterPos+1:characterPos+2),'(I2)') I-1
    ELSE
      WRITE(line(rowPos)(characterPos+1:characterPos+2),'(I2)') I
    END IF
    line(rowPos)(characterPos+4:characterPos+3+codeLength)=data(1:codeLength)
    line(rowPos)(characterPos+5+codelength:characterPos+columnWidth-2)=data(codeLength+1:recordLength)

    rowPos=rowPos+1
    IF(rowPos>numberInRow) THEN
      rowPos=1
      column=column+1
    END IF
  END DO

  CALL SYSTEM('clear')
  WRITE(*,*) "* * * Police Information System '"//trim(dataType)//"' Database * * *"
  WRITE(*,*)

! The following code prints out each line string in order
  DO I=1,numberInRow
    WRITE(*,*) line(I)
  END DO

  CLOSE(10)
  WRITE(*,*)
  WRITE(*,'(A,I2,A)') " End of data. Wrote ",nRecs," data items"
  WRITE(*,*)
  WRITE(*,*) "Press enter to continue"
  READ(*,*)
END SUBROUTINE dDisplay
