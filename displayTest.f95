! This program serves as a main program
! for testing the display command
! Jon Platt 11/30/17
PROGRAM displayTest
  INTEGER::numberInRow,recordLength,codeLength
  CHARACTER::datatype*6
  WRITE(*,*) "Please enter your data in this order: Data Type, Record Length, Code Length, # in Row"
  READ(*,*) dataType,recordLength,codeLength,numberInRow
  CALL display(dataType,recordLength,codeLength,numberInRow)
END PROGRAM displayTest
