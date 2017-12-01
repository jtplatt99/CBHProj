! This program serves as a main program
! for testing the load command
! Jon Platt 11/30/17
PROGRAM loadTest
  INTEGER::recordLength,maxRecord
  CHARACTER::datatype*6
  WRITE(*,*) "Please enter your data in this order: Data Type, Record Length, Max Record"
  READ(*,*) dataType,recordLength,maxRecord
  CALL load(dataType,recordLength,maxRecord)
END PROGRAM loadTest
