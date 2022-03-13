      ******************************************************************
      * Author:  Mohammed Ahmadi
      * Date:    3/12/2022
      * Purpose: Here is the application to validate books ISBN number.
      *          You can run this application using COBOL.
      * Tectonics: cobc
      ******************************************************************
      ****************************** MAIN ******************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. isbn.
       DATA DIVISION.
       FILE SECTION.

      *>  Shared Structure between subprograms
       WORKING-STORAGE SECTION.
       01 i PIC 99 VALUE 0.
       01 isbnStruct.
           05 inputFileName PIC x(20).
           05 isbnArray PIC x(10) OCCURS 10 TIMES.
           05 isbnMessage OCCURS 10 TIMES.
               10 iMessage PIC x(50) VALUES SPACES.
           05 inputISBN PIC x(10).
           05 checkSumStatus PIC 9 VALUE 0.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.

           DISPLAY
           "..:|Welcome to ISBN Validation Application in COBOL |:..".

      *>   Calling two sub programs by reference
           CALL "readISBN" USING BY REFERENCE isbnStruct END-CALL.
           CALL "isValid" USING BY REFERENCE isbnStruct END-CALL.

      *>   Print final status of the structure.
           SET i TO 1.
           PERFORM UNTIL i > 10
               DISPLAY isbnArray(i) " " isbnMessage(i)
               COMPUTE i = i + 1
           END-PERFORM.

           STOP RUN.
       END PROGRAM isbn.

      ******************************************************************
      ***************************** readISBN ***************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. readISBN.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT section.
       FILE-CONTROL.  *> filename is received via user input
           SELECT inputfile ASSIGN TO inputFileName OF isbnStruct
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS fileStatus.

       DATA DIVISION.
       FILE SECTION.
       FD inputfile.  *> file structure
       01 code-records.
           05 isbn PIC x(10).

       WORKING-STORAGE SECTION.
       01  fileStatus PIC x(02).
       77 feof       pic A(1).

      *>  Shared Structure between subprograms
       LINKAGE SECTION.
       01 isbnStruct.
           05 inputFileName PIC x(20).
           05 isbnArray PIC x(10) OCCURS 10 TIMES INDEXED BY i.

       PROCEDURE DIVISION USING isbnStruct.
       MAIN-PROCEDURE.

      *>   Receive user input as file name.
           DISPLAY "Please enter ISBN-data filename. (Type z to exit): "
           ACCEPT inputFileName.

      *>   Start reading file by checking file status
           OPEN INPUT inputfile
           EVALUATE TRUE
           WHEN fileStatus = "35"     *> not found status
               DISPLAY "File not found!"
               GO TO MAIN-PROCEDURE
           WHEN fileStatus = "00"
               SET i TO 1
               PERFORM UNTIL feof='Y'
      *>   read until end of file
                   READ inputfile INTO isbnArray OF isbnStruct(i)
                   AT END
                       MOVE 'Y' TO feof
                   NOT AT END
                       COMPUTE i = i + 1
                   END-READ
               END-PERFORM
           WHEN OTHER
               DISPLAY "Error...!"
           END-EVALUATE
           CLOSE inputfile.
           GOBACK.

       END PROGRAM readISBN.

      ******************************************************************
      ***************************** isValid ****************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. isValid.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT section.

       DATA DIVISION.
       FILE SECTION.

       WORKING-STORAGE SECTION.
       01  correctBit PIC 9 VALUE 1.  *> Correct Flag
       01  validBit PIC 9 VALUE 0.    *> Valid Flag

      *>  Shared Structure between subprograms
       LINKAGE SECTION.
       01 isbnStruct.
           05 inputFileName PIC x(20).
           05 isbnArray PIC x(10) OCCURS 10 TIMES INDEXED BY i.
           05 isbnMessage OCCURS 10 TIMES.
               10 iMessage PIC x(50) VALUES SPACES.
           05 inputISBN PIC x(10).
           05 checkSumStatus PIC 9 VALUE 0.

       PROCEDURE DIVISION USING isbnStruct.

      *>   LOOP to validate all the ISBN inside the array
       A-PARA.
           SET i TO 1.
           PERFORM B-PARA WITH TEST AFTER UNTIL i>9.
           CONTINUE.

       B-PARA.
      *>   init
           SET correctBit TO 1.
           SET validBit TO 0.

      *>   1st check for incorrect isbn
           IF isbnArray(i)(1:9) NOT NUMERIC
               MOVE 'incorrect, contains a non-digit'
               TO isbnMessage(i)
               SET correctBit TO 0
           END-IF

      *>   2nd check for incorrect isbn
           IF isbnArray(i)(10:1) NOT NUMERIC AND
               NOT (isbnArray(i)(10:1) EQUALS TO "x" OR
               isbnArray(i)(10:1) EQUALS TO "X") THEN
               MOVE 'incorrect, contains a non-digit/X in check digit'
               TO isbnMessage(i)
               SET correctBit TO 0
           END-IF

      *>   checkSum Validating
           IF correctBit = 1 THEN
               MOVE isbnArray(i) TO inputISBN
               CALL 'checkSUM' USING BY REFERENCE isbnStruct END-CALL
               SET validBit TO  checkSumStatus
           END-IF

      *>   Valid and Correct message
           IF validBit = 1 AND correctBit = 1
               MOVE 'correct and valid' TO isbnMessage(i)
           END-IF

      *>   Invalid but Correct message
           IF validBit = 0 AND correctBit = 1
               MOVE 'correct, but not valid (invalid check digit)'
               TO isbnMessage(i)
           END-IF

      *>   Valid and Correct  leading and trailing message
           IF validBit = 1 AND correctBit = 1 AND
               isbnArray(i)(1:1) EQUALS TO "0" AND
               isbnArray(i)(10:1) EQUALS TO "0" THEN
                   MOVE
                   'correct and valid with leading and trailing zero'
                   TO isbnMessage(i)
           END-IF

      *>   Valid and Correct  leading and trailing message
           IF isbnArray(i)(1:1) EQUALS TO "0" AND
               isbnArray(i)(10:1) NOT EQUALS TO "0" THEN
                   MOVE 'correct and valid with leading zero'
                   TO isbnMessage(i)
           END-IF

      *>   Valid and Correct  leading and trailing message
           IF isbnArray(i)(1:1) NOT EQUALS TO "0" AND
               isbnArray(i)(10:1) EQUALS TO "0" THEN
                   MOVE 'correct and valid with trailing zero'
                   TO isbnMessage(i)
           END-IF

      *>   Valid and Correct  leading and trailing message
           IF isbnArray(i)(1:1) EQUALS TO "0" AND
               isbnArray(i)(10:1) EQUALS TO "x" THEN
               MOVE 'correct and valid with leading zero, trailing x'
               TO isbnMessage(i)
           ELSE IF isbnArray(i)(1:1) EQUALS TO "0" AND
               isbnArray(i)(10:1) EQUALS TO "X" THEN
               MOVE 'correct and valid with leading zero, trailing X'
               TO isbnMessage(i)
           END-IF

      *>   Valid and Correct  leading and trailing message
           IF isbnArray(i)(1:1) NOT EQUALS TO "0" AND
               isbnArray(i)(10:1) EQUALS TO "x" THEN
                   MOVE 'correct and valid with trailing lowercase x'
                   TO isbnMessage(i)
           END-IF

      *>   Valid and Correct  leading and trailing message
           IF isbnArray(i)(1:1) NOT EQUALS TO "0" AND
               isbnArray(i)(10:1) EQUALS TO "X" THEN
               MOVE 'correct and valid with trailing uppercase X'
               TO isbnMessage(i)
           END-IF
           COMPUTE i = i + 1.  *> isbn counter

       END PROGRAM isValid.

      ******************************************************************
      ***************************** checkSUM ****************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. checkSUM.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT section.

       DATA DIVISION.
       FILE SECTION.

       WORKING-STORAGE SECTION.
       01 tt PIC 99999 VALUE 0.       *> temporary variable
       01 residue PIC 99999 VALUE 0.  *> modolus
       01 result  PIC 99999 VALUE 0.  *> checksum result
       01 temp01  PIC 99999 VALUE 0.  *> temporary variable
       01 temp02  PIC 99999 VALUE 0.  *> temporary variable

      *>  Shared Structure between subprograms
       LINKAGE SECTION.
       01 isbnStruct.
           05 inputFileName  PIC    x(20).
           05 isbnArray      PIC    x(10) OCCURS 10 TIMES INDEXED BY i.
           05 isbnMessage    OCCURS 10 TIMES.
               10 iMessage   PIC    x(50) VALUES SPACES.
           05 inputISBN      PIC    x(10).
           05 checkSumStatus PIC    9 VALUE 0.

       PROCEDURE DIVISION USING isbnStruct.

           COMPUTE
           tt = FUNCTION NUMVAL(inputISBN(1:1)) * 10
           + FUNCTION NUMVAL(inputISBN(2:1)) * 9
           + FUNCTION NUMVAL(inputISBN(3:1)) * 8
           + FUNCTION NUMVAL(inputISBN(4:1)) * 7
           + FUNCTION NUMVAL(inputISBN(5:1)) * 6
           + FUNCTION NUMVAL(inputISBN(6:1)) * 5
           + FUNCTION NUMVAL(inputISBN(7:1)) * 4
           + FUNCTION NUMVAL(inputISBN(8:1)) * 3
           + FUNCTION NUMVAL(inputISBN(9:1)) * 2
           DIVIDE tt BY 11 GIVING temp02 REMAINDER residue
           COMPUTE result = 11 - residue.

      *>   Special check for 11 as result
           IF result = 11 THEN
               COMPUTE result = 0
           END-IF

      *>   Change X to 10 if met
           IF inputISBN(10:1) IS NUMERIC THEN
               COMPUTE temp01 = FUNCTION NUMVAL(inputISBN(10:1))
           ELSE
               COMPUTE temp01 = 10
           END-IF

      *>   compare and save status of checksum
           IF result = temp01 THEN
               COMPUTE checkSumStatus = 1
           ELSE
               COMPUTE checkSumStatus = 0
           END-IF.

       END PROGRAM checkSUM.
