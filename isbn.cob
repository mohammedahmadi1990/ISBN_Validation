      ******************************************************************
      * Author:  XXXX XXXXXXX
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
       WORKING-STORAGE SECTION.
       01 i PIC 99 VALUE 0.
       01 isbnStruct.
           05 inputFileName PIC x(20).
           05 isbnArray PIC x(10) OCCURS 10 TIMES.
           05 isbnMessage OCCURS 10 TIMES.
               10 iMessage PIC x(50) VALUES SPACES.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.

           DISPLAY
           "..:|Welcome to ISBN Validation Application in COBOL |:..".

           CALL "readISBN" USING BY REFERENCE isbnStruct END-CALL.
           CALL "isValid" USING BY REFERENCE isbnStruct END-CALL.
           CALL "checkSUM" USING BY REFERENCE isbnStruct END-CALL.

           SET i TO 1.
           PERFORM UNTIL i > 10
               DISPLAY isbnArray(i) " " isbnMessage(i)
               COMPUTE i = i + 1
           END-PERFORM.

           STOP RUN.

      ******************************************************************
      ***************************** readISBN ***************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. readISBN.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT section.
       FILE-CONTROL.
           SELECT inputfile ASSIGN TO inputFileName OF isbnStruct
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS fileStatus.

       DATA DIVISION.
       FILE SECTION.
       FD inputfile.
       01 code-records.
           05 isbn PIC x(10).

       WORKING-STORAGE SECTION.
       01  fileStatus PIC x(02).
       77 feof       pic A(1).

       LINKAGE SECTION.
       01 isbnStruct.
           05 inputFileName PIC x(20).
           05 isbnArray PIC x(10) OCCURS 10 TIMES INDEXED BY i.

       PROCEDURE DIVISION USING isbnStruct.
       MAIN-PROCEDURE.

           DISPLAY "Please enter ISBN-data filename. (Type z to exit): "
           ACCEPT inputFileName.

           OPEN INPUT inputfile
           EVALUATE TRUE
           WHEN fileStatus = "35"
               DISPLAY "File not found!"
               GO TO MAIN-PROCEDURE
           WHEN fileStatus = "00"
               SET i TO 1
               PERFORM UNTIL feof='Y'
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

       LINKAGE SECTION.
       01 isbnStruct.
           05 inputFileName PIC x(20).
           05 isbnArray PIC x(10) OCCURS 10 TIMES INDEXED BY i.
           05 isbnMessage OCCURS 10 TIMES.
               10 iMessage PIC x(50) VALUES SPACES.

       PROCEDURE DIVISION USING isbnStruct.

       A-PARA.
           SET i TO 1.
           PERFORM B-PARA WITH TEST AFTER UNTIL i>3.
           STOP RUN.
       B-PARA.
           IF isbnArray(i)(1:1) EQUALS TO "0" THEN
               DISPLAY "xxxxx"
               MOVE 'leading zero' TO isbnMessage(i)
           END-IF
           IF isbnArray(i)(10:1) EQUALS TO "0" THEN
               MOVE "trailing zero" TO isbnMessage(i)
           END-IF
           IF isbnArray(i)(10:1) EQUALS TO "X" THEN
               MOVE "trailing uppercase X" TO isbnMessage(i)
           END-IF
           IF isbnArray(i)(10:1) EQUALS TO "x" THEN
               MOVE "trailing lowercase X" TO isbnMessage(i)
           END-IF
           COMPUTE i = i + 1

       EXIT PROGRAM.
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

       LINKAGE SECTION.
       01 isbnStruct.
           05 inputFileName PIC x(20).
           05 isbnArray PIC x(10) OCCURS 10 TIMES INDEXED BY i.

       PROCEDURE DIVISION USING isbnStruct.
       MAIN-PROCEDURE.


           GOBACK.
       END PROGRAM checkSUM.
