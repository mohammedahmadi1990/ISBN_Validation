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
       01 isbnStruct.
           05 inputFileName PIC x(20).
           05 isbnArray PIC x(10) OCCURS 10 TIMES INDEXED BY i.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           DISPLAY
           "..:|Welcome to ISBN Validation Application in COBOL |:..".
           CALL "readISBN" USING BY REFERENCE isbnStruct.

           DISPLAY isbnArray(2).
           STOP RUN.
       END PROGRAM isbn.

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
       01  fileReadStatus PIC x(1).
           88 endOfFile VALUE 'Y'.
           88 notEndOfFile VALUE 'N'.
       01  requests PIC 9(1) VALUE ZERO.

       77 n          pic 99.
       77 feof       pic A(1).
       77 isbnVar PIC x(10).
       01 r-isbnData PIC x(10).

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
               SET i TO 0
               PERFORM UNTIL feof='Y'
                   READ inputfile INTO r-isbnData
                   AT END
                       MOVE 'Y' TO feof
                   NOT AT END
                       COMPUTE i = i + 1
                       MOVE r-isbnData TO isbnArray OF isbnStruct(i)
                   END-READ
               END-PERFORM
           WHEN OTHER
               DISPLAY "Error...!"
           END-EVALUATE
           CLOSE inputfile.
           GOBACK.

       END PROGRAM readISBN.
