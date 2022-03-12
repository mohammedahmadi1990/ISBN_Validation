




000100*> *>  MAIN-PARA.

      *> *>      PERFORM LOOP-PARA UNTIL r-fileName IS EQUAL TO "0".
      *> *>      STOP RUN.

      *> *>  LOOP-PARA.
      *> *>      DISPLAY "(Type 0 to exit)!"
      *> *>      ACCEPT r-fileName.





      *>  IDENTIFICATION DIVISION.
      *>  PROGRAM-ID. isValid.

      *>  ENVIRONMENT DIVISION.
      *>  INPUT-OUTPUT section.
      *>  FILE-CONTROL.
      *>  SELECT ifile ASSIGN TO "info.dat"
      *>      ORGANIZATION IS LINE SEQUENTIAL.

      *>  DATA DIVISION.
      *>  FILE SECTION.
      *>  FD ifile.
      *>  01 isbn pic x(10).

      *>  WORKING-STORAGE SECTION.
      *>  77 eof-switch pic 9 value 1.
      *>  77 numcont pic 999.
      *>  01 out-record.
      *>  05 out1 pic x(8) value "Email is".
      *>  05 filler pic x.

      *>  PROCEDURE DIVISION.
      *>      DISPLAY "Hello, World!".
      *>  END PROGRAM isValid.





      *>  IDENTIFICATION DIVISION.
      *>  PROGRAM-ID. checkSUM.

      *>  ENVIRONMENT DIVISION.
      *>  INPUT-OUTPUT section.
      *>  FILE-CONTROL.
      *>  SELECT ifile ASSIGN TO "info.dat"
      *>      ORGANIZATION IS LINE SEQUENTIAL.

      *>  DATA DIVISION.
      *>  FILE SECTION.
      *>  FD ifile.
      *>  01 isbn pic x(10).

      *>  WORKING-STORAGE SECTION.
      *>  77 eof-switch pic 9 value 1.
      *>  77 numcont pic 999.
      *>  01 out-record.
      *>  05 out1 pic x(8) value "Email is".
      *>  05 filler pic x.

      *>  PROCEDURE DIVISION.
      *>      DISPLAY "Hello, World!".
      *>  END PROGRAM checkSUM.
