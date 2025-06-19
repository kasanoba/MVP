       IDENTIFICATION DIVISION.
       PROGRAM-ID. DAN8.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 I        PIC 9 VALUE 1.
       01 RESULT   PIC 99.

       PROCEDURE DIVISION.
           PERFORM UNTIL I > 9
               COMPUTE RESULT = 8 * I
               DISPLAY "8 * " I " = " RESULT
               ADD 1 TO I
           END-PERFORM
           GOBACK.
       END PROGRAM DAN8.
