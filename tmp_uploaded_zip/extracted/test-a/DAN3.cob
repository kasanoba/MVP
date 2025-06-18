       IDENTIFICATION DIVISION.
       PROGRAM-ID. DAN3.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 I        PIC 9 VALUE 1.
       01 RESULT   PIC 99.

       PROCEDURE DIVISION.
           PERFORM UNTIL I > 9
               COMPUTE RESULT = 3 * I
               DISPLAY "3 * " I " = " RESULT
               ADD 1 TO I
           END-PERFORM
           GOBACK.
       END PROGRAM DAN3.
