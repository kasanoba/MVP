IDENTIFICATION DIVISION.
PROGRAM-ID. MAIN.

DATA DIVISION.
WORKING-STORAGE SECTION.
01 I           PIC 9 VALUE 2.

PROCEDURE DIVISION.
    PERFORM VARYING I FROM 2 BY 1 UNTIL I > 9
        DISPLAY "Calling multiplication for " I
        EVALUATE I
            WHEN 2
                CALL "MULTIPLY2"
            WHEN 3
                CALL "MULTIPLY3"
            WHEN 4
                CALL "MULTIPLY4"
            WHEN 5
                CALL "MULTIPLY5"
            WHEN 6
                CALL "MULTIPLY6"
            WHEN 7
                CALL "MULTIPLY7"
            WHEN 8
                CALL "MULTIPLY8"
            WHEN 9
                CALL "MULTIPLY9"
        END-EVALUATE
    END-PERFORM.
    STOP RUN.
