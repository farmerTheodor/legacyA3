IDENTIFICATION DIVISION.                                        
    PROGRAM-ID. primes1.                                   
environment division.
    INPUT-OUTPUT SECTION.
    FILE-CONTROL.
       select INPUT-FILE assign to 'inFile.txt'.
       select OUTPUT-FILE assign to 'outFile.txt'.

DATA DIVISION.
    FILE SECTION.
        FD OUTPUT-FILE.
        01 OUT-LINE pic X(81).
        FD INPUT-FILE.
        01  IN-CARD.
            02 IN-N   PICTURE 9(9).
            02 FILLER PICTURE X(71).
WORKING-STORAGE SECTION.
    77  N  PICTURE S9(9).
    77  R  PICTURE S9(9) USAGE IS COMPUTATIONAL.
    77  I  PICTURE S9(9) USAGE IS COMPUTATIONAL.
    01  TITLE-LINE.
        02 FILLER PICTURE X(6) VALUE SPACES.
        02 FILLER PICTURE X(20) VALUE 'PRIME NUMBER RESULTS'.
    01  UNDER-LINE.
        02 FILLER PICTURE X(32) VALUE       ' -------------------------------'.
    01  NOT-A-PRIME-LINE.
        02 FILLER PICTURE X VALUE SPACE.
        02 OUT-N-2 PICTURE Z(8)9.
        02 FILLER PICTURE X(15) VALUE ' IS NOT A PRIME'.
    01  PRIME-LINE.
        02 FILLER PICTURE X VALUE SPACE.
        02 OUT-N-3 PICTURE Z(8)9.
        02 FILLER PICTURE X(11) VALUE ' IS A PRIME'.
    01  ERROR-MESS.
        02 FILLER PICTURE X VALUE SPACE.
        02 OUT-N PICTURE Z(8)9.
        02 FILLER PICTURE X(14) VALUE ' ILLEGAL INPUT'.
    01 eof-switch pic 9 VALUE 1.
PROCEDURE DIVISION.
OPEN INPUT INPUT-FILE, OUTPUT OUTPUT-FILE.
WRITE OUT-LINE FROM TITLE-LINE AFTER ADVANCING 0 LINES.
WRITE OUT-LINE FROM UNDER-LINE AFTER ADVANCING 1 LINE.

perform isPrime until eof-switch is equal to 0.

CLOSE INPUT-FILE, OUTPUT-FILE.
STOP RUN.

isPrime.
    READ INPUT-FILE INTO IN-CARD AT END MOVE 0 to eof-switch.
    if eof-switch is not equal to zero
        MOVE IN-N TO N
        display N
        if N IS GREATER THAN 1
            if N IS LESS THAN 4
                MOVE IN-N TO OUT-N-3
                WRITE OUT-LINE FROM PRIME-LINE AFTER ADVANCING 1 LINE
            else
                MOVE 2 TO R
                perform loop2 until R is greater than or equal to N or I is equal to N
                if I is not equal to N 
                    MOVE IN-N TO OUT-N-3
                    WRITE OUT-LINE FROM PRIME-LINE AFTER ADVANCING 1 LINE
                end-if
            end-if
        else
            MOVE IN-N TO OUT-N
            WRITE OUT-LINE FROM ERROR-MESS AFTER ADVANCING 1 LINE
        end-if
    end-if.

loop2.
    DIVIDE R INTO N GIVING I.
    MULTIPLY R BY I.
    IF I IS NOT EQUAL TO N
        ADD 1 TO R
    else
        MOVE IN-N TO OUT-N-2
        WRITE OUT-LINE FROM NOT-A-PRIME-LINE AFTER ADVANCING 1 LINE
    end-if.
