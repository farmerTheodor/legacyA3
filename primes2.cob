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
        01 lineOut pic X(81).
        FD INPUT-FILE.
        01  lineIn.
            02 numIn   PICTURE 9(9).
            02 FILLER PICTURE X(71).
WORKING-STORAGE SECTION.
    77  num  PICTURE S9(9).
    77  divisor  PICTURE S9(9) USAGE IS COMPUTATIONAL.
    77  divResult  PICTURE S9(9) USAGE IS COMPUTATIONAL.
    01  TITLE-LINE.
        02 FILLER PICTURE X(6) VALUE SPACES.
        02 FILLER PICTURE X(20) VALUE 'PRIME NUMBER RESULTS'.
    01  UNDER-LINE.
        02 FILLER PICTURE X(32) VALUE       ' -------------------------------'.
    01  NOT-A-PRIME-LINE.
        02 FILLER PICTURE X VALUE SPACE.
        02 notPrime PICTURE Z(8)9.
        02 FILLER PICTURE X(15) VALUE ' IS NOT A PRIME'.
    01  PRIME-LINE.
        02 FILLER PICTURE X VALUE SPACE.
        02 isPrime PICTURE Z(8)9.
        02 FILLER PICTURE X(11) VALUE ' IS A PRIME'.
    01  ERROR-MESS.
        02 FILLER PICTURE X VALUE SPACE.
        02 illInput PICTURE Z(8)9.
        02 FILLER PICTURE X(14) VALUE ' ILLEGAL INPUT'.
    01 eof-switch pic 9 VALUE 1.
PROCEDURE DIVISION.
OPEN INPUT INPUT-FILE, OUTPUT OUTPUT-FILE.
WRITE lineOut FROM TITLE-LINE AFTER ADVANCING 0 LINES.
WRITE lineOut FROM UNDER-LINE AFTER ADVANCING 1 LINE.

perform checkPrime until eof-switch is equal to 0.

CLOSE INPUT-FILE, OUTPUT-FILE.
STOP RUN.

checkPrime.
    READ INPUT-FILE INTO lineIn AT END MOVE 0 to eof-switch.
    if eof-switch is not equal to zero
        MOVE numIn TO num
        display " "
        display num
        display "--------------------------------"
        if num IS GREATER THAN 1
            if num IS LESS THAN 4
                MOVE numIn TO isPrime
                WRITE lineOut FROM PRIME-LINE AFTER ADVANCING 1 LINE
            else
                MOVE 2 TO divisor
                perform loopFun2 until divisor is greater than or equal to num
                if divResult is not equal to num and divisor is not equal to num
                    MOVE numIn TO isPrime
                    WRITE lineOut FROM PRIME-LINE AFTER ADVANCING 1 LINE
                end-if
            end-if
        else
            MOVE numIn TO illInput
            WRITE lineOut FROM ERROR-MESS AFTER ADVANCING 1 LINE
        end-if
    end-if.

loopFun2.
    DIVIDE divisor INTO num GIVING divResult.
    MULTIPLY divisor BY divResult.
    display divResult
    IF divResult IS NOT EQUAL TO num
        ADD 1 TO divisor
    else
        display divResult
        MOVE numIn TO notPrime
        WRITE lineOut FROM NOT-A-PRIME-LINE AFTER ADVANCING 1 LINE
        MOVE divisor to num
    end-if.
