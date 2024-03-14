       IDENTIFICATION DIVISION.
       PROGRAM-ID. luhn.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       REPOSITORY. FUNCTION ALL INTRINSIC.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-CARD-NUMBER         PIC X(32).
       01  WS-CARD-DIGITS         PIC 9(32).
       01  WS-CHECKSUM            PIC 9(2).
       01  WS-VALID               PIC X(5).
       01  WS-R                   PIC 9(3)       VALUE 0.
       01  WS-Q                   PIC 9(3)       VALUE 0.  
       01  WS-IX                  PIC 9(2).
       01  WS-CNT                 PIC 9(2)       VALUE 1.
       01  WS-PNTR                PIC 9(32)      VALUE 1.
       01  WS-IPT                 PIC X.
       01  WS-DIGIT               PIC 9(2).
       01  WS-CNTR                PIC 9(32).
       01  WS-PNTR-CNTR           PIC 9(2).
       01  WS-IX-CNTR             PIC 9(32).
       
       PROCEDURE DIVISION.
       LUHN.
           
           PERFORM 0010-INIT-VAR.
           DISPLAY "INPUT : " WITH NO ADVANCING. *>---------------------
           ACCEPT WS-CARD-NUMBER.                *>---------------------
           PERFORM 0020-RMV-SPC.    
           DISPLAY "-------------------------".
           IF WS-VALID ="VALID"
              PERFORM VARYING WS-CNTR FROM 1 BY 1 UNTIL
                 WS-CNTR > WS-IX-CNTR 
                 PERFORM 0030-GET-SUM-DGT
              END-PERFORM
              PERFORM 0040-CHK-OTP
           END-IF.
           DISPLAY WS-VALID.                     *>---------------------
           STOP RUN.

       0010-INIT-VAR.
           MOVE 0 TO WS-CHECKSUM.
           MOVE "0" TO WS-CARD-NUMBER.
           MOVE 0 TO WS-CARD-DIGITS.    
           MOVE "VALID" TO WS-VALID.
           MOVE 0 TO WS-PNTR.

       0020-RMV-SPC.
           DISPLAY "EXECUTING 0020-RMV-SPC".
           PERFORM VARYING WS-IX FROM 1 BY 1 UNTIL 
              WS-IX > FUNCTION LENGTH(FUNCTION TRIM(WS-CARD-NUMBER))
              IF FUNCTION LENGTH(FUNCTION TRIM(WS-CARD-NUMBER)) <= 1
                 MOVE "FALSE" TO WS-VALID
                 EXIT PARAGRAPH
              END-IF
              IF WS-CARD-NUMBER(WS-IX:1) = SPACE
                 CONTINUE
              ELSE IF WS-CARD-NUMBER(WS-IX:1) NOT NUMERIC
                 MOVE "FALSE" TO WS-VALID
                 EXIT PARAGRAPH
              ELSE
                 MOVE WS-CARD-NUMBER(WS-IX:1)    TO 
                      WS-CARD-DIGITS(WS-CNT:1)     
                 ADD 1 TO WS-CNT 
                 ADD 1 TO WS-PNTR-CNTR
                 ADD 1 TO WS-IX-CNTR
              END-IF 
           END-PERFORM.

       0030-GET-SUM-DGT.
           DISPLAY "EXECUTING 0030-GET-SUM-DGT".
           DIVIDE WS-IX-CNTR BY 2 GIVING WS-Q REMAINDER WS-R
           IF WS-R IS EQUAL TO 0
              PERFORM 0050-CHK-WS-PNTR-CNTR
           ELSE
              PERFORM 0060-CHK-WS-PNTR
           END-IF.

       0040-CHK-OTP.
           DISPLAY "EXECUTING 0040-CHK-OTP".
           DIVIDE WS-CHECKSUM BY 10 GIVING WS-Q REMAINDER WS-R.
           IF WS-R IS EQUAL TO 0
              MOVE "VALID" TO WS-VALID
           ELSE
              MOVE "FALSE" TO WS-VALID
           END-IF.

       0050-CHK-WS-PNTR-CNTR.
           DISPLAY "EXECUTING 0050-CHK-WS-PNTR-CNTR".
           IF WS-PNTR-CNTR = 1
              DISPLAY "TESTTT"
              MOVE WS-CARD-DIGITS(WS-PNTR-CNTR:1) TO WS-DIGIT
              DISPLAY "POINTER: " WS-PNTR-CNTR   *>------------------
              COMPUTE WS-PNTR-CNTR = WS-PNTR-CNTR - 1
              DISPLAY "NEW POINTER: " WS-PNTR-CNTR   
              COMPUTE WS-DIGIT = WS-DIGIT * 2
              DISPLAY "DIGIT: " WS-DIGIT         *>------------------
              IF WS-DIGIT > 9
                 COMPUTE WS-DIGIT = WS-DIGIT - 9
                 DISPLAY "NEW DIGIT: " WS-DIGIT  *>------------------
                 COMPUTE WS-CHECKSUM = WS-CHECKSUM + WS-DIGIT 
                 DISPLAY "SUM: " WS-CHECKSUM     *>------------------
                 ADD 1 TO WS-PNTR
              ELSE
                 COMPUTE WS-CHECKSUM = WS-CHECKSUM + WS-DIGIT
                 DISPLAY "SUM: " WS-CHECKSUM     *>------------------
                 ADD 1 TO WS-PNTR
              END-IF 
              EXIT PARAGRAPH
           END-IF.
           DIVIDE WS-PNTR-CNTR BY 2 GIVING WS-Q REMAINDER WS-R.
           IF WS-R IS EQUAL TO 0    
              MOVE WS-CARD-DIGITS(WS-PNTR-CNTR:1) TO WS-DIGIT
              DISPLAY "POINTER: " WS-PNTR-CNTR   *>------------------
              COMPUTE WS-PNTR-CNTR = WS-PNTR-CNTR - 1
              DISPLAY "DIGIT: " WS-DIGIT         *>------------------
              COMPUTE WS-CHECKSUM = WS-CHECKSUM + WS-DIGIT  
              DISPLAY "SUM: " WS-CHECKSUM        *>------------------
              ADD 1 TO WS-PNTR
           ELSE
              MOVE WS-CARD-DIGITS(WS-PNTR-CNTR:1) TO WS-DIGIT
              DISPLAY "POINTER: " WS-PNTR-CNTR   *>------------------
              COMPUTE WS-PNTR-CNTR = WS-PNTR-CNTR - 1
              DISPLAY "NEW POINTER: " WS-PNTR-CNTR   
              COMPUTE WS-DIGIT = WS-DIGIT * 2
              DISPLAY "DIGIT: " WS-DIGIT         *>------------------
              IF WS-DIGIT > 9
                 COMPUTE WS-DIGIT = WS-DIGIT - 9
                 DISPLAY "NEW DIGIT: " WS-DIGIT  *>------------------
                 COMPUTE WS-CHECKSUM = WS-CHECKSUM + WS-DIGIT 
                 DISPLAY "SUM: " WS-CHECKSUM     *>------------------
                 ADD 1 TO WS-PNTR
              ELSE
                 COMPUTE WS-CHECKSUM = WS-CHECKSUM + WS-DIGIT
                 DISPLAY "SUM: " WS-CHECKSUM     *>------------------
                 ADD 1 TO WS-PNTR
              END-IF 
           END-IF.

       0060-CHK-WS-PNTR.
           DISPLAY "EXECUTING 0060-CHK-WS-PNTR".
           IF WS-PNTR-CNTR = 1
              MOVE WS-CARD-DIGITS(WS-PNTR-CNTR:1) TO WS-DIGIT
              COMPUTE WS-CHECKSUM = WS-CHECKSUM + WS-DIGIT  
              ADD 1 TO WS-PNTR
              EXIT PARAGRAPH
           END-IF.
           DIVIDE WS-PNTR-CNTR BY 2 GIVING WS-Q REMAINDER WS-R.
           IF WS-R IS EQUAL TO 0    
              MOVE WS-CARD-DIGITS(WS-PNTR-CNTR:1) TO WS-DIGIT
              DISPLAY "POINTER: " WS-PNTR-CNTR   *>------------------
              COMPUTE WS-PNTR-CNTR = WS-PNTR-CNTR - 1
              DISPLAY "NEW POINTER: " WS-PNTR-CNTR   
              COMPUTE WS-DIGIT = WS-DIGIT * 2
              DISPLAY "DIGIT: " WS-DIGIT         *>------------------
              IF WS-DIGIT > 9
                 COMPUTE WS-DIGIT = WS-DIGIT - 9
                 DISPLAY "NEW DIGIT: " WS-DIGIT  *>------------------
                 COMPUTE WS-CHECKSUM = WS-CHECKSUM + WS-DIGIT 
                 DISPLAY "SUM: " WS-CHECKSUM     *>------------------
                 ADD 1 TO WS-PNTR
              ELSE
                 COMPUTE WS-CHECKSUM = WS-CHECKSUM + WS-DIGIT
                 DISPLAY "SUM: " WS-CHECKSUM     *>------------------
                 ADD 1 TO WS-PNTR
              END-IF 
           ELSE
              MOVE WS-CARD-DIGITS(WS-PNTR-CNTR:1) TO WS-DIGIT
              DISPLAY "POINTER: " WS-PNTR-CNTR   *>------------------
              COMPUTE WS-PNTR-CNTR = WS-PNTR-CNTR - 1
              DISPLAY "NEW POINTER: " WS-PNTR-CNTR   
              DISPLAY "DIGIT: " WS-DIGIT         *>------------------
              COMPUTE WS-CHECKSUM = WS-CHECKSUM + WS-DIGIT  
              DISPLAY "SUM: " WS-CHECKSUM        *>------------------
              ADD 1 TO WS-PNTR
           END-IF.
