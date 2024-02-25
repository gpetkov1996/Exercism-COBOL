       IDENTIFICATION DIVISION.
       PROGRAM-ID. PANGRAM.    

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION. 

       DATA DIVISION. 
       FILE SECTION. 
       WORKING-STORAGE SECTION. 
       01  WS-SENTENCE       PIC X(60).
       01  WS-RESULT         PIC 9 VALUE 0. 
       01  WS-LETTERS        PIC A(26)
              VALUE 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'.
       01  WS-EOP            PIC X VALUE 'N'.
       01  WS-SPL-CHAR-LET   PIC X.
       01  WS-SPL-CHAR-SEN   PIC X.
       01  WS-COUNTER        PIC 9(2).
       01  WS-COUNTER-RSLT   PIC 9(2) VALUE 0.
       01  WS-LET-POS        PIC 9(2) VALUE 1.

       PROCEDURE DIVISION.
       PANGRAM.
           PERFORM 0100-GET-USR-INPUT.
           PERFORM 0200-PERF-CHECK UNTIL WS-EOP = 'Y'
           DISPLAY WS-RESULT.
           STOP RUN.
      
       0100-GET-USR-INPUT.
           DISPLAY 'ENTER TEXT'.
           ACCEPT WS-SENTENCE.
           MOVE FUNCTION UPPER-CASE(WS-SENTENCE) TO WS-SENTENCE.

       0200-PERF-CHECK.
           IF FUNCTION LENGTH(FUNCTION TRIM(WS-SENTENCE)) < 26
                 MOVE 'Y' TO WS-EOP
           END-IF
              PERFORM VARYING WS-COUNTER FROM 1 BY 1 
                 UNTIL WS-COUNTER > 
                    FUNCTION LENGTH(FUNCTION TRIM(WS-SENTENCE)) 
                 MOVE WS-SENTENCE(WS-COUNTER:1) TO WS-SPL-CHAR-SEN
                 MOVE WS-LETTERS(WS-LET-POS:1) TO WS-SPL-CHAR-LET
                 IF WS-SPL-CHAR-LET  = WS-SPL-CHAR-SEN
                    MOVE 0 TO WS-COUNTER
                    ADD 1 TO WS-LET-POS
                    ADD 1 TO WS-COUNTER-RSLT
                 END-IF
                 IF WS-COUNTER-RSLT = 26
                    MOVE 1 TO WS-RESULT
                    MOVE 'Y' TO WS-EOP
                 END-IF
              END-PERFORM
              IF WS-SPL-CHAR-LET NOT EQUAL TO WS-SPL-CHAR-SEN
                 MOVE 'Y' TO WS-EOP
              END-IF.
              