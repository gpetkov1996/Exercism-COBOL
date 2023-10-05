       IDENTIFICATION DIVISION.
       PROGRAM-ID. LEAP.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-YEAR PIC 9(4) VALUE 1994.
       01 WS-RESULT PIC 9.
       PROCEDURE DIVISION.
       LEAP.
           IF FUNCTION MOD (WS-YEAR, 4) = 0 THEN
              IF FUNCTION MOD (WS-YEAR, 100) = 0 THEN
                 IF FUNCTION MOD (WS-YEAR, 400) = 0 THEN
                    MOVE 1 TO WS-RESULT
                    DISPLAY WS-RESULT
                 ELSE 
                      MOVE 0 TO WS-RESULT
                    DISPLAY WS-RESULT
                 END-IF
              ELSE
                 MOVE 1 TO WS-RESULT
                 DISPLAY WS-RESULT
              END-IF
           ELSE 
              MOVE 0 TO WS-RESULT
                 DISPLAY WS-RESULT  
           END-IF
           CONTINUE.
       LEAP-EXIT.
           EXIT.