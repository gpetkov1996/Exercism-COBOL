       IDENTIFICATION DIVISION.
       PROGRAM-ID. LEAP.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-YEAR PIC 9(4).
       01 WS-RESULT PIC A(3).
       PROCEDURE DIVISION.
       LEAP.
           DISPLAY "Choose a year: ".
           ACCEPT WS-YEAR 
           IF FUNCTION MOD (WS-YEAR, 4) = 0 THEN
              IF FUNCTION MOD (WS-YEAR, 100) = 0 THEN
                 IF FUNCTION MOD (WS-YEAR, 400) = 0 THEN
                    MOVE "YES" TO WS-RESULT
                    DISPLAY WS-RESULT
                 ELSE 
                      MOVE "YES" TO WS-RESULT
                    DISPLAY WS-RESULT
                 END-IF
              ELSE
                 MOVE "YES" TO WS-RESULT
                 DISPLAY WS-RESULT
              END-IF
           ELSE 
              MOVE "NO" TO WS-RESULT
                 DISPLAY WS-RESULT  
           END-IF
           CONTINUE.
       LEAP-EXIT.
           EXIT.