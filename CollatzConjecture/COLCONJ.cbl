       IDENTIFICATION DIVISION.
       PROGRAM-ID. collatz-conjecture.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-NUMBER   PIC S9(8).
       01  WS-STEPS    PIC 9(4).
       01  WS-ERROR    PIC X(35).
       01  WS-R        PIC S9(8).
       01  WS-Q        PIC S9(8).

       PROCEDURE DIVISION.

       COLLATZ-CONJECTURE.
      *---------------  
           
           IF WS-NUMBER <= 0
              MOVE "Only positive integers are allowed" TO WS-ERROR
           ELSE
              PERFORM UNTIL WS-NUMBER IS EQUAL TO 1
                 DIVIDE WS-NUMBER BY 2 GIVING WS-R  REMAINDER WS-Q 
                 IF WS-Q IS EQUAL TO 0 
                    COMPUTE WS-NUMBER = WS-NUMBER / 2
                    ADD 1 TO WS-STEPS 
                 ELSE
                    COMPUTE WS-NUMBER = WS-NUMBER * 3 + 1
                    ADD 1 TO WS-STEPS
                 END-IF
              END-PERFORM
           END-IF.
           DISPLAY WS-STEPS.
           DISPLAY WS-ERROR.
