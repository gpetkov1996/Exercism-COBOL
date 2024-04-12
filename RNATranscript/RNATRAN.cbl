       IDENTIFICATION DIVISION. 
      *************************

       PROGRAM-ID. RNATRAN.
       AUTHOR. GENADI PETKOV.

       
       DATA DIVISION. 
      ***************

       WORKING-STORAGE SECTION. 
      *========================

       01  WS-COMPLEMENT     PIC X(64).
       01  WS-DNA            PIC X(04) VALUE "ACGT".
       01  WS-IX             PIC 9(02) VALUE 1.
       01  WS-IX2            PIC 9(02) VALUE 1.
       01  WS-CHAR           PIC X(1). 
       01  WS-QTT            PIC 9(02).


       PROCEDURE DIVISION.
      ********************

       RNA-TRANSCRIPTION.  
      *------------------

           PERFORM A010-DSP-ACC.
           PERFORM A020-PRFM-CHK.
           PERFORM A030-DSP-RLT.

           STOP RUN.


       A010-DSP-ACC.
      *-------------

           DISPLAY 'ENTER INPUT: ' WITH NO ADVANCING.
           ACCEPT WS-COMPLEMENT. 


       A020-PRFM-CHK.
      *--------------
              
           MOVE FUNCTION LENGTH(FUNCTION TRIM(WS-COMPLEMENT)) 
                                  TO WS-QTT.

           PERFORM VARYING WS-IX FROM 1 BY 1 
              UNTIL WS-IX > WS-QTT
                     
              MOVE WS-COMPLEMENT(WS-IX:1) 
                                  TO WS-CHAR
              PERFORM Z010-EVL-CHAR

           END-PERFORM.


       A030-DSP-RLT.
      *-------------

           DISPLAY 'THE RESULT IS: ' WS-COMPLEMENT.         


       Z010-EVL-CHAR.
      *--------------

           EVALUATE WS-CHAR

              WHEN WS-DNA(1:1)
                 MOVE 'U'         TO WS-COMPLEMENT(WS-IX2:1)
              WHEN WS-DNA(2:1)
                 MOVE 'G'         TO WS-COMPLEMENT(WS-IX2:1)
              WHEN WS-DNA(3:1) 
                 MOVE 'C'         TO WS-COMPLEMENT(WS-IX2:1)
              WHEN WS-DNA(4:1)
                 MOVE 'A'         TO WS-COMPLEMENT(WS-IX2:1)
              WHEN OTHER        
                 MOVE SPACES      TO WS-COMPLEMENT(WS-IX2:1)

           END-EVALUATE.     

           ADD 1 TO WS-IX2.

      ******************************************************************
         *>DISPLAT INFORMATION ABOUT EACH LOOP*<
           DISPLAY 'WS-IX IS: ' WS-IX
           DISPLAY 'LENGTH IS: ' 
              FUNCTION LENGTH(FUNCTION TRIM(WS-COMPLEMENT))  .      
           DISPLAY 'NEW CHAR: ' WS-COMPLEMENT(WS-IX:1).
      ****************************************************************** 
