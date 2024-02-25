       IDENTIFICATION DIVISION. 
      *********************

       PROGRAM-ID. ROTCIPER.
       AUTHOR. GENADI PETKOV.
       DATE-WRITTEN. 21/02/24.


       ENVIRONMENT DIVISION. 
      *********************

       INPUT-OUTPUT SECTION. 
      *====================

       FILE-CONTROL. 
      *==================== 


       DATA DIVISION. 
      *********************

       WORKING-STORAGE SECTION.
      *====================    
       
       01  WS-CV.
      *--------------------
       
           05 WCV-LT               PIC X.
           05 WCV-NO               PIC 9(2).
      
       01  WS-CN.
      *--------------------

           05 WCN-CNT              PIC 9(2).
           05 WCN-CNT-PNT          PIC 9(2).

       
       01  WS-UI.
      *--------------------

           05 WS-TEXT              PIC X(128).
           05 WS-KEY               PIC 9(2).
       
       01  WS-OT.
      *-------------------- 

           05 WS-CIPHER             PIC X(128).
           05 WOT-OT-TOT-FIN        PIC X(60).


       PROCEDURE DIVISION.
      *********************

       ROTATIONAL-CIPHER.
      *--------------------     
      
           PERFORM A010-GET-IU.
      
           PERFORM A020-CHECK-LOGIC. 
      
           PERFORM A030-SHOW-END-RLT.
      
           STOP RUN.    


      *-------------------- LOGIC ---------------

       A010-GET-IU.
      *--------------------   
           
           PERFORM Z015-DIS-IN.
           PERFORM Z040-GET-UI.    
       

       A020-CHECK-LOGIC.
      *--------------------   

           PERFORM Z010-DECODE-SNT.


       A030-SHOW-END-RLT.
      *--------------------   
 

           PERFORM Z050-SHOW-END-RLT.


      *-------------------- EXECUTION --------------------

       Z015-DIS-IN.
      *-------------------- 
           
           DISPLAY 'WELCOME TO THE CAESAR CIPHER'.

           DISPLAY 'ENTER ROT AND INPUT'.

           DISPLAY 'EXAMPLE          --> 25  --> PRESS ENTER'.

           DISPLAY '                 --> OMG --> PRESS ENTER'.

           DISPLAY 'OUTPUT SHOULD BE --> TRL'.

           DISPLAY 'YOUR ROT AND INPUT: '.


       Z010-DECODE-SNT.
      *--------------------  
           
           PERFORM VARYING WCN-CNT FROM 1 BY 1 
              UNTIL WCN-CNT > 
                 FUNCTION LENGTH (FUNCTION TRIM
                 (WS-TEXT, TRAILING))
              MOVE WS-TEXT(WCN-CNT:1) TO WCV-LT      
              PERFORM ZO70-EVL-CHAR
           END-PERFORM.


       Z040-GET-UI.
      *-------------------- 

           ACCEPT WS-KEY.
           ACCEPT WS-TEXT.
           MOVE FUNCTION UPPER-CASE(WS-TEXT) TO WS-TEXT.

       
       Z050-SHOW-END-RLT.
      *-------------------- 

           INSPECT WS-CIPHER REPLACING ALL '-' BY ' '.
           DISPLAY ' '.
           DISPLAY '++++++++++++++++++++++++++++++'.
           DISPLAY 'THE RUSULT IS: ' WS-CIPHER.
           DISPLAY '++++++++++++++++++++++++++++++'.


       Z060-CALC-ROT.
      *-------------------- 

           COMPUTE WCV-NO = WCV-NO - 26.


       ZO70-EVL-CHAR.
      *--------------------
       
           DISPLAY '****************************'
           DISPLAY 'ORIGINAL-----INPUT--------> ' WCV-LT 
           IF WCV-LT IS ALPHABETIC
              PERFORM Z020-EV-LT
              IF WCV-NO = 99
                 STRING WS-CIPHER DELIMITED BY SPACE  
                 '-' DELIMITED BY SIZE INTO WS-CIPHER
                 EXIT PARAGRAPH
              END-IF
              COMPUTE WCV-NO = WCV-NO + WS-KEY 
              IF WCV-NO > 26
                 PERFORM Z060-CALC-ROT
              END-IF
              PERFORM Z030-EV-NO
              DISPLAY 'CIPHER-------OUTPUT-------> ' WCV-LT 
              DISPLAY '****************************'
              DISPLAY ' '
              STRING WS-CIPHER DELIMITED BY SPACE  
                  WCV-LT DELIMITED BY SIZE INTO WS-CIPHER
              EXIT PARAGRAPH 
           END-IF.     

           IF WCV-LT IS NUMERIC 
                 STRING WS-CIPHER DELIMITED BY SPACE  
                  WCV-LT DELIMITED BY SIZE INTO WS-CIPHER

           END-IF.
           

       Z020-EV-LT.    
      *-------------------- 

           EVALUATE WCV-LT
              WHEN 'A'
                 MOVE 1 TO WCV-NO
              WHEN 'B'
                 MOVE 2 TO WCV-NO
              WHEN 'C'
                 MOVE 3 TO WCV-NO
              WHEN 'D'
                 MOVE 4 TO WCV-NO
              WHEN 'E'
                 MOVE 5 TO WCV-NO          
              WHEN 'F'
                 MOVE 6 TO WCV-NO
              WHEN 'G'
                 MOVE 7 TO WCV-NO
              WHEN 'H'
                 MOVE 8 TO WCV-NO
              WHEN 'I'
                 MOVE 9 TO WCV-NO
              WHEN 'J'
                 MOVE 10 TO WCV-NO
              WHEN 'K'
                 MOVE 11 TO WCV-NO
              WHEN 'L'
                 MOVE 12 TO WCV-NO
              WHEN 'M'
                 MOVE 13 TO WCV-NO
              WHEN 'N'
                 MOVE 14 TO WCV-NO
              WHEN 'O'
                 MOVE 15 TO WCV-NO      
              WHEN 'P'
                 MOVE 16 TO WCV-NO
              WHEN 'Q'
                 MOVE 17 TO WCV-NO    
              WHEN 'R'
                 MOVE 18 TO WCV-NO
              WHEN 'S'
                 MOVE 19 TO WCV-NO
              WHEN 'T'
                 MOVE 20 TO WCV-NO
              WHEN 'U'
                 MOVE 21 TO WCV-NO       
              WHEN 'V'
                 MOVE 22 TO WCV-NO
              WHEN 'W'
                 MOVE 23 TO WCV-NO
              WHEN 'X'
                 MOVE 24 TO WCV-NO
              WHEN 'Y'
                 MOVE 25 TO WCV-NO
              WHEN 'Z'
                 MOVE 26 TO WCV-NO    
              WHEN ' '
                 MOVE 99 TO WCV-NO 
           END-EVALUATE.


       Z030-EV-NO.  
      *--------------------  

           EVALUATE WCV-NO 
              WHEN 1
                 MOVE 'A' TO WCV-LT
              WHEN 2
                 MOVE 'B' TO WCV-LT
              WHEN 3
                 MOVE 'C' TO WCV-LT
              WHEN 4
                 MOVE 'D' TO WCV-LT
              WHEN 5
                 MOVE 'E' TO WCV-LT          
              WHEN 6
                 MOVE 'F' TO WCV-LT
              WHEN 7
                 MOVE 'G' TO WCV-LT
              WHEN 8
                 MOVE 'H' TO WCV-LT
              WHEN 9
                 MOVE 'I' TO WCV-LT
              WHEN 10
                 MOVE 'J' TO WCV-LT
              WHEN 11
                 MOVE 'K' TO WCV-LT
              WHEN 12
                 MOVE 'L' TO WCV-LT
              WHEN 13
                 MOVE 'M' TO WCV-LT
              WHEN 14
                 MOVE 'N' TO WCV-LT
              WHEN 15
                 MOVE 'O' TO WCV-LT      
              WHEN 16
                 MOVE 'P' TO WCV-LT
              WHEN 17
                 MOVE 'Q' TO WCV-LT    
              WHEN 18
                 MOVE 'R' TO WCV-LT
              WHEN 19
                 MOVE 'S' TO WCV-LT
              WHEN 20
                 MOVE 'T' TO WCV-LT
              WHEN 21
                 MOVE 'U' TO WCV-LT       
              WHEN 22
                 MOVE 'V' TO WCV-LT
              WHEN 23
                 MOVE 'W' TO WCV-LT
              WHEN 24
                 MOVE 'X' TO WCV-LT
              WHEN 25
                 MOVE 'Y' TO WCV-LT
              WHEN 26
                 MOVE 'Z' TO WCV-LT       
              WHEN 99
                 MOVE ' ' TO WCV-LT                  
           END-EVALUATE.     
