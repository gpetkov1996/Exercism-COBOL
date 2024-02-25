       IDENTIFICATION DIVISION. 
       PROGRAM-ID. rotational-cipher.
       DATA DIVISION. 
       WORKING-STORAGE SECTION.
       01  WS-CV.
           05 WCV-LT               PIC X.
           05 WCV-NO               PIC 9(2).
       01  WS-CN.
           05 WCN-CNT              PIC 9(2).
       01  WS-UI.
           05 WS-TEXT              PIC X(128).
           05 WS-KEY               PIC 9(2).
       01  WS-OT.
           05 WS-CIPHER             PIC X(128).
       PROCEDURE DIVISION.
       ROTATIONAL-CIPHER.      
           IF WS-CIPHER NOT EQUAL TO SPACE
              MOVE SPACE TO WS-CIPHER
           END-IF.
           MOVE FUNCTION UPPER-CASE(WS-TEXT) TO WS-TEXT.
           PERFORM VARYING WCN-CNT FROM 1 BY 1 
              UNTIL WCN-CNT > 
                 FUNCTION LENGTH (FUNCTION TRIM
                 (WS-TEXT, TRAILING))
              MOVE FUNCTION UPPER-CASE(WS-TEXT(WCN-CNT:1)) TO WCV-LT 
              IF WCV-LT IS EQUAL TO ' '
                 STRING WS-CIPHER DELIMITED BY SPACE  
                 '-' DELIMITED BY SIZE INTO WS-CIPHER 
              ELSE IF WCV-LT IS ALPHABETIC
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
                 END-EVALUATE
                 COMPUTE WCV-NO = WCV-NO + WS-KEY 
                 IF WCV-NO > 26
                    COMPUTE WCV-NO = WCV-NO - 26
                 END-IF
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
                 END-EVALUATE 
                 STRING WS-CIPHER DELIMITED BY SPACE  
                 WCV-LT DELIMITED BY SIZE INTO WS-CIPHER
              ELSE IF WCV-LT IS NUMERIC
                 STRING WS-CIPHER DELIMITED BY SPACE  
                 WCV-LT DELIMITED BY SIZE INTO WS-CIPHER
              ELSE 
                 STRING WS-CIPHER DELIMITED BY SPACE  
                 WCV-LT DELIMITED BY SIZE INTO WS-CIPHER
              END-IF          
           END-PERFORM. 
           INSPECT WS-CIPHER REPLACING ALL '-' BY ' '. 
           