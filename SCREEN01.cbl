       IDENTIFICATION DIVISION.
       PROGRAM-ID. SCREEN01.
      

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CSVFILE ASSIGN TO
           "C:\Projects\InkassoForLajF\csvout.csv"
           ORGANIZATION IS LINE SEQUENTIAL.
 
       DATA DIVISION.
       FILE SECTION.
       FD CSVFILE.
       01 FS-CSVFILE PIC X(100).
       WORKING-STORAGE SECTION.
       
       01 SWITCH PIC 9 VALUE 0.
         88 EOF VALUE 1.
       
       01 LINE-INX PIC 9(2) VALUE 1.
    
       01 CSV-INDEX PIC 9(2) VALUE 1.
 
       01 TOTAL-RECORDS PIC 9(2) VALUE 0.
      
       01 USER-INPUT PIC X VALUE SPACE.
 
       01 SCREEN-NUMBER PIC 9 VALUE 1.
      
       01 CSV-SCREEN-EMPLOYEE.
         05 CSV-SCREEN-ROW OCCURS 20 TIMES.
           10 CSV-SCREEN-FIRST-NAME PIC X(20).
           10 CSV-SCREEN-LAST-NAME PIC X(20).
           10 CSV-SCREEN-DATEOFBIRTH PIC 9(8).
           10 CSV-SCREEN-FEE PIC 9(5).

       SCREEN SECTION.
      

       01 DISPLAY-SCREEN.
         05 LINE 1 COL 10
            VALUE "-------------------------------------------".
         05 LINE 2 COL 10
            VALUE "        CSV Data Display Screen          ".
         05 LINE 3 COL 10
            VALUE "-------------------------------------------".
         05 LINE 4 COL 2 VALUE
         "Förnamn               Efternamn             Fodd      Avgift"
           .
         05 LINE 5 COL 2 VALUE
           "---------------------------------------------------------"
           .

       PROCEDURE DIVISION.

       MAIN-PROCESS.
         
           PERFORM MOVE-SCREEN-CSV
           PERFORM DISPLAY-SCREEN-LOGIC
           PERFORM SCREEN-LOOP.
           PERFORM CLEAR-SCREEN.
           PERFORM DISPLAY-RECORDS.
           PERFORM DISPLAY-EMPLOYEES.

         
           STOP RUN.

       MOVE-SCREEN-CSV SECTION.
           

           OPEN INPUT CSVFILE
        
           PERFORM UNTIL EOF OR CSV-INDEX > 20
               READ CSVFILE INTO FS-CSVFILE
                   AT END
                       MOVE 1 TO SWITCH
               END-READ

              
               UNSTRING FS-CSVFILE DELIMITED BY ";"
                 INTO CSV-SCREEN-FIRST-NAME(CSV-INDEX)
                 CSV-SCREEN-LAST-NAME(CSV-INDEX)
                 CSV-SCREEN-DATEOFBIRTH(CSV-INDEX)
                 CSV-SCREEN-FEE(CSV-INDEX)
               END-UNSTRING

               ADD 1 TO CSV-INDEX
           END-PERFORM.
         SUBTRACT 1 FROM CSV-INDEX GIVING TOTAL-RECORDS.
           CLOSE CSVFILE.

       DISPLAY-SCREEN-LOGIC SECTION.
       SCREEN-LOOP.
          

           PERFORM DISPLAY-RECORDS.
           DISPLAY "Välj: (N = Next, P = Previous, Q = End)" AT LINE 19
             COL 10.
           ACCEPT USER-INPUT.

        
           EVALUATE USER-INPUT
               WHEN "N"
                   IF SCREEN-NUMBER = 1 THEN
                       MOVE 2 TO SCREEN-NUMBER
                       MOVE 11 TO LINE-INX
                       PERFORM CLEAR-SCREEN
                       PERFORM DISPLAY-RECORDS
                       GO TO SCREEN-LOOP
                   END-IF
               WHEN "P"
                   IF SCREEN-NUMBER = 2 THEN
                       MOVE 1 TO SCREEN-NUMBER
                       MOVE 1 TO LINE-INX
                       PERFORM CLEAR-SCREEN
                       PERFORM DISPLAY-RECORDS
                       GO TO SCREEN-LOOP
                   END-IF
               WHEN "Q"
             
                   STOP RUN
           END-EVALUATE.

           GO TO SCREEN-LOOP.

       CLEAR-SCREEN SECTION.
           
           DISPLAY SPACES UPON CONSOLE.

       DISPLAY-RECORDS SECTION.
        

           DISPLAY DISPLAY-SCREEN.
           IF SCREEN-NUMBER = 1 THEN
               PERFORM DISPLAY-EMPLOYEES VARYING LINE-INX FROM 1 BY 1
                 UNTIL LINE-INX > 10
           ELSE
               PERFORM DISPLAY-EMPLOYEES VARYING LINE-INX FROM 11 BY 1
                 UNTIL LINE-INX > 20
           END-IF.

       DISPLAY-EMPLOYEES SECTION.
          
           PERFORM VARYING LINE-INX FROM LINE-INX BY 1 UNTIL LINE-INX >
             (SCREEN-NUMBER * 10)

               
               DISPLAY CSV-SCREEN-FIRST-NAME(LINE-INX)
                 AT LINE (LINE-INX - ((SCREEN-NUMBER - 1) * 10)) + 5 COL
                 2
                 WITH FOREGROUND-COLOR 2 

              
               DISPLAY CSV-SCREEN-LAST-NAME(LINE-INX)
                 AT LINE (LINE-INX - ((SCREEN-NUMBER - 1) * 10)) + 5 COL
                 24
                 WITH FOREGROUND-COLOR 1 

             
               DISPLAY CSV-SCREEN-DATEOFBIRTH(LINE-INX)
                 AT LINE (LINE-INX - ((SCREEN-NUMBER - 1) * 10)) + 5 COL
                 46
                 WITH FOREGROUND-COLOR 4 

             
               DISPLAY CSV-SCREEN-FEE(LINE-INX)
                 AT LINE (LINE-INX - ((SCREEN-NUMBER - 1) * 10)) + 5 COL
                 56
                 WITH FOREGROUND-COLOR 3 
           END-PERFORM.
