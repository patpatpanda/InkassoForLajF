       IDENTIFICATION DIVISION.
       PROGRAM-ID. Main01.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT EmployeeFile ASSIGN TO
                  "C:\Projects\InkassoForLajF\employees.txt"
                  ORGANIZATION IS LINE SEQUENTIAL.
           SELECT TaxFile ASSIGN TO "tax_output.xml"
                  ORGANIZATION IS LINE SEQUENTIAL.

           SELECT JSONFILE ASSIGN TO
                  "C:\Projects\InkassoForLajF\bruttotax.json"
                  ORGANIZATION IS LINE SEQUENTIAL.

           SELECT XMLFILE ASSIGN TO
                  "C:\Projects\InkassoForLajF\xmlout.xml"
                  ORGANIZATION IS LINE SEQUENTIAL.

           SELECT CSVFILE ASSIGN TO
                  "C:\Projects\InkassoForLajF\csvout.csv"
                  ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD EmployeeFile.
       01 EMPLOYEEFILE-DATA PIC X(54).

       FD TaxFile.
       01 TaxRecord PIC X(256).

       FD JSONFILE.
       01 FS-JSONFILE PIC X(10000).

       FD XMLFILE.
       01 FS-XMLFILE PIC X(10000).

       FD CSVFILE.
       01 FS-CSVFILE PIC X(30).
       01 COLOR-VARIABLES.
         05 COLOR-TITLE PIC 9 VALUE 1.          *> Blå
         05 COLOR-TEXT PIC 9 VALUE 2.           *> Grön
         05 COLOR-ERROR PIC 9 VALUE 3.          *> Röd
         05 COLOR-WHITE PIC 9 VALUE 7.          *> Vit (standard)

       WORKING-STORAGE SECTION.
       COPY "W_EMP01.CPY".
       COPY "W_JSON02.CPY".
       COPY "W_XML03.CPY".
       COPY "W_CSV04.CPY".

       01 CSV-STRING PIC X(50).

       01 I PIC 9(2) VALUE 1.
       01 WS-TaxAmount PIC 9(5).
       01 WS-ChurchMember PIC X.
       01 WS-Salary PIC 9(5).
       01 WS-DATEOFBIRTH PIC 9(8).
       01 WS-FEE PIC 9(5).
       01 SWITCHES PIC 9.
         88 EOF VALUE 0 FALSE 1.

       01 JSON-OUTPUT PIC X(10000).

       PROCEDURE DIVISION.

           PERFORM WRITE-TO-FILE
           CALL "SCREEN01"

           GOBACK.
       COMPUTE-TAX SECTION.
           OPEN INPUT EMPLOYEEFILE
           DISPLAY '------------------------'
           PERFORM 20 TIMES
               READ EMPLOYEEFILE INTO EMPLOYEE-ROW(I)
               IF NOT EOF
                   MOVE EMPLOYEE-SALARY(I) TO WS-Salary
                   MOVE EMPLOYEE-CHURCH(I) TO WS-ChurchMember
                   MOVE EMPLOYEE-DATEOFBIRTH(I) TO  WS-DATEOFBIRTH

                   CALL "TaxModule" USING WS-Salary
                                          WS-ChurchMember
                                          WS-TaxAmount

                   DISPLAY 'Employee: ' FUNCTION TRIM (
                     EMPLOYEE-FIRST-NAME(I))
                     SPACE FUNCTION TRIM (EMPLOYEE-LAST-NAME(I))

                   DISPLAY 'Salary:   ' WS-Salary
                   DISPLAY 'Tax:      ' WS-TaxAmount
                   DISPLAY 'DATE:     ' WS-DATEOFBIRTH
                   DISPLAY '------------------------'
                   ADD 1 TO I

               END-IF
           END-PERFORM
           CLOSE EmployeeFile.
       WRITE-TO-FILE SECTION.

           OPEN INPUT EMPLOYEEFILE
           OPEN OUTPUT JSONFILE
           OPEN OUTPUT XMLFILE
           OPEN OUTPUT CSVFILE
           MOVE 1 TO I
           PERFORM 20 TIMES

               READ EMPLOYEEFILE INTO EMPLOYEE-ROW(I)
               IF NOT EOF
                   MOVE EMPLOYEE-SALARY(I) TO WS-Salary
                   MOVE EMPLOYEE-CHURCH(I) TO WS-ChurchMember
                   MOVE EMPLOYEE-DATEOFBIRTH(I) TO WS-DATEOFBIRTH

                   CALL "TaxModule" USING WS-Salary
                                          WS-ChurchMember
                                          WS-TaxAmount

                   CALL "FeeModule" USING WS-Salary
                                          WS-DATEOFBIRTH
                                          WS-FEE

                   PERFORM MOVE-JSON-DATA
                   PERFORM MOVE-XML-DATA
                   PERFORM MOVE-CSV-DATA
                   PERFORM STRING-CSV

                   WRITE FS-CSVFILE
                   ADD 1 TO I
               END-IF
           END-PERFORM

           JSON GENERATE JSON-OUTPUT FROM JSON-EMPLOYEE
           WRITE FS-JSONFILE FROM JSON-OUTPUT

           XML GENERATE FS-XMLFILE FROM XML-EMPLOYEE
           WRITE FS-XMLFILE

           CLOSE EmployeeFile
           CLOSE JSONFILE
           CLOSE XMLFILE
           CLOSE CSVFILE.
           CALL "SCREEN01".

       MOVE-JSON-DATA SECTION.
           MOVE EMPLOYEE-FIRST-NAME(I) TO JSON-EMPLOYEE-FIRST-NAME(I)
           MOVE EMPLOYEE-LAST-NAME(I) TO JSON-EMPLOYEE-LAST-NAME(I)
           MOVE WS-DATEOFBIRTH TO JSON-EMPLOYEE-DATEOFBIRTH(I)
           MOVE WS-TaxAmount TO JSON-TAX-AMOUNT(I)
           
       .
       MOVE-XML-DATA SECTION.
           MOVE EMPLOYEE-FIRST-NAME(I) TO XML-EMPLOYEE-FIRST-NAME(I)
           MOVE EMPLOYEE-LAST-NAME(I) TO XML-EMPLOYEE-LAST-NAME(I)
           MOVE WS-DATEOFBIRTH TO XML-EMPLOYEE-DATEOFBIRTH(I)
           COMPUTE XML-SALARY(I) = WS-SALARY - WS-TAXAMOUNT.

       MOVE-CSV-DATA SECTION.
           MOVE EMPLOYEE-FIRST-NAME(I) TO CSV-EMPLOYEE-FIRST-NAME(I)
           MOVE EMPLOYEE-LAST-NAME(I) TO CSV-EMPLOYEE-LAST-NAME(I)
           MOVE WS-DATEOFBIRTH TO CSV-EMPLOYEE-DATEOFBIRTH(I)
           MOVE WS-FEE TO CSV-FEE(I).

       STRING-CSV SECTION.
           INITIALIZE FS-CSVFILE

           STRING CSV-EMPLOYEE-FIRST-NAME(I) ";"
             CSV-EMPLOYEE-LAST-NAME(I) ";"
             CSV-EMPLOYEE-DATEOFBIRTH(I) ";"
             CSV-FEE(I) ";"
             DELIMITED BY SPACES INTO FS-CSVFILE.



       COMPUTE-FEE SECTION.

           OPEN INPUT EMPLOYEEFILE
           DISPLAY '------------------------'
           PERFORM 20 TIMES
               READ EMPLOYEEFILE INTO EMPLOYEE
               IF NOT EOF
                   MOVE EMPLOYEE-SALARY(I) TO WS-Salary
                   MOVE EMPLOYEE-DATEOFBIRTH(I) TO WS-DATEOFBIRTH

                   CALL "FeeModule" USING WS-Salary
                                          WS-DATEOFBIRTH
                                          WS-FEE

                   DISPLAY 'Employee: ' FUNCTION TRIM (
                     EMPLOYEE-FIRST-NAME(I))
                     SPACE FUNCTION TRIM (EMPLOYEE-LAST-NAME(I))

                   DISPLAY 'DATE:   ' WS-DATEOFBIRTH
                   DISPLAY 'Fee:      ' WS-FEE

                   DISPLAY '------------------------'
               END-IF
           END-PERFORM.
           CLOSE EmployeeFile.

       SHOW-TAX-AMOUNT SECTION.

           OPEN INPUT EMPLOYEEFILE
           DISPLAY '------------------------'
           PERFORM 20 TIMES
               READ EMPLOYEEFILE INTO EMPLOYEE
               IF NOT EOF
                   MOVE EMPLOYEE-SALARY(I) TO WS-Salary
                   MOVE EMPLOYEE-CHURCH(I) TO WS-ChurchMember
                   MOVE EMPLOYEE-DATEOFBIRTH(I) TO WS-DATEOFBIRTH

                   CALL "TaxModule" USING WS-Salary
                                          WS-ChurchMember
                                          WS-TaxAmount

                   DISPLAY 'Employee: ' FUNCTION TRIM (
                     EMPLOYEE-FIRST-NAME(I))
                     SPACE FUNCTION TRIM (EMPLOYEE-LAST-NAME(I))

                   
                   DISPLAY 'DATE:     ' WS-DATEOFBIRTH
                   DISPLAY 'Tax:      ' WS-TaxAmount

                  
                   DISPLAY '------------------------'
               END-IF
           END-PERFORM

           CLOSE EmployeeFile.


       
