       IDENTIFICATION DIVISION.
       PROGRAM-ID. Main01.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      *    SELECT EmployeeFile ASSIGN TO
      *           "C:\Projects\InkassoForLajF\employees.txt"
      *           ORGANIZATION IS LINE SEQUENTIAL.
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
      *FD EmployeeFile.
      *01 EMPLOYEEFILE-DATA PIC X(54).

       FD TaxFile.
       01 TaxRecord PIC X(256).

       FD JSONFILE.
       01 FS-JSONFILE PIC X(10000).

       FD XMLFILE.
       01 FS-XMLFILE PIC X(10000).

       FD CSVFILE.
       01 FS-CSVFILE PIC X(32).

       WORKING-STORAGE SECTION.

           EXEC SQL
           DECLARE EmployeeCursor CURSOR FOR
           SELECT ID_EMPLOYEE, FIRST_NAME, LAST_NAME, BIRTH_DATE, SALARY
           , CHURCH
           FROM REDWARRIOR.dbo.Employees
           END-EXEC.

       COPY "W_EMP01.CPY".
       COPY "W_JSON02.CPY".
       COPY "W_XML03.CPY".
       COPY "W_CSV04.CPY".
       COPY "C:\mrcopy\Employees.cpy".

       01 CSV-STRING PIC X(50).

       01 I PIC 9(2) VALUE 1.

       01 WS-TaxAmount PIC 9(5).

       01 WS-ChurchMember PIC X.

       01 WS-GrossIncome PIC 9(5).

       01 WS-DATEOFBIRTH PIC 9(8).

       01 WS-FEE PIC 9(5).

       01 SWITCHES PIC 9.

         88 EOF VALUE 0 FALSE 1.

       01 JSON-OUTPUT PIC X(10000).

           EXEC SQL

                INCLUDE SQLCA

             END-EXEC.

       PROCEDURE DIVISION.

       MAIN SECTION.

           PERFORM CONNECT-TO-DATABASE.

           PERFORM INITIALIZE-FILE-HANDLING.

           PERFORM PROCESS-FILE-WRITE.

           PERFORM TERMINATE-FILE-HANDLING.

           CALL "SCREEN01"

           GOBACK.

       INITIALIZE-FILE-HANDLING SECTION.

           EXEC SQL

              OPEN EmployeeCursor

          END-EXEC.

           OPEN OUTPUT JSONFILE

           OPEN OUTPUT XMLFILE

           OPEN OUTPUT CSVFILE.

       TERMINATE-FILE-HANDLING SECTION.

           EXEC SQL

              CLOSE EmployeeCursor

          END-EXEC.

           CLOSE JSONFILE

           CLOSE XMLFILE

           CLOSE CSVFILE.

       PROCESS-FILE-WRITE SECTION.

           PERFORM UNTIL EOF

               EXEC SQL

                   FETCH EmployeeCursor INTO

                       :Employees-ID-EMPLOYEE,

                       :Employees-FIRST-NAME,

                       :Employees-LAST-NAME,

                       :Employees-BIRTH-DATE,

                       :Employees-SALARY,

                       :Employees-CHURCH

               END-EXEC

               EVALUATE TRUE

                   WHEN SQLCODE = 100

                       SET EOF TO TRUE

                   WHEN SQLCODE = 0

                       MOVE Employees-SALARY TO WS-GROSSINCOME

                       MOVE Employees-CHURCH TO WS-ChurchMember

                       MOVE Employees-BIRTH-DATE TO WS-DATEOFBIRTH

                       CALL "TaxModule" USING WS-GROSSINCOME
                                              WS-ChurchMember
                                              WS-TaxAmount

                       CALL "FeeModule" USING WS-GROSSINCOME
                                              WS-DATEOFBIRTH
                                              WS-FEE

                      
                       PERFORM MOVE-ALL-DATA

                       PERFORM STRING-CSV

                       WRITE FS-CSVFILE
                   WHEN OTHER

                       DISPLAY 'ERROR FETCHING DATA: ' SQLCODE

                       STOP RUN
               END-EVALUATE

           END-PERFORM.

           EXEC SQL COMMIT END-EXEC

           JSON GENERATE JSON-OUTPUT FROM JSON-EMPLOYEE

           WRITE FS-JSONFILE FROM JSON-OUTPUT

           XML GENERATE FS-XMLFILE FROM XML-EMPLOYEE

           WRITE FS-XMLFILE.

       MOVE-ALL-DATA SECTION.

           MOVE Employees-ID-EMPLOYEE TO I

           MOVE Employees-FIRST-NAME TO JSON-EMPLOYEE-FIRST-NAME(I)
             XML-EMPLOYEE-FIRST-NAME(I)
             CSV-EMPLOYEE-FIRST-NAME(I)

           MOVE Employees-LAST-NAME TO JSON-EMPLOYEE-LAST-NAME(I)
             XML-EMPLOYEE-LAST-NAME(I)
             CSV-EMPLOYEE-LAST-NAME(I)

           MOVE WS-DATEOFBIRTH TO JSON-EMPLOYEE-DATEOFBIRTH(I)
             XML-EMPLOYEE-DATEOFBIRTH(I)
             CSV-EMPLOYEE-DATEOFBIRTH(I)

           MOVE WS-TaxAmount TO JSON-TAX-AMOUNT(I)

           MOVE WS-FEE TO CSV-FEE(I).

           COMPUTE XML-SALARY(I) = WS-GROSSINCOME - WS-TAXAMOUNT.

           .

       STRING-CSV SECTION.

           INITIALIZE FS-CSVFILE

           STRING CSV-EMPLOYEE-FIRST-NAME(I) ";"
             CSV-EMPLOYEE-LAST-NAME(I) ";"
             CSV-EMPLOYEE-DATEOFBIRTH(I) ";"
             CSV-FEE(I) ";"
             DELIMITED BY SPACES INTO FS-CSVFILE.

       CONNECT-TO-DATABASE SECTION.

           EXEC SQL

               CONNECT TO 'redwarriordb'

           END-EXEC.

           DISPLAY 'Database connection successful.'

           DISPLAY 'SQL CODE: ' SQLCODE.
