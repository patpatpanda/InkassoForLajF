
           

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

       COPY "FILES.CPY".

       WORKING-STORAGE SECTION.




       COPY "EMPLOYEECURSOR.CPY".

       COPY "W_EMP01.CPY".
       COPY "W_JSON02.CPY".
       COPY "W_XML03.CPY".
       COPY "W_CSV04.CPY".
       COPY "C:\mrcopy\Employees.cpy".
      
       COPY "EMPLOYEEDEDUCTIONS-ARRAY.CPY".

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
           MOVE 0 TO I

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

                       PERFORM MOVE-ALL-TO-DB
                       PERFORM STRING-CSV

                       WRITE FS-CSVFILE
                   WHEN OTHER

                       DISPLAY 'ERROR FETCHING DATA: ' SQLCODE

                       STOP RUN
               END-EVALUATE

           END-PERFORM.

           PERFORM INSERT-ALL-TO-DB

           EXEC SQL COMMIT END-EXEC

           JSON GENERATE JSON-OUTPUT FROM JSON-EMPLOYEE

           WRITE FS-JSONFILE FROM JSON-OUTPUT

           XML GENERATE FS-XMLFILE FROM XML-EMPLOYEE

           WRITE FS-XMLFILE.

       MOVE-ALL-DATA SECTION.

           ADD 1 TO I

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

           MOVE WS-FEE TO CSV-FEE(I)

           COMPUTE XML-SALARY(I) = WS-GROSSINCOME - WS-TAXAMOUNT.

           

       MOVE-ALL-TO-DB SECTION.

           ADD 1 TO WS-Index

           MOVE Employees-ID-EMPLOYEE TO EmpId(WS-Index)
           MOVE WS-GROSSINCOME TO Salary(WS-Index)
           MOVE WS-TaxAmount TO TaxAmount(WS-Index)
           MOVE WS-FEE TO FeeAmount(WS-Index)

           COMPUTE NetSalary(WS-Index) =
             Salary(WS-Index) -
             TaxAmount(WS-Index) -
             FeeAmount(WS-Index).

       INSERT-ALL-TO-DB SECTION.

           MOVE WS-Index TO Total-Employees

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > Total-Employees
               MOVE EmpId(I) TO Host-EmpId
               MOVE Salary(I) TO Host-Salary
               MOVE TaxAmount(I) TO Host-TaxAmount
               MOVE FeeAmount(I) TO Host-FeeAmount
               MOVE NetSalary(I) TO Host-NetSalary

               EXEC SQL
                   INSERT INTO REDWARRIOR.dbo.EmployeeDeductions
                   (ID_EMPLOYEE, Salary, TaxAmount, FeeAmount, 
                   NetSalary)
                   VALUES
                   (:Host-EmpId,
                    :Host-Salary,
                    :Host-TaxAmount,
                    :Host-FeeAmount,
                    :Host-NetSalary)
               END-EXEC

           END-PERFORM.

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

           DISPLAY 'SQL CODE: ' SQLCODE