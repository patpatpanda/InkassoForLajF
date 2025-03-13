       IDENTIFICATION DIVISION.
       PROGRAM-ID. TaxModule.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 TaxRate PIC 9(2)V99.

       01 INCOME-CHECK PIC 9(8).
         88 HIGH-INCOME VALUE 46242 THRU 9999999.
         88 LOW-INCOME VALUE 0 THRU 46242.

       LINKAGE SECTION.
       01 WS-Salary PIC 9(5).
       01 WS-ChurchMember PIC X.
       01 WS-TaxAmount PIC 9(5).

       PROCEDURE DIVISION USING WS-Salary
                                WS-ChurchMember
                                WS-TaxAmount.
           MOVE WS-Salary TO INCOME-CHECK

           EVALUATE TRUE
               WHEN HIGH-INCOME
                   MOVE 0.4 TO TaxRate
               WHEN LOW-INCOME
                   MOVE 0.2 TO TaxRate
                   IF WS-ChurchMember = 'Y'
                       ADD 0.1 TO TaxRate
                   END-IF
           END-EVALUATE

           COMPUTE WS-TaxAmount = WS-Salary * TaxRate.

           EXIT PROGRAM.
       END PROGRAM TaxModule.
