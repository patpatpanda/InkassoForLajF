       IDENTIFICATION DIVISION.
       PROGRAM-ID. TaxModule.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 TaxRate PIC 9(2)V99.

       01 GrossIncome PIC 9(5).
       01 IsChurchMember PIC X.
       01 TaxAmount PIC 9(5).

       01 Income-Check PIC 9(8).
         88 HIGH-INCOME VALUE 46242 THRU 9999999.
         88 LOW-INCOME VALUE 0 THRU 46241.

       LINKAGE SECTION.
       01 WS-GrossIncome PIC 9(5).
       01 WS-IsChurchMember PIC X.
       01 WS-TaxAmount PIC 9(5).


       PROCEDURE DIVISION USING WS-GrossIncome
                                WS-IsChurchMember
                                WS-TaxAmount.

           MOVE WS-GrossIncome TO GrossIncome.
           MOVE WS-IsChurchMember TO IsChurchMember.
           MOVE GrossIncome TO Income-Check.

           PERFORM CALCULATE-TAX.

           MOVE TaxAmount TO WS-TaxAmount.
           EXIT PROGRAM.

       CALCULATE-TAX SECTION.
           IF HIGH-INCOME THEN
               MOVE 0.4 TO TaxRate
           ELSE
               MOVE 0.2 TO TaxRate
           END-IF

           IF IsChurchMember = 'Y' THEN
               ADD 0.1 TO TaxRate
           END-IF

           COMPUTE TaxAmount = GrossIncome * TaxRate.



       
       