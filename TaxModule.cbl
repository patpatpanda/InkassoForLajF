       IDENTIFICATION DIVISION.
       PROGRAM-ID. TaxModule.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       COPY "TAXINFO.CPY".
      
           EXEC SQL
           INCLUDE SQLCA

           END-EXEC


       01 TaxRate PIC 9(2)V99.

       01 GrossIncome PIC 9(5).
       01 IsChurchMember PIC X.
       01 TaxAmount PIC 9(5).

       01 Income-Check PIC 9(5).
        
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

           EXEC SQL
         SELECT TaxPercentage INTO :TaxRate
         FROM REDWARRIOR.dbo.Taxinfo
        WHERE :GrossIncome BETWEEN MinSalary AND MaxSalary 
        AND ChurchMember = :IsChurchMember 
         END-EXEC


        

           COMPUTE TaxAmount = GrossIncome * TaxRate.


           MOVE TaxAmount TO WS-TaxAmount.
           EXIT PROGRAM.

     
         
       