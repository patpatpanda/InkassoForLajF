      *> -------------------------------------------
      *> DECLARE TABLE for Taxinfo
      *> -------------------------------------------
           EXEC SQL DECLARE Taxinfo TABLE 
           ( TaxId                int
           , MinSalary            int          NOT NULL
           , MaxSalary            int          NOT NULL
           , ChurchMember         char(1)      NOT NULL
           , TaxPercentage        decimal(5,2) NOT NULL
           ) END-EXEC.
      *> -------------------------------------------
      *> COBOL HOST VARIABLES FOR TABLE Taxinfo
      *> -------------------------------------------
       01  DCLTaxinfo.
           03 Taxinfo-TaxId                   PIC S9(09)  COMP-5.
           03 Taxinfo-MinSalary               PIC S9(09)  COMP-5.
           03 Taxinfo-MaxSalary               PIC S9(09)  COMP-5.
           03 Taxinfo-ChurchMember            PIC X(1).
           03 Taxinfo-TaxPercentage           PIC S9(3)V9(2)  COMP-3.
