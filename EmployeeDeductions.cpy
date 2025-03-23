      *> -------------------------------------------
      *> DECLARE TABLE for EmployeeDeductions
      *> -------------------------------------------
           EXEC SQL DECLARE EmployeeDeductions TABLE 
           ( EmployeeId           int          NOT NULL
           , Salary               int          NOT NULL
           , TaxAmount            int          NOT NULL
           , FeeAmount            int          NOT NULL
           , NetSalary            int
           ) END-EXEC.
      *> -------------------------------------------
      *> COBOL HOST VARIABLES FOR TABLE EmployeeDeductions
      *> -------------------------------------------
       01  DCLEmployeeDeductions.
           03 EmployeeDeductions-EmployeeId   PIC S9(09)  COMP-5.
           03 EmployeeDeductions-Salary       PIC S9(09)  COMP-5.
           03 EmployeeDeductions-TaxAmount    PIC S9(09)  COMP-5.
           03 EmployeeDeductions-FeeAmount    PIC S9(09)  COMP-5.
           03 EmployeeDeductions-NetSalary    PIC S9(09)  COMP-5.
      *> -------------------------------------------
      *> COBOL INDICATOR VARIABLES FOR TABLE EmployeeDeductions
      *> -------------------------------------------
       01  DCLEmployeeDeductions-NULL.
           03 Col-5-NULL                      PIC S9(04)  COMP-5.
