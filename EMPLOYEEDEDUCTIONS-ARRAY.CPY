       01 EmployeeDeductions-Array.
         05 EmployeeDeductions-Entry OCCURS 100 TIMES.
           10 EmpId PIC S9(09) COMP-5.
           10 Salary PIC S9(09) COMP-5.
           10 TaxAmount PIC S9(09) COMP-5.
           10 FeeAmount PIC S9(09) COMP-5.
           10 NetSalary PIC S9(09) COMP-5.

       01 WS-Index PIC 9(3) VALUE 0.
       01 Total-Employees PIC 9(3) VALUE 0.

       01 Host-EmpId PIC S9(09) COMP-5.
       01 Host-Salary PIC S9(09) COMP-5.
       01 Host-TaxAmount PIC S9(09) COMP-5.
       01 Host-FeeAmount PIC S9(09) COMP-5.
       01 Host-NetSalary PIC S9(09) COMP-5.
