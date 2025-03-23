           EXEC SQL
        DECLARE EmployeeCursor CURSOR FOR
           SELECT ID_EMPLOYEE, FIRST_NAME, LAST_NAME, BIRTH_DATE, SALARY
           , CHURCH
        FROM REDWARRIOR.dbo.Employees
           END-EXEC.
