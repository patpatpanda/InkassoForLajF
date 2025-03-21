      *> -------------------------------------------
      *> DECLARE TABLE for FEE_TABLE
      *> -------------------------------------------
           EXEC SQL DECLARE FEE_TABLE TABLE 
           ( MIN_BIRTHYEAR        int
           , MAX_BIRTHYEAR        int
           , FEE_PERCENTAGE       decimal(4,2)
           ) END-EXEC.
      *> -------------------------------------------
      *> COBOL HOST VARIABLES FOR TABLE FEE_TABLE
      *> -------------------------------------------
       01  DCLFEE_TABLE.
           03 FEE-TABLE-MIN-BIRTHYEAR         PIC S9(09)  COMP-5.
           03 FEE-TABLE-MAX-BIRTHYEAR         PIC S9(09)  COMP-5.
           03 FEE-TABLE-FEE-PERCENTAGE        PIC S9(2)V9(2)  COMP-3.
      *> -------------------------------------------
      *> COBOL INDICATOR VARIABLES FOR TABLE FEE_TABLE
      *> -------------------------------------------
       01  DCLFEE_TABLE-NULL.
           03 FEE-TABLE-MIN-BIRTHYEAR-NULL    PIC S9(04)  COMP-5.
           03 FEE-TABLE-MAX-BIRTHYEAR-NULL    PIC S9(04)  COMP-5.
           03 FEE-TABLE-FEE-PERCENTAGE-NULL   PIC S9(04)  COMP-5.
