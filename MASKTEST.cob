
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MASKTEST.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01 ACCOUNT-ID       PIC X(20).
       01 MASKED-ID        PIC X(20).
       01 TRANS-STATUS     PIC X(10).
       01 OUTPUT-PATH      PIC X(20).
       01 RESULT-MSG       PIC X(30).
       01 ID-LENGTH        PIC 99.
       01 I                PIC 99.
       01 VALID-FLAG       PIC X VALUE 'Y'.

       PROCEDURE DIVISION.

       MAIN-PARA.
           DISPLAY "ENTER ACCOUNT ID: "
           ACCEPT ACCOUNT-ID

           DISPLAY "ENTER STATUS: "
           ACCEPT TRANS-STATUS

           DISPLAY "ENTER OUTPUT PATH:"
           DISPLAY "NORMAL / VALIDATION_ERROR / FILE_ERROR"
           ACCEPT OUTPUT-PATH

           MOVE FUNCTION LENGTH(
               FUNCTION TRIM(ACCOUNT-ID)
           ) TO ID-LENGTH

           MOVE ACCOUNT-ID TO MASKED-ID

           IF ID-LENGTH < 5
               MOVE 'N' TO VALID-FLAG
               MOVE ALL '*' TO MASKED-ID
           ELSE
               PERFORM VARYING I FROM 1 BY 1
                   UNTIL I > ID-LENGTH - 4
                   MOVE '*' TO MASKED-ID(I:1)
               END-PERFORM
           END-IF

           IF VALID-FLAG = 'N'
               MOVE 'VALIDATION ERROR' TO RESULT-MSG
           ELSE
               PERFORM CHECK-STATUS
           END-IF

           DISPLAY "MASKED ID: "
               MASKED-ID(1:ID-LENGTH)
           DISPLAY "OUTPUT RESULT: " RESULT-MSG

           STOP RUN.

       CHECK-STATUS.
           IF TRANS-STATUS NOT = 'SUCCESS'
              AND TRANS-STATUS NOT = 'PENDING'
              AND TRANS-STATUS NOT = 'FAILED'
               MOVE 'UNEXPECTED STATUS' TO RESULT-MSG
           ELSE
               EVALUATE OUTPUT-PATH
                   WHEN 'NORMAL'
                       MOVE 'NORMAL REPORT' TO RESULT-MSG
                   WHEN 'VALIDATION_ERROR'
                       MOVE 'VALIDATION ERROR' TO RESULT-MSG
                   WHEN 'FILE_ERROR'
                       MOVE 'FILE ERROR' TO RESULT-MSG
                   WHEN OTHER
                       MOVE 'INVALID OUTPUT PATH'
                           TO RESULT-MSG
               END-EVALUATE
           END-IF.
