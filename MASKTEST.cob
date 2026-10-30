
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MASKTEST.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT LOG-FILE ASSIGN TO "masktest.log"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS LOG-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD LOG-FILE.
       01 LOG-RECORD PIC X(100).

       WORKING-STORAGE SECTION.

       01 ACCOUNT-ID       PIC X(20).
       01 MASKED-ID        PIC X(20).
       01 TRANS-STATUS     PIC X(10).
       01 OUTPUT-PATH      PIC X(20).
       01 DEBUG-FLAG       PIC X.
       01 RESULT-MSG       PIC X(30).
       01 LOG-STATUS       PIC XX.
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

           DISPLAY "ENABLE DEBUG? (Y/N):"
           ACCEPT DEBUG-FLAG

           MOVE FUNCTION UPPER-CASE(TRANS-STATUS)
               TO TRANS-STATUS
           MOVE FUNCTION UPPER-CASE(OUTPUT-PATH)
               TO OUTPUT-PATH
           MOVE FUNCTION UPPER-CASE(DEBUG-FLAG)
               TO DEBUG-FLAG

           MOVE FUNCTION LENGTH(
               FUNCTION TRIM(ACCOUNT-ID)
           ) TO ID-LENGTH

           PERFORM CHECK-VALIDATION
           PERFORM MASK-ACCOUNT-ID

           IF VALID-FLAG = 'N'
               MOVE 'VALIDATION ERROR' TO RESULT-MSG
           ELSE
               PERFORM CHECK-STATUS
           END-IF

           DISPLAY "MASKED ID: "
               MASKED-ID(1:ID-LENGTH)
           DISPLAY "OUTPUT RESULT: " RESULT-MSG

           IF DEBUG-FLAG = 'Y'
               PERFORM WRITE-DEBUG-LOG
           END-IF

           STOP RUN.

       CHECK-VALIDATION.
           MOVE 'Y' TO VALID-FLAG

           IF ID-LENGTH NOT = 10
               MOVE 'N' TO VALID-FLAG
           END-IF

           PERFORM VARYING I FROM 1 BY 1
               UNTIL I > ID-LENGTH
               IF ACCOUNT-ID(I:1) < '0'
                   OR ACCOUNT-ID(I:1) > '9'
                   MOVE 'N' TO VALID-FLAG
               END-IF
           END-PERFORM.

       MASK-ACCOUNT-ID.
           MOVE ALL '*' TO MASKED-ID

           IF ID-LENGTH > 4
               MOVE ACCOUNT-ID(ID-LENGTH - 3:4)
                   TO MASKED-ID(ID-LENGTH - 3:4)
           END-IF.

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

       WRITE-DEBUG-LOG.
           OPEN EXTEND LOG-FILE

           IF LOG-STATUS NOT = '00'
               OPEN OUTPUT LOG-FILE
           END-IF

           IF LOG-STATUS = '00'
               MOVE SPACES TO LOG-RECORD

               STRING
                   'MASKED ID: ' DELIMITED BY SIZE
                   FUNCTION TRIM(MASKED-ID) DELIMITED BY SIZE
                   ' RESULT: ' DELIMITED BY SIZE
                   FUNCTION TRIM(RESULT-MSG) DELIMITED BY SIZE
                   INTO LOG-RECORD
               END-STRING

               WRITE LOG-RECORD
               CLOSE LOG-FILE
           ELSE
               DISPLAY 'LOG FILE ERROR'
           END-IF.
