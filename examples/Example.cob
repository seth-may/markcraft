       IDENTIFICATION DIVISION.
       PROGRAM-ID. MARKCRAFT-LEDGER.
       AUTHOR. MARKCRAFT-TEAM.
      *> Modern COBOL: Financial ledger with indexed file I/O

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT LEDGER-FILE
               ASSIGN TO "ledger.dat"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS TRANS-ID
               ALTERNATE KEY IS ACCOUNT-NUM WITH DUPLICATES
               FILE STATUS IS WS-FILE-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD LEDGER-FILE.
       01 LEDGER-RECORD.
           05 TRANS-ID          PIC X(12).
           05 ACCOUNT-NUM       PIC 9(10).
           05 TRANS-DATE        PIC 9(8).
           05 TRANS-TYPE        PIC X(1).
               88 IS-DEBIT      VALUE "D".
               88 IS-CREDIT     VALUE "C".
           05 AMOUNT            PIC 9(10)V99.
           05 DESCRIPTION       PIC X(50).
           05 BALANCE-AFTER     PIC S9(12)V99.

       WORKING-STORAGE SECTION.
       01 WS-FILE-STATUS        PIC XX.
           88 FILE-OK           VALUE "00".
           88 FILE-NOT-FOUND    VALUE "23".
           88 FILE-DUP-KEY      VALUE "22".
       01 WS-CURRENT-DATE       PIC 9(8).
       01 WS-TOTAL-DEBITS       PIC S9(14)V99 VALUE ZERO.
       01 WS-TOTAL-CREDITS      PIC S9(14)V99 VALUE ZERO.
       01 WS-NET-BALANCE        PIC S9(14)V99 VALUE ZERO.
       01 WS-TRANS-COUNT        PIC 9(8) VALUE ZERO.
       01 WS-DISPLAY-AMT        PIC Z(10)9.99.
       01 WS-DISPLAY-BAL        PIC -(12)9.99.
       01 WS-EOF                PIC X VALUE 'N'.
           88 END-OF-FILE       VALUE 'Y'.

       01 WS-REPORT-LINE.
           05 RPT-ID            PIC X(12).
           05 FILLER            PIC X(2) VALUE SPACES.
           05 RPT-DATE          PIC 9999/99/99.
           05 FILLER            PIC X(2) VALUE SPACES.
           05 RPT-TYPE          PIC X(6).
           05 FILLER            PIC X(2) VALUE SPACES.
           05 RPT-AMOUNT        PIC Z(10)9.99.
           05 FILLER            PIC X(2) VALUE SPACES.
           05 RPT-DESC          PIC X(30).

       PROCEDURE DIVISION.
       MAIN-PROGRAM.
           PERFORM INITIALIZE-SYSTEM
           PERFORM PROCESS-TRANSACTIONS
           PERFORM GENERATE-REPORT
           PERFORM CLEANUP
           STOP RUN.

       INITIALIZE-SYSTEM.
           ACCEPT WS-CURRENT-DATE FROM DATE YYYYMMDD
           OPEN I-O LEDGER-FILE
           IF NOT FILE-OK
               OPEN OUTPUT LEDGER-FILE
               IF NOT FILE-OK
                   DISPLAY "ERROR: Cannot open ledger file"
                   STOP RUN
               END-IF
           END-IF
           DISPLAY "=== MarkCraft Ledger System ==="
           DISPLAY "Date: " WS-CURRENT-DATE.

       PROCESS-TRANSACTIONS.
           PERFORM ADD-TRANSACTION
           PERFORM ADD-TRANSACTION
           DISPLAY "Processed " WS-TRANS-COUNT " transactions".

       ADD-TRANSACTION.
           ADD 1 TO WS-TRANS-COUNT
           MOVE WS-TRANS-COUNT TO TRANS-ID
           MOVE 1234567890 TO ACCOUNT-NUM
           MOVE WS-CURRENT-DATE TO TRANS-DATE
           SET IS-CREDIT TO TRUE
           MOVE 1500.00 TO AMOUNT
           MOVE "MarkCraft subscription" TO DESCRIPTION
           COMPUTE BALANCE-AFTER = WS-NET-BALANCE + AMOUNT
           MOVE BALANCE-AFTER TO WS-NET-BALANCE
           WRITE LEDGER-RECORD
           IF FILE-OK
               ADD AMOUNT TO WS-TOTAL-CREDITS
           END-IF.

       GENERATE-REPORT.
           DISPLAY SPACES
           DISPLAY "=== Transaction Report ==="
           MOVE LOW-VALUES TO TRANS-ID
           START LEDGER-FILE KEY >= TRANS-ID
           PERFORM READ-NEXT UNTIL END-OF-FILE
           DISPLAY "=========================="
           MOVE WS-TOTAL-CREDITS TO WS-DISPLAY-AMT
           DISPLAY "Total Credits: " WS-DISPLAY-AMT
           MOVE WS-NET-BALANCE TO WS-DISPLAY-BAL
           DISPLAY "Net Balance:   " WS-DISPLAY-BAL.

       READ-NEXT.
           READ LEDGER-FILE NEXT RECORD
               AT END SET END-OF-FILE TO TRUE
               NOT AT END PERFORM DISPLAY-RECORD
           END-READ.

       DISPLAY-RECORD.
           MOVE TRANS-ID TO RPT-ID
           MOVE TRANS-DATE TO RPT-DATE
           IF IS-CREDIT MOVE "CREDIT" TO RPT-TYPE
           ELSE MOVE "DEBIT" TO RPT-TYPE END-IF
           MOVE AMOUNT TO RPT-AMOUNT
           MOVE DESCRIPTION(1:30) TO RPT-DESC
           DISPLAY WS-REPORT-LINE.

       CLEANUP.
           CLOSE LEDGER-FILE.
