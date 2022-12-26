       IDENTIFICATION DIVISION.
       PROGRAM-ID. CBL01.
       AUTHOR. BRANDON PAXTON.
       DATE-COMPILED.
      **************************************************************
      * PURPOSE:  THIS PROGRAM WILL CREATE A SALES ANALYSIS REPORT
      * THE SORTED DATA SET WILL BE FURTHER DIVIDED BY CREATING
      *A REPORT FOR STORES THAT ARE BELOW QUOTA AND THOSE ABOVE
      * SORTED BY DISTRIC, SALESPERSON, STORE
      **************************************************************
       ENVIRONMENT DIVISION.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT STORE-REPORT
           ASSIGN TO SALSDATI.

           SELECT BELOW-QUOTA-REPORT
           ASSIGN TO BLWQTAO.

           SELECT MET-QUOTA-REPORT
           ASSIGN TO METQTAo.

           select sales-report
           ASSIGN TO SLSRPRT.

       DATA DIVISION.
       FILE SECTION.

       FD  STORE-REPORT
           RECORD CONTAINS 74 CHARACTERS.

       01  SALES-DATA.
           05  SD-DIST-NO          PIC X(4).
           05                      PIC X(5).
           05  SD-BRANCH-NO        PIC X(2).
           05  sd-sales-no         pic x(2).
           05                      pic x(27).
           05  SD-STORE-NAME       PIC X(15).
           05                      PIC X(12).
           05  SD-SALES-AMOUNT     PIC 9(5)V99.

       FD  BELOW-QUOTA-REPORT
           RECORD CONTAINS 30 CHARACTERS.

       01  BELOW-QUOTA.
            05  BQ-DISTRICT-NUM      PIC 9(4).
            05  BQ-BRANCH-NUM        PIC 9(2).
            05  BQ-SALES-NO          PIC 9(2).
            05  BQ-STORE-NAME        PIC X(15).
            05  BQ-SALES-AMOUNT      PIC 9(5)V99.

       FD  MET-QUOTA-REPORT
            RECORD CONTAINS 30 CHARACTERS.

       01  MET-QUOTA.
            05  MQ-DISTRICT-NUM      PIC 9(4).
            05  MQ-BRANCH-NUM        PIC 9(2).
            05  MQ-SALES-NO          PIC 9(2).
            05  MQ-STORE-NAME        PIC X(15).
            05  MQ-SALES-AMOUNT      PIC 9(5)V99.

       FD  SALES-REPORT
             RECORD CONTAINS 132 CHARACTERS.

       01  PRINT-REC        PIC X(132).

       WORKING-STORAGE SECTION.

       01  WORK-FIELDS.
             05 MORE-RECORDS      PIC X   VALUE 'Y'.
                  88 NO-RECORDS            VALUE 'N'.
             05 WS-QUOTA          PIC 9(4)v99 value 4500.
             05 WS-DIST-NO        PIC 9999.
             05 WS-BRANCH-NO      PIC 99.
             05 WS-SALES-NO       PIC 99.
             05 WS-FIRST-REC      PIC X  VALUE "Y".
                 88  NOT-FIRST         VALUE "N".

       01  PRINT-FIELDS.
             05  PAGE-COUNT       PIC 9(3)  VALUE ZERO.
             05  LINES-ON-PAGE    PIC 9(2)  VALUE 43.
             05  LINE-COUNT       PIC 9(2)  VALUE 99.
             05  SPACE-CONTROL    PIC 9.

       01  TOTAL-FIELDS.
            05  TOTAL-FINAL-AMT          PIC 9(13)V99.
             05  TOTAL-DISTRICT-AMT       PIC 9(12)V99.
             05  TOTAL-BRANCH-AMT         PIC 9(12)V99.
           05  TOTAL-PERSON-AMT         PIC 9(12)V99.
           05  TOTAL-STORES             PIC 999.
           05  TOTAL-STORES-BELOW  PIC 99.


       01  CURRENT-DATE-AND-TIME.
             05  CD-YEAR          PIC 9999.
             05  CD-MONTH         PIC 99.
             05  CD-DAY           PIC 99.
             05  CD-HOURS         PIC 99.
             05  CD-MINUTES       PIC 99.
             05                   PIC X(9).

       01  TSO-ID               PIC X(7)  VALUE 'KC03AF5'.
       01  HEADING-LINE-1.
             05                   PIC X(46) VALUE SPACES.
             05   PIC X(32)  VALUE 'EVERYTHING  U  NEED  SUPPLY  CO.'.



       01 HEADING-LINE-2.
            05                    PIC X(11)   VALUE SPACES.
            05  HL2-MONTH         PIC x(9).
            05                    PIC X(1)   VALUE SPACE.
            05  HL2-DAY           PIC Z9.
            05                    PIC X(3)   VALUE ',  '.
            05  HL2-YEAR          PIC 9(4).
            05                    PIC X(22)  VALUE SPACES.
            05      PIC X(22)  VALUE 'S A L E S  R E P O R T'.
            05                    PIC X(38) VALUE SPACES.
            05                    pic x(6) value 'PAGE  '.
            05  HL2-PAGE-NUMBER   PIC ZZ9.

       01 HEADING-LINE-3.
           05                    PIC X(17)   VALUE spaces.
           05                    PIC x(8)    VALUE 'DISTRICT'.
           05                    PIC X(3)   VALUE spaces.
           05                    PIC X(6)   VALUE  'BRANCH'.
           05                    PIC X(3)   VALUE spaces.
           05                    PIC X(11)   VALUE 'SALESPERSON'.
           05                    PIC X(7)   VALUE spaces.
           05                    PIC X(5)   VALUE 'STORE'.
           05                    PIC X(16)   VALUE spaces.
           05                    PIC X(12)   VALUE 'SALES AMOUNT'.

       01 HEADING-LINE-4.
            05                    PIC X(18)   VALUE spaces.
            05                    PIC x(6)    VALUE 'NUMBER'.
            05                    PIC X(4)   VALUE spaces.
            05                    PIC X(6)   VALUE  'NUMBER'.
           05                    PIC X(5)   VALUE spaces.
           05                    PIC X(6)   VALUE 'NUMBER'.
           05                    PIC X(10)   VALUE spaces.
           05                    PIC X(4)   VALUE 'NAME'.
           05                    PIC X(18)   VALUE spaces.
           05                    PIC X(12)   VALUE 'PER STORE'.


        01 DETAIL-LINE.
            05                    PIC X(19) VALUE SPACES.
            05  DL-DIST-NO        PIC ZZZZ.
            05                    PIC X(7) VALUE SPACE.
            05  DL-BRANCH-NO      PIC ZZ.
            05                    PIC X(9)  VALUE SPACE.
            05  DL-SALES-NO       PIC ZZ.
            05                    PIC X(7)  VALUE SPACE.
            05  DL-Store-name     PIC x(15).
            05                    PIC X(12).
            05  DL-SALES-AMOUNT   PIC ZZ,ZZ9.99.
            05          pic x(2) value space.
            05 DL-BELOW-QUOTA.
               10 DL-MESSAGE PIC X(16) VALUE "BELOW QUOTA BY: ".
               10 DL-BQ-SUM  PIC $$,$$9.99.

       01 SALES-PERSON-TOTALS-LINE.
            05                     PIC X(54) VALUE SPACES.
            05                  PIC X(12) VALUE "SALESPERSON ".
            05 SPTL-PERSON-NUM     PIC X(2).
            05                     PIC X(8) VALUE " TOTAL  ".
            05 SPTL-PERSON-TOTAL   PIC ZZZ,ZZ9.99.
            05                     PIC x(3) VALUE "  *".

       01 BRANCH-TOTALS-LINE.
            05                     PIC X(57) VALUE SPACES.
            05                     PIC X(7) VALUE "BRANCH ".
            05 BTL-BRANCH-NUM      PIC ZZ.
            05                     PIC X(8) VALUE " TOTAL  ".
            05 BTL-BRANCH-TOTAL    PIC Z,ZZZ,ZZ9.99.
            05                     PIC x(5) VALUE "  * *".

       01 DISTRICT-TOTALS-LINE.
              05                     PIC X(52) VALUE SPACES.
              05                     PIC X(9) VALUE "DISTRICT ".
              05 DTL-DISTRICT-NUM    PIC ZZZZ.
              05                     PIC X(7) VALUE " TOTAL ".
              05 DTL-DISTRICT-TOTAL  PIC $$$,$$$,$$9.99.
              05                     PIC X(7) VALUE "  * * *".

       01 FINAL-TOTAL-LINE.
              05                   PIC X(59) VALUE SPACES.
              05                   PIC X(12)
                              VALUE "FINAL TOTAL ".
              05 FTL-TOTAL         PIC $$$$,$$$,$$9.99.

       01  TOTALS-LINE-1.
             05                    PIC X(29) VALUE SPACES.
             05                    PIC X(14)
                  VALUE  'TOTAL STORES: '.
             05 TL-TOTAL-STORES    PIC Z,ZZ9.
             05                    PIC X(31)  VALUE SPACES.
             05                    PIC X(26)
                      VALUE "TOTAL STORES BELOW QUOTA: ".
             05 TL-STORES-BELOW    PIC Z,ZZ9.



       01  TOTALS-LINE-2.
            05                     PIC X(38) VALUE SPACES.
            05                     PIC X(45)
              VALUE "* * * * *    END    OF    REPORT    * * * * *".



       PROCEDURE DIVISION.
      ***************************************************************

      * 000-MAIN.
      *  - PERFORM 100-HSK
      *  - PERFORM 110-WAGE-ROUTINE UNTIL ALL RECORDS HAVE BEEN READ
      *  - PERFORM 120-EOJ
      *  - RETURN CONTROL BACK TO THE OS
      ***************************************************************
       000-MAIN.

              PERFORM 100-HSK.

              PERFORM UNTIL MORE-RECORDS = 'N'
                  READ STORE-REPORT
                      AT END
                        PERFORM 140-SALES-BREAK
                        PERFORM 150-BRANCH-BREAK
                        PERFORM 160-DIST-BREAK
                        SET NO-RECORDS TO TRUE
                      NOT AT END
                        PERFORM 110-SALES-ROUTINE
                  END-READ
              END-PERFORM.

              PERFORM 120-EOJ.

              STOP RUN.

      ***************************************************************
      * 100-HSK.
      *  - FILES ARE OPENED TO BEGIN PROCESSING
      **************************************************************
       100-HSK.

           OPEN INPUT STORE-REPORT
           OUTPUT MET-QUOTA-REPORT
                  BELOW-QUOTA-REPORT
                  SALES-REPORT.

           MOVE FUNCTION CURRENT-DATE TO CURRENT-DATE-AND-TIME
           MOVE CD-MONTH             TO HL2-MONTH.
           MOVE CD-DAY               TO HL2-DAY.
           MOVE CD-YEAR              TO HL2-YEAR.

           EVALUATE CD-MONTH
              WHEN 01        MOVE "JANUARY"  TO HL2-MONTH
              WHEN 02        MOVE "FEBUARY"  TO HL2-MONTH
              WHEN 03        MOVE "MARCH"    TO HL2-MONTH
              WHEN 04        MOVE "APRIL"    TO HL2-MONTH
              WHEN 05        MOVE "MAY"      TO HL2-MONTH
              WHEN 06        MOVE "JUNE"     TO HL2-MONTH
              WHEN 07        MOVE "JULY"     TO HL2-MONTH
              WHEN 08        MOVE "AUGUST"   TO HL2-MONTH
              WHEN 09        MOVE "SEPTEMBER"  TO HL2-MONTH
              WHEN 10        MOVE "OCTOBER"  TO HL2-MONTH
              WHEN 11        MOVE "NOVEMBER"  TO HL2-MONTH
              WHEN 12        MOVE "DECEMBER"  TO HL2-MONTH
           END-EVALUATE.
      ***************************************************************
      * 110-SALES-ROUTINE.
      *  - DATA IS MOVED FROM THE INPUT BUFFER TO THE OUTPUT BUFFER
      *  - THE FIRST RECORD IS PRINTED
      *         AND THE TEMP FLAGS ARE SET
      *  - THE RECORDS ARE CHECKED TO SEE IF THEY ARE THE SAME AS
      *  - THE LAST, IF NOT FLAGS ARE PLACED AND SENT TO 140
      ***************************************************************
       110-SALES-ROUTINE.

                IF WS-FIRST-REC = "Y"
                 MOVE SD-DIST-NO TO WS-DIST-NO
                 MOVE SD-BRANCH-NO TO WS-BRANCH-NO
                 MOVE SD-SALES-NO TO  WS-SALES-NO
                 MOVE "N" TO WS-FIRST-REC
                END-IF.
                MOVE 1 TO SPACE-CONTROL.

                IF sd-dist-no not = ws-dist-no
                 perform 140-sales-break
                 perform 150-branch-break
                 perform 160-dist-break
                 PERFORM 200-PRINT-HEADING-LINES
               else
               IF SD-BRANCH-NO NOT = WS-BRANCH-NO
                   PERFORM 140-SALES-BREAK
                   PERFORM 150-BRANCH-BREAK
                   MOVE 3 TO SPACE-CONTROL
               else
                  if sd-sales-no not = ws-sales-no
                    perform 140-sales-break
                    MOVE 3 TO SPACE-CONTROL
                  end-if
               end-if
              end-if.

              IF LINE-COUNT >= LINES-ON-PAGE
                PERFORM 200-PRINT-HEADING-LINES
              END-IF.


              perform 130-CREATE-DETAIL-LINE.

      ***************************************************************
      * 130-CREATE-DETAIL-LINE.
      *  - ONCE ANY TOTALS LINES HAVE PRINTED THIS WILL SET THE
      *  - RECORDS TO THE CORRECT FILES AND PRINT THEM OFF
      *  -
      *  -
      ***************************************************************
       130-CREATE-DETAIL-LINE.

            MOVE SD-DIST-NO TO BQ-DISTRICT-NUM
                               MQ-DISTRICT-NUM.

            MOVE SD-BRANCH-NO TO BQ-BRANCH-NUM
                                 MQ-BRANCH-NUM.

            MOVE SD-SALES-NO TO BQ-SALES-NO
                                MQ-SALES-NO.

            MOVE SD-STORE-NAME TO DL-Store-name
                                  BQ-STORE-NAME
                                  MQ-STORE-NAME.

            MOVE SD-SALES-AMOUNT TO DL-SALES-AMOUNT
                                  BQ-SALES-AMOUNT
                                  MQ-SALES-AMOUNT.

              IF SD-SALES-AMOUNT IS LESS THAN WS-QUOTA
                  COMPUTE DL-BQ-SUM = WS-QUOTA - SD-SALES-AMOUNT
                  move "BELOW QUOTA BY: " TO DL-MESSAGE
                  ADD 1 TO TOTAL-STORES-BELOW
                  WRITE BELOW-QUOTA
              ELSE
                  MOVE SPACES TO DL-BELOW-QUOTA
                  WRITE MET-QUOTA
              END-IF.

              MOVE DETAIL-LINE  TO PRINT-REC.
              WRITE PRINT-REC AFTER SPACE-CONTROL
              ADD SPACE-CONTROL TO LINE-COUNT.
              ADD 1 TO TOTAL-STORES.
              ADD SD-SALES-AMOUNT TO TOTAL-PERSON-AMT.
              MOVE SPACES TO DETAIL-LINE.
              MOVE 2 TO SPACE-CONTROL.


      ***************************************************************

       140-sales-break.

            MOVE 2 TO SPACE-CONTROL.
            MOVE WS-SALES-NO TO SPTL-PERSON-NUM
            MOVE TOTAL-PERSON-AMT TO SPTL-PERSON-TOTAL
            ADD  TOTAL-PERSON-AMT TO TOTAL-BRANCH-AMT
            MOVE 0 TO TOTAL-PERSON-AMT
            MOVE SALES-PERSON-TOTALS-LINE TO PRINT-REC
            WRITE PRINT-REC AFTER SPACE-CONTROL
            ADD SPACE-CONTROL TO LINE-COUNT
            MOVE SD-SALES-NO TO DL-SALES-NO, ws-sales-no.

      ***************************************************************
       150-BRANCH-break.
            MOVE WS-BRANCH-NO TO BTL-BRANCH-NUM
            MOVE TOTAL-BRANCH-AMT TO BTL-BRANCH-TOTAL
            ADD TOTAL-BRANCH-AMT  TO TOTAL-DISTRICT-AMT
            MOVE 0 TO TOTAL-BRANCH-AMT
            MOVE BRANCH-TOTALS-LINE TO PRINT-REC
            WRITE PRINT-REC AFTER SPACE-CONTROL
            ADD SPACE-CONTROL TO LINE-COUNT
            MOVE SD-BRANCH-NO TO DL-BRANCH-NO, WS-BRANCH-NO.

      ***************************************************************
       160-DIST-break.
            MOVE WS-DIST-NO TO DTL-DISTRICT-NUM
            MOVE TOTAL-DISTRICT-AMT TO DTL-DISTRICT-TOTAL
            ADD TOTAL-DISTRICT-AMT  TO TOTAL-FINAL-AMT
            MOVE 0 TO TOTAL-DISTRICT-AMT
            MOVE DISTRICT-TOTALS-LINE TO PRINT-REC
            WRITE PRINT-REC AFTER SPACE-CONTROL
            MOVE SD-DIST-NO TO DL-DIST-NO, ws-dist-no.

      **************************************************************
      * 200-HEADINGS
      * PRINT HEADING LINES.
      * PAGE COUNT IS INCREMENTED
      * HEADING LINES ARE MOVED AND PRINTED
      * LINE COUNT IS RESET AND SPACING INITIALIZED
      ***************************************************************
       200-PRINT-HEADING-LINES.

            ADD 1 TO PAGE-COUNT.
            MOVE PAGE-COUNT  TO  hL2-PAGE-NUMBER
            MOVE TSO-ID TO PRINT-REC.
            WRITE PRINT-REC
                 AFTER ADVANCING PAGE.
            MOVE HEADING-LINE-1 TO PRINT-REC.
            WRITE PRINT-REC
                 AFTER ADVANCING 2 LINES.
            MOVE HEADING-LINE-2 TO PRINT-REC.
            WRITE PRINT-REC
                 AFTER ADVANCING 3 LINES.
            MOVE HEADING-LINE-3 TO PRINT-REC.
            WRITE PRINT-REC
                 AFTER ADVANCING 3 LINES.
            MOVE HEADING-LINE-4 TO PRINT-REC.
            WRITE PRINT-REC
                 AFTER ADVANCING 1 LINES.


            MOVE ZERO TO LINE-COUNT.
            MOVE 2 TO SPACE-CONTROL.
            MOVE SD-DIST-NO TO dl-dist-no
            MOVE SD-BRANCH-NO TO dl-bRANCH-No
            MOVE SD-SALES-NO TO dl-SALES-NO.
      ***************************************************************
      * 120-EOJ.
      *  - FILES ARE CLOSED
      ***************************************************************
       120-EOJ.

      * DONT FORGET TO ADD THE MOVES AND THE WRITES FOR THE END
      * OF JOB TOTALS


            MOVE TOTAL-FINAL-AMT TO FTL-TOTAL.
            MOVE FINAL-TOTAL-LINE TO PRINT-REC
                    WRITE PRINT-REC AFTER ADVANCING 3 LINES.

             MOVE TOTAL-STORES TO TL-TOTAL-STORES.
             MOVE TOTAL-STORES-BELOW TO TL-STORES-BELOW.
             MOVE TOTALS-LINE-1 TO PRINT-REC
                    WRITE PRINT-REC AFTER ADVANCING 2 LINES.

             MOVE TOTALS-LINE-2 TO PRINT-REC
                    WRITE PRINT-REC AFTER ADVANCING 2 LINES.


            CLOSE STORE-REPORT
                  MET-QUOTA-REPORT
                  BELOW-QUOTA-REPORT
                  SALES-REPORT.


