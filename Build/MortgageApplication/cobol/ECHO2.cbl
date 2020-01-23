       IDENTIFICATION DIVISION.
       PROGRAM-ID. ECHO2.
      *****************************************************************
      *                                                               *
      *   Simple Prog for zUnit                                       *
      *                                                               *
      *****************************************************************
      /
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       LINKAGE SECTION.
       01 RCV-PARMS.
          05 IN-TEXT0   Pic X(05).
          05 OUT-TEXT0  Pic X(11).
          05 IN-TEXT1   Pic 9(05).
          05 OUT-TEXT1  Pic 9(11).
          05 IN-TEXT2   Pic A(05).
          05 OUT-TEXT2  Pic A(11).

       PROCEDURE DIVISION USING RCV-PARMS.
       MAIN.
           DISPLAY "Start zero 0"

           Initialize OUT-TEXT0.

           MOVE IN-TEXT0 TO OUT-TEXT0(1:5).
           MOVE SPACE TO OUT-TEXT0(6:1).
           MOVE IN-TEXT0 TO OUT-TEXT0(7:5).

           DISPLAY OUT-TEXT0

           DISPLAY "Start 1"

           Initialize OUT-TEXT1.

           MOVE IN-TEXT1 TO OUT-TEXT1(1:5).
           MOVE ZERO TO OUT-TEXT1(6:1).
           MOVE IN-TEXT1 TO OUT-TEXT1(7:5).

           DISPLAY OUT-TEXT1
           DISPLAY "Start two 2 "

           Initialize OUT-TEXT2.

           MOVE IN-TEXT2 TO OUT-TEXT2(1:5).
           MOVE SPACE TO OUT-TEXT2(6:1).
           MOVE IN-TEXT2 TO OUT-TEXT2(7:5).

           DISPLAY OUT-TEXT2
           DISPLAY "End"
           GOBACK.

       END PROGRAM ECHO2.