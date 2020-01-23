      PROCESS DLL,NODYNAM,EXPORTALL,TEST(NOSEP),NOCICS
      *+---------------------------------------------------------------+
      *| Product: IBM Rational Developer for z Systems                 |
      *| Component: IBM z/OS Automated Unit Testing Framework (zUnit)  |
      *|   for Enterprise COBOL and PL/I                               |
      *| Program: Enterprise COBOL zUnit Test Case                     |
      *| Date Generated: 11/25/2019 14:55                              |
      *| ID: 310a2425-fd93-4b7e-8c06-b2d260f78e0e                      |
      *+---------------------------------------------------------------+
      *+---------------------------------------------------------------+
      *| TECHO10                                                       |
      *|     This program is immediately called by the IBM             |
      *|     z/OS Automated Unit Testing Framework (zUnit)             |
      *|     Test Runner to allow for initialization of the            |
      *|     Test Case. Upon return from this program, the             |
      *|     Test Runner will attempt to call the ADDTESTS             |
      *|     program.                                                  |
      *|                                                               |
      *| @param TEST-CASE-PTR (input),                                 |
      *|     A pointer-by-value to an area maintained by the           |
      *|     zUnit Test Runner that identifies the Test Case           |
      *|     and associated resources.                                 |
      *+---------------------------------------------------------------+
       IDENTIFICATION DIVISION.
       PROGRAM-ID. 'TECHO10'.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       1 CBLTESTC-ID PIC X(36)
            VALUE '310a2425-fd93-4b7e-8c06-b2d260f78e0e'.
       1 CBLTESTC-NAME PIC X(7)
            VALUE 'TECHO10'.
       1 CBLTESTC-ID-LEN PIC S9(9) COMP-5.
       1 CBLTESTC-NAME-LEN PIC S9(9) COMP-5.
       1 CBLTESTC-SETUP    FUNCTION-POINTER.
       1 CBLTESTC-TEARDOWN FUNCTION-POINTER.
       1 CBLTESTC-ADDTESTS FUNCTION-POINTER.
       LINKAGE SECTION.
       1 TEST-CASE-PTR POINTER.
       PROCEDURE DIVISION USING BY VALUE TEST-CASE-PTR.
      *    this program does not require editing.
           MOVE LENGTH OF CBLTESTC-ID TO CBLTESTC-ID-LEN
           MOVE LENGTH OF CBLTESTC-NAME TO CBLTESTC-NAME-LEN
           SET CBLTESTC-ADDTESTS TO ENTRY 'ADDTESTS'
           SET CBLTESTC-SETUP    TO ENTRY 'SETUP'
           SET CBLTESTC-TEARDOWN TO ENTRY 'TEARDOWN'
           CALL 'AZUTCINI' USING
               BY VALUE     TEST-CASE-PTR
               BY REFERENCE CBLTESTC-ID
               BY VALUE     CBLTESTC-ID-LEN
               BY REFERENCE CBLTESTC-NAME
               BY VALUE     CBLTESTC-NAME-LEN
               BY VALUE     CBLTESTC-ADDTESTS
               BY VALUE     CBLTESTC-SETUP
               BY VALUE     CBLTESTC-TEARDOWN
           .
        END PROGRAM 'TECHO10'.
      *+---------------------------------------------------------------+
      *| ADDTESTS                                                      |
      *|     This program is called by the zUnit Test Runner           |
      *|     to allow for adding Tests to the Test Case. Upon          |
      *|     return from this program, the Test Runner will            |
      *|     call the added Tests, surrounding each with calls         |
      *|     to the SETUP and TEARDOWN programs.                       |
      *|                                                               |
      *| @param TEST-CASE-PTR (input),                                 |
      *|     A pointer-by-value to an area maintained by the           |
      *|     zUnit Test Runner that identifies the Test Case           |
      *|     and associated resources.                                 |
      *+---------------------------------------------------------------+
       IDENTIFICATION DIVISION.
       PROGRAM-ID. 'ADDTESTS'.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       1 TEST-ENTRY     FUNCTION-POINTER.
       1 TEST-NAME      PIC X(254).
       1 TEST-NAME-LEN  PIC S9(9) COMP-5.
       LINKAGE SECTION.
       1 TEST-CASE-PTR POINTER.
       PROCEDURE DIVISION USING BY VALUE TEST-CASE-PTR.
      *    add tests to the test case.
           SET TEST-ENTRY TO ENTRY 'TEST1'
           MOVE 'TEST1' TO TEST-NAME
           MOVE 5 TO TEST-NAME-LEN
           CALL 'AZUTCADD' USING
               BY VALUE     TEST-CASE-PTR
               BY VALUE     TEST-ENTRY
               BY REFERENCE TEST-NAME
               BY VALUE     TEST-NAME-LEN
           .
       END PROGRAM 'ADDTESTS'.
      *+---------------------------------------------------------------+
      *| SETUP                                                         |
      *|     This program is invoked by the zUnit Test Runner          |
      *|     prior to each Test to allow for allocation of             |
      *|     resources (e.g., memory, connections) that are            |
      *|     required to create the Test Fixture.                      |
      *|                                                               |
      *| @param TEST-CASE-PTR (input),                                 |
      *|     A pointer-by-value to an area maintained by the           |
      *|     zUnit Test Runner that identifies the Test Case           |
      *|     and associated resources.                                 |
      *|                                                               |
      *| @param TEST-FIXTURE-PTR (output),                             |
      *|     A pointer-by-reference in which to store the address      |
      *|     of a user-defined structure that represents the Test      |
      *|     Fixture. References to all allocated resources should     |
      *|     be maintained in this structure so that they may be       |
      *|     accessed in the respective Test program, and released     |
      *|     in the TEARDOWN program.                                  |
      *|                                                               |
      *| @param TEST-NAME-PTR (input),                                 |
      *|     A pointer-by-value to an area containing the name         |
      *|     of the Test for which a Test Fixture should be            |
      *|     allocated.                                                |
      *|                                                               |
      *| @param TEST-NAME-LEN (input),                                 |
      *|     A integer-by-value that specifies the length in           |
      *|     bytes of the value contained in parameter                 |
      *|     TEST-NAME-PTR.                                            |
      *+---------------------------------------------------------------+
       IDENTIFICATION DIVISION.
       PROGRAM-ID. 'SETUP'.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       1 CEEGTST-SIZE PIC S9(9) COMP-5.
       1 CEEGTST-HEAP PIC S9(9) COMP-5.
       1 PARM-SIZE    PIC S9(9) COMP-5.
       1 CURRENT-TEST-FIXTURE-PTR POINTER.
       1 CURRENT-TEST-FIXTURE-PTR-VALUE REDEFINES
           CURRENT-TEST-FIXTURE-PTR PIC S9(9) COMP-5.
       1 NEXT-TEST-FIXTURE-PTR POINTER.
       1 NEXT-TEST-FIXTURE-PTR-VALUE REDEFINES
           NEXT-TEST-FIXTURE-PTR  PIC S9(9) COMP-5.
       1 FAIL-MESSAGE-TXT PIC X(254).
       1 FAIL-MESSAGE-LEN PIC S9(9) COMP-5.
       1 AZ-TEST-INPUT-DATA-VALUE.
          3 ZUT00000000.
            5 PIC X(5) DISPLAY VALUE 'hell0'.
          3 ZUT00000001.
            5 PIC X(5) DISPLAY VALUE 'hello'.
       1 AZ-TEST-NAME-FOR-SUB   PIC X(254) EXTERNAL.
       1 AZ-TEST-NAME-LEN  PIC S9(9) COMP-5 EXTERNAL.
       1 AZ-TEST-CASE-PTR  POINTER EXTERNAL.
       1 AZ-TEST-CASE-PTR-VALUE REDEFINES
           AZ-TEST-CASE-PTR  PIC 9(9) COMP-5.
       LINKAGE SECTION.
       1 RCV-PARMS.
         5 IN-TEXT0 Pic X(05).
         5 OUT-TEXT0 Pic X(11).
         5 IN-TEXT1 Pic 9(05).
         5 OUT-TEXT1 Pic 9(11).
         5 IN-TEXT2 Pic A(05).
         5 OUT-TEXT2 Pic A(11).
       1 TEST-CASE-PTR    POINTER.
       1 TEST-CASE-PTR-VALUE REDEFINES
           TEST-CASE-PTR  PIC 9(9) COMP-5.
       1 TEST-FIXTURE-PTR POINTER.
       1 TEST-NAME-PTR    POINTER.
       1 TEST-NAME-LEN    PIC S9(9) COMP-5.
       1 TEST-NAME        PIC X(254).
       PROCEDURE DIVISION USING BY VALUE TEST-CASE-PTR
                          BY REFERENCE TEST-FIXTURE-PTR
                          BY VALUE TEST-NAME-PTR
                          BY VALUE TEST-NAME-LEN.
           SET ADDRESS OF TEST-NAME TO TEST-NAME-PTR
           MOVE TEST-NAME(1:TEST-NAME-LEN) TO
              AZ-TEST-NAME-FOR-SUB(1:TEST-NAME-LEN)
           MOVE TEST-NAME-LEN TO AZ-TEST-NAME-LEN
           EVALUATE TEST-NAME(1:TEST-NAME-LEN)
              WHEN 'TEST1'
                DISPLAY 'SETUP (' TEST-NAME(1:TEST-NAME-LEN) ')'
      *       setup for test TEST1
                PERFORM ALLOCATE-PARM
      *       initialize parameter
                PERFORM INITIALIZE-PARM
      *       set input data to parameter
                MOVE ZUT00000000 TO IN-TEXT0 OF RCV-PARMS
                MOVE 12345 TO IN-TEXT1 OF RCV-PARMS
                MOVE ZUT00000001 TO IN-TEXT2 OF RCV-PARMS
                MOVE TEST-CASE-PTR-VALUE TO AZ-TEST-CASE-PTR-VALUE
           END-EVALUATE
           GOBACK
           .
        ALLOCATE-PARM.
      *    allocate an instance of parameter structure
           INITIALIZE CEEGTST-HEAP CEEGTST-SIZE
      *    get a parameter size
           INITIALIZE PARM-SIZE
      *    LENGTH OF RCV-PARMS: 48
           IF 48 > PARM-SIZE
             MOVE 48 TO PARM-SIZE
           END-IF
           ADD PARM-SIZE TO CEEGTST-SIZE
      *    get heap storage
           CALL 'CEEGTST' USING CEEGTST-HEAP CEEGTST-SIZE
                TEST-FIXTURE-PTR OMITTED
           SET NEXT-TEST-FIXTURE-PTR TO TEST-FIXTURE-PTR
      *    set address of a parameter
           SET ADDRESS OF RCV-PARMS TO NEXT-TEST-FIXTURE-PTR
           EXIT.
        INITIALIZE-PARM.
           INITIALIZE RCV-PARMS
           EXIT.
       END PROGRAM 'SETUP'.
      *+---------------------------------------------------------------+
      *| TEARDOWN                                                      |
      *|     This program is invoked by the zUnit Test Runner          |
      *|     after each Test to allow for releasing resources          |
      *|     (e.g., memory, connection) which were allocated           |
      *|     during creation of the Test Fixture in the SETUP          |
      *|     program.                                                  |
      *|                                                               |
      *| @param TEST-CASE-PTR (input),                                 |
      *|     A pointer-by-value to an area maintained by the           |
      *|     zUnit Test Runner that identifies the Test Case           |
      *|     and associated resources.                                 |
      *|                                                               |
      *| @param TEST-FIXTURE-PTR (input),                              |
      *|     A pointer-by-value to a user-defined structure,           |
      *|     established previously in the SETUP program, that         |
      *|     represents the Test Fixture.                              |
      *|                                                               |
      *| @param TEST-NAME-PTR (input),                                 |
      *|     A pointer-by-value to an area containing the name         |
      *|     of the Test for which a Test Fixture should be            |
      *|     allocated.                                                |
      *|                                                               |
      *| @param TEST-NAME-LEN (input),                                 |
      *|     A integer-by-value that specifies the length in           |
      *|     bytes of the value contained in parameter                 |
      *|     TEST-NAME-PTR.                                            |
      *+---------------------------------------------------------------+
       IDENTIFICATION DIVISION.
       PROGRAM-ID. 'TEARDOWN'.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       1 FAIL-MESSAGE-TXT PIC X(254).
       1 FAIL-MESSAGE-LEN PIC S9(9) COMP-5.
       LINKAGE SECTION.
       1 TEST-CASE-PTR    POINTER.
       1 TEST-FIXTURE-PTR POINTER.
       1 TEST-NAME-PTR    POINTER.
       1 TEST-NAME-LEN    PIC S9(9) COMP-5.
       1 TEST-NAME        PIC X(254).
       PROCEDURE DIVISION USING BY VALUE TEST-CASE-PTR
                          BY VALUE TEST-FIXTURE-PTR
                          BY VALUE TEST-NAME-PTR
                          BY VALUE TEST-NAME-LEN.
           SET ADDRESS OF TEST-NAME TO TEST-NAME-PTR
           EVALUATE TEST-NAME(1:TEST-NAME-LEN)
              WHEN 'TEST1'
      *         free test fixture for 'TEST1'
                CALL 'CEEFRST' USING TEST-FIXTURE-PTR OMITTED
                DISPLAY 'TEARDOWN (' TEST-NAME(1:TEST-NAME-LEN) ')'
           END-EVALUATE
           .
       END PROGRAM 'TEARDOWN'.
      *+---------------------------------------------------------------+
      *| TEST1                                                         |
      *|     A Test (supply more detail).                              |
      *|                                                               |
      *| @param TEST-CASE-PTR (input),                                 |
      *|     A pointer-by-value to an area maintained by the           |
      *|     zUnit Test Runner that identifies the Test Case           |
      *|     and associated resources.                                 |
      *|                                                               |
      *| @param TEST-FIXTURE-PTR (input),                              |
      *|     A pointer-by-value to a user-defined structure,           |
      *|     established previously in the SETUP program, that         |
      *|     represents the Test Fixture.                              |
      *|                                                               |
      *| @param TEST-NAME-PTR (input),                                 |
      *|     A pointer-by-value to an area containing the name         |
      *|     of the Test for which a Test Fixture should be            |
      *|     allocated.                                                |
      *|                                                               |
      *| @param TEST-NAME-LEN (input),                                 |
      *|     A integer-by-value that specifies the length in           |
      *|     bytes of the value contained in parameter                 |
      *|     TEST-NAME-PTR.                                            |
      *+---------------------------------------------------------------+
       IDENTIFICATION DIVISION.
       PROGRAM-ID. 'TEST1'.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       1 FAIL-MESSAGE-TXT PIC X(254).
       1 FAIL-MESSAGE-LEN PIC S9(9) COMP-5.
       1 PARM-SIZE        PIC S9(9) COMP-5.
       1 AZ-COMPARE EXTERNAL.
         3 AZ-COMPARE-ITEM-NAME-PTR POINTER.
         3 AZ-COMPARE-ITEM-NAME-LEN PIC S9(9) COMP-5.
         3 AZ-COMPARE-ITEM-VALUE-PTR POINTER.
         3 AZ-COMPARE-ITEM-VALUE-LEN PIC S9(9) COMP-5.
         3 AZ-COMPARE-ITEM-EXP-VALUE-PTR POINTER.
         3 AZ-COMPARE-ITEM-EXP-VALUE-LEN PIC S9(9) COMP-5.
       1 AZ-TEST-EXPECTED-DATA-VALUE.
          3 ZUT00000002.
            5 PIC X(11) DISPLAY VALUE 'hell0 hell0'.
          3 ZUT00000003.
            5 PIC X(11) DISPLAY VALUE 'hello hello'.
       LOCAL-STORAGE SECTION.
       1 AZ-COMPARE-ITEM-NAMES.
         3 ZUT00000004.
            5 PIC X(20) DISPLAY VALUE 'OUT-TEXT0 OF RCV-PAR'.
            5 PIC X(2) DISPLAY VALUE 'MS'.
         3 ZUT00000006.
            5 PIC X(20) DISPLAY VALUE 'OUT-TEXT1 OF RCV-PAR'.
            5 PIC X(2) DISPLAY VALUE 'MS'.
         3 ZUT00000008.
            5 PIC X(20) DISPLAY VALUE 'OUT-TEXT2 OF RCV-PAR'.
            5 PIC X(2) DISPLAY VALUE 'MS'.
       1 AZ-COMPARE-WORK-ITEMS.
          3 ZUT00000005 PIC X(11) OCCURS 2.
          3 ZUT00000007 PIC 9(11) OCCURS 2.
          3 ZUT00000009 PIC X(11) OCCURS 2.
       1 AZ-CONVERT.
         3 AZ-CONVERT-HEXIN  PIC X(1).
         3 AZ-CONVERT-HEXVAL PIC X(2).
         3 AZ-HEXSTR PIC X(16) VALUE "0123456789ABCDEF".
         3 AZ-DEC  PIC S9(4) COMP VALUE 0.
         3 FILLER REDEFINES AZ-DEC.
           5 FILLER PIC X.
           5 AZ-DECBYTE PIC X.
         3 AZ-I PIC S9(8) COMP.
         3 AZ-J PIC S9(8) COMP.
         3 AZ-Q PIC S9(8) COMP.
         3 AZ-R PIC S9(8) COMP.
         3 AZ-Q1 PIC S9(8) COMP.
         3 AZ-R1 PIC S9(8) COMP.
       LINKAGE SECTION.
       1 RCV-PARMS.
         5 IN-TEXT0 Pic X(05).
         5 OUT-TEXT0 Pic X(11).
         5 IN-TEXT1 Pic 9(05).
         5 OUT-TEXT1 Pic 9(11).
         5 IN-TEXT2 Pic A(05).
         5 OUT-TEXT2 Pic A(11).
       1 TEST-CASE-PTR    POINTER.
       1 TEST-FIXTURE-PTR POINTER.
       1 TEST-FIXTURE-PTR-VALUE REDEFINES
           TEST-FIXTURE-PTR  PIC S9(9) COMP-5.
       1 TEST-NAME-PTR    POINTER.
       1 TEST-NAME-LEN    PIC S9(9) COMP-5.
       1 TEST-NAME        PIC X(254).
       PROCEDURE DIVISION USING BY VALUE TEST-CASE-PTR
                          BY VALUE TEST-FIXTURE-PTR
                          BY VALUE TEST-NAME-PTR
                          BY VALUE TEST-NAME-LEN.
       MAIN SECTION.
           SET ADDRESS OF TEST-NAME TO TEST-NAME-PTR
      *    display test name on entry
           DISPLAY TEST-NAME(1:TEST-NAME-LEN) ' Started...'
           INITIALIZE AZ-COMPARE-WORK-ITEMS
      *    establish addressability to test fixture
      *    set address of a parameter
           SET ADDRESS OF RCV-PARMS TO TEST-FIXTURE-PTR
      *    call test program
           DISPLAY 'CALL ECHO2'
           CALL 'ECHO2'
           USING RCV-PARMS
           .
           IF OUT-TEXT0 OF RCV-PARMS = ZUT00000002 THEN
             CONTINUE
           ELSE
             MOVE OUT-TEXT0 OF RCV-PARMS TO ZUT00000005(1)
             MOVE ZUT00000002 TO ZUT00000005(2)
             SET AZ-COMPARE-ITEM-NAME-PTR TO ADDRESS OF ZUT00000004
             MOVE LENGTH OF ZUT00000004 TO AZ-COMPARE-ITEM-NAME-LEN
             SET AZ-COMPARE-ITEM-VALUE-PTR TO ADDRESS OF ZUT00000005(1)
             MOVE 11 TO AZ-COMPARE-ITEM-VALUE-LEN
             SET AZ-COMPARE-ITEM-EXP-VALUE-PTR TO ADDRESS OF
           ZUT00000005(2)
             MOVE 11 TO AZ-COMPARE-ITEM-EXP-VALUE-LEN
             MOVE 1 TO FAIL-MESSAGE-LEN
             STRING
               'Compare failed in PROCEDURE DIVISION.'
               DELIMITED BY SIZE INTO FAIL-MESSAGE-TXT
               WITH POINTER FAIL-MESSAGE-LEN
             END-STRING
             SUBTRACT 1 FROM FAIL-MESSAGE-LEN
             PERFORM THROW-ASSERTION
           END-IF
           IF (OUT-TEXT1 OF RCV-PARMS IS NUMERIC)
               AND (OUT-TEXT1 OF RCV-PARMS = 12345012345) THEN
             CONTINUE
           ELSE
             MOVE OUT-TEXT1 OF RCV-PARMS TO ZUT00000007(1)
             MOVE 12345012345 TO ZUT00000007(2)
             SET AZ-COMPARE-ITEM-NAME-PTR TO ADDRESS OF ZUT00000006
             MOVE LENGTH OF ZUT00000006 TO AZ-COMPARE-ITEM-NAME-LEN
             SET AZ-COMPARE-ITEM-VALUE-PTR TO ADDRESS OF ZUT00000007(1)
             MOVE 11 TO AZ-COMPARE-ITEM-VALUE-LEN
             SET AZ-COMPARE-ITEM-EXP-VALUE-PTR TO ADDRESS OF
           ZUT00000007(2)
             MOVE 11 TO AZ-COMPARE-ITEM-EXP-VALUE-LEN
             MOVE 1 TO FAIL-MESSAGE-LEN
             STRING
               'Compare failed in PROCEDURE DIVISION.'
               DELIMITED BY SIZE INTO FAIL-MESSAGE-TXT
               WITH POINTER FAIL-MESSAGE-LEN
             END-STRING
             SUBTRACT 1 FROM FAIL-MESSAGE-LEN
             PERFORM THROW-ASSERTION
           END-IF
           IF OUT-TEXT2 OF RCV-PARMS = ZUT00000003 THEN
             CONTINUE
           ELSE
             MOVE OUT-TEXT2 OF RCV-PARMS TO ZUT00000009(1)
             MOVE ZUT00000003 TO ZUT00000009(2)
             SET AZ-COMPARE-ITEM-NAME-PTR TO ADDRESS OF ZUT00000008
             MOVE LENGTH OF ZUT00000008 TO AZ-COMPARE-ITEM-NAME-LEN
             SET AZ-COMPARE-ITEM-VALUE-PTR TO ADDRESS OF ZUT00000009(1)
             MOVE 11 TO AZ-COMPARE-ITEM-VALUE-LEN
             SET AZ-COMPARE-ITEM-EXP-VALUE-PTR TO ADDRESS OF
           ZUT00000009(2)
             MOVE 11 TO AZ-COMPARE-ITEM-EXP-VALUE-LEN
             MOVE 1 TO FAIL-MESSAGE-LEN
             STRING
               'Compare failed in PROCEDURE DIVISION.'
               DELIMITED BY SIZE INTO FAIL-MESSAGE-TXT
               WITH POINTER FAIL-MESSAGE-LEN
             END-STRING
             SUBTRACT 1 FROM FAIL-MESSAGE-LEN
             PERFORM THROW-ASSERTION
           END-IF
      *    display test name on exit
           DISPLAY TEST-NAME(1:TEST-NAME-LEN)
           ' Successful.'.
           GOBACK.
       CONVERT.
           MOVE AZ-CONVERT-HEXIN TO AZ-DECBYTE
           DIVIDE AZ-DEC BY 16 GIVING AZ-Q REMAINDER AZ-R
           COMPUTE AZ-Q1 = AZ-Q + 1
           COMPUTE AZ-R1 = AZ-R + 1
           MOVE AZ-HEXSTR(AZ-Q1:1) TO AZ-CONVERT-HEXVAL(1:1)
           MOVE AZ-HEXSTR(AZ-R1:1) TO AZ-CONVERT-HEXVAL(2:1)
           EXIT.
       THROW-ASSERTION.
      *    throw an assertion exception (ends test)
           CALL 'AZUASTFC' USING BY VALUE TEST-CASE-PTR
                   BY REFERENCE FAIL-MESSAGE-TXT
                   BY VALUE FAIL-MESSAGE-LEN
                   BY VALUE AZ-COMPARE-ITEM-NAME-PTR
                   BY VALUE AZ-COMPARE-ITEM-NAME-LEN
                   BY VALUE AZ-COMPARE-ITEM-VALUE-PTR
                   BY VALUE AZ-COMPARE-ITEM-VALUE-LEN
                   BY VALUE AZ-COMPARE-ITEM-EXP-VALUE-PTR
                   BY VALUE AZ-COMPARE-ITEM-EXP-VALUE-LEN
           EXIT.
       END PROGRAM 'TEST1'.