VTOCFORM TITLE 'VTOC COMMAND FORMAT ROUTINE'
***********************************************************************
*                                                                     *
*                                                                     *
* TITLE -      VTOC COMMAND FORMAT ROUTINE                            *
*                                                                     *
* FUNCTION -   FORMAT THE DATA INTO THE VTFMT  DSECT FROM THE         *
*              FORMAT 1 ( AND 3 IF NEEDED ) DSCB.  THIS ROUTINE       *
*              ALSO GETS THE AREA TO CONTAIN THE FORMATTED            *
*              DSCB INFORMATION.                                      *
*                                                                     *
* OPERATION -  FIRST GET AN AREA FROM THE CURRENT BLOCK, OR GET       *
*              A BLOCK ( 32K ) OF STORAGE TO USE FOR THE FORMATTED    *
*              DSCB'S.  MOVE THE DATA OVER FROM THE FORMAT 1 DSCB.    *
*              THE SPACE CALCULATIONS MAY NEED THE FORMAT 3 DSCB.     *
*              CATALOG INFORMATION IS OBTIANED VIA LOCATE.  SOME      *
*              OF THE DSCB INFORMATION IS CONVERTED HERE.             *
*                                                                     *
* INPUT -      VTOC COMMON AREA ( VTOCOM )                            *
*              POINTED TO BY REGISTER 1                               *
*              USE PARSE DATA, CURRENT FORMATTED DSCB, LOCATE         *
*                                                                     *
* OUTPUT -     THE FORMATTED DSCB INFORMATION WITH ITS ADDRESS IN     *
*              FORMATAD.                                              *
*                                                                     *
* ATTRIBUTES - REENTRANT, REUSEABLE, REFRESHABLE.                     *
*                                                                     *
*                                                                     *
*         PROGRAMMED BY R. L. MILLER  (415) 485-6241                  *
*                                                                     *
*                                                                     *
* 1/04/83 - UPDATED BY LAUREEN BEAUCHAINE - CBT:          LMB 1/4/83  *
*         - UPDATED TO USE KEYWORDS: CYLS/KBYTES/MBYTES   LMB 1/4/83  *
*                                                                     *
* 9/26/84 - MODIFIED BY A. BRUCE LELAND AT HITACHI TO SUPPORT   ABL-ICF
*           123 EXTENTS FOR VSAM DATA SETS IN AN ICF CATALOG.   ABL-ICF
*                                                                     *
***********************************************************************
*
         EJECT
         MACRO
&LAB     DS1TST  &FIELD,&VALUE,&CODE
&LAB     TM    DS1&FIELD,X'&VALUE'  TEST IT
         BNO   D&SYSNDX       IF NOT THERE, SKIP ALONG
         MVC   VTF&FIELD,=CL3'&CODE'
D&SYSNDX DS    0H
         MEND
*
*
         EJECT
VTOCFORM ENTER 12,16          DO THE HOUSEKEEPING
         LR    R11,R1         SAVE ADDR OF VTOCOM
         USING VTOCOM,R11     SET ITS ADDRESSABILITY
         L     R9,ADDRANSR    POINT TO THE PARSE ANSWER
         USING PDL,R9         SET ITS ADDRESSABILITY
         USING FORMWORK,R13   SET ADDRESSABILITY FOR LOCAL WORK AREA
         SPACE 3
*
*        CHECK FOR THE FIRST TIME THROUGH
*        IF SO, PERFORM SOME INITIALIZATION
*
         CLI   FIRSTFRM,0     IS THIS THE FIRST TIME?
         BNE   GETAREA        NO, KEEP ON TRUCKIN'
*
*        ROUTINE INITIALIZATION
*
         MVI   FIRSTFRM,255   NOTE THE INITIALIZATION AS DONE
         MVC   CAMLOC(CAMLEN),CAMCONST  SET UP THE CAMLST
*
*        FIND OR GET AN AREA FOR THE FORMATTED DSCB
*              FIRST SEE HOW BIG IT IS
*
GETAREA  L     R7,DSCBADDR    POINT TO THE DSCB
         LA    R7,8(R7)       GET PAST THE HEADER
         USING DSCB1,R7       SET ADDRESSABILITY
         LH    R1,DSNLEN     GET THE DSNAME LENGTH
         LA    R4,VTFMTL(R1)  GET THE FORMATTED DSCB LENGTH
*
*        SEE IF THE CURRENT BLOCK CAN HANDLE IT
*
FORMFIT  L     R3,VTCCURLN    GET THE CURRENT AVAILABLE
         SR    R3,R4          SEE IF IT WILL FIT
         BM    GOGETMN        NO, GET ANOTHER BLOCK
*
*        NO SWEAT, GET THE SPACE FROM THIS BLOCK
*
         ST    R3,VTCCURLN    STORE THE NEW ( REDUCED ) CURRENT LENGTH
         L     R3,VTCCURAD    POINT TO THE CURRENT ADDRESS
         LA    R5,0(R3,R4)    POINT TO THE END OF THE BLOCK
         ST    R5,VTCCURAD    AND PLACE THE NEW AVAILABLE ADDRESS
*
*        NOW FILL IN THE DATA IN THE FORMATTED DSCB
*
         USING VTFMT,R3       SET FORMATTED DSCB ADDRESSABILITY
         ST    R3,FORMATAD    SAVE THIS BLOCK'S ADDRESS
         XC    VTFNEXT,VTFNEXT  CLEAR THE SORT POINTER
         MVC   VTFVOLUM,VOLID SAVE THE VOLUME SERIAL NUMBER
         LH    R1,DSNLEN     GET THE LENGTH OF THE DSNAME
         STH   R1,VTFDSNL     SAVE THE DSNAME LENGTH
         BCTR  R1,0           SUBTRACT ONE FOR THE EX
         EX    R1,MOVEDSN     MOVE IN THE DSNAME
         MVC   VTFNOEPV,DS1NOEPV  NUMBER OF EXTENTS
         MVC   VTFLRECL,DS1LRECL  LOGICAL RECORD LENGTH
         MVC   VTFBLKSZ,DS1BLKL   BLOCK SIZE
*
*     MOVE IN THE CREATION DATE, EXPIRATION DATE, AND LAST ACCESS DATE
*
         MVC   VTFCREDT,DS1CREDT  MOVE OVER CREATION DATE
         MVC   VTFEXPDT,DS1EXPDT  MOVE OVER EXPIRATION DATE
         MVC   VTFLSTAC,DS1REFD   MOVE OVER LAST ACCESS DATE
*
*        FORMAT THE RECORD FORMAT INTO CHARACTERS
*
*
         MVC   VTFRECFM,BLANKS  BLANK THE FIELD TO START
         MVC   VTFACTON,BLANKS  ANOTHER BLANK FIELD
         MVI   VTFDSTYP,C' ' AND STILL ANOTHER
         LA    R2,VTFRECFM    POINT TO THE FIELD
         TM    DS1RECFM,X'C0' UNKNOWN RECFM?
         BZ    RECFM2         YES, TROUBLE
         TM    DS1RECFM,X'40' IS IT FIXED?
         BNZ   RECFM3         NO, KEEP TRYING
         MVI   0(R2),C'F'     YES, SET UP THE FIRST CHAR
         LA    R2,1(R2)       AND BUMP THE POINTER
         B     RECFM2         CHECK OTHER ATTRIBUTES
RECFM3   TM    DS1RECFM,X'80' SEE IF IT'S V OR U
         BZ    RECFM4         VARIABLE RECFM
         MVI   0(R2),C'U'     RECFM = U
         B     RECFM4A        ADD TO THE POINTER AND KEEP LOOKING
RECFM4   MVI   0(R2),C'V'     VARIABLE
RECFM4A  LA    R2,1(R2)       GET PAST THIS CHAR
RECFM2   DS    0H
RECFM5   TM    DS1RECFM,X'10' IS IT BLOCKED?
         BZ    RECFM6         NO, SKIP ON
         MVI   0(R2),C'B'     YES, SET THE SYMBOL
         LA    R2,1(R2)       GET PAST THE CHAR
RECFM6   TM    DS1RECFM,X'08' IS IT SPANNED OR STANDARD?
         BZ    RECFM6A        NO
         MVI   0(R2),C'S'     YES, SET IT
         LA    R2,1(R2)       GET PAST THIS CHARACTER
RECFM6A  TM    DS1RECFM,X'20' CHECK TRACK OVERFLOW
         BZ    RECFM7         NO DICE
         MVI   0(R2),C'T'     YES, SET IT
         LA    R2,1(R2)       PUSH THE POINTER ON
RECFM7   TM    DS1RECFM,X'04' IS IT ASA CONTROL
         BZ    RECFM8         NO, SKIP ON
         MVI   0(R2),C'A'     YES, SET IT
         LA    R2,1(R2)       GET PAST THIS CHAR
RECFM8   TM    DS1RECFM,X'02' HOW ABOUT MACHINE CARRIAGE CONTROL
         BZ    RECFM9         NO, SKIP ON
         MVI   0(R2),C'M'     YES, SET IT
RECFM9   DS    0H
*
*        FORMAT THE DSORG
*
         MVC   VTFDSORG,=CL3'   '  CLEAR THE FIELD
         DS1TST DSORG,80,IS   TRY ISAM
         DS1TST DSORG,40,PS   TRY SEQUENTIAL
         DS1TST DSORG,20,DA   TRY DIRECT ACCESS
         DS1TST DSORG,02,PO   TRY PARTITIONED
         CLC   DS1DSORG(2),=X'0008'  IS IT VSAM?
         BNE   DSORG05       NO, KEEP LOOKING
         MVC   VTFDSORG,=CL3'VS ' YES, FLAG IT
DSORG05  TM    DS1DSORG,X'01'      IS IT UNMOVEABLE?
         BNO   DSORG06       NO, KEEP ON TRUCKIN'
         MVI   VTFDSORG+2,C'U'     YES, NOTE IT
DSORG06  DS    0H
*
*        FORMAT THE SECONDARY ALLOCATION
*
         SR    R1,R1          CLEAR A WORK REGISTER
         IC    R1,DS1SCALO    GET THE ALLOCATION FLAG
         SRL   R1,6           REMOVE THE BOTTOM 6 BITS ( 75 CENTS )
         IC    R2,SECAL(R1)   GET THE CHARACTER CODE
         STC   R2,VTFSECAL    AND SAVE IT FOR LATER
         MVC   VTFSECAM,DS1SCALO+2  SAVE THE SECONDARY AMOUNT TOO
         MVI   VTFROUND,C'N'  SET CODE FOR NO ROUND
         TM    DS1SCALO,X'01' SEE IF ROUND WAS SET
         BNO   PROTFORM       NO, THE CODE IS SET RIGHT
         MVI   VTFROUND,C'R'  YES, RESET THE CODE
*
*        FORMAT THE PASSWORD PROTECTION
*
PROTFORM TM    DS1DSIND,X'14' CHECK THE PASSWORD BITS
         BO    PROTWRIT       WRITE PROTECT IS X'14'
         BM    PROTREAD       READ PROTECT IS X'10'
         MVI   VTFPROT,C'N'   NO PASSWORD PROTECTION
         B     PROTEND        END OF PROTECTION FORMATTING
PROTWRIT MVI   VTFPROT,C'W'   SET CODE FOR WRITE PROTECT
         B     PROTEND        THEN CHECK OTHER ITEMS
PROTREAD MVI   VTFPROT,C'R'   SET CODE FOR READ/WRITE PROTECT
PROTEND  DS    0H             END OF PROTECTION FORMATTING
*
*        FORMAT THE RACF INDICATOR
*
         MVI   VTFRACF,C'N'
         TM    DS1DSIND,DS1IND40
         BNO   RACFEND
         MVI   VTFRACF,C'Y'
RACFEND  DS    0H
*
*        FORMAT THE UPDATED INDICATOR
*
         MVI   VTFUPD,C'N'
         TM    DS1DSIND,DS1IND02
         BNO   UPDEND
         MVI   VTFUPD,C'Y'
UPDEND   DS    0H
*
*        FORMAT THE CATLG
*
         MVI   VTFCATLG,C' ' INITIALIZE IT TO BLANKS
         CLI   CATK+1,0       SHOULD WE DO THE LOCATE?
         BE    CATEND         NO, SKIP PAST IT
*
*        SET UP THE CAMLST
*
         LA    R1,DS1DSNAM    POINT TO THE DSNAME
         ST    R1,CAMLOC+4    SAVE IT IN THE CAMLST
         LA    R1,LOCWORK     LOCATE WORKAREA
         ST    R1,CAMLOC+12   SAVE IT IN THE CAMLST
         LOCATE CAMLOC        CHECK THE CATALOG
         LTR   R15,R15        TEST THE CATALOG RETURN CODE
         BZ    CATOK          ZERO, THERE IS AN ENTRY
         MVI   VTFCATLG,C'N'  SET CODE FOR NOT CATALOGED
         CH    R15,H8         SEE IF THAT'S THE CASE
         BE    CATEND         YES, LET IT STAND
         MVI   VTFCATLG,C'E'  CATALOG ERROR, PROBLEMS
*
*        CATALOG ENTRY IS THERE, SEE THAT THE VOLUME IS THIS ONE
*
CATOK    MVI   VTFCATLG,C'C'  SET UP AS A GOOD ENTRY
         CLC   VOLID,LOCWORK+6  COMPARE THE VOLUME SERIAL NUMBERS
         BE    CATEND         GOOD, WE'RE DONE
         MVI   VTFCATLG,C'W'  WRONG VOLUME, NOT CATALOGED
CATEND   DS    0H
*
*        FORMAT THE ALLOCATION AND USED QUANTITIES
*
         SPACE
*        CHECK THROUGH THE EXTENTS
         SPACE
         SR    R2,R2          CLEAR A WORK REGISTER
         ICM   R2,B'0001',DS1NOEPV  GET THE NUMBER OF EXTENTS
         BZ    SPACEND        NO EXTENTS MEANS NO SPACE
         SR    R4,R4          ZERO THE SPACE COUNTER FOR THE DATA SET
*
*        GET EACH EXTENT AND PROCESS IT
*
         SR    R6,R6          FIRST EXTENT
EXTNEXT  LR    R14,R6                                           ABL-ICF
         S     R14,=F'3'                                        ABL-ICF
         SRDA  R14,32                                           ABL-ICF
         D     R14,=F'13'     (EXTENT-3) / 13                   ABL-ICF
         LR    R14,R15        EXTENT TABLE IN USE               ABL-ICF
         MH    R14,=H'13'                                       ABL-ICF
         MH    R15,=H'148'                                      ABL-ICF
         LR    R5,R6          EXTENT - ((EXTENT-3) / 13) * 13   ABL-ICF
         SR    R5,R14         EXTENT INSTRUCTION ADDRESS        ABL-ICF
         SLL   R5,2           MULTIPLY IT BY FOUR
         EX    R0,GETEXT(R5)  GET THE CORRECT ADDRESS
*
*        PROCESS THIS EXTENT
*
         USING XTDSECT,R5     SET ADDRESSABILITY
         CLI   XTFLAGS,XTNOEXT  IS THERE AN EXTENT
         BE    NOEXT          NO, THE EXTENT ISN'T THERE
         CLI   XTFLAGS,XTCYLBD  IS IT ON CYLINDER BOUNDARIES
         BNE   FORMALOC       NO, DO IT FOR CYLS AND TRACKS
*
*        CYLINDER BOUNDS - BE SURE THE ALLOCATION IS CORRECT
*
         ICM   R1,B'0011',XTLOWHH GET THE LOWER TRACK
         BZ    LOWOK          IT'S ZERO
         MVC   VTFACTON(6),=C'CYLERR'  NOTE THE ERROR
         MVI   VTFACTON+6,C'L'  ON THE LOW CCHH
LOWOK    LH    R1,XTHIHH      GET THE HIGH TRACK
         LA    R1,1(R1)       ADD ONE FOR ZERO ADDRESSING
         CH    R1,DS4DEVSZ+2  IS THIS THE NUMBER OF TRACKS/CYL
         BE    FORMALOC       YES, GO CALCULATE
         MVC   VTFACTON(6),=C'CYLERR'  NOTE THE ERROR
         MVI   VTFACTON+7,C'H'  ON THE HIGH CCHH
*
*        GET THE SPACE FOR NON-CYLINDER ALLOCATIONS
*
FORMALOC LH    R1,XTHICC      GET THE HIGH CYLINDER
         SH    R1,XTLOWCC     MINUS THE LOW CYLINDER
         MH    R1,DS4DEVSZ+2  TIMES THE NUMBER OF TRACKS PER CYLINDER
         LH    R8,XTHIHH      GET THE HIGH TRACK
         SH    R8,XTLOWHH     MINUS THE LOW TRACK
         AR    R8,R1          TRACKS IN THIS EXTENT ( MINUS 1 )
         LA    R4,1(R4,R8)    ADD THE TRACKS TOGETHER FOR THIS DATA SET
*
*        GET THE NEXT EXTENT
*
NOEXT    LA    R6,1(R6)       INCREMENT THE EXTENT COUNTER
         CR    R6,R2          CHECK FOR THE END
         BL    EXTNEXT        NOT YET, KEEP GOING
*
*        ALL THE EXTENTS ARE SUMMED REGISTER 4 HAS THE SUM
*
         BAL   R8,SPACUNIT    CHANGE IT TO THE APPROPRIATE UNITS
         ST    R4,VTFALLOC    STORE IT FOR LATER
SPACEND  DS    0H
*
*        GET THE TRACKS USED
*
         SR    R4,R4          CLEAR THE TRACK ( WOO WOO )
         CLC   DS1LSTAR,ZEROES IS THE TRACK USED COUNTER SET?
         BNE   USEDOK         YES, ACCEPT IT
*        NO, SEE IF THE ZERO IS VALID
         TM    DS1DSORG,X'40' IS IT SEQUENTIAL?
         BO    USEDOK0        YES,THE ZERO IS VALID
         CLC   DSORG(4),ZEROES  MAYBE IT WASN'T EVER OPENED
         BE    USEDOK0        THEN NO SPACE USED IS OK
         TM    DS1DSORG,X'0C' CHECK FOR AN INVALID DSORG
         BO    USEDOK0        NO SPACE USED IS STILL OK
         MVC   VTFUSED,FMIN1  SET A FLAG UNUSED SPACE UNKNOWN
         B     USEDEND        USED SPACE IS SET
*
*        THE TRACKS USED COUNTER SEEMS OK
*
USEDOK   LH    R4,DS1LSTAR    GET THE LAST TRACK USED
         LA    R4,1(R4)       ADD ONE ( ZERO ADDRESSING )
         BAL   R8,SPACUNIT    CONVERT TO APPROPRIATE UNITS
USEDOK0  ST    R4,VTFUSED     SAVE THE AMOUNT OF SPACE USED
USEDEND  DS    0H
         L     R14,VTFALLOC   ALLOCATED TRACKS
         S     R14,VTFUSED    MINUS USED TRACKS
         ST    R14,VTFUNUSD   EQUALS UNUSED TRACKS
         SR    R14,R14
         SR    R15,R15
         CLC   VTFALLOC(4),=F'0'
         BE    USEDEND1
         L     R15,VTFUSED    USED TRACKS
         M     R14,=F'100'    MULT BY 100 TO GET PCT
         D     R14,VTFALLOC   DIVIDE BY ALLOC TO GET PCT USED
USEDEND1 STH   R15,VTFPCT     SAVE PCT USED
*
*        RETURN
*
FORMRET  LEAVE EQ,RC=0
*
*
         EJECT
*
*        ROUTINES USED ABOVE
*
*
*        CONVERT FROM TRACKS TO THE APPROPRIATE UNITS
*              KBYTES, MBYTES, TRKS, OR CYLS
*
SPACUNIT LH    R1,SPACEK      GET THE UNIT TYPE
         SLL   R1,2           MULTIPLY BY 4
         B     *+4(R1)        THEN BRANCH TO THE CORRECT ROUTINE
         B     SPACKB         R1=0  KILOBYTES             LMB 1/4/83
         B     SPACKB         R1=1  KILOBYTES             LMB 1/4/83
         B     SPACMB         R1=2  MEGABYTES             LMB 1/4/83
         B     SPACTRK        R1=3  TRACKS                LMB 1/4/83
         B     SPACCYL        R1=4  CYLINDERS             LMB 1/4/83
*        TRACKS
SPACTRK  BR    R8             WAS SET WHEN WE STARTED
*        CYLINDERS
SPACCYL  SR    R0,R0          CLEAR A REGISTER
         LR    R1,R4          GET THE NUMBER OF TRACKS
         LH    R4,DS4DEVSZ+2  GET THE NUMBER OF TRACKS PER CYLINDER
         SRL   R4,2           DIVIDE BY 2 FOR ROUNDING
         AR    R1,R4          ADD IT IN
         LH    R4,DS4DEVSZ+2  GET THE NUMBER OF TRACKS PER CYLINDER
         DR    R0,R4          DIVIDE TO GET ROUNDED CYLINDERS
         LR    R4,R1          GET THE ANSWER BACK INTO R4
         BR    R8             THEN RETURN
*        KILOBYTES
SPACKB   MH    R4,DS4DEVTK    MULTIPLY BY BYTES PER TRACK
         SR    R0,R0          CLEAR THE TOP
         LR    R1,R4          GET THE NUMBER TO DIVIDE
         A     R1,F500        ADD UP TO ROUND
         D     R0,F1000       DIVIDE TO GET KILOBYTES
         LR    R4,R1          GET THE ANSWER BACK INTO R4
         BR    R8             THEN RETURN
*        MEGABYTES
SPACMB   MH    R4,DS4DEVTK    MULTIPLY BY BYTES PER TRACK
         SR    R0,R0          CLEAR THE TOP
         LR    R1,R4          GET THE NUMBER TO DIVIDE
         A     R1,F500000     ADD UP TO ROUND
         D     R0,F1000000    DIVIDE TO GET MEGABYTES
         LR    R4,R1          GET THE ANSWER BACK INTO R4
         BR    R8             THEN RETURN
*
*        GET A NEW BLOCK OF MAIN STORAGE
*
GOGETMN  GETMAIN R,LV=VTCGETMS  GET SOME
         ST    R1,VTCCURAD    SET UP THE AVAILABLE ADDRESS
         LA    R2,VTCGETMS/1024   GET THE SIZE OF THE BLOCK IN K
         SLL   R2,10          GET IT INTO BYTES ( TIMES 1024 )
         ST    R2,VTCCURLN    SO THE FORMATTED DSCB'S CAN USE IT
*
*        SAVE THE BLOCK ADDRESS IN THE VTCGETMN TABLE
*
         LA    R2,VTCGETMN    POINT TO THE TABLE
         LA    R5,VTCGETMX    GET THE NUMBER OF ENTRIES IN THE TABLE
GOGETTAB ICM   R3,B'1111',0(R2) GET THIS ENTRY
         BNZ   GOGETINC       IF NOT ZERO, KEEP LOOKING
         ST    R1,0(R2)       SAVE THE NEW ENTRY
         B     FORMFIT        THEN GO ALLOCATE A FORMATTED DSCB
*
*        THIS ENTRY WAS TAKEN, GET THE NEXT ONE
*
GOGETINC LA    R2,4(R2)       POINT TO THE NEXT ENTRY
         BCT   R5,GOGETTAB    COUNT AND LOOP
*
*        TABLE OVERFLOW  - ISSUE ERROR MSG
*              SET A FLAG TO STOP INPUT
*
         VTOCMSG TABOVFLW,TABOVSEC  ISSUE A MESSAGE
         MVI   TABFULL,255    SET A STOP FLAG
         B     FORMRET        RETURN FROM FORMATTING
         EJECT
*
*
*
*        PROGRAM CONSTANTS
*
         SPACE
*        INSTRUCTIONS EXECUTED TO GET THE NEXT EXTENT
GETEXT   LA    R5,DS1EXT1           1ST EXTENT
         LA    R5,DS1EXT2           2ND EXTENT
         LA    R5,DS1EXT3           3RD EXTENT
         LA    R5,DS3EXTNT+00(R15)  4TH EXTENT                  ABL-ICF
         LA    R5,DS3EXTNT+10(R15)  5TH EXTENT                  ABL-ICF
         LA    R5,DS3EXTNT+20(R15)  6TH EXTENT                  ABL-ICF
         LA    R5,DS3EXTNT+30(R15)  7TH EXTENT                  ABL-ICF
         LA    R5,DS3ADEXT+00(R15)  8TH EXTENT                  ABL-ICF
         LA    R5,DS3ADEXT+10(R15)  9TH EXTENT                  ABL-ICF
         LA    R5,DS3ADEXT+20(R15) 10TH EXTENT                  ABL-ICF
         LA    R5,DS3ADEXT+30(R15) 11TH EXTENT                  ABL-ICF
         LA    R5,DS3ADEXT+40(R15) 12TH EXTENT                  ABL-ICF
         LA    R5,DS3ADEXT+50(R15) 13TH EXTENT                  ABL-ICF
         LA    R5,DS3ADEXT+60(R15) 14TH EXTENT                  ABL-ICF
         LA    R5,DS3ADEXT+70(R15) 15TH EXTENT                  ABL-ICF
         LA    R5,DS3ADEXT+80(R15) 16TH EXTENT                  ABL-ICF
MOVEDSN  MVC   VTFDSN(0),DS1DSNAM   EXECUTED COMPARE
ZEROES   DC    2F'0'
FMIN1    DC    F'-1'
F500     DC    F'500'
F1000    DC    F'1000'
F500000  DC    F'500000'
F1000000 DC    F'1000000'
BLANKS   DC    CL8'                '
CAMCONST CAMLST NAME,*,,*
H8       DC    H'8'
SECAL    DC    C'ABTC'        SECONDARY ALLOCATION CODES
*              ABSOLUTE TRK, BLOCKS, TRACKS, CYLINDERS
*
*
*
         PRINT NOGEN
*
*        PROGRAM MESSAGES
*
TABOVFLW MSG   ' THE VTOC TABLES (1.6 MEG) ARE NOT LARGE ENOUGH TO HANDX
               LE THIS REQUEST'
TABOVSEC MSG   ' PARTIAL PROCESSING WILL CONTINUE '
*
*
*
*
*
*
         EJECT
*
*
*        P A R S E   C O N T R O L   L I S T
*
*
         PRINT OFF
         COPY  VTOCPARS
         PRINT ON
*
*        DYNAMIC WORK AREA
*
         SPACE 3
FORMWORK DSECT
         DS    18A            PRINT ROUTINE SAVE AREA
FIRSTFRM DS    X              INITIALIZATION FOR THIS ROUTINE
CHARS    DS    CL16           CONVERSION TO CHARACTERS
CAMLOC   CAMLST NAME,*,,*
CAMLEN   EQU   *-CAMLOC
         DS    0D
LOCWORK  DS    265C
         SPACE
         DS    0D
LENWORK  EQU   *-FORMWORK
*
*        VTOC COMMAND COMMON AREA
*
         PRINT NOGEN
         VTOCOM
         SPACE 3
*
*        FORMATTED DSCB
*
         PRINT GEN
         VTFMT
         PRINT NOGEN
         SPACE 3
         PDEDSNAM
         SPACE 3
         SPACE 3
DSCB1    DSECT
         IECSDSL1 1
         SPACE 3
*        FORMAT 1 AND 3 EXTENT DESCRIPTION
XTDSECT  DSECT
XTFLAGS  DS    X
XTNOEXT  EQU   X'00'          NO EXTENT
XTDATAB  EQU   X'01'          DAT BLOCKS
XTOVFLW  EQU   X'02'          OVERFLOW AREA
XTINDEX  EQU   X'04'          INDEX AREA
XTUSRLAB EQU   X'40'          USER LABEL EXTENT
XTSHRCYL EQU   X'80'          SHARING CYLINDERS
XTCYLBD  EQU   X'81'          CYLINDER BOUNDARIES
XTSEQ    DS    X              EXTENT SEQUENCE NUMBER
XTLOWCC  DS    H              LOWER CYLINDER
XTLOWHH  DS    H              LOWER TRACK
XTHICC   DS    H              UPPER CYLINDER
XTHIHH   DS    H              UPPER TRACK
         END
