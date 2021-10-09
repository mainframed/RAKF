//RAKFPREP JOB (RAKF),
//             'RAKF Installation',
//             CLASS=A,
//             MSGCLASS=A,
//             REGION=8192K,
//             MSGLEVEL=(1,1)
//* ------------------------------------------------------------------*
//* Prepare system for RAKF 1.2.0 installation                        *
//*                                                                   *
//* Expected return codes: Step UCLIN:  00                            *
//*                        Step LIBS:   00                            *
//*                        Step DELETE: 08 or lower                   *
//* ------------------------------------------------------------------*
//*
//* ------------------------------------------------------------------*
//* Delete MVS stub modules from SMP                                  *
//* ------------------------------------------------------------------*
//UCLIN   EXEC SMPAPP
//SMPCNTL  DD  *
 UCLIN CDS .
  DEL SYSMOD(EBB1102) MOD(ICHRIN00) .
  DEL MOD(IEFBR14) LMOD(ICHSEC00) .
  DEL MOD(ICHRIN00) .
  DEL LMOD(ICHSEC00) .
  DEL LMOD(IGC0013{) .
 ENDUCL .
 UCLIN ACDS .
  DEL SYSMOD(EBB1102) MOD(ICHRIN00) .
  DEL MOD(ICHRIN00) .
 ENDUCL .
/*
//* ------------------------------------------------------------------*
//* Allocate target und and distribution libraries                    *
//* ------------------------------------------------------------------*
//LIBS    EXEC PGM=IEFBR14
//ASAMPLIB DD  DISP=(,CATLG),DSN=SYSGEN.RAKF.V1R2M0.ASAMPLIB,
//             VOL=SER=PUB001,
//             UNIT=SYSDA,DCB=(RECFM=FB,LRECL=80,BLKSIZE=19040),
//             SPACE=(TRK,(120,40,10))
//SAMPLIB  DD  DISP=(,CATLG),DSN=SYSGEN.RAKF.V1R2M0.SAMPLIB,
//             VOL=SER=PUB001,
//             UNIT=SYSDA,DCB=(RECFM=FB,LRECL=80,BLKSIZE=19040),
//             SPACE=(TRK,(120,40,10))
//AMACLIB  DD   DISP=(,CATLG),DSN=SYSGEN.RAKF.V1R2M0.AMACLIB,
//             VOL=SER=PUB001,
//             UNIT=SYSDA,DCB=(RECFM=FB,LRECL=80,BLKSIZE=5600),
//             SPACE=(TRK,(2,1,1))
//MACLIB   DD  DISP=(,CATLG),DSN=SYSGEN.RAKF.V1R2M0.MACLIB,
//             VOL=SER=PUB001,
//             UNIT=SYSDA,DCB=(RECFM=FB,LRECL=80,BLKSIZE=5600),
//             SPACE=(TRK,(2,1,1))
//ASRCLIB  DD  DISP=(,CATLG),DSN=SYSGEN.RAKF.V1R2M0.ASRCLIB,
//             VOL=SER=PUB001,
//             UNIT=SYSDA,DCB=(RECFM=FB,LRECL=80,BLKSIZE=5600),
//             SPACE=(TRK,(30,10,4))
//SRCLIB   DD  DISP=(,CATLG),DSN=SYSGEN.RAKF.V1R2M0.SRCLIB,
//             VOL=SER=PUB001,
//             UNIT=SYSDA,DCB=(RECFM=FB,LRECL=80,BLKSIZE=5600),
//             SPACE=(TRK,(30,10,4))
//APROCLIB DD  DISP=(,CATLG),DSN=SYSGEN.RAKF.V1R2M0.APROCLIB,
//             VOL=SER=PUB001,
//             UNIT=SYSDA,DCB=(RECFM=FB,LRECL=80,BLKSIZE=19040),
//             SPACE=(TRK,(2,1,1))
//APARMLIB DD  DISP=(,CATLG),DSN=SYSGEN.RAKF.V1R2M0.APARMLIB,
//             VOL=SER=PUB001,
//             UNIT=SYSDA,DCB=(RECFM=F,LRECL=80,BLKSIZE=80),
//             SPACE=(TRK,(1,1,1))
//* ------------------------------------------------------------------*
//* Delete MVS stub modules and/or pre-SMP RAKF modules               *
//* from LINKLIB and LPALIB                                           *
//* ------------------------------------------------------------------*
//DELETE  EXEC PGM=IEHPROGM
//SYSPRINT DD  SYSOUT=*
//DD1      DD  VOL=SER=MVSRES,DISP=OLD,UNIT=3350
//SYSIN    DD  *
   SCRATCH VOL=3350=MVSRES,DSNAME=SYS1.LPALIB,MEMBER=ICHSFR00
   SCRATCH VOL=3350=MVSRES,DSNAME=SYS1.LPALIB,MEMBER=IGC0013A
   SCRATCH VOL=3350=MVSRES,DSNAME=SYS1.LPALIB,MEMBER=IGC0013B
   SCRATCH VOL=3350=MVSRES,DSNAME=SYS1.LPALIB,MEMBER=IGC0013C
   SCRATCH VOL=3350=MVSRES,DSNAME=SYS1.LPALIB,MEMBER=IGC0013{
   SCRATCH VOL=3350=MVSRES,DSNAME=SYS1.LPALIB,MEMBER=ICHRIN00
   SCRATCH VOL=3350=MVSRES,DSNAME=SYS1.LINKLIB,MEMBER=ICHSEC00
   SCRATCH VOL=3350=MVSRES,DSNAME=SYS1.LINKLIB,MEMBER=RAKFPROF
   SCRATCH VOL=3350=MVSRES,DSNAME=SYS1.LINKLIB,MEMBER=RAKFUSER
   SCRATCH VOL=3350=MVSRES,DSNAME=SYS1.LINKLIB,MEMBER=RAKFPWUP
   SCRATCH VOL=3350=MVSRES,DSNAME=SYS1.LINKLIB,MEMBER=RAKFINIT
/*
//* ------------------------------------------------------------------*
//* SMP receive of RAKF 1.2.6                                         *
//* Expected return code: 00                                          *
//* ------------------------------------------------------------------*
//RECEIVER EXEC SMPREC
//SMPPTFIN DD DATA,DLM='??'
++FUNCTION(TRKF126).
++VER(Z038).
++JCLIN.
//TRKF120  JOB 1,'RAKF 1.2',MSGLEVEL=1,CLASS=A
//*
//* JCLIN for RAKF 1.2
//*
//ASMSEC   EXEC PGM=IFOX00,PARM=(NOOBJ,DECK)
//SYSLIB   DD  DISP=SHR,DSN=SYS1.MACLIB
//         DD  DISP=SHR,DSN=SYS1.AMODGEN
//         DD  DISP=SHR,DSN=RAKF.MACLIB
//SYSIN    DD  DISP=SHR,DSN=RAKF.SRCLIB(ICHSEC00)
//SYSPUNCH DD  DISP=(,PASS),DSN=&&OBJ(ICHSEC00)
//ASMRCVT  EXEC PGM=IFOX00,PARM=(NOOBJ,DECK)
//SYSLIB   DD  DISP=SHR,DSN=SYS1.MACLIB
//         DD  DISP=SHR,DSN=SYS1.AMODGEN
//         DD  DISP=SHR,DSN=RAKF.MACLIB
//SYSIN    DD  DISP=SHR,DSN=RAKF.SRCLIB(CJYRCVT)
//SYSPUNCH DD  DISP=(OLD,PASS),DSN=&&OBJ(CJYRCVT)
//ICHSEC00 EXEC  PGM=IEWL,PARM='MAP,LIST,LET,NCAL,AC=1'
//SYSLMOD  DD  DISP=SHR,DSN=SYS1.LINKLIB
//SYSPUNCH DD  DISP=(OLD,PASS),DSN=&&OBJ
//SYSLIN   DD  *
 INCLUDE SYSPUNCH(ICHSEC00)
 INCLUDE SYSPUNCH(CJYRCVT)
 ENTRY   ICHSEC00
 NAME    ICHSEC00(R)
/*
//*
//* JCLIN for RAKF 1.2 PTF RRKF002
//*
//ASMUSER  EXEC PGM=IFOX00,PARM=(NOOBJ,DECK)
//SYSLIB   DD  DISP=SHR,DSN=SYS1.MACLIB
//         DD  DISP=SHR,DSN=SYS1.AMODGEN
//         DD  DISP=SHR,DSN=RAKF.MACLIB
//SYSIN    DD  DISP=SHR,DSN=RAKF.SRCLIB(RAKFUSER)
//SYSPUNCH DD  DISP=(OLD,PASS),DSN=&&OBJ(RAKFUSER)
//ASMPSAV  EXEC PGM=IFOX00,PARM=(NOOBJ,DECK)
//SYSLIB   DD  DISP=SHR,DSN=SYS1.MACLIB
//         DD  DISP=SHR,DSN=SYS1.AMODGEN
//         DD  DISP=SHR,DSN=RAKF.MACLIB
//SYSIN    DD  DISP=SHR,DSN=RAKF.SRCLIB(RAKFPSAV)
//SYSPUNCH DD  DISP=(OLD,PASS),DSN=&&OBJ(RAKFPSAV)
//RAKFUSER EXEC  PGM=IEWL,PARM='MAP,LIST,LET,NCAL,AC=1'
//SYSLMOD  DD  DISP=SHR,DSN=SYS1.LINKLIB
//SYSPUNCH DD  DISP=(OLD,PASS),DSN=&&OBJ
//SYSLIN   DD  *
 INCLUDE SYSPUNCH(RAKFUSER)
 INCLUDE SYSPUNCH(RAKFPSAV)
 ENTRY   CJYRUIDS
 NAME    RAKFUSER(R)
/*
//*
//ASMPROF  EXEC PGM=IFOX00,PARM=(NOOBJ,DECK)
//SYSLIB   DD  DISP=SHR,DSN=SYS1.MACLIB
//         DD  DISP=SHR,DSN=SYS1.AMODGEN
//         DD  DISP=SHR,DSN=RAKF.MACLIB
//SYSIN    DD  DISP=SHR,DSN=RAKF.SRCLIB(RAKFPROF)
//SYSPUNCH DD  DISP=(OLD,PASS),DSN=&&OBJ(RAKFPROF)
//RAKFPROF EXEC  PGM=IEWL,PARM='MAP,LIST,LET,NCAL,AC=1'
//SYSLMOD  DD  DISP=SHR,DSN=SYS1.LINKLIB
//SYSPUNCH DD  DISP=(OLD,PASS),DSN=&&OBJ
//SYSLIN   DD  *
 INCLUDE SYSPUNCH(RAKFPROF)
 ENTRY   CJYRPROF
 NAME    RAKFPROF(R)
/*
//*
//ASMPWUP  EXEC PGM=IFOX00,PARM=(NOOBJ,DECK)
//SYSLIB   DD  DISP=SHR,DSN=SYS1.MACLIB
//         DD  DISP=SHR,DSN=SYS1.AMODGEN
//         DD  DISP=SHR,DSN=RAKF.MACLIB
//SYSIN    DD  DISP=SHR,DSN=RAKF.SRCLIB(RAKFPWUP)
//SYSPUNCH DD  DISP=(OLD,PASS),DSN=&&OBJ(RAKFPWUP)
//RAKFPWUP EXEC  PGM=IEWL,PARM='MAP,LIST,LET,NCAL,AC=1'
//SYSLMOD  DD  DISP=SHR,DSN=SYS1.LINKLIB
//SYSPUNCH DD  DISP=(OLD,PASS),DSN=&&OBJ
//SYSLIN   DD  *
 INCLUDE SYSPUNCH(RAKFPWUP)
 ENTRY   RAKFPWUP
 NAME    RAKFPWUP(R)
/*
//*
//ASMSFR   EXEC PGM=IFOX00,PARM=(NOOBJ,DECK)
//SYSLIB   DD  DISP=SHR,DSN=SYS1.MACLIB
//         DD  DISP=SHR,DSN=SYS1.AMODGEN
//         DD  DISP=SHR,DSN=RAKF.MACLIB
//SYSIN    DD  DISP=SHR,DSN=RAKF.SRCLIB(ICHSFR00)
//SYSPUNCH DD  DISP=(OLD,PASS),DSN=&&OBJ(ICHSFR00)
//ICHSFR00 EXEC  PGM=IEWL,PARM='MAP,LIST,NCAL,LET,RENT,REFR,REUS,AC=1'
//SYSLMOD  DD  DISP=SHR,DSN=SYS1.LPALIB
//SYSPUNCH DD  DISP=(OLD,PASS),DSN=&&OBJ
//SYSLIN   DD  *
 INCLUDE SYSPUNCH(ICHSFR00)
 ENTRY   ICHSFR00
 NAME    ICHSFR00(R)
/*
//*
//ASMRIN   EXEC PGM=IFOX00,PARM=(NOOBJ,DECK)
//SYSLIB   DD  DISP=SHR,DSN=SYS1.MACLIB
//         DD  DISP=SHR,DSN=SYS1.AMODGEN
//         DD  DISP=SHR,DSN=RAKF.MACLIB
//SYSIN    DD  DISP=SHR,DSN=RAKF.SRCLIB(ICHRIN00)
//SYSPUNCH DD  DISP=(OLD,PASS),DSN=&&OBJ(ICHRIN00)
//ASM130   EXEC PGM=IFOX00,PARM=(NOOBJ,DECK)
//SYSLIB   DD  DISP=SHR,DSN=SYS1.MACLIB
//         DD  DISP=SHR,DSN=SYS1.AMODGEN
//         DD  DISP=SHR,DSN=RAKF.MACLIB
//SYSIN    DD  DISP=SHR,DSN=RAKF.SRCLIB(IGC00130)
//SYSPUNCH DD  DISP=(OLD,PASS),DSN=&&OBJ(IGC00130)
//ASM13A   EXEC PGM=IFOX00,PARM=(NOOBJ,DECK)
//SYSLIB   DD  DISP=SHR,DSN=SYS1.MACLIB
//         DD  DISP=SHR,DSN=SYS1.AMODGEN
//         DD  DISP=SHR,DSN=RAKF.MACLIB
//SYSIN    DD  DISP=SHR,DSN=RAKF.SRCLIB(IGC0013A)
//SYSPUNCH DD  DISP=(OLD,PASS),DSN=&&OBJ(IGC0013A)
//ASM13C   EXEC PGM=IFOX00,PARM=(NOOBJ,DECK)
//SYSLIB   DD  DISP=SHR,DSN=SYS1.MACLIB
//         DD  DISP=SHR,DSN=SYS1.AMODGEN
//         DD  DISP=SHR,DSN=RAKF.MACLIB
//SYSIN    DD  DISP=SHR,DSN=RAKF.SRCLIB(IGC0013C)
//SYSPUNCH DD  DISP=(OLD,PASS),DSN=&&OBJ(IGC0013C)
//ICHRIN00 EXEC  PGM=IEWL,PARM='MAP,LIST,NCAL,LET,RENT,REFR,REUS,AC=1'
//SYSLMOD  DD  DISP=SHR,DSN=SYS1.LPALIB
//SYSPUNCH DD  DISP=(OLD,DELETE),DSN=&&OBJ
//SYSLIN   DD  *
 INCLUDE SYSPUNCH(ICHRIN00)
 INCLUDE SYSPUNCH(IGC00130)
 INCLUDE SYSPUNCH(IGC0013A)
 INCLUDE SYSPUNCH(IGC0013C)
 ENTRY   ICHRIN00
 ALIAS   IGC0013{
 ALIAS   IGC0013A
 ALIAS   IGC0013B
 ALIAS   IGC0013C
 NAME    ICHRIN00(R)
/*
//*
//* JCLIN for RAKF 1.2 PTF RRKF005
//*
//ASMIND   EXEC PGM=IFOX00,PARM=(NOOBJ,DECK)
//SYSLIB   DD  DISP=SHR,DSN=SYS1.MACLIB
//         DD  DISP=SHR,DSN=SYS1.AMODGEN
//         DD  DISP=SHR,DSN=RAKF.MACLIB
//SYSIN    DD  DISP=SHR,DSN=RAKF.SRCLIB(RACIND)
//SYSPUNCH DD  DISP=(OLD,PASS),DSN=&&OBJ(RACIND)
//RACIND   EXEC  PGM=IEWL,PARM='MAP,LIST,LET,NCAL,AC=1'
//SYSLMOD  DD  DISP=SHR,DSN=SYS1.LINKLIB
//SYSPUNCH DD  DISP=(OLD,PASS),DSN=&&OBJ
//SYSLIN   DD  *
 INCLUDE SYSPUNCH(RACIND)
 ENTRY   RACIND
 NAME    RACIND(R)
/*
++MAC(CJYPCBLK) DISTLIB(AMACLIB)  SYSLIB(MACLIB).
RPECBLK  DSECT ,                    RESOURCE PROFILE ELEMENT            00010000
RPENEXT  DS    F                    NEXT RPE                            00020001
RPECLASS DS    0XL9                 CLASS LEN/NAME                      00030007
RPECLASL DS    X                    CLASS NAME                          00040007
RPECLASN DS    XL8                  CLASS NAME                          00050007
RPEENTTY DS    0XL45                ENTITY LENGTH/NAME                  00060007
RPEENTYL DS    X                    ENTITY LENGTH                       00070007
RPEENTYN DS    XL44                 ENTITY                              00080007
RPEUACC  DS    X                    UNIVERSAL ACCESS                    00090001
RPEACCPT DS    F                    POINTER TO RPE PERMITS              00100008
RPEEND   DS    0F                   END OF FIXED PART                   00110002
RPEL     EQU   *-RPECBLK                                                00120004
*                                                                       00130000
RPEACCLE DSECT ,                    ACCESS LIST                         00140000
RPEANEXT DS    F                    NEXT RPE                            00150008
RPEAUSR  DS    CL8                  USER/GROUP ID                       00160008
RPEACS   DS    BL1                  ACCESS AUTHORITY                    00170000
         DS    0F                                                       00180009
RPEACCLN EQU   *-RPEACCLE                                               00190000
*                                                                       00200004
RACFALTR EQU   X'80'                ALTER AUTHORITY                     00210004
RACFCNTL EQU   X'40'                CONTROL AUTHORITY                   00220006
RACFUPDT EQU   X'20'                UPDATE AUTHORITY                    00230006
RACFREAD EQU   X'10'                READ AUTHORITY                      00240006
RACFEXEC EQU   X'09'                EXEC AUTHORITY                      00250004
RACFNONE EQU   X'01'                NONE AUTHORITY                      00260006
++MAC(CJYRCVTD) DISTLIB(AMACLIB)  SYSLIB(MACLIB).
CJYRCVTD DSECT                                                          00010001
CJYUSERS DS    F                    POINTER TO USERIDS                  00020000
CJYPROFS DS    F                    POINTER TO PROFILES                 00030002
CJYRL    EQU   *-CJYRCVTD                                               00040001
++MAC(CJYUCBLK) DISTLIB(AMACLIB)  SYSLIB(MACLIB).
CBLK     DSECT                                                          00010000
CBLKNEXT DS    F                  NEXT CBLK                             00020002
CBLKGRPS DS    F                  POINTER TO GROUPS                     00030002
CBLKUSER DS    0CL9               USERID                                00040002
CBLKUSRL DS    X                  USERID LENGTH                         00050002
CBLKUSRI DS    CL8                USERID NAME                           00060002
CBLKGRP  DS    0CL9               GROUP                                 00070002
CBLKGRPL DS    X                  GROUP LENGTH                          00080002
CBLKGRPN DS    CL8                GROUP NAME                            00090002
CBLKPSWD DS    0CL9               PASSWORD                              00100002
CBLKPWDL DS    X                  PASSWORD LENGTH                       00110002
CBLKPWDE DS    CL8                PASSWORD ENCRYPTED                    00120002
CBLKFLG1 DS    C                  OPER AUTHORITY                        00130002
         DS    0F                 END THE BLOCK                         00140002
CBLKL    EQU   *-CBLK                                                   00150002
*                                                                       00160000
CONNGRUP DSECT ,                   CONNECTS                             00170002
CONNNEXT DS    F                   NEXT CONNECT                         00180002
CONNGRP  DS    0CL9                GROUP                                00190002
CONNGRPL DS    X                   GROUP LENGTH                         00200002
CONNGRPN DS    CL8                 GROUP NAME                           00210002
         DS    0F                                                       00220000
CONNL    EQU   *-CONNGRUP                                               00230001
++MAC(IEZCTGFL) DISTLIB(AMACLIB)  SYSLIB(MACLIB).
*  %GO TO CTGFLX01;
*  /*
         MACRO
         IEZCTGFL &DSECT=YES
         AIF   ('&DSECT' EQ 'NO').NODS
CTGFL    DSECT ,
         AGO   .BOTHDS
.NODS    ANOP  ,
CTGFL    DS    0F
.BOTHDS  ANOP  ,                        */
*%CTGFLX01:;
*/********************************************************************/
*/*                                                                  */
*/*   MACRO NAME = IEZCTGFL                                          */
*/*                                                                  */
*/*   DESCRIPTIVE NAME = CATALOG FIELD PARAMETER LIST                */
*/*                                                                  */
*/*   FUNCTION = THE FIELD PARAMETER LIST (CTGFL) DEFINES ONE OF     */
*/*              THE CATALOG RECORD'S FIELDS OR A GROUP OF           */
*/*              LOGICALLY RELATED FIELDS.  THE CTGFL IS BUILT       */
*/*              BEFORE AN OS/VS COMPONENT ISSUES THE CATLG          */
*/*              MACRO INSTRUCTION (SVC 26) TO PROCESS A             */
*/*              CATALOG RECORD.  THE CTGFL IS USED IN TWO           */
*/*              SITUATIONS:                                         */
*/*                (1)  IT IDENTIFIES CATALOG RECORD INFORMATION     */
*/*                     TO RETRIEVE OR UPDATE.  THE CTGPL CONTAINS   */
*/*                     THE ADDRESS OF EACH CTGFL USED IN THIS WAY.  */
*/*                (2)  IT IDENTIFIES CATALOG RECORD INFORMATION     */
*/*                     TO COMPARE AGAINST CALLER-SUPPLIED DATA.     */
*/*                     THIS IS A "TEST" CTGFL AND IS ADDRESSED BY   */
*/*                     ANOTHER CTGFL.                               */
*/*                                                                  */
*/*   CONTROL BLOCK STRUCTURE = THE CTGFL IS POINTED TO BY THE       */
*/*                             CCA ('CCAFLPT' OR 'CCATEST').        */
*/*                                                                  */
*/*   INCLUDED MACROS = NONE                                         */
*/*                                                                  */
*/*   METHOD OF ACCESS = PL/S - NO DECLARES NECESSARY                */
*/*                                                                  */
*/*   STATUS = VS/2 RELEASE 3   (CHANGE FLAG @Y30SSXX)               */
*/*                                                                  */
*/*   DATE OF LAST CHANGE = 17 JUL 74                                */
*/*                                                                  */
*/********************************************************************/
         AGO   .CTGFL01                 */
*%DECLARE (CTGFLLEN, CTGFL999, CTGFLLVL) CHAR;
*%CTGFLLEN = 'LENGTH(CTGFL)';       /* LENGTH OF CTGFL               */
*%GOTO CTGFLX03;
* /*
*%CTGFLX03:;
*%IF CTGFL999 ¬= ','                /* IF BLOCK NOT CONTINUED,       */
*  %THEN %CTGFL999 = ';';           /*   THEN CLOSE DECLARE STMNT    */
*%IF CTGFLLVL  = ''                 /* IF BLOCK NOT CONCATENATED,    */
*  %THEN %GOTO CTGFL001;            /*   THEN GENERATE DECLARE       */
*%CTGFLDUM = CTGFLLVL||' CTGFL';    /* SET MINOR LEVEL NUMBER        */
*   CTGFLDUM                        /* CTGFL CONCATENATED LEVEL      */
*%GOTO CTGFL002;                    /* SKIP DECLARE                  */
*%CTGFL001:;                        /* DECLARE                       */
    DECLARE
*     1 CTGFL BASED(CTGFLPTR)       /* DECLARE CTGFL LEVEL ONE       */
*%CTGFL002:;                        /* SKIP DECLARE                  */
*        BDY(WORD),                 /* WORD BOUNDARY                 */
*       5 CTGFLDNO PTR(8),          /* NO. PAIRS DATA LENGTH/ADDR    */
*       5 CTGFLDCD BIT(8),          /* TEST CONDITION                */
*       5 CTGFLDGC PTR(8),          /* GROUP CODE NUMBER             */
*       5 CTGFLDRE BIT(8),          /* TEST RESULT                   */
*         10 *        BIT(7),       /* RESERVED                      */
*         10 CTGFLDTS BIT(1),       /* 0 = SUCCESSFUL TEST,          */
*                                   /* 1 = TEST FAILED               */
*       5 CTGFLDWA BIT(32),         /* CATALOG WORK AREA             */
*       5 CTGFLDNM PTR(31),         /* FIELD NAME ADDRESS            */
*       5 CTGFLCHN PTR(31),         /* ADDR OF NEXT FIELD MACRO      */
*                                   /* OR ZERO                       */
*       5 CTGFLDAT(*),              /* PAIRS OF DATA LENGTH/ADDR     */
*         10 CTGFLNG FIXED(31),     /* DATA LENGTH                   */
*         10 CTGFLPT  PTR(31) CTGFL999 /* DATA ADDRESS               */
*/********************************************************************/
*/*     CONSTANTS USED TO SET AND/OR TEST FIELDS DECLARED ABOVE      */
*/********************************************************************/
    DECLARE                         /* TEST CONDITION - CTGFLDCD     */
      CTGFLDEQ BIT(8) CONSTANT('80'X), /* EQUAL                      */
      CTGFLDNE BIT(8) CONSTANT('60'X), /* NOT EQUAL                  */
      CTGFLDGT BIT(8) CONSTANT('20'X), /* GREATER THAN               */
      CTGFLDLT BIT(8) CONSTANT('40'X), /* LESS THAN                  */
      CTGFLDGE BIT(8) CONSTANT('A0'X), /* GREATER THAN OR EQUAL      */
      CTGFLDLE BIT(8) CONSTANT('C0'X), /* LESS THAN OR EQUAL         */
      CTGFLDZ  BIT(8) CONSTANT('80'X), /* TEST UNDER MASK FOR ZEROES */
      CTGFLDON BIT(8) CONSTANT('10'X), /* TEST UNDER MASK FOR ONES   */
      CTGFLDMX BIT(8) CONSTANT('40'X); /* TEST UNDER MASK FOR MIXED  */
* %GO TO CTGFLX02;
*/*
.CTGFL01 ANOP
*
CTGFLDNO DS    XL1                      NUMBER PAIRS DATA LNG/ADDR
*
CTGFLDCD DS    XL1                      TEST CONDITION
CTGFLDEQ EQU   X'80'                    EQUAL
CTGFLDNE EQU   X'60'                    NOT EQUAL
CTGFLDGT EQU   X'20'                    GREATER THAN
CTGFLDLT EQU   X'40'                    LESS THAN
CTGFLDGE EQU   X'A0'                    GREATER THAN OR EQUAL
CTGFLDLE EQU   X'C0'                    LESS THAN OR EQUAL
CTGFLDZ  EQU   X'80'                    TEST UNDER MASK FOR ZEROES
CTGFLDON EQU   X'10'                    TEST UNDER MASK FOR ONES
CTGFLDMX EQU   X'40'                    TEST UNDER MASK FOR MIXED
CTGFLDGF EQU   X'FF'                    GENERIC FILTER TEST        @SCA
*
CTGFLDGC DS    XL1                      GROUP CODE NUMBER
*
CTGFLDRE DS    XL1                      TEST RESULT
CTGFLDTS EQU   X'01'                    TEST MASK - CTGFLDRE
*                                       = 0 - SUCCESSFUL TEST,
*                                       = 1 - TEST FAILED
*
CTGFLDWA DS    F                        CATALOG WORK AREA
*
CTGFLDNM DS    A                        FIELD NAME ADDRESS
*
CTGFLCHN DS    A                        ADDRESS OF NEXT FIELD
*
*
CTGFLDAT DS    0CL8                     PAIRS OF DATA LENGTH/ADDRESS
CTGFLNG  DS    F                        DATA LENGTH
CTGFLPT  DS    A                        DATA ADDRESS
*
*
CTGFLLEN EQU   *-CTGFL                  LENGTH OF CTGFL
         MEND  ,                        */
* %CTGFLX02:;
++MAC(IEZCTGPL) DISTLIB(AMACLIB)  SYSLIB(MACLIB).
* %GOTO CTGPLX01; /*                                                    00050000
         MACRO                                                          00100000
         IEZCTGPL ,                                                     00150000
         DSECT ,                        */                              00160000
* %CTGPLX01:;                                                           00200000
*/********************************************************************/ 03000000
*/*                                                                  */ 03001000
*/*   MACRO NAME = IEZCTGPL                                          */ 03002000
*/*                                                                  */ 03003000
*/*   DESCRIPTIVE NAME = CATALOG PARAMETER LIST                      */ 03004000
*/*                                                                  */ 03005000
*/*   FUNCTION = THE CATALOG PARAMETER LIST (CTGPL) DEFINES THE      */ 03006000
*/*              CATALOG MANAGEMENT REQUEST AND ITS OPTIONS, THE     */ 03007000
*/*              CATALOG RECORD TO BE PROCESSED, AND THE VSAM        */ 03008000
*/*              CATALOG THAT CONTAINES THE RECORD.  THE CTGPL IS    */ 03009000
*/*              BUILT BEFORE AN OS/VS COMPONENT ISSUES THE CATLG    */ 03010000
*/*              MACRO INSTRUCTION (SVC 26) TO PROCESS A CATALOG     */ 03011000
*/*              RECORD.  WHEN THE CATALOG MANAGEMENT ROUTINES       */ 03012000
*/*              BUILD A CCA TO SUPPORT THE REQUEST, THE ADDRESS     */ 03013000
*/*              OF THE CTGPL IS PUT INTO THE CCA (CCACPL).          */ 03014000
*/*                                                                  */ 03015000
*/*   CONTROL BLOCK STRUCTURE = THE CTGPL IS POINTED TO BY           */ 03016000
*/*                             REGISTER 1.                          */ 03017000
*/*                                                                  */ 03018000
*/*   INCLUDED MACROS = NONE                                         */ 03019000
*/*                                                                  */ 03020000
*/*   METHOD OF ACCESS = PL/S - NO DECLARES NECESSARY                */ 03021000
*/*                                                                  */ 03022000
*/*   STATUS = VS/2 RELEASE 4   (CHANGE FLAG @Z40WSXX)       @Z40WSSG*/ 03023000
*/*   A 343000 A 346000                                      @ZA18274*/ 03024000
*/*                                                                  */ 03024600
*/*   DATE OF LAST CHANGE = 03 DEC 76                        @ZA18274*/ 03025200
*/*   JES3 SUPPORT - I 850084,892500,895500                          */ 03025300
*/*                                                              @HES*/ 03026000
*/********************************************************************/ 03027000
*%GOTO CTGPLX03;                                                        03077000
*/*                                                                     03200000
         AGO   .CTGPL01                 */                              03250000
*%CTGPLX03:;                                                            03300000
*%DECLARE (CTGPLLEN, CTGPL999, CTGPLLVL) CHAR;                          04000000
*%CTGPLLEN = 'LENGTH(CTGPL)';       /* LENGTH OF CTGPL               */ 04500000
*%IF CTGPL999 ^= ','                /* IF BLOCK NOT CONTINUED,       */ 05000000
*   %THEN %CTGPL999 = ';';          /*   THEN CLOSE DCL STATEMENT    */ 06000000
*%IF CTGPLLVL  = ''                 /* IF BLOCK NOT CONCATENATED,    */ 07000000
*   %THEN %GOTO CTGPL001;           /*   THEN GENERATE DCL           */ 08000000
*%CTGPLDUM = CTGPLLVL||' CTGPL';    /* SET MINOR LEVEL NUMBER        */ 09000000
*        CTGPLDUM                   /* CTGPL CONCATENATED LEVEL      */ 10000000
*%GOTO CTGPL002;                    /* SKIP DECLARE                  */ 11000000
*%CTGPL001:;                        /* DECLARE                       */ 12000000
*   DECLARE                                                             12500000
*     1 CTGPL BASED(CTGPLPTR)       /* DECLARE CTGPL LEVEL ONE       */ 13000000
*%CTGPL002:;                        /* SKIP DECLARE                  */ 14000000
*        BDY(WORD),                 /* WORD BOUNDARY                 */ 15000000
*       5 CTGOPTN1 BIT(8),          /* FIRST OPTION INDICATOR        */ 16000000
*         10 CTGBYPSS BIT(1),       /* BYPASS                        */ 17000000
*         10 CTGMAST  BIT(1),       /* VERIFY MASTER PASSWORD        */ 18000000
*         10 CTGCI    BIT(1),       /* VERIFY CONTROL INTERVAL       */ 19000000
*         10 CTGUPD   BIT(1),       /* VERIFY UPDATE                 */ 20000000
*         10 CTGREAD  BIT(1),       /* VERIFY READ                   */ 21000000
*         10 CTGNAME  BIT(1),       /* 1 = 44-BYTE NAME OR VOL SER,  */ 22000000
*                                   /* 0 = ENTRY ID NUMBER           */ 23000000
*         10 CTGCNAME BIT(1),       /* 1 = 44-BYTE NAME,             */ 24000000
*                                   /* 0 = ACB ADDRESS               */ 25000000
*         10 CTGGENLD BIT(1),       /* GENERIC LOCATE REQUEST  Y02020*/ 26000000
*       5 CTGOPTN2 BIT(8),          /* SECOND OPTION INDICATOR       */ 27000000
*         10 CTGEXT   BIT(1),       /* EXTEND       (UPDATE)         */ 28000000
*            15 CTGNSVS BIT(1),     /* CATLG CLEANUP REQUEST @ZA00605*/ 28050000
*         10 CTGERASE BIT(1),       /* ERASE        (DELETE)         */ 29000000
*            15 CTGSMF   BIT(1),    /* WRITE SMF    (LSPACE)         */ 30000000
*               20 CTGREL   BIT(1), /* RELEASE      (UPDATE)         */ 31000000
*                  25 CTGGTALL BIT(1),/* CONCAT SEARCH  FOR    Y02020*/ 31050000
*                                   /* (LISTCAT)               Y02020*/ 31100000
*         10 CTGPURG  BIT(1),       /* PURGE        (DELETE)         */ 32000000
*           15 CTGVMNT BIT(1),      /* VOLUME MOUNT CALLER           */ 33000000
*              20 CTGRCATN BIT(1),  /* RTN CATLG NAME(GLOC)    Y02020*/ 33010000
*                 25 CTGSWAP BIT(1),/* SWAPSPACE (DEFINE)    @Z40WSSG*/ 33050000
*         10 CTGGTNXT BIT(1),       /* GET NEXT     (LIST CATALOG)   */ 34000000
*           15 CTGUCRAX BIT(1),     /* UCRA EXTEND OPTION    @ZA18274*/ 34300000
*                                   /* (WITH UPDATE)                 */ 34600000
*         10 CTGDISC BIT(1),        /* DISCONNECT   (DELETE)         */ 35000000
*         10 CTGOVRID BIT(1),       /* ERASE OVERRIDE (DELETE)       */ 36000000
*         10 CTGSCR   BIT(1),       /* SCRATCH SPACE (DELETE)        */ 37000000
*         10 *        BIT(1),       /* RESERVED                      */ 38000000
*       5 CTGOPTN3 BIT(8),          /* THIRD OPTION INDICATOR        */ 39000000
*         10 CTGFUNC  BIT(3),       /* CATALOG FUNCTION              */ 40000000
*         10 CTGSUPLT BIT(1),       /* SUPER LOCATE                  */ 45000000
*         10 CTGGDGL  BIT(1),       /* GDG LOCATE REQUEST      Y02020*/ 46000000
*                                   /* WITH BASE LEVEL GIVEN   Y02020*/ 46100000
*                                   /* (CTGWAGB IN CTGWA)      Y02020*/ 46150000
*         10 CTGSRH   BIT(1),       /* 0 = SEARCH MASTER CATLG Y02020*/ 47000000
*                                   /*     ONLY                Y02020*/ 47050000
*                                   /* 1 = SEARCH OS CATALOG FIRST   */ 48000000
*         10 CTGNUM   BIT(1),       /* 0 = SEARCH BOTH CATALOGS,     */ 49000000
*                                   /* 1 = SEARCH ONE CATALOG        */ 50000000
*         10 CTGAM0   BIT(1),       /* VSAM REQ VERSUS NONVSAM       */ 51000000
*       5 CTGOPTN4 BIT(8),          /* GDG FLAGS                     */ 52000000
*         10 CTGLBASE BIT(1),       /* LOCATE GDG BASE ONLY Y02020   */ 52050000
*         10 CTGDOCAT BIT(1),       /* DO NOT OPEN NEEDED CATALOG    */ 52100000
*         10 CTGNPROF BIT(1),       /* NO (RAC) PROFILE SHOULD BE       52150000
*                                      DEFINED OR DELETED    @Z40RSRC*/ 52170000
*         10 CTGCOIN  BIT(1),       /* CONTROLLER INTERCEPT REQUESTED   52190000
*                                                            @ZA20773*/ 52250000
*         10 CTGBYPMT BIT(1),       /* BYPASS SECURITY PROMPTING        52300000
*                                       TO SYSTEM OPERATOR   @ZA07531*/ 52350000
*         10 CTGTIOT  BIT(1),       /* CALLER OWNS SYSZTIOT EXCLUSIVE   52400000
*                                                            @ZA20773*/ 52410000
*         10 *        BIT(2),       /* RESERVED              @ZA20773*/ 52460000
*       5 CTGENT   PTR(31),         /* USER ENTRY ADDR OR PTR TO VOLUME 53000000
*                                        SERIAL NUMBER (LSPACE)      */ 54000000
*         10 CTGFVT   PTR(31),      /* FVT ADDRESS (DEFINE, ALTER)   */ 55000000
*       5 CTGCAT   PTR(31),         /* CATALOG POINTER               */ 56000000
*         10 CTGCVOL PTR(31),       /* CVOL PTR (SUPER LOCATE)       */ 57000000
*       5 CTGWKA  PTR(31),          /* WORKAREA ADDR                 */ 58000000
*       5 CTGDSORG CHAR(2),         /* DATA SET ORG - SUPERLOCATE    */ 59000000
*         10 CTGOPTNS BIT(5),       /* CMS OPTIONS                   */ 60000000
*         10 *        BIT(4),       /* RESERVED                  @HES*/ 60050000
*         10 CTGHDLET BIT(1),       /* HSM HAS DELETED A MIGRATED       60100000
*                                      DATA SET                  @HES*/ 60150000
*         10 *        BIT(6),       /* RESERVED                  @HES*/ 60200000
*       5 CTGTYPE  CHAR(1),         /* ENTRY TYPE - LISTCAT, DELETE  */ 66000000
*       5 CTGNOFLD PTR(8),          /* NUMBER OF FIELD POINTERS      */ 74000000
*       5 CTGDDNM PTR(31),          /* DD NAME ADDR                  */ 75000000
*         10 CTGNEWNM PTR(31),      /* NEWNAME ADDRESS - ALTER       */ 76000000
*            15 CTGFDBK  PTR(16),   /* SUPER LOCATE FEEDBACK         */ 77000000
*            15 CTGFBFLG BIT(16),   /* SUPER LOCATE FLAGS            */ 78000000
*               20 CTGPAR   BIT(1), /* PARALLEL MOUNT -SUPERLOCATE   */ 79000000
*               20 CTGKEEP  BIT(1), /* FORCED KEEP - SUPERLOCATE     */ 80000000
*               20 CTGGDGB  BIT(1), /* GDG BASE LOCATE         Y02020*/ 81000000
*               20 CTGNGDSN BIT(1), /* GDG NAME GENERATED      Y02020*/ 81050000
*               20 CTGCLV   BIT(1), /* CANDIDATE VOLUME LIST @ZA76423*/ 81100000
*               20 *        BIT(11), /* RESERVED             @ZA76423*/ 81150000
*       5 CTGJSCB  PTR(31),         /* JSCB ADDR                     */ 82000000
*         10 CTGPSWD  PTR(31),      /* PASSWORD ADDR                 */ 83000000
*       5 CTGFIELD(*) PTR(31) CTGPL999 /* FIELD POINTERS             */ 84000000
*/********************************************************************/ 85000000
*/*     CONSTANTS USED TO SET AND/OR TEST FIELDS DECLARED ABOVE      */ 85000200
*/********************************************************************/ 85000400
*   DECLARE                         /* CATALOG FUNCTION - CTGFUNC    */ 85000600
*     CTGLOC   BIT(3) CONSTANT('001'B), /* LOCATE                    */ 85000900
*     CTGLSP   BIT(3) CONSTANT('010'B), /* LSPACE                    */ 85001200
*     CTGUPDAT BIT(3) CONSTANT('011'B), /* UPDATE                    */ 85001500
*     CTGCMS   BIT(3) CONSTANT('100'B); /* CMS FUNCTION              */ 85001800
*   DECLARE                         /* CMS OPTIONS - CTGOPTNS        */ 85002100
*     CTGDEFIN BIT(5) CONSTANT('00001'B), /* DEFINE                  */ 85002400
*     CTGALTER BIT(5) CONSTANT('00010'B), /* ALTER                   */ 85002700
*     CTGDELET BIT(5) CONSTANT('00011'B), /* DELETE                  */ 85003000
*     CTGLTCAT BIT(5) CONSTANT('00100'B), /* LIST CATALOG            */ 85003300
*     CTGCNVTV BIT(5) CONSTANT('00110'B); /* CONVERTV        @Y30LSPS*/ 85003600
*   DECLARE                           /* RECORD ENTRY TYPE - CTGTYPE */ 85003900
*     CTGTDATA CHAR(1) CONSTANT('D'), /* DATA                        */ 85004200
*     CTGTINDX CHAR(1) CONSTANT('I'), /* INDEX                       */ 85004500
*     CTGTALIN CHAR(1) CONSTANT('A'), /* ALIEN                       */ 85004800
*     CTGTUCAT CHAR(1) CONSTANT('U'), /* USER CATALOG                */ 85005100
*     CTGTVOL  CHAR(1) CONSTANT('V'), /* VOLUME                      */ 85005400
*     CTGTCL   CHAR(1) CONSTANT('C'), /* CLUSTER                     */ 85005700
*     CTGTAIX  CHAR(1) CONSTANT('G'), /* ALTERNATE INDEX     @Y30SSPJ*/ 85006000
*     CTGTPATH CHAR(1) CONSTANT('R'), /* PATH                @Y30SSPJ*/ 85006300
*     CTGTFREE CHAR(1) CONSTANT('F'), /* FREE                @Y30SSPJ*/ 85006600
*     CTGTPTH  CHAR(1) CONSTANT('R'), /* PATH                @Y30SSSB*/ 85006900
*     CTGTUPG  CHAR(1) CONSTANT('Y'), /* UPGRADE             @Y30SSSB*/ 85007200
*     CTGTGBS  CHAR(1) CONSTANT('B'), /* GDG BASE              Y02020*/ 85007500
*     CTGTANM  CHAR(1) CONSTANT('X'), /* ALIAS NAME            Y02020*/ 85007800
*     CTGTPGSP CHAR(1) CONSTANT('P'), /* PAGE SPACE            Y02020*/ 85008100
*     CTGTMCAT CHAR(1) CONSTANT('M'), /* MASTER CATALOG              */ 85008400
*     CTGTJES3 BIT(8) CONSTANT('01'X);/* JES3 ORIGINATED, SUPERLOCATE   85008500
*                                        REQUEST                     */ 85008600
*/********************************************************************/ 85008900
*/*                 PROBLEM  DETERMINATION  FIELDS                   */ 85009000
*/********************************************************************/ 85009300
*   DECLARE                                                             85012000
*     1 * DEF(CTGDDNM),             /* PROBLEM DETERMINATION @Y30SSJG*/ 85013000
*       2 CTGPROB CHAR(4),          /* PROBLEM DETERMINATION @Y30SSJG*/ 85014000
*         3 CTGMODID CHAR(2),       /* MODULE IDENTIFICATION @Y30SSJG*/ 85015000
*         3 CTGREASN CHAR(2),       /* REASON CODE           @Y30SSJG*/ 85016000
*           4 CTGREAS1 CHAR(1),     /* HIGH ORDER BYTE ZERO  @Y30SSJG*/ 85017000
*           4 CTGREAS2 CHAR(1);     /* REASON CODE LOW BYTE  @Y30SSJG*/ 85018000
* %GOTO CTGPLX02;                                                   /*  85050000
.CTGPL01 ANOP                                                           85100000
*                                                                       85150000
CTGPL    DS    0H                                                       85250000
*                                                                       85260000
CTGOPTN1 DS    XL1                      FIRST OPTION INDICATOR          85300000
CTGBYPSS EQU   X'80'                    BYPASS                          85350000
CTGMAST  EQU   X'40'                    VERIFY MASTER PASSWORD          85400000
CTGCI    EQU   X'20'                    VERIFY CONTROL INDICATOR        85450000
CTGUPD   EQU   X'10'                    VERIFY UPDATE                   85500000
CTGREAD  EQU   X'08'                    VERIFY READ                     85550000
CTGNAME  EQU   X'04'                    1 - 44-BYTE NAME OR VOLSER      85600000
*                                       0 - ENTRY ID NUMBER             85650000
CTGCNAME EQU   X'02'                    1 - 44-BYTE NAME                85700000
*                                       0 - ACB ADDRESS                 85750000
CTGGENLD EQU   X'01'                    GENERIC LOCATE REQUEST          85800000
*                                                                       85850000
CTGOPTN2 DS    XL1                      SECOND OPTION INDICATOR         85900000
CTGEXT   EQU   X'80'                    EXTEND(UPDATE)                  85950000
CTGERASE EQU   X'40'                    ERASE(DELETE)                   86000000
CTGSMF   EQU   X'40'                    WRITE SMF(LSPACE)               86050000
CTGREL   EQU   X'40'                    RELEASE(UPDATE)                 86100000
CTGGTALL EQU   X'40'                    CONCAT SEARCH (LISTCAT) Y02020  86150000
CTGPURG  EQU   X'20'                    PURGE (DELETE)                  86200000
CTGVMNT  EQU   X'20'                    VOLUME MOUNT CALLER             86250000
CTGRCATN EQU   X'20'                    RTN CAT NAME(GLOC)      Y02020  86300000
CTGGTNXT EQU   X'10'                    GET NEXT (LIST CTLG)            86350000
CTGDISC  EQU   X'08'                    DISCONNECT (DELETE)             86400000
CTGOVRID EQU   X'04'                    ERASE OVERRIDE (DELETE)         86450000
CTGSCR   EQU   X'02'                    SCRATCH SPACE (DELETE)          86500000
*    X'01' - RESERVED                                                   86550000
*                                                                       86600000
CTGOPTN3 DS    XL1                      THIRD OPTION INDICATOR          86650000
CTGFUNC  EQU   X'E0'                    HIGH ORDER THREE BITS DEFINE    86700000
*                                       FUNCTION                        86710000
*   LOCATE     -   001* ****                                            86760000
CTGLOC   EQU   X'20'                    LOCATE - BITS ON                86800000
*   LSPACE     -   010* ****                                            86900000
CTGLSP   EQU   X'40'                    LSPACE - BITS ON                86910000
*   UPDATE     -   011* ****                                            86950000
CTGUPDAT EQU   X'60'                    UPDATE - BITS ON                86960000
*   CMS FUNCTION - 100* ****                                            87000000
CTGCMS   EQU   X'80'                    CMS FUNCTION - BITS ON          87050000
*                                                                       87150000
CTGSUPLT EQU   X'10'                    SUPER LOCATE                    87200000
CTGGDGL  EQU   X'08'                    GDG LOCATE FUNCTION (CTGWAGB IN 87250000
*                                       CTGWA)                          87300000
CTGSRH   EQU   X'04'                    0 - SEARCH MASTER CAT ONLY      87350000
*                                       1 - SEARCH OS CAT FIRST         87400000
CTGNUM   EQU   X'02'                    0 - SEARCH BOTH CATALOGS        87450000
*                                       1 - SEARCH ONE CATALOG          87500000
CTGAM0   EQU   X'01'                    VSAM REQ VERSUS NONVSAM         87550000
*                                                                       87600000
CTGOPTN4 DS    XL1                      FOURTH OPTION INDICATOR  Y02020 87650000
CTGLBASE EQU   X'80'                    LOCATE GDG BASE ONLY            87750000
CTGDOCAT EQU   X'40'                    DO NOT OPEN NEEDED CATLG        87800000
CTGNPROF EQU   X'20'                    NO (RAC) PROFILE SHOULD BE      87810000
*                                       DEFINED OR DELETED     @Z40RSRC 87820000
CTGCOIN  EQU   X'10'                    CONTROLLER INTERCEPT REQUESTED  87860000
*                                                              @ZA20773 87870000
CTGBYPMT EQU   X'08'                    BYPASS SECURITY PROMPTING TO    87880000
*                                       SYSTEM OPERATOR        @ZA07531 87890000
CTGTIOT  EQU   X'04'                    CALLER OWNS SYSZTIOT EXCLUSIVE  87900000
*                                                              @ZA20773 87910000
*        BITS 7-8 RESERVED                                     @ZA20773 87920000
CTGENT   DS    0A                       USER ENTRY ADDRESS OR POINTER   87950000
*                                       TO VOLUME SERIAL NUMBER(LSPACE) 88000000
CTGFVT   DS    A                        FVT ADDRESS (DEFINE, ALTER)     88050000
CTGCAT   DS    0A                       CATALOG POINTER                 88100000
*                                                                       88110000
CTGCVOL  DS    A                        CVOL PTR (SUPER LOCATE)         88150000
*                                                                       88160000
CTGWKA   DS    A                        WORKAREA ADDRESS                88200000
*                                                                       88210000
CTGDSORG DS    CL2                      DATA SET ORG (SUPER LOCATE)     88250000
*   BITS 0-4 DEFINE ORGANIZATION                                        88300000
CTGOPTNS EQU   X'F8'                    TOP 5 BITS                      88310000
*   DEFINE          - 0000 1*** **** ****                               88350000
CTGDEFIN EQU   X'08'                    DEFINE                          88360000
*   ALTER           - 0001 0*** **** ****                               88400000
CTGALTER EQU   X'10'                    ALTER                           88410000
*   DELETE          - 0001 1*** **** ****                               88450000
CTGDELET EQU   X'18'                    DELETE                          88460000
*   LIST CATALOG    - 0010 0*** **** ****                               88500000
CTGLTCAT EQU   X'20'                    LIST CATALOG                    88550000
*   CONVERTV        - 0011 0*** **** ****                               88600000
CTGCNVTV EQU   X'30'                    CONVERTV                        88610000
*   BITS 6-16 RESERVED                                                  88650000
*                                                                       88700000
CTGTYPE  DS    CL1                      ENTRY TYPE-LISTCAT,DELETE       88750000
CTGTDATA EQU   C'D'                     DATA - D                        88800000
CTGTINDX EQU   C'I'                     INDEX - I                       88850000
CTGTALIN EQU   C'A'                     ALIEN - A                       88900000
CTGTUCAT EQU   C'U'                     USER CATALOG - U                88950000
CTGTVOL  EQU   C'V'                     VOLUME - V                      89000000
CTGTCL   EQU   C'C'                     CLUSTER - C                     89050000
CTGTMCAT EQU   C'M'                     MASTER CATALOG - M              89100000
CTGTGBS  EQU   C'B'                     GDG BASE - B                    89150000
CTGTANM  EQU   C'X'                     ALIAS BASE -X                   89200000
CTGTPGSP EQU   C'P'                     PAGE SPACE - P                  89250000
CTGTJES3 EQU   X'01'                    JES3 SUPERLOCATE REQUEST        89270000
*                                                                       89300000
CTGNOFLD DS    XL1                      NUMBER FIELD POINTERS           89350000
CTGDDNM  DS    0A                       DD NAME ADDRESS                 89400000
CTGNEWNM DS    0A                       NEWNAME ADDRESS - ALTER         89450000
CTGFDBK  DS    XL2                      SUPER LOCATE FEEDBACK           89500000
CTGFBFLG DS    0XL2                     SUPER LOCATE FLAGS              89550000
CTGREAS1 DS    XL1                      HIGH ORDER BYTE ZERO            89560000
CTGPAR   EQU   X'80'                    PARALLEL MOUNT - SUPER LOC      89600000
CTGKEEP  EQU   X'40'                    FORCED KEEP = SUPER LOCATE      89650000
CTGGDGB  EQU   X'20'                    GDG BASE LOCATED                89700000
CTGNGDSN EQU   X'10'                    GDG NAME GENERATED              89750000
CTGCLV   EQU   X'08'                    CANDIDATE VOLUME LIST  @ZA76423 89770000
*        6-8  RESERVED                                         @ZA76423 89790000
CTGREAS2 DS    XL1                      REASON CODE LOW BYTE            89810000
*                                                                       89850000
CTGJSCB  DS    0A                       JSCB ADDRESS                    89900000
CTGPSWD  DS    A                        PASSWORD ADDRESS                89950000
CTGFIELD DS    A                        FIELD POINTERS - MAY BE MORE    90000000
*                                       THAN ONE                        90050000
CTGPLLEN EQU   *-CTGPL                  LENGTH OF CTG WITH ONE FIELD    90100000
*                                       POINTER                         90150000
         MEND                                                           90200000
* */ %CTGPLX02:;                                                        90250000
++MAC(YREGS) DISTLIB(AMACLIB)  SYSLIB(MACLIB).
         MACRO                                                          00010000
         YREGS &DUMMY                                                   00020000
         GBLA  &REGS                                                    00030000
&REGS    SETA  1                                                        00040000
         SPACE 1                                                        00050000
R0       EQU   0                                                        00060000
R1       EQU   1                                                        00070000
R2       EQU   2                                                        00080000
R3       EQU   3                                                        00090000
R4       EQU   4                                                        00100000
R5       EQU   5                                                        00110000
R6       EQU   6                                                        00120000
R7       EQU   7                                                        00130000
R8       EQU   8                                                        00140000
R9       EQU   9                                                        00150000
R10      EQU   10                                                       00160000
R11      EQU   11                                                       00170000
R12      EQU   12                                                       00180000
R13      EQU   13                                                       00190000
R14      EQU   14                                                       00200000
R15      EQU   15                                                       00210000
         SPACE 1                                                        00220000
         MEND                                                           00230000
++SRC(CJYRCVT) DISTLIB(ASRCLIB)  SYSLIB(SRCLIB).
         TITLE 'INITIALIZE OUR RACF CVT'                                00000123
* CPARM='XREF(SHORT),OBJ,NODECK',LPARM='AC=1'                 CJYRCVT   00000224
*      DD DSN=SYS1.MACLIB,DISP=SHR                                      00000322
*      DD DSN=SYS1.MODGEN,DISP=SHR                                      00001022
*      DD DSN=SYS3.SAF.PARMLIB,DISP=SHR                                 00010022
         EJECT                                                          00020023
CJYRCVT  CSECT                                                          00050004
*                                                                       00061025
**********************************************************************  00062025
*                                                                    *  00063025
*    COPYRIGHT (C) 1991 BY CRAIG J. YASUNA.  ALL RIGHTS RESERVED.    *  00064025
*                                                                    *  00065025
*    THIS SOFTWARE PRODUCT OR ANY OF ITS COMPONENTS MUST NOT BE      *  00066025
*    SOLD, GIVEN, OR OTHERWISE DISTRIBUTED TO ANY OTHER COMPANY      *  00067025
*    WITHOUT THE PRIOR WRITTEN PERMISSION OF:                        *  00068025
*                                                                    *  00069025
*                                  CRAIG J. YASUNA, PRESIDENT        *  00069125
*                                  ENTERPRISE SYSTEMS GROUP          *  00069225
*                                  2 MARC COURT                      *  00069325
*                                  EDISON, NEW JERSEY 08820          *  00069425
*                                                                    *  00069525
**********************************************************************  00069625
*                                                                       00069725
         PRINT NOGEN                                                    00070001
         SAVE  (14,12),,CJYRCVT_&SYSDATE._&SYSTIME                      00080005
         LR    R12,R15             USE ENTRY AS BASE                    00090001
         USING CJYRCVT,R12         " " "                                00100004
         ST    R13,SAVEAREA+4      SAVE CALLER'S SAVE AREA ADDR         00110001
         LA    R2,SAVEAREA         MY SAVEAREA                          00120001
         ST    R2,8(R13)           SAVE MY AREA                         00130001
         LR    R13,R2              LOAD REGISTER 13 WITH SAVEAREA       00140001
*                                                                       00150001
         GETMAIN RU,LV=GMLEN,SP=245 GET SQA                             00160380
         LR    R4,R1               SAVE AREA                            00170001
         LA    R5,GMLEN            LENGTH                               00180005
         LR    R6,R4               TARGET                               00190001
         LA    R7,0                ZERO FILL                            00200001
         MVCL  R4,R6               MOVE IT                              00210001
         LR    R4,R1               BRING BACK R1                        00220001
         USING RCVT,R4             ADDRESS IT                           00230001
         MVC   RCVTID,=C'RCVT'     MOVE NAME                            00240001
         MVI   RCVTVERS,X'06'      V1R7 OF RACF                         00260380
         MVI   RCVTFLG1,RCVTTAPE TAPE                                   00270380
         MVI   RCVTSTA1,X'FF'                                           00274380
         MVI   RCVTAUOP,X'7A'                                           00278380
         LA    R2,RCVTEND          POINT TO G.M. AREA AFTER RCVT        00290009
         LA    R3,CJYRCVTL         LENGTH                               00300009
         LA    R6,CJYRCVTX         POINT TO OUR CODE                    00310009
         LR    R7,R3               LENGTH EQUAL                         00320005
         MVCL  R2,R6               MOVE IT                              00330005
         LA    R2,RCVTEND          ADDRESS OF CJYRCVT                   00340010
         ST    R2,RCVTISTL         INSTALLATION POINTER                 00350009
         LA    R2,RCVTEND+RACSTAT-CJYRCVTX CDT POINTER                  00360009
         ST    R2,RCVTREXP         RACSTAT POINTER                      00370009
         LA    R2,RCVTEND+CDT-CJYRCVTX CDT POINTER                      00380009
         ST    R2,RCVTCDTP         SAVE IT                              00390005
         LA    R2,RCVTEND+ICHAUTAB-CJYRCVTX CDT POINTER                 00400012
         ST    R2,RCVTAUTP         SAVE IT                              00410012
         L     R2,FLCCVT-PSA(0)    LOAD CVT                             00420014
         ST    R4,CVTRAC-CVTMAP(R2) SAVE IN CVT                         00430001
         WTO   'RCVT WAS PROCESSED SUCCESSFULLY'                        00440380
         L     R13,SAVEAREA+4      LOAD OLD REG 13                      00450004
         RETURN (14,12),RC=0       RETURN TO CALLER                     00460001
*                                                                       00470001
         LTORG                                                          00480001
SAVEAREA DS    18F                                                      00490002
CJYRCVTX DC    XL8'0'                                                   00505380
RACSTAT  SAVE  (14,12),,RACSTAT_&SYSDATE._&SYSTIME                      00510009
         DROP  ,                                                        00520009
         LR    R12,R15             USE ENTRY AS BASE                    00530005
         USING RACSTAT,R12         " " "                                00540005
         LA    R15,0               RC = 0                               00550005
         ICM   R2,B'1111',0(R1)    CLASS TO CHECK FOR                   00560005
         BZ    RETURN              NONE, RETURN                         00570005
         LA    R3,CDT              CDT                                  00580005
         LA    R4,CDTE#            NUMBER OF CDTS.                      00590005
CDTLOOP  CLC   0(8,R2),3(R3)       US?                                  00600007
         BE    FOUNDCDT            YES, GO ON                           00610005
         LA    R3,CDTELEN(R3)      NEXT                                 00620005
         BCT   R4,CDTLOOP          GO ON                                00630005
         LA    R15,8               SORRY                                00640005
         B     RETURN              ....                                 00650005
FOUNDCDT LA    R15,0               RACF ACTIVE, CLASS ACTIVE            00660011
         ICM   R2,B'1111',4(R1)    ENTRY                                00670008
         BZ    RETURN              NONE ...                             00680005
         ST    R3,0(R2)            SAVE CDT ADDRESS                     00690007
RETURN   RETURN (14,12),RC=(15)    BYE ...                              00700005
*                                                                       00710012
CDT      DC    X'003005',C'DASDVOL GDASDVOL',X'06E0E00078',XL24'00'     00720005
CDTELEN  EQU   *-CDT                                                    00730005
         DC    X'003012',C'FACILITY        ',X'27F0F00118',XL24'00'     00740005
         DC    X'00302D',C'TSOPROC         ',X'08C0E00110',XL24'00'     00750005
         DC    X'00302E',C'ACCTNUM         ',X'27F0F00110',XL24'00'     00760005
         DC    X'00302F',C'PERFGRP         ',X'0320200110',XL24'00'     00770005
         DC    X'003027',C'GDASDVOLDASDVOL ',X'06E0E000E0',XL24'00'     00780005
         DC    X'003028',C'PMBR    PROGRAM ',X'27C0E00100',XL24'00'     00790005
         DC    X'003029',C'PROGRAM PMBR    ',X'08C0E00180',XL24'00'     00800005
         DC    X'003030',C'TSOAUTH         ',X'08E0E00110',XL24'00'     00810005
         DC    X'003038',C'PROPCNTL        ',X'08E0E00110',XL24'00'     00820005
CDTE#    EQU   (*-CDT)/CDTELEN                                          00830005
*                                                                       00840012
ICHAUTAB DC    CL8'DSIOST',XL4'80000000'   NCCF                         00850012
         DC    CL8'DSILAR',XL4'80000000'   NCCF                         00860012
         DC    XL4'00000000'               END OF CHAIN                 00870012
*                                                                       00880012
CJYRCVTL EQU   *-CJYRCVTX                                               00890009
*                                                                       00900010
         ICHPRCVT                                                       00910001
RCVTEND  DS    0H                                                       00920005
GMLEN    EQU   *-RCVT+CJYRCVTL                                          00930009
         CVT     DSECT=YES                                              00940001
         IHAPSA  DSECT=YES                                              00950380
         IHASCVT                                                        00960014
         COPY    CJYRCVTD                                               00970010
*                                                                       00980001
R0       EQU   00                                                       00990001
R1       EQU   01                                                       01000001
R2       EQU   02                                                       01010001
R3       EQU   03                                                       01020001
R4       EQU   04                                                       01030001
R5       EQU   05                                                       01040001
R6       EQU   06                                                       01050001
R7       EQU   07                                                       01060001
R8       EQU   08                                                       01070001
R9       EQU   09                                                       01080001
R10      EQU   10                                                       01090001
R11      EQU   11                                                       01100001
R12      EQU   12                                                       01110001
R13      EQU   13                                                       01120001
R14      EQU   14                                                       01130001
R15      EQU   15                                                       01140001
         END                                                            01150001
++SRC(ICHRIN00) DISTLIB(ASRCLIB)  SYSLIB(SRCLIB).
         TITLE 'RAKF SVCs'                                              00010000
ICHRIN00 CSECT                                                          00020000
         ENTRY IGC0013A,IGC0013B,IGC0013C                               00030000
*                                                                       00040000
**********************************************************************  00050000
*                                                                    *  00060000
* NAME: ICHRIN00                                                     *  00070000
*                                                                    *  00080000
* TYPE: Assembler Source                                             *  00090000
*                                                                    *  00100000
* DESC: Stub module for RAKF SVCs                                    *  00110000
*                                                                    *  00120000
* FUNCTION: provide dummy RACF SVC 132                               *  00130000
*           provide real RAKF SVCs 130, 131, 133                     *  00140000
*                                                                    *  00150000
**********************************************************************  00160000
*                                                                       00170000
         USING ICHRIN00,R6         SCV entry R6 = EP                    00180000
         B     SVC130              continue with IGC00130               00190000
         DC    C'IGC00130 RAKF  &SYSDATE. ' eye catcher                 00200000
SVC130   L     R6,RAKF0130         address of SVC 130                   00210000
         BR    R6                  go for it                            00220000
*                                                                       00230000
         USING IGC0013A,R6         SCV entry R6 = EP                    00240000
IGC0013A B     SVC131              continue with IGC0013A               00250000
         DC    C'IGC0013A RAKF  &SYSDATE. ' eye catcher                 00260000
SVC131   L     R6,RAKF013A         address of SVC 131                   00270000
         BR    R6                  go for it                            00280000
*                                                                       00290000
         USING IGC0013B,R6         SCV entry R6 = EP                    00300000
IGC0013B B     SVC132              continue with IGC0013B (dummy)       00310000
         DC    C'ICHRIN00 DUMMY &SYSDATE. ' eye catcher                 00320000
SVC132   LA    R15,0               indicate RACLIST successful ..       00330000
         LA    R0,4                 .. but no profiles mapped           00340000
         BR    R14                 return to the caller                 00350000
*                                                                       00360000
         USING IGC0013C,R6         SCV entry R6 = EP                    00370000
IGC0013C B     SVC133              continue with IGC0013C               00380000
         DC    C'IGC0013C RAKF  &SYSDATE. ' eye catcher                 00390000
SVC133   L     R6,RAKF013C         address of SVC 133                   00400000
         BR    R6                  go for it                            00410000
*                                                                       00420000
*                                                                       00430000
RAKF0130 DC    V(RAKF0130)         SVC 130 (RACHECK)                    00440000
RAKF013A DC    V(RAKF013A)         SVC 131 (RACINIT)                    00450000
RAKF013C DC    V(RAKF013C)         SVC 133 (RACDEF)                     00460000
         YREGS                     register equates                     00470000
         END   ICHRIN00                                                 00480000
++SRC(ICHSEC00) DISTLIB(ASRCLIB)  SYSLIB(SRCLIB).
         TITLE 'RAKF Initialization Program'                            00010000
ICHSEC00 CSECT                                                          00020000
*                                                                       00030000
**********************************************************************  00040000
*                                                                    *  00050000
* NAME: ICHSEC00                                                     *  00060000
*                                                                    *  00070000
* TYPE: Assembler Source                                             *  00080000
*                                                                    *  00090000
* DESC: Initialize RAKF                                              *  00100000
*                                                                    *  00110000
* FUNCTION: - output banner to console                               *  00120000
*           - check if SAFV is already defined and initialized       *  00130000
*             o if yes: skip initialization and go directly to       *  00140000
*                       profile and user table initialization        *  00150000
*           - try to read initialization directive from              *  00160000
*             parmlib member RAKFINIT:                               *  00170000
*             o if first line of member starts with YES:    continue *  00180000
*             o if first line of member starts with NO:     exit     *  00190000
*             o else, of if member doesn't exist:                    *  00200000
*               + ask operator for permission to initialize          *  00210000
*                 if operator denies permission:            exit     *  00220000
*           - get SQA storage for SAFV and initialize it             *  00230000
*           - put entry address of SAF router ICHSFR00 in SAFV       *  00240000
*           - initialize RCVT                                        *  00250000
*           - initialize profile table                               *  00260000
*           - initialize user table                                  *  00270000
*                                                                    *  00280000
* REQUIREMENTS: - ICHSFR00 in LPA                                    *  00290000
*               - RAKFPROF and RAKFUSER in linklist                  *  00300000
*               - parmlib defined through ddname IEFPARM             *  00310000
*               - user and profile table DDs as needed               *  00320000
*                 by RAKFPROF and RAKFUSER                           *  00330000
*                                                                    *  00340000
**********************************************************************  00350000
*                                                                       00360000
         PRINT NOGEN                                                    00370000
         SAVE  (14,12),,ICHSEC00_&SYSDATE._&SYSTIME                     00380000
         USING ICHSEC00,R15        establish => program EP              00390000
         ST    R13,SAVEAREA+4      save HSA                             00400000
         LA    R11,SAVEAREA        establish => savearea                00410000
         ST    R11,8(R13)          save LSA                             00420000
         LR    R13,R11             setup => our savearea                00430000
         USING SAVEAREA,R13        new addressability                   00440000
         DROP  R15                 program EP no longer needed          00450000
         B     CONTINUE            branch around savearea               00460000
SAVEAREA DS    18F                 savearea                             00470000
*                                                                       00480000
* identify                                                              00490000
*                                                                       00500000
CONTINUE WTO   'RAKF is based on the ESG Security System'               00510000
         WTO   'written by Craig J. Yasuna               (Mar 1991)'    00520000
         WTO   'adapted to MVS 3.8J: A. Philip Dickinson (Aug 2005)'    00530000
         WTO   '                     Phil Roberts        (Apr 2011)'    00540000
         WTO   '                     Juergen Winkelmann  (Apr 2011)'    00550000
*                                                                       00560000
* RAKF already active?                                                  00570000
*                                                                       00580000
         L     R2,FLCCVT-PSA(0)    get CVT address from PSA             00590000
         ICM   R3,B'1111',CVTSAF(R2) SAFV already defined?              00600000
         BZ    GOFORIT             no RAC active, continue activation   00610000
         USING SAFV,R3             addressability of SAFV               00620000
         CLC   SAFVIDEN(4),SAFVID  SAFV initialized?                    00630000
         BNE   GOFORIT             no RAC active, continue activation   00640000
         DROP  R3                  SAFV no longer needed                00650000
         WTO   'RAKF005I RAKF is already active' tell operator          00660000
         B     PROFUSER            initialize user and profile table    00670000
*                                                                       00680000
* read parmlib                                                          00690000
*                                                                       00700000
GOFORIT  LA    R11,PARMLIB         establish ..                         00710000
         USING IHADCB,R11           .. DCB addressability               00720000
         OPEN  PARMLIB             open PARMLIB                         00730000
         LH    R12,DCBBLKSI        get blocksize                        00740000
         GETMAIN EU,LV=(R12),A=BLKADDR get storage for block            00750000
         FIND  PARMLIB,RAKFINIT,D  find member RAKFINIT                 00760000
         LTR   R15,R15             parmlib member found?                00770000
         BNZ   CLOSE                --> no, don't read                  00780000
         L     R12,BLKADDR         get block address                    00790000
         READ  INDECB,SF,PARMLIB,(R12),'S' read block                   00800000
         CHECK INDECB              wait for data to arrive              00810000
         MVC   RAKSTART(4),0(R12)  get parmlib input                    00820000
CLOSE    LH    R12,DCBBLKSI        get blocksize                        00830000
         FREEMAIN EU,LV=(R12),A=BLKADDR free storage                    00840000
         CLOSE PARMLIB             close parmlib                        00850000
         DROP  R11                 DCB no longer needed                 00860000
*                                                                       00870000
* decide initialization                                                 00880000
*                                                                       00890000
         CLC   RAKSTART(2),NO      if RAKFINIT directive is NO ..       00900000
         BE    NOINIT               .. talk dirrty and don't initialize 00910000
         CLC   RAKSTART(3),YES     if RAKFINIT directive is YES ..      00920000
         BE    INITMSG              .. initialize RAKF                  00930000
         WTO   MF=(E,INITWTO)       .. else ask operator                00940000
         WTOR  'RAKF002A Reply YES to continue or NO to cancel',       X00950000
               REPLY,4,SECECB,ROUTCDE=(1) .. shall we?                  00960000
         WAIT  ECB=SECECB          wait for reply                       00970000
         CLC   REPLY(2),NO         if operator replied NO ..            00980000
         BNE   INIT                 .. talk dirrty and don't initialize 00990000
NOINIT   WTO   'RAKF004W RAKF not initialized' tell operator            01000000
RETURN   L     R13,SAVEAREA+4      get caller's savearea                01010000
         RETURN (14,12),,RC=0      return                               01020000
*                                                                       01030000
* initialize                                                            01040000
*                                                                       01050000
INITMSG  WTO   MF=(E,INITWTO)      tell operator                        01060000
INIT     MODESET MODE=SUP,KEY=ZERO authorize ourselves                  01070000
         GETMAIN RU,LV=SAFVLEN,SP=245 get SQA storage for SAFV          01080000
         LR    R3,R1               establish ..                         01090000
         USING SAFV,R3              .. addressability of SAFV           01100000
         XC    SAFV(SAFVLEN),SAFV  clear SAFV                           01110000
         MVC   SAFVIDEN(4),SAFVID  move identifier into SAFV            01120000
         LOAD  EP=ICHSFR00         get SFR address (LPA)                01130000
         ST    R0,SAFVSAFR         store SFR address in SAFV            01140000
         DROP  R3                  SAFV addressability no longer needed 01150000
         L     R2,FLCCVT-PSA(0)    get CVT address from PSA             01160000
         ST    R3,CVTSAF(R2)       save SAFV address in CVT             01170000
         L     R15,CJYRCVT         get RCVT loader address              01180000
         BALR  R14,R15             call it                              01190000
         MODESET MODE=PROB,KEY=NZERO return to problem state            01200000
         WTO   'RAKF003I RAKF is now active',ROUTCDE=(2) tell operator  01210000
PROFUSER LOAD  EP=RAKFPROF         load profile updater                 01220000
         LR    R15,R0              address of entry point               01230000
         BALR  R14,R15             call it                              01240000
         DELETE EP=RAKFPROF        remove it                            01250000
         LOAD  EP=RAKFUSER         load user updater                    01260000
         LR    R15,R0              address of entry point               01270000
         BALR  R14,R15             call it                              01280000
         DELETE EP=RAKFUSER        remove it                            01290000
         B     RETURN              return                               01300000
*                                                                       01310000
* data area                                                             01320000
*                                                                       01330000
BLKADDR  DS    F                   address of parmlib read buffer       01340000
CJYRCVT  DC    V(CJYRCVT)          RCVT loader                          01350000
RAKFINIT DC    C'RAKFINIT'         parmlib member name                  01360000
RAKSTART DC    CL4' '              parameter read from parmlib          01370000
REPLY    DC    CL4' '              reply from operator                  01380000
SAFVID   DC    CL4'SAFV'           SAFV eye catcher                     01390000
SECECB   DC    A(0)                ECB for WTOR                         01400000
NO       DC    CL2'NO'             NO                                   01410000
YES      DC    CL3'YES'            YES                                  01420000
INITWTO  WTO   'RAKF001I RAKF is now being activated',                 X01430000
               ROUTCDE=(1),MF=L    tell operator                        01440000
PARMLIB  DCB   DDNAME=IEFPARM,DSORG=PO,MACRF=R,EODAD=CLOSE              01450000
*                                                                       01460000
* equates                                                               01470000
*                                                                       01480000
CVTSAF   EQU   248 CVTSAF doesn't exist but is a reserved field in 3.8J 01490000
         YREGS                     register equates                     01500000
*                                                                       01510000
* control block mappings                                                01520000
*                                                                       01530000
         CVT   DSECT=YES           map CVT                              01540000
         IHAPSA   DSECT=YES        map PSA                              01550000
         ICHSAFV  DSECT=YES        map SAFV                             01560000
         DCBD  DSORG=PO,DEVD=DA    map DCB                              01570000
         END   ICHSEC00                                                 01580000
++SRC(ICHSFR00) DISTLIB(ASRCLIB)  SYSLIB(SRCLIB).
ICHSFR00 TITLE 'SAF ROUTER - ENTRY FOR ALL SECURITY CALLS'              00010000
* CPARM='XREF(SHORT),RENT,OBJ,NODECK',LPARM='RENT,REUS,MAP'   ICHSFR00  00020000
*      DD DSN=SYS1.MACLIB,DISP=SHR                                      00030000
*      DD DSN=SYS1.MODGEN,DISP=SHR                                      00040000
*      DD DSN=SYS3.SAF.PARMLIB,DISP=SHR                                 00050000
         EJECT                                                          00060000
ICHSFR00 CSECT                                                          00070000
*                                                                       00080000
**********************************************************************  00090000
*                                                                    *  00100000
*    COPYRIGHT (C) 1991 BY CRAIG J. YASUNA.  ALL RIGHTS RESERVED.    *  00110000
*                                                                    *  00120000
*    THIS SOFTWARE PRODUCT OR ANY OF ITS COMPONENTS MUST NOT BE      *  00130000
*    SOLD, GIVEN, OR OTHERWISE DISTRIBUTED TO ANY OTHER COMPANY      *  00140000
*    WITHOUT THE PRIOR WRITTEN PERMISSION OF:                        *  00150000
*                                                                    *  00160000
*                                  CRAIG J. YASUNA, PRESIDENT        *  00170000
*                                  ENTERPRISE SYSTEMS GROUP          *  00180000
*                                  2 MARC COURT                      *  00190000
*                                  EDISON, NEW JERSEY 08820          *  00200000
*                                                                    *  00210000
**********************************************************************  00220000
*                                                                   @02 00220302
*    Change History                                                 @02 00220502
*                                                                   @02 00220702
*    2011/04/03 TRKF120 base version                                @02 00222002
*    2011/04/26 RRKF002 introduce change history                    @02 00222302
*                       enable end users to change their passwords  @02 00222502
*                       permanently: store user's password change   @02 00222702
*                       request in fetch protected CSA and pass the @02 00224002
*                       request address to RAKFPWUP which queues it @02 00224302
*                       for replacement in the RAKF users table     @02 00224502
*                       at next run of RAKFUSER                     @02 00224702
*                                                                   @02 00226002
********************************************************************@02 00226302
*                                                                       00230000
         USING AUTHDS,R10                                               00240000
         USING ICHSFR00,R15                                             00250000
         SAVE  (14,12),,ICHSFR00.&SYSDATE..&SYSTIME                     00260000
         B     AROUNDZP            GO AROUND OUR ZAP CODE               00270000
         L     R15,NEWCODE         GET NEW CODE                         00280000
         BR    R15                 AND BRANCH TO IT                     00290000
NEWCODE  DC    F'0'                FOR NEW CODE (OMON ZAPS..)           00300000
         DROP  R15                                                      00310000
AROUNDZP LR    R12,R15                                                  00320000
         USING ICHSFR00,R12        PROGRAM BASE                         00330000
         USING ACEE,ACEEREG        ADDRESS ACEE                         00340000
         USING PSA,0               ADDRESS PSA                          00350000
         LR    R12,R15             FIRST BASE                           00360000
         L     R9,24(R1)           ADDRESS EXIT WORK AREA               00370000
         USING WORKSAVE,R9         ... HELLO SAVE AREA                  00380000
         XC    WORKAREA(WORKAREL),WORKAREA CLEAR WORKAREA               00390000
         ST    R13,WORKSAVE+4      SAVE HSA                             00400000
         ST    R9,8(R13)           SAVE LSA                             00410000
         LR    R13,R9              ADDRESS SAVE AREA                    00420000
         DROP  R9                                                       00430000
         USING WORKSAVE,R13        ... HELLO SAVE AREA                  00440000
         MVC   WORKWTO,REALWTO     MOVE WTO SKEL                        00450000
*                                                                       00460000
STRTROUT LR    R9,R1               GET ADDR. OF SAF PARAMETER LIST      00470000
         USING SAFP,R9             BASE FOR SAFP PARAMETER LIST         00480000
         LA    R2,SAFPAU           RACHECK                              00490000
         CH    R2,SAFPREQT         CHECK REQUEST TYPE                   00500000
         BE    RACHECK             YES, GO ON                           00510000
         LA    R2,SAFPFAU          FRACHECK                             00520000
         CH    R2,SAFPREQT         CHECK REQUEST TYPE                   00530000
         BE    FRACHECK            YES, GO ON                           00540000
         LA    R2,SAFPDEF          RACDEF                               00550000
         CH    R2,SAFPREQT         CHECK REQUEST TYPE                   00560000
         BE    RACDEF              YES, GO ON                           00570000
         LA    R2,SAFPVER          RACINIT                              00580000
         CH    R2,SAFPREQT         CHECK REQUEST TYPE                   00590000
         BE    RACINIT             YES, GO ON                           00600000
         LA    R2,SAFPEXT          RACXTRT                              00610000
         CH    R2,SAFPREQT         CHECK REQUEST TYPE                   00620000
         BE    RACXTRT             YES, GO ON                           00630000
         LA    R2,SAFPLIS          RACLIST                              00640000
         CH    R2,SAFPREQT         CHECK REQUEST TYPE                   00650000
         BE    RTRNGOOD            YES, GO ON                           00660000
         B     RTRN0000            GO AWAY                              00670000
         TITLE 'RACHECK PROCESSING (EXISTING RESOURCE) '                00680000
RACHECK  L     R8,SAFPRACP         OFFSET TO RACHECK PARAMETER LIST     00690000
         AR    R8,R9               ADDR. OF RACHECK PARAMETER LIST.     00700000
         USING ACHKLIST,R8                                              00710000
RACHLN31 XR    R2,R2               CLEAR R2                             00720000
         ICM   R2,B'0111',ACHKCLN  GET CLASS                            00730000
         BNZ   RACHCLNE            found --> continue                   00740000
         LA    R2,RACDDSNC         use our DSN CLASS                    00750000
RACHCLNE XR    ENTYREG,ENTYREG     CLEAR ENTYREG                        00760000
         ICM   ENTYREG,B'0111',ACHKENT GET ENTITY                       00770000
*        BNZ   RACHACEE            found --> continue                   00780000
*        B     RACHGOOD            not found --> exit                   00790000
*                                                                       00800000
RACHACEE MVC   WORKCLAS,=CL8' '    BLANK IT OUT                         00810000
         IC    R1,0(R2)            LENGTH                               00820000
*        BNZ   RACHNBLK            not zero --> continue                00830000
*        B     RACHGOOD            zero --> exit                        00840000
         BCTR  R1,0                SUB 1 FOR MVC                        00850000
         EX    R1,RACHMVCC         MVC WORKCLAS(0),1(R2)                00860000
         CLC   WORKCLAS(8),=CL8' ' CLASS blank?                         00870000
         BNE   RACHNBLK               --> no, continue                  00880000
         MVC   WORKCLAS,=CL8'DATASET' --> yes, assume DATASET           00890000
RACHNBLK ICM   ACEEREG,B'1111',ACHKACEE POINT TO ACEE                   00900000
         BNZ   RACHCHCK            NO ACEE -> ALLOW ACCES               00910000
         L     R2,PSAAOLD          OUR ASCB                             00920000
         L     R2,ASCBASXB-ASCB(R2) OUR ASXB                            00930000
         ICM   ACEEREG,B'1111',ASXBSENV-ASXB(R2) ACEE                   00940000
         BZ    RACHGOOD            WHO KNOWS                            00950000
RACHCHCK IC    R2,ACHKFLG2         GET AUTH. REQ.                       00960000
         SLL   R2,3                CONFORM TO RACDEF                    00970000
         STC   R2,WORKAUTH         SAVE IT                              00980000
         OC    WORKAUTH,ACHKFLG2   ADD BACK X'80'                       00990000
         NI    WORKAUTH,X'F0'      TURN OFF LOW ORDER                   01000000
*                                                                       01010000
         TM    ACHKFLG1,ACHKLOGS   LOG SUPPRESSED?                      01020000
         BZ    RACHCHK1            DO ENTITY CHECK                      01030000
         OI    WORKFLAG,WORKNLOG   SUPPRESS LOG                         01040000
*                                                                       01050000
RACHCHK1 BAL   R10,ENTYCHCK        GO DO ENTITY CHECK                   01060000
         B     RACHGOOD            GOOD RETURN                          01070000
         B     RACHUNKN            RC4 - UNKN                           01080000
         B     RACHFAIL            RC8 - FAIL                           01090000
RACHMVCC MVC   WORKCLAS(0),1(R2)   MOVE CLASS                           01100000
*                                                                       01110000
RACHGOOD XC    SAFPRRET,SAFPRRET   NOTHING ...                          01120000
         XC    SAFPRREA,SAFPRREA   NOTHING ...                          01130000
         B     RTRNGOOD            GO TO RC 200 CODE                    01140000
*                                                                       01150000
RACHUNKN MVC   SAFPRRET,=F'04'     NOT PROTECTED ...                    01160000
         XC    SAFPRREA,SAFPRREA   NOTHING ...                          01170000
         B     RTRNWARN            GO TO RC 204 CODE                    01180000
*                                                                       01190000
RACHFAIL MVC   SAFPRRET,=F'08'     FAIL ....                            01200000
         XC    SAFPRREA,SAFPRREA   NOTHING ...                          01210000
         B     RTRNFAIL            DO ANY EXCEPTIONS                    01220000
         TITLE 'FRACHECK PROCESSING (EXISTING RESOURCE IN-CORE) '       01230000
FRACHECK L     R8,SAFPRACP         OFFSET TO RACHECK PARAMETER LIST     01240000
         AR    R8,R9               ADDR. OF RACHECK PARAMETER LIST.     01250000
         L     R2,8(R8)            GET CLASS FRACHECK CLASS             01260000
         MVC   WORKCLAS,0(R2)      MOVE CLASS                           01270000
         L     ENTYREG,4(R8)       GET ENTITY FRACHECK ENTITY           01280000
         ICM   ACEEREG,B'1111',12(R8) POINT TO ACEE IN FRACHECK         01290000
         BNZ   FRACCHCK            GO DO CHECK                          01300000
         L     R2,PSAAOLD          OUR ASCB                             01310000
         L     R2,ASCBASXB-ASCB(R2) OUR ASXB                            01320000
         ICM   ACEEREG,B'1111',ASXBSENV-ASXB(R2) ACEE                   01330000
         BZ    FRACUNKN            UNKNOWN                              01340000
FRACCHCK IC    R2,0(R8)            GET AUTH. REQ.                       01350000
         SLL   R2,3                CONFORM TO RACDEF                    01360000
         STC   R2,WORKAUTH         SAVE IT                              01370000
         OC    WORKAUTH,0(R8)      ADD BACK X'80'                       01380000
         NI    WORKAUTH,X'F0'      TURN OFF LOW ORDER                   01390000
         BAL   R10,ENTYCHCK        GO DO ENTITY CHECK                   01400000
         B     FRACGOOD            GOOD RETURN                          01410000
         B     FRACUNKN            RC4 - UNKN                           01420000
         B     FRACFAIL            RC8 - FAIL                           01430000
*                                                                       01440000
FRACGOOD XC    SAFPRRET,SAFPRRET   NOTHING ...                          01450000
         XC    SAFPRREA,SAFPRREA   NOTHING ...                          01460000
         B     RTRNGOOD            GO TO RC 200 CODE                    01470000
FRACUNKN MVC   SAFPRRET,=F'04'     NOT PROTECTED ...                    01480000
         XC    SAFPRREA,SAFPRREA   NOTHING ...                          01490000
         B     RTRNWARN            GO TO RC 204 CODE                    01500000
FRACFAIL MVC   SAFPRRET,=F'08'     FAIL ....                            01510000
         XC    SAFPRREA,SAFPRREA   NOTHING ...                          01520000
         B     RTRNFAIL            DO ANY EXCEPTIONS                    01530000
         TITLE 'RACDEF PROCESSING (NEW RESOURCE) '                      01540000
RACDEF   L     R8,SAFPRACP         OFFSET TO RACDEF  PARAMETER LIST     01550000
         AR    R8,R9               ADDR. OF RACDEF PARAMETER LIST.      01560000
         USING RDDFLIST,R8                                              01570000
         XC    SAFPRRET,SAFPRRET   PRIME RC/REAS = 0                    01580000
         XC    SAFPRREA,SAFPRREA   PRIME RC/REAS = 0                    01590000
         TM    RDDFFLGS,RDDFTDEL+RDDFNEWN DELETE OR NEWNAME             01600000
         BZ    RACDNDEL            NOT DELETE                           01610000
         TM    RDDFFLG2,RDDFCHKA   CHECKAUTH SPEC.?                     01620000
         BZ    RACDGOOD            NO, RC=0                             01630000
*                                                                       01640000
RACDNDEL ICM   R2,B'0111',RDDFCLNW CLASS ???                            01650000
         BNZ   RACDGCLS            GOT CLASS                            01660000
         LA    R2,RACDDSNC         USE OUR DSN CLASS                    01670000
RACDGCLS TM    RDDFFLGS,RDDF31IN   31 BIT MODE?                         01680000
RACDEN31 XR    ENTYREG,ENTYREG     CLEAR ENTYREG                        01690000
         ICM   ENTYREG,B'0111',RDDFENT GET ENTITY                       01700000
RACDACEE MVC   WORKCLAS,=CL8' '    BLANK IT OUT                         01710000
         IC    R1,0(R2)            LENGTH                               01720000
         BCTR  R1,0                SUB 1 FOR MVC                        01730000
         EX    R1,RACDMVCC         MVC WORKCLAS(0),1(R2)                01740000
         CLC   WORKCLAS(8),=CL8' ' CLASS blank?                         01750000
         BNE   RACDNBLK               --> no, continue                  01760000
         MVC   WORKCLAS,=CL8'DATASET' --> yes, assume DATASET           01770000
RACDNBLK ICM   ACEEREG,B'1111',RDDFACEE POINT TO ACEE                   01780000
         BNZ   RACDCHCK            GO CHECK                             01790000
         L     R2,PSAAOLD          OUR ASCB                             01800000
         L     R2,ASCBASXB-ASCB(R2) OUR ASXB                            01810000
         ICM   ACEEREG,B'1111',ASXBSENV-ASXB(R2) ACEE                   01820000
         BZ    RACDGOOD            WHO KNOWS                            01830000
RACDCHCK MVI   WORKAUTH,RACFALTR   MOVE AUTH REQ.                       01840000
*                                                                       01850000
         TM    RDDFFLG2,RDDFRFI    RACFIND IND?                         01860000
         BZ    RACDCHK1            NO, DO ENTITY CHECK                  01870000
         OI    WORKFLAG,WORKNLOG   YES, SUPPRESS LOG                    01880000
*                                                                       01890000
RACDCHK1 BAL   R10,ENTYCHCK        GO DO ENTITY CHECK                   01900000
         B     RACDNEW             GOOD RETURN - CHECK RENAME           01910000
         B     RACDNEW             RC4 - UNKN                           01920000
         B     RACDFAIL            RC8 - FAIL                           01930000
RACDMVCC MVC   WORKCLAS(0),1(R2)   MOVE CLASS                           01940000
RACDDSNC DC    X'07',CL8'DATASET'  DATASET CLASS                        01950000
*                                                                       01960000
RACDNEW  TM    RDDFFLGS,RDDFNEWN   NEWNAME SPEC?                        01970000
         BNO   RACDCK18            CHECK 1.8 STUFF                      01980000
         L     ENTYREG,RDDFNNAM    LOAD NEWNAME                         01990000
         BAL   R10,ENTYCHCK        GO DO ENTITY CHECK                   02000000
         B     RACDCK18            CHECK 1.8 STUFF                      02010000
         B     RACDCK18            CHECK 1.8 STUFF                      02020000
         B     RACDFAIL            RC8 - FAIL                           02030000
*                                                                       02040000
RACDCK18 EQU   *                                                        02050000
*                                                                       02060000
RACDCMCL EQU   *                                                        02070000
         B     RACDGOOD            RC4 - UNKN                           02080000
*                                                                       02090000
RACDGOOD TM    WORKENTY,8          RC=8                                 02100000
         BO    RACDFAIL            BYPASS RC=4                          02110000
RACDNVFY TM    WORKENTY,4          RC=4                                 02120000
         BO    RACDUNKN            AT LEAST 1 WARN                      02130000
         TM    RDDFFLGS,RDDFCHGV   DELETE OR ADD                        02140000
         BNZ   RACDGDD0            GOOD 0                               02150000
         TM    RDDFFLG2,RDDFRFIY   RACFIND=YES                          02160000
         BO    RACDGDD0            --> Yes, RC=0                        02170000
         TM    RDDFFLG2,RDDFRFI    RACFIND=NO                           02180000
         BNO   RACDGDD0            --> No, RC=0                         02190000
         MVC   SAFPRREA,=F'04'     --> Yes, RC=0 & Reason=4             02200000
RACDGDD0 XC    SAFPRRET,SAFPRRET   NOTHING ...                          02210000
         B     RTRNGOOD            GO TO RC 200 CODE                    02220000
*                                                                       02230000
RACDUNKN MVC   SAFPRRET,=F'04'     NOT PROTECTED ...                    02240000
         XC    SAFPRREA,SAFPRREA   NOTHING ...                          02250000
         B     RTRNWARN            GO TO RC 204 CODE                    02260000
*                                                                       02270000
RACDFAIL MVC   SAFPRRET,=F'08'     FAIL ....                            02280000
         B     RTRNFAIL            DO ANY EXCEPTIONS                    02290000
         TITLE 'RACXTRT CODE - RETRIEVE DATA FROM PROFILE'              02300000
RACXTRT  L     R8,SAFPRACP         OFFSET TO RACXTRT PARAMETER LIST     02310000
         AR    R8,R9               ADDR. OF RACXTRT PARAMETER LIST.     02320000
         TM    3(R8),X'01'         EXTRACT ...                          02330000
         BO    RACXEXTR            GO ON                                02340000
         TM    3(R8),X'03'         EXTRACTN...                          02350000
         BO    RACXEXTR            GO ON                                02360000
         B     RACXUNKN            NOT YET SUPPORTED ...                02370000
*                                                                       02380000
RACXEXTR XR    R2,R2               CLEAR R2                             02390000
         LH    R2,6(R8)            GET OFFSET TO VARIABLE STUFF.        02400000
         AR    R2,R8               ADD BEGINNING OF RACXTRT             02410000
         ICM   R3,B'1111',0(R2)    GET CLASS ADDRESS                    02420000
         BZ    RACXUNKN            NONE ....                            02430000
         CLC   =C'USER',0(R3)      USER CLASS ????                      02440000
         BNE   RACXUNKN            NO, NOT SUPPORTED                    02450000
*                                                                       02460000
         ICM   R3,B'1111',8(R8)    GET ENTITY ADDRESS                   02470000
         BNZ   RACXENTY            GOT ENTITY(USERNAME)                 02480000
         ICM   ACEEREG,B'1111',X'14'(R2) ACEE REQ?                      02490000
         BNZ   RACXACEE            GO ON...                             02500000
         L     R3,PSAAOLD-PSA(0)   OUR ASCB                             02510000
         L     R3,ASCBASXB-ASCB(R3) OUR ASXB                            02520000
         ICM   ACEEREG,B'1111',ASXBSENV-ASXB(R3) ACEE                   02530000
         BZ    RACXUNKN            WHO KNOWS                            02540000
RACXACEE LA    R3,ACEEUSRI         GET ACEEUSER                         02550000
RACXENTY ICM   R8,B'1111',X'0C'(R2) CHECK SEGMENT                       02560000
         BZ    RACXUNKN            BASE NOT SUPPORTED                   02570000
         CLC   =C'TSO',0(R8)       TSO SEGMENT?                         02580000
         BNE   RACXUNKN            NO, NOT SUPPORTED                    02590000
         B     RACXUNKN            GO THERE ANYWAY                      02600000
*        FIELDS ...                                                     02610000
*  GETMAIN AREA IN SP... & RETURN IN ...                                02620000
*                                                                       02630000
RACXUNKN MVC   SAFPRRET,=F'08'     NOT PROTECTED ...                    02640000
         XC    SAFPRREA,SAFPRREA   NOTHING ...                          02650000
         B     RTRNWARN            GO TO RC 208 CODE                    02660000
*                                                                       02670000
         TITLE 'RACINIT PROCESS. (GET INTO/OUT OF SYSTEM) - CHECK AUTH' 02680000
RACINIT  L     R8,SAFPRACP         OFFSET TO RACINIT PARAMETER LIST     02690000
         AR    R8,R9               ADDR. OF RACINIT PARAMETER LIST.     02700000
         TM    3(R8),X'C0'         TEST FOR VERIFY                      02710000
         BO    RTRN0000            ENVIR=VERIFY USED BY JES2 EARLY VFY  02720000
*                                                                       02730000
         L     R2,PSATOLD          ADDRESS TCB                          02740000
         L     R2,TCBRBP-TCB(R2)   ADDRESS RB                           02750000
         TM    RBSTAB1-RBSECT(R2),RBFTP CHECK RBTYPE                    02760000
         BZ    RACIRACR            PRB DID RACROUTE                     02770000
*                                                                       02780000
         XR    R3,R3               CLEAR R3                             02790000
         ICM   R3,B'0111',RBLINKB-RBSECT(R2) PRIOR RB                   02800000
         S     R3,=A(RBSECT-RBPREFIX) SUBTRACT PFIX LENG                02810000
         CLI   RBINTCOD-RBPREFIX+1(R3),X'83' SVC83 (RACINIT)            02820000
         BNE   RACIRACR            SVC ISSUED RACROUTE                  02830000
*                                                                       02840000
         ICM   R2,B'0111',RBLINKB-RBSECT(R2) PRIOR RB                   02850000
         TM    RBSTAB1-RBSECT(R2),RBFTP PRB?????                        02860000
         BNZ   RACINAUT            NO, DON'T BOTHER WITH TABLE          02870000
*                                                                       02880000
         ICM   R2,B'0111',RBCDE1-RBSECT(R2) PRIOR RB                    02890000
         BZ    RACINAUT            NO CDE                               02900000
         L     R3,FLCCVT           LOAD CVT                             02910000
         ICM   R3,B'1111',CVTRAC-CVTMAP(R3) CHECK RCVT                  02920000
         BZ    RACIERR1            WHO KNOWS                            02930000
         ICM   R3,B'1111',RCVTAUTP-RCVT(R3) CHECK AUTHORIZED CALLERS    02940000
         BZ    RACINAUT            NO TABLE                             02950000
*                                                                       02960000
RACIAUTL CLC   =F'0',0(R3)         END OF TABLE?                        02970000
         BE    RACINAUT            YES, SORRY                           02980000
         CLC   CDNAME-CDENTRY(,R2),0(R3) IS THIS US?                    02990000
         BE    RACIMDST            YES, BYPASS TESTAUTH                 03000000
         LA    R3,12(R3)           NEXT ENTRY                           03010000
         BE    RACIAUTL            GO ON                                03020000
*                                                                       03030000
RACINAUT TESTAUTH FCTN=1,STATE=YES,KEY=YES    APF/SUP/KEY0-7            03040000
         LTR   R15,R15             GOT IT?                              03050000
         BZ    RACIMDST            YES, GO ON                           03060000
         ABEND 1667,,,SYSTEM       ABEND 683                            03070000
*                                                                       03080000
RACIRACR TESTAUTH FCTN=1,STATE=YES,KEY=YES,RBLEVEL=1 APF/SUP/KEY0-7     03090000
         LTR   R15,R15             GOT IT?                              03100000
         BZ    RACISCHP            YES, GO ON                           03110000
         ABEND 1667,,,SYSTEM       ABEND 683                            03120000
*                                                                       03130000
RACISCHP TESTAUTH STATE=YES,RBLEVEL=1 SUPV STATE?                       03140000
         LTR   R15,R15             GOT IT?                              03150000
         BZ    RACIMDST            YES, GO ON                           03160000
         OI    WORKFLAG,WORKSUPO   SUP. ON                              03170000
         MODESET MODE=SUP          SET STATE                            03180000
*                                                                       03190000
RACIMDST MODESET EXTKEY=ZERO,SAVEKEY=WORKKEY,WORKREG=2                  03200000
         TITLE 'RACINIT PROCESS. (GET INTO/OUT OF SYSTEM) '             03210000
         TM    3(R8),X'40'         TEST FOR CHANGE                      03220000
         BO    RACIGOOD            NOT YET SUPPORTED ...                03230000
         TM    3(R8),X'80'         CREATE = ZERO                        03240000
         BO    RACINDEL            NO, GO DELETE ACEE                   03250000
*                                                                       03260000
         ICM   R5,B'1111',4(R8)    GO TO USERID                         03270000
         BZ    RACINOID            NONE, CHECK FOR STC ...              03280000
         L     R4,FLCCVT           LOAD CVT                             03290000
         ICM   R4,B'1111',CVTRAC-CVTMAP(R4) CHECK RCVT                  03300000
         BZ    RACIERR1            WHO KNOWS                            03310000
         ICM   R4,B'1111',RCVTISTL-RCVT(R4) OUR STUFF?                  03320000
         BZ    RACIERR1            WHO KNOWS                            03330000
         ICM   R4,B'1111',CJYUSERS-CJYRCVTD(R4) OUR STUFF?              03340000
         BZ    RACIERR1            WHO KNOWS                            03350000
         USING CBLK,R4             ADDRESS THE BALL -- HELLO BALL       03360000
RACICKLP IC    R1,CBLKUSRL         LENGTH OF UID                        03370000
         EX    R1,RACINCLC         CLC CBLKUSER(0),0(R5)                03380000
         BE    RACIIDOK            ID OK                                03390000
         ICM   R4,B'1111',CBLKNEXT GO TO NEXT ID                        03400000
         BNZ   RACICKLP            GO FOR NEXT                          03410000
RACICSTC LA    R4,STCUID-L'CBLKNEXT POINT TO UID  (TEST FOR STC)        03420000
         EX    R1,RACINCLC         CLC CBLKUSER(0),0(R5)                03430000
         BE    RACIIDOK            ID OK                                03440000
         LA    R4,PRODUID-L'CBLKNEXT POINT TO UID                       03450000
         EX    R1,RACINCLC         CLC CBLKUSER(0),0(R5)                03460000
         BE    RACIIDOK            ID OK                                03470000
         B     RACIREVK            WHO KNOWS ...                        03480000
RACINCLC CLC   CBLKUSER(0),0(R5)   CORE UID VS RACINIT                  03490000
*                                                                       03500000
RACIIDOK TM    3(R8),X'08'         PASSCHK=NO?                          03510000
         BO    RACIACEE            YES, GO MAKE ACEE                    03520000
         ICM   R3,B'1111',8(R8)    GO TO RACINIT PASSWORD               03530000
         BZ    RACIINPW            NO PASSWORD -> INVALID               03540000
         MVC   WORKPASS,0(R3)      MOVE PASSWORD                        03550000
         XC    WORKPASS+1(8),=C'SECURITY' ENCRYPT                       03560000
         IC    R1,CBLKPWDL         LENGTH OF PSWD IN CORE               03570000
         EX    R1,RACICKPW         check password                       03580000
         BE    RACIPWOK            password is correct                  03590000
         IC    R1,WORKPASS         LENGTH OF PSWD                       03600000
         EX    R1,RACISTPW         check STC password                   03610000
         BE    RACISSTC            YES, IT IS STC                       03620000
         B     RACIINPW            NO, INVALID PWD.                     03630000
RACIPWOK EQU   *                                                        03640000
RACICHPW EQU   *                                                        03650000
RACINCL2 EQU   *                                                        03660000
*                                                                       03670000
         ICM   R3,15,24(R8)        set new password?                    03680000
         BZ    RACIACEE            no, go make ACEE                     03690000
         MVC   WORKPASS,0(R3)      yes, move new password               03700000
         GETMAIN RU,LV=PWUPL,SP=227 get fetch protected CSA         @02 03702002
         MVC   0(PWUPCMDL,R1),PWUPSTRT move SVC 34 plist to CSA area@02 03704002
         L     R3,PWUPXTBA         get translate table address      @02 03706002
         USING PWUPSTRT,R1         address CSA area now             @02 03708002
         ST    R1,PWUPADDR         store CSA area address for unpack@02 03710000
         UNPK  PWUPUNPK(9),PWUPADDR(5) unpack CSA area address      @02 03712002
         MVC   PWUPAHEX(8),PWUPUNPK unpacked address to SVC 34 plist@02 03714002
         TR    PWUPAHEX(8),0(R3)   translate address to printable   @02 03716002
         DROP  R1                  revert to standard addressability@02 03718002
         MVC   PWUPCMDL(PWUPRECL,R1),PWUP initialize change record  @02 03720000
         IC    R3,0(,R5)           length of user                   @02 03722002
         BCTR  R3,0                subtract 1 for MVC               @02 03724002
         EX    R3,PWUPMVCU         copy user                        @02 03726002
         IC    R3,WORKPASS         length of new password           @02 03728002
         BCTR  R3,0                subtract 1 for MVC               @02 03730000
         EX    R3,PWUPMVCP         copy new password                @02 03732002
         XR    R0,R0               set R0 = 0 for SVC 34            @02 03734002
         SVC   34                  S RAKFPWUP,PARM='PWUPAHEX'       @02 03736002
         XC    WORKPASS+1(8),=C'SECURITY' encrypt                       03740000
         IC    R1,WORKPASS         length of new password               03750000
         EX    R1,RACIRPWD         replace password                     03760000
         B     RACIACEE            go make ACEE                         03770000
PWUPSTRT DC    AL2(PWUPCMDL)       parameter list to start RAKFPWUP>@02 03780000
         DC    X'0000'              >is copied to CSA subpool 227  >@02 03790000
         DC    C'S RAKFPWUP,PARM=''' >fetch protected storage with >@02 03790302
PWUPAHEX DS    CL8                  >address passed in PARM field  >@02 03790502
         DC    C''''                >in hexadecimal printable format@02 03790702
PWUPCMDL EQU   *-PWUPSTRT          length of parameter list         @02 03792002
PWUP     DS    0C                  changes queue record             @02 03792302
PWUPUSER DC    CL8' '              userid                           @02 03792502
         DC    C' '                filler                           @02 03792702
PWUPPSWD DC    CL8' '              new password                     @02 03794002
         DC    C' '                filler                           @02 03794302
PWUPRECL EQU   *-PWUP              record length of changes queue   @02 03794502
PWUPL    EQU   *-PWUPSTRT          total length of CSA area         @02 03794702
         ORG   PWUP                changes queue record is >        @02 03796002
PWUPADDR DS    F                      > temporarily used   >        @02 03796302
         DS    X                      > for conversion of  >        @02 03796502
PWUPUNPK DS    CL8                    > CSA area to        >        @02 03796702
         DS    X                      > unpacked format             @02 03798002
         ORG   PWUP+PWUPRECL       restore program counter          @02 03798302
         USING PWUPSTRT,R1         address CSA area now             @02 03798502
PWUPMVCU MVC   PWUPUSER(1),1(R5)   get user and new password >      @02 03798702
PWUPMVCP MVC   PWUPPSWD(1),WORKPASS+1  > into change queue record   @02 03799002
         DROP  R1                  revert to standard addressability@02 03799302
RACIRPWD MVC   CBLKPWDL(1),WORKPASS replace in core password            03800000
RACICKPW CLC   CBLKPWDL(1),WORKPASS check password                      03810000
RACISTPW CLC   STCPASS(1),WORKPASS  check STC password                  03820000
RACIERR1 MVC   WORKWTO+4(60),WTOMSG1 CJYUSERS INVALID                   03830000
         WTO   MF=(E,WORKWTO)      WRITE IT                             03840000
         MVC   WORKWTO+4(60),WTOMSG2 STC ACCESS                         03850000
         ICM   R5,B'1111',4(R8)    GO TO USERID                         03860000
         IC    R1,0(R5)            GET LENGTH                           03870000
         EX    R1,RACIWTO1         MOVE NAME FOR WTO                    03880000
         WTO   MF=(E,WORKWTO)      WRITE IT                             03890000
         B     RACISSTC            ASSUME STC                           03900000
RACIWTO1 MVC   WORKWTO+5+WTOMSG2U(0),1(R5) MOVE USERID                  03910000
*                                                                       03920000
RACINOID CLC   =F'0',X'C'(R8)      STC ?                                03930000
         BNE   RACISSTC            YES, DO IT ...                       03940000
         CLC   =F'0',X'28'(R8)     TERMID SPEC?                         03950000
         BNE   RACIREVK            TSO ---- > REVOKED                   03960000
         CLC   =F'0',X'2C'(R8)     JOBNAME SPEC.?                       03970000
         BNE   RACISBAT            BATCH, OK ....                       03980000
         ICM   R2,B'1111',PSAAOLD   POINT TO ASCB OLD                   03990000
         BZ    RACISSTC            ASSUME STC                           04000000
         CLC   =F'0',ASCBJBNI-ASCB(R2) JOBNAME SPEC.?                   04010000
         BNE   RACISBAT            BATCH, OK ....                       04020000
RACISSTC LA    R4,STCUID-L'CBLKNEXT POINT TO UID                        04030000
         B     RACIACEE            GO BUILD ACEE                        04040000
RACISBAT LA    R4,PRODUID-L'CBLKNEXT POINT TO UID                       04050000
         B     RACIACEE            GO BUILD ACEE                        04060000
         TITLE 'RACINIT PROCESS. (GET INTO/OUT OF SYSTEM) - BUILD ACEE' 04070000
RACIACEE LA    R2,255              PRE-FILL WITH LSQA SUBPOOL           04080000
         TM    3(R8),X'10'         S.P. SPEC?                           04090000
         BNO   RACINDGM            DO GETMAIN                           04100000
         IC    R2,1(R8)            GET SP NUMBER                        04110000
RACINDGM GETMAIN RC,LV=ACEEL,SP=(R2) DO GETMAIN                         04120000
         LR    ACEEREG,R1          ADDRESS AREA                         04130000
         XC    ACEE(ACEEL),ACEE    CLEAR IT                             04140000
         MVC   ACEEACEE,=C'ACEE'   ACEE NAME                            04150000
         STC   R2,ACEESP           SP OF G.M.                           04160000
         MVC   ACEELEN,=AL3(ACEEL) LENGTH                               04170000
         MVI   ACEEVRSN,C'1'       VERSION 1                            04180000
         MVC   ACEEUSER,CBLKUSER   USERID                               04190000
         MVC   ACEEGRP,CBLKGRP     GROUP                                04200000
         OI    ACEEFLG1,ACEERACF+ACEECNTL                               04210000
         OI    ACEEFLG2,ACEENONE   DEFAULT UACC                         04220000
         L     R2,FLCCVT           CVT                                  04230000
         MVC   ACEEDATE,CVTDATE+1-CVT(R2) MOVE DATE                     04240000
         CLI   CBLKFLG1,C'Y'       OPER AUTHORITY                       04250000
         BNE   RACINNOP            NO, SORRY ....                       04260000
         OI    ACEEFLG1,ACEEOPER   MAKE OPER AUTH.                      04270000
RACINNOP ICM   R2,B'1111',X'C'(R8) STC ?                                04280000
         BZ    RACINANS            NO, BATCH                            04290000
         MVC   ACEEPROC,0(R2)      MOVE PROC NAME FOR STC               04300000
RACINANS ICM   R2,B'1111',X'28'(R8) TERMID SPEC?                        04310000
         BZ    RACINNTR            NO, NONE SPEC.                       04320000
         MVC   ACEETRID,0(R2)      MOVE PROC NAME FOR STC               04330000
RACINNTR CLM   ACEEREG,B'1000',=X'00' ABOVE THE LINE?                   04340000
         BE    RACICGRP            NO, ADD GROUPS                       04350000
*                                                                       04360000
RACICGRP ICM   R3,B'1111',CBLKGRPS POINTER TO C.GRPS                    04370000
         BNZ   RACICGP1            NONE IN PROFS                        04380000
         LA    R3,STCGROUP         STC GROUP IF STC                     04390000
         CLC   =CL8'STC',ACEEUSRI  STC?                                 04400000
         BE    RACICGP1            YES, GO ON                           04410000
         LA    R3,PRDGROUP         NO USE PRD GROUP                     04420000
RACICGP1 LA    R5,ACEECGRP         FIRST POINTER                        04430000
         XR    R2,R2               CLEAR R2                             04440000
         IC    R2,ACEESP           GET SUBPOOL                          04450000
RACICGPL GETMAIN RC,LV=CONNL,SP=(R2)                                    04460000
         XC    0(CONNL,R1),0(R1)   CLEAR AREA                           04470000
         ST    R1,0(R5)            SAVE IN PTR                          04480000
         MVC   CONNGRP-CONNGRUP(,R1),CONNGRP-CONNGRUP(R3) THEM TO US    04490000
         LA    R5,CONNNEXT-CONNGRUP(R1) NEXT POINTER                    04500000
         ICM   R3,B'1111',CONNNEXT-CONNGRUP(R3) NEXT CONN.              04510000
         BNZ   RACICGPL            NEXT CGRP.                           04520000
*                                                                       04530000
         CLI   0(R8),X'34'         check length of RACINIT plist        04540000
         BNL   RACIAPPL            -> ge x'34' continue                 04550000
         B     RACINSTA            -> lt x'34' store ACEE addr in ASXB  04560000
RACIAPPL ICM   ENTYREG,B'1111',X'30'(R8) ANY APPL REQ?                  04570000
         BZ    RACITERM            NO, GO ON                            04580000
         MVC   WORKCLAS,=CL8'APPL' APPL CLASS                           04590000
         MVI   WORKAUTH,RACFREAD   READ REQ                             04600000
         BAL   R10,ENTYCHCK        CHECK ENTITY                         04610000
         B     RACITERM            0 -> OK                              04620000
         B     RACITERM            4 -> OK                              04630000
         B     RACIFAPL            8 -> NG                              04640000
*                                                                       04650000
RACITERM ICM   ENTYREG,B'1111',X'28'(R8) ANY APPL REQ?                  04660000
         BZ    RACIALOK            NO, GO ON                            04670000
         MVC   WORKCLAS,=CL8'TERMINAL' TERM CLASS                       04680000
         MVI   WORKAUTH,RACFREAD   READ REQ                             04690000
         BAL   R10,ENTYCHCK        CHECK ENTITY                         04700000
         B     RACIALOK            0 -> OK                              04710000
         B     RACIALOK            4 -> OK                              04720000
         B     RACIFTRM            8 -> NG                              04730000
*                                                                       04740000
RACIALOK ICM   R2,B'1111',X'34'(R8) ACEE PTR                            04750000
         BZ    RACINSTA            SAVE ACEE                            04760000
         ST    ACEEREG,0(R2)       SAVE ACEE ADDRESS                    04770000
         B     RACIGOOD            OK ..........                        04780000
RACINSTA EQU   *                                                        04790000
         L     R2,PSAAOLD          ADDRESS ASCB                         04800000
         L     R2,ASCBASXB-ASCB(R2) ADDRESS ASCB                        04810000
         ST    ACEEREG,ASXBSENV-ASXB(R2) STORE IN ASXB                  04820000
         B     RACIGOOD            OK ..........                        04830000
*                                                                       04840000
         DROP  R4                                                       04850000
         TITLE 'RACINIT PROCESS. (GET INTO/OUT OF SYSTEM) - DEL. ACEE'  04860000
RACINDEL ICM   R2,B'1111',X'34'(R8) ACEE PTR                            04870000
         BZ    RACIDNAC            DELETE, NO ACEE                      04880000
         L     ACEEREG,0(R2)       GET ACEE ADDRESS                     04890000
         B     RACINFRE            GO DO FREEMAIN                       04900000
RACIDNAC EQU   *                                                        04910000
         L     R2,PSAAOLD          OUR ASCB                             04920000
         L     R2,ASCBASXB-ASCB(R2) OUR ASXB                            04930000
         C     ACEEREG,ASXBSENV-ASXB(R2) ACEE SAME?                     04940000
         BNE   RACINTDM            NO, GO DO FREE                       04950000
         XC    ASXBSENV-ASXB(,R2),ASXBSENV-ASXB(R2) YES, CLEAR          04960000
         B     RACINFRE            GO FREE ACEE                         04970000
*                                                                       04980000
RACINTDM L     R2,PSAAOLD          OUR ASCB                             04990000
         L     R2,ASCBASXB-ASCB(R2) OUR ASXB                            05000000
         ICM   ACEEREG,B'1111',ASXBSENV-ASXB(R2) ACEE                   05010000
         BZ    RACIGOOD            ASXB & TCB  BOTH DUMMY               05020000
         XC    ASXBSENV-ASXB(,R2),ASXBSENV-ASXB(R2) CLEAR ASXB          05030000
RACINFRE ICM   R2,B'1111',ACEEIEP  ANY OTHER F.M?                       05040000
         BZ    RACINIEP            NO, SO BYE...                        05050000
         XR    R3,R3               CLEAR R3                             05060000
         IC    R3,0(R2)            SUBPOOL                              05070000
         XR    R4,R4               CLEAR R4                             05080000
         ICM   R4,B'0111',1(R2)    LENGTH                               05090000
         FREEMAIN RC,A=(R2),SP=(R3),LV=(R4) FREE ????                   05100000
*                                                                       05110000
RACINIEP XR    R2,R2               CLEAR R2                             05120000
         IC    R2,ACEESP           SUBPOOL                              05130000
         LA    R3,CONNL            CLEAR R3                             05140000
         L     R5,ACEECGRP         CONN. GROUPS                         05150000
         B     RACIFCN2            START FREEMAINS                      05160000
RACIFCNL L     R5,CONNNEXT-CONNGRUP(R4) LOOP THRU OLD TABLE             05170000
         FREEMAIN RC,A=(R4),SP=(R2),LV=(R3) FREE ACEE                   05180000
RACIFCN2 LTR   R4,R5               MORE ENTRIES?                        05190000
         BNZ   RACIFCNL             Y - NEXT ENTRY                      05200000
*                                                                       05210000
RACINCGP XR    R2,R2               CLEAR R2                             05220000
         IC    R2,ACEESP           SUBPOOL                              05230000
         XR    R3,R3               CLEAR R3                             05240000
         ICM   R3,B'0111',ACEELEN  LENGTH                               05250000
         FREEMAIN RC,A=(ACEEREG),SP=(R2),LV=(R3) FREE ACEE              05260000
         B     RACIGOOD            GO TO GOOD RETURN CODE               05270000
         TITLE 'RACINIT PROCESS. (GET INTO/OUT OF SYSTEM) - BYEEEE'     05280000
RACIGOOD XC    SAFPRRET,SAFPRRET   NOTHING ...                          05290000
         XC    SAFPRREA,SAFPRREA   NOTHING ...                          05300000
         XC    WORKPASS,WORKPASS   CLEAR JUST IN CASE                   05310000
         MODESET KEYADDR=WORKKEY,WORKREG=2                              05320000
         TM    WORKFLAG,WORKSUPO   SUP. ON                              05330000
         BNO   RTRNGOOD            NO, GO TO RC 200 CODE                05340000
         MODESET MODE=PROB         SET STATE                            05350000
         B     RTRNGOOD            GO TO RC 200 CODE                    05360000
*                                                                       05370000
RACIUNKN MVC   SAFPRRET,=F'04'     NOT PROTECTED ...                    05380000
         MODESET KEYADDR=WORKKEY,WORKREG=2                              05390000
         TM    WORKFLAG,WORKSUPO   SUP. ON                              05400000
         BNO   RTRNWARN            NO, GO TO RC 204 CODE                05410000
         MODESET MODE=PROB         SET STATE                            05420000
         B     RTRNWARN            GO TO RC 204 CODE                    05430000
*                                                                       05440000
RACIINPW MVC   SAFPRRET,=F'08'     invalid or no password entered       05450000
         XC    WORKPASS,WORKPASS   clear just in case                   05460000
         B     RACIFAIL            BRANCH TO FAIL COMMON                05470000
*                                                                       05480000
RACIREVK MVC   SAFPRRET,=F'28'     REVOKED....                          05490000
         B     RACIFAIL            BRANCH TO FAIL COMMON                05500000
*                                                                       05510000
RACIFAPL MVC   SAFPRRET,=F'52'     NOT AUTH TO APPL...                  05520000
         B     RACIFAFM            FAIL & FREEMAIN                      05530000
RACIFTRM MVC   SAFPRRET,=F'48'     NOT AUTH TO TERM...                  05540000
*                                                                       05550000
RACIFAFM ICM   R4,B'1111',ACEECGRP CONN. GROUPS                         05560000
         BZ    RACIFACF            NO, GO FREEMAIN ACEE                 05570000
         XR    R2,R2               CLEAR R2                             05580000
         IC    R2,ACEESP           SUBPOOL                              05590000
         LA    R3,CONNL            CLEAR R3                             05600000
         B     RACIFAG2            BYPASS MOVE                          05610000
RACIFAGL L     R5,CONNNEXT-CONNGRUP(R4) LOOP THRU OLD TABLE             05620000
         FREEMAIN RC,A=(R4),SP=(R2),LV=(R3) FREE ACEE                   05630000
RACIFAG2 LTR   R4,R5               MORE ENTRIES?                        05640000
         BNZ   RACIFAGL             Y - NEXT ENTRY                      05650000
*                                                                       05660000
RACIFACF XR    R2,R2               CLEAR R2                             05670000
         IC    R2,ACEESP           SUBPOOL                              05680000
         XR    R3,R3               CLEAR R3                             05690000
         ICM   R3,B'0111',ACEELEN  LENGTH                               05700000
         FREEMAIN RC,A=(ACEEREG),SP=(R2),LV=(R3) FREE ACEE              05710000
         B     RACIFAIL            BRANCH TO FAIL COMMON                05720000
*                                                                       05730000
RACIFAIL MODESET KEYADDR=WORKKEY,WORKREG=2                              05740000
         TM    WORKFLAG,WORKSUPO   SUP. ON                              05750000
         BNO   RACINPWF            no  --> check for WTO suppression    05760000
         MODESET MODE=PROB         SET STATE                            05770000
RACINPWF CLC   SAFPRRET,=F'08'     invalid or no password entered?      05780000
         BNE   RACIFWTO            no  --> write message                05790000
         LTR   R3,R3               yes --> no password entered?         05800000
         BZ    RTRNFAIL            yes --> don't write message          05810000
RACIFWTO MVC   WORKWTO+4(60),WTOMSG4  ATTEMPTED ACCESS                  05820000
         ICM   R5,B'1111',4(R8)    GO TO USERID                         05830000
         IC    R1,0(R5)            GET LENGTH                           05840000
         EX    R1,RACIWTO2         MOVE NAME FOR WTO                    05850000
         WTO   MF=(E,WORKWTO)      WRITE IT                             05860000
         XC    SAFPRREA,SAFPRREA   NOTHING ...                          05870000
         XC    WORKPASS,WORKPASS   CLEAR JUST IN CASE                   05880000
         B     RTRNFAIL            BYE ............                     05890000
RACIWTO2 MVC   WORKWTO+4+WTOMSG4U(0),1(R5) MOVE USERID                  05900000
         TITLE 'ALL ROADS LEAD HERE'                                    05910000
RTRN0000 LA    R15,0               RC = 0                               05920000
         B     RTRNRTRN            GO BACK                              05930000
*                                                                       05940000
RTRNGOOD LA    R15,0               RACF RC=0                            05950000
         B     RTRNRTRN            GO BACK                              05960000
*                                                                       05970000
RTRNWARN LA    R15,0               RACF RC=4                            05980000
         B     RTRNRTRN            GO BACK                              05990000
*                                                                       06000000
RTRNFAIL LA    R15,0               RACF RC=8                            06010000
         B     RTRNRTRN            GO BACK                              06020000
*                                                                       06030000
RTRNRTRN L     R13,WORKSAVE+4      RETURN R13                           06040000
         RETURN (14,12),RC=(15)    BYE                                  06050000
*                                                                       06060000
         TITLE 'CHECK FOR ACCESS'                                       06070000
ENTYCHCK L     R2,FLCCVT           LOAD CVT                             06080000
         ICM   R2,B'1111',CVTRAC-CVTMAP(R2) CHECK RCVT                  06090000
         BZ    ENTYERR1            WHO KNOWS?                           06100000
         ICM   R2,B'1111',RCVTISTL-RCVT(R2) OUR STUFF?                  06110000
         BZ    ENTYERR1            WHO KNOWS?                           06120000
         ICM   R2,B'1111',CJYPROFS-CJYRCVTD(R2) OUR STUFF?              06130000
         BZ    ENTYERR1            WHO KNOWS?                           06140000
         USING RPECBLK,R2          ADDRESS CBLK                         06150000
         CLC   WORKCLAS(8),=CL8' '    CLASS blank?                      06160000
         BNE   ENTYNBLK                  --> no, continue               06170000
         MVC   WORKCLAS(8),=CL8'DATASET' --> yes, assume DATASET        06180000
ENTYNBLK CLC   =C'DATASET',WORKCLAS  CHECK FOR OUR OWN DSNS.            06190000
         BNE   ENTYCLLP            NO, GO ON                            06200000
         IC    R1,ACEEUSRL         GET LENGTH                           06210000
         BCTR  R1,0                DOWN 1 FOR CLC                       06220000
         EX    R1,ENTYEXUS         CLC 0(0,ENTYREG),ACEEUSRN            06230000
         BE    AUTHGOOD            GO FOR IT                            06240000
         CLC   0(3,ENTYREG),=C'SYS' is it a ..                          06250000
         BNE   ENTYCLLP              --> no                             06260000
         CLC   8(2,ENTYREG),=C'.T'            .. temporary ..           06270000
         BNE   ENTYCLLP              --> no                             06280000
         CLC   16(3,ENTYREG),=C'.RA'                         .. dataset 06290000
         BNE   ENTYCLLP              --> no, go on                      06300000
         B     AUTHGOOD              --> yes, go for it                 06310000
ENTYCLLP IC    R1,RPECLASL         GET LENGTH                           06320000
         BCTR  R1,0                SUBTRACT 1 FOR CLC                   06330000
         EX    R1,ENTYEXEC         CLC RPECLASN(0),WORKCLAS             06340000
         BL    ENTYNFND            NOT FOUND                            06350000
         BNE   ENTYNCLS            NO, GET NEXT                         06360000
         ICM   R1,B'0001',RPEENTYL GET LENGTH                           06370000
         BZ    ENTYENFN            '*' ONLY ENTITY?                     06380000
         BCTR  R1,0                NO, SUBTRACT 1 FOR CLC               06390000
         EX    R1,ENTYEXEN         CLC RPEENTYN(0),0(ENTYREG)           06400000
         BE    ENTYENFN            FOUND ENTITY                         06410000
ENTYNCLS ICM   R2,B'1111',RPENEXT  GET NEXT                             06420000
         BNZ   ENTYCLLP            GO FOR NEXT                          06430000
ENTYNFND B     ENTYWARN            RETURN WARN                          06440000
ENTYEXEC CLC   RPECLASN(0),WORKCLAS SAME CLASS?                         06450000
ENTYEXEN CLC   RPEENTYN(0),0(ENTYREG) CHECK ENTITY                      06460000
ENTYEXUS CLC   0(0,ENTYREG),ACEEUSRI US?                                06470000
*                                                                       06480000
ENTYENFN ICM   R3,B'1111',RPEACCPT POINTER TO ACCESSES                  06490000
         BZ    ENTYUACC            GO CHECK UACC                        06500000
*                                                                       06510000
         USING RPEACCLE,R3         VARIABLE SECTION                     06520000
ENTYULOP CLC   RPEAUSR,ACEEUSRI    USER EXCEPTION?                      06530000
         BE    ENTYEXCP            MATCH, GO CHECK                      06540000
         ICM   R3,B'1111',RPEANEXT NEXT POINTER                         06550000
         BNZ   ENTYULOP            GO CHECK NEXT                        06560000
*                                                                       06570000
ENTYCONN ICM   R4,B'1111',ACEECGRP CONNECT GROUPS                       06580000
         BZ    ENTYUACC            NONE, GO ON                          06590000
         USING CONNGRUP,R4         ADDRESS IT                           06600000
ENTYCONL L     R3,RPEACCPT         GET POINTER AGAIN                    06610000
ENTYCON2 CLC   RPEAUSR,CONNGRPN    GROUP EXCEPTION?                     06620000
         BNE   ENTYNCRP            NO, NEXT RPE                         06630000
         OI    WORKFLAG,WORKOPRB   SET OPER BYPASS                      06640000
         CLC   WORKAUTH,RPEACS     SUFFICENT ACCESS IN PERMIT?          06650000
         BH    ENTYNCRP            NO, CHECK NEXT AUTH.                 06660000
         B     AUTHGOOD            YES, GO ON ...                       06670000
ENTYNCRP ICM   R3,B'1111',RPEANEXT NEXT POINTER                         06680000
         BNZ   ENTYCON2            GO CHECK NEXT EXCEPTION              06690000
ENTYNCON ICM   R4,B'1111',CONNNEXT NEXT POINTER                         06700000
         BNZ   ENTYCONL            GO CHECK NEXT CONNECT                06710000
         TM    WORKFLAG,WORKOPRB   BYPASS OPER CHECK?                   06720000
         BNO   ENTYUACC            NO, GO UACC                          06730000
         B     ENTYFAIL            ALL CONN.S FAILED                    06740000
*                                                                       06750000
ENTYUACC CLC   WORKAUTH,RPEUACC    SUFFICENT ACCESS IN UACC?            06760000
         BH    ENTYOPER            NO, RETURN CHECK FOR OP. AUTH        06770000
         B     AUTHGOOD            YES, GO ON ...                       06780000
*                                                                       06790000
ENTYEXCP CLC   WORKAUTH,RPEACS     SUFFICENT ACCESS IN PERMIT?          06800000
         BH    ENTYFAIL            NO, RETURN CHECK FOR OP. AUTH        06810000
         B     AUTHGOOD            YES, GO ON ...                       06820000
*                                                                       06830000
ENTYOPER CLC   =C'DATASET',RPECLASN  DATASET CLASS                      06840000
         BE    ENTYOPR2            YES, CHECK OPER AUTH                 06850000
         CLC   =C'DASDVOL',RPECLASN DASDVOL CLASS                       06860000
         BE    ENTYOPR2            YES, CHECK OPER AUTH                 06870000
         B     ENTYFAIL            TOUGH                                06880000
ENTYOPR2 TM    ACEEFLG1,ACEEOPER   MAKE OPER AUTH.                      06890000
         BNO   ENTYFAIL            TOUGH                                06900000
*                                                                       06910000
         DROP  R4,R3,R2                                                 06920000
         CLC   =C'PROD',ACEEUSRI   PROD USERID?                         06930000
         BNE   AUTHGOOD            NO MSG NEC. -> OPER ALLOWED          06940000
         TM    WORKFLAG,WORKNLOG   NO LOG REQUESTED?                    06950000
         BO    AUTHGOOD            YES, NO MESS..                       06960000
         MVC   WORKWTO+4(60),WTOMSG7 INVALID ACCESS TO RESOURCE         06970000
         MVC   WORKWTO+4+WTOMSG7U(L'WTOMSG7U),ACEEUSRI MOVE USERID      06980000
         WTO   MF=(E,WORKWTO)      WRITE IT                             06990000
         MVC   WORKWTO+4(60),WTOMSGE ENTITY SKELETON                    07000000
         MVC   WORKWTO+4(9),=C'RAKF0006 ' USERID/CLASS                  07010000
         BAL   R11,ENTYENTY        ENTY COMMON                          07020000
         B     AUTHGOOD            ALLOW ACCESS                         07030000
*                                                                       07040000
ENTYERR1 MVC   WORKWTO+4(60),WTOMSG3 PROFS IN ERROR                     07050000
         MVC   WORKWTO+4+WTOMSG3U(L'WTOMSG3U),ACEEUSRI MOVE USERID      07060000
         WTO   MF=(E,WORKWTO)      WRITE IT                             07070000
         MVC   WORKWTO+4(60),WTOMSGE ENTITY SKELETON                    07080000
         MVC   WORKWTO+4(9),=C'RAKF0009 ' USERID/CLASS                  07090000
         BAL   R11,ENTYENTY        ENTY COMMON                          07100000
         B     AUTHGOOD            ALLOW ACCESS                         07110000
*                                                                       07120000
ENTYWARN OI    WORKENTY,4          RC = 4                               07130000
         TM    WORKFLAG,WORKNLOG   NO LOG REQUESTED?                    07140000
         BO    AUTHWARN            SORRY ....                           07150000
         MVC   WORKWTO+4(60),WTOMSG8 INVALID ACCESS TO RESOURCE         07160000
         WTO   MF=(E,WORKWTO)      WRITE IT                             07170000
         MVC   WORKWTO+4(60),WTOMSGE ENTITY SKELETON                    07180000
         MVC   WORKWTO+4(9),=C'RAKF000B ' USERID/CLASS                  07190000
         BAL   R11,ENTYENTY        ENTY COMMON                          07200000
         B     AUTHGOOD            ALLOW ACCESS                         07210000
*                                                                       07220000
ENTYFAIL OI    WORKENTY,8          RC = 8                               07230000
         TM    WORKFLAG,WORKNLOG   NO LOG REQUESTED?                    07240000
         BO    AUTHFAIL            SORRY ....                           07250000
         MVC   WORKWTO+4(60),WTOMSG5 INVALID ACCESS TO RESOURCE         07260000
         WTO   MF=(E,WORKWTO)      WRITE IT                             07270000
         MVC   WORKWTO+4(60),WTOMSGE ENTITY SKELETON                    07280000
         MVC   WORKWTO+4(9),=C'RAKF000A ' USERID/CLASS                  07290000
         BAL   R11,ENTYENTY        ENTY COMMON                          07300000
         B     AUTHFAIL            FAIL ACCESS                          07310000
*                                                                       07320000
ENTYENTY MVC   WORKWTO+4+WTOMSGEU(L'WTOMSGEU),ACEEUSRI MOVE USERID      07330000
         MVC   WORKWTO+4+WTOMSGEC(L'WTOMSGEC),WORKCLAS MOVE CLASS       07340000
         L     R2,PSAAOLD          ASCB ADDRESS                         07350000
         ICM   R3,B'1111',ASCBJBNI-ASCB(R2) JOBNAME SPEC.?              07360000
         BNZ   ENTYJOBN            YES, GO USE NAME                     07370000
         L     R3,ASCBJBNS-ASCB(R2) STC ... NAME                        07380000
ENTYJOBN MVC   WORKWTO+4+WTOMSGEJ(L'WTOMSGEJ),0(R3) MOVE JOBNAME        07390000
         LA    R1,43               DEFAULT ENTITY LEN.                  07400000
         CLC   =C'DATASET',WORKCLAS CLASS = DATASET                     07410000
         BE    ENTYWTO1            GO DO WTO                            07420000
         LA    R1,5                LEN = 6                              07430000
         CLC   =C'VOL',WORKCLAS+4  CLASS = DASDVOL/TAPEVOL              07440000
         BE    ENTYWTO1            DO WTO ...                           07450000
         LA    R1,38               FACILITY = 39                        07460000
         CLC   =C'FACILITY',WORKCLAS CLASS=FACILITY                     07470000
         BE    ENTYWTO1            DO WTO ...                           07480000
         LA    R1,7                ALL ELSE = 8                         07490000
ENTYWTO1 LA    R2,L'WTOMSGEE-1     MAX LENGTH                           07500000
         CR    R2,R1               TOO MUCH ?                           07510000
         BH    ENTYWTO2            NO, ITS OK                           07520000
         LR    R1,R2               SORRY, MOVE MAX                      07530000
ENTYWTO2 EX    R1,ENTYERRX         MVC WORKWTO+4+WTOMSGEE(0),0(ENTYREG) 07540000
         WTO   MF=(E,WORKWTO)      WRITE IT                             07550000
         BR    R11                 RETURN                               07560000
*                                                                       07570000
ENTYERRX MVC   WORKWTO+4+WTOMSGEE(0),0(ENTYREG) MOVE ENTITY             07580000
*                                                                       07590000
         TITLE 'CONSTANTS AND SUCH'                                     07600000
         LTORG                                                          07610000
*                                                                       07620000
STCUID   DC    AL4(0),X'03',CL8'STC',X'08',C'STCGROUP'                  07630000
STCPASS  DC    X'08251C06253A1F362D',C'Y'                               07640000
STCGROUP DC    AL4(0),AL1(8),CL8'STCGROUP'                              07650000
PRODUID  DC    AL4(0),X'04',CL8'PROD',X'08',C'PRDGROUP'                 07660000
PRODPASS DC    X'08251C06253A1F362D',C'N'                               07670000
PRDGROUP DC    AL4(0),AL1(8),CL8'PRDGROUP'                              07680000
*                                                                       07690000
REALWTO  WTO   '1234567 101234567 201234567 301234567 401234567 5012345X07700000
               67 60',MF=L                                              07710000
REALWTOL EQU   *-REALWTO           LENGTH                               07720000
*                                                                       07730000
WTOMSG1  DC    CL60'RAKF0001 USERID TABLE INVALID OR MISSING'           07740000
WTOMSG2  DC    C'RAKF0002 STC ACCESS ALLOWED: USER:'                    07750000
WTOMSG2U EQU   *-WTOMSG2,8                                              07760000
         DC    CL8' ',CL(60-(*-WTOMSG2))' '                             07770000
WTOMSG3  DC    C'RAKF0003 RESOURCE TABLE INVALID/MISSING: ALLOWED:'     07780000
*        DC    C': USER ALLOWED:'                                       07790000
WTOMSG3U EQU   *-WTOMSG3,8                                              07800000
         DC    CL8' ',CL(60-(*-WTOMSG3))' '                             07810000
WTOMSG4  DC    C'RAKF0004 INVALID ATTEMPT TO ACCESS SYSTEM:  USER:'     07820000
WTOMSG4U EQU   *-WTOMSG4,8                                              07830000
         DC    CL8' ',CL(60-(*-WTOMSG4))' '                             07840000
WTOMSG5  DC    CL60'RAKF0005 INVALID ATTEMPT TO ACCESS RESOURCE'        07850000
WTOMSG7  DC    C'RAKF0007 ACCESS ALLOWED VIA OPERATIONS:  USER:'        07860000
WTOMSG7U EQU   *-WTOMSG7,8                                              07870000
         DC    CL8' ',CL(60-(*-WTOMSG7))' '                             07880000
WTOMSG8  DC    CL60'RAKF0008 UNDEFINED RESOURCE - ACCESS ALLOWED'       07890000
WTOMSGE  DC    C'XXXXXXXXX '                                            07900000
WTOMSGEU EQU   *-WTOMSGE,8                                              07910000
         DC    CL8' ',C','                                              07920000
WTOMSGEJ EQU   *-WTOMSGE,8                                              07930000
         DC    CL8' ',C','                                              07940000
WTOMSGEC EQU   *-WTOMSGE,8                                              07950000
         DC    CL8' ',C','                                              07960000
WTOMSGEE EQU   *-WTOMSGE,20                                             07970000
         DC    CL20' ',CL(60-(*-WTOMSGE))' '                            07980000
         DC    CL133' '                                                 07990000
*                                  spaceholder blanks removed       @02 08000000
         DC    CL23' '             rest of spaceholder blanks       @02 08010000
PWUPHXTB DC    C'0123456789ABCDEF' translate RAKFPWUP address to >  @02 08020000
PWUPXTBA DC    A(PWUPHXTB-240)      > hex printable format          @02 08030000
         TITLE 'REGISTERS AND SUCH'                                     08040000
R0       EQU   0                                                        08050000
R1       EQU   1                                                        08060000
R2       EQU   2                                                        08070000
R3       EQU   3                                                        08080000
R4       EQU   4                                                        08090000
R5       EQU   5                                                        08100000
ENTYREG  EQU   6                                                        08110000
ACEEREG  EQU   7                                                        08120000
R8       EQU   8                                                        08130000
R9       EQU   9                                                        08140000
R10      EQU   10                                                       08150000
R11      EQU   11                                                       08160000
R12      EQU   12                                                       08170000
R13      EQU   13                                                       08180000
R14      EQU   14                                                       08190000
R15      EQU   15                                                       08200000
         PRINT NOGEN                                                    08210000
         TITLE 'DSECTS AND SUCH'                                        08220000
WORKAREA DSECT                     150 BYTE AREA                        08230000
WORKSAVE DS    18F                 SAVE AREA                            08240000
WORKWTO  DS    XL(REALWTOL)        FOR WTO                              08250000
WORKKEY  DS    X                   RACINIT KEY SAVE                     08260000
WORKENTY DS    X                   ENTITY RETURN                        08270000
WORKFLAG DS    X                   FLAG BYTES                           08280000
WORKSUPO EQU   X'80'                  WE SET SUPERVISOR STATE           08290000
WORKNLOG EQU   X'40'                  NO LOG IF FAILURE                 08300000
WORKOPRB EQU   X'20'                  RPE MATCH ON USERID/CLASS         08310000
WORKPASS DS    XL9                 RACINIT LEN/PSWD                     08320000
         ORG   WORKPASS                                                 08330000
WORKCLAS DS    XL8                 RACHECK/DEF/FRAC CLASS               08340000
WORKAUTH DS    X                   RACHECK/DEF/FRAC AUTHORITY REQ.      08350000
         ORG                                                            08360000
         DS    XL(150-(*-WORKAREA)) MAKE SURE                           08370000
WORKAREL EQU   *-WORKAREA           S.B 150                             08380000
*                                                                       08390000
AUTHDS   DSECT                     USED FOR RETURN FROM SUBS(R10)       08400000
AUTHGOOD DS    F'0'                                                     08410000
AUTHWARN DS    F'0'                                                     08420000
AUTHFAIL DS    F'0'                                                     08430000
*                                                                       08440000
         COPY  CJYUCBLK            USERID CBLK                          08450000
*                                                                       08460000
         COPY  CJYPCBLK            PROFILE CBLK                         08470000
*                                                                       08480000
         COPY  CJYRCVTD            RCVT CBLK                            08490000
*                                                                       08500000
         CVT     DSECT=YES,LIST=NO                                      08510000
         ICHACHKL                                                       08520000
         ICHPRCVT                                                       08530000
         ICHRDDFL                                                       08540000
         ICHSAFP                                                        08550000
         IEFAJCTB                                                       08560000
         IEZJSCB                                                        08570000
         IHAASCB DSECT=YES                                              08580000
         IHAASXB DSECT=YES                                              08590000
         IHACDE                                                         08600000
         IHAPSA  DSECT=YES                                              08610000
         IKJRB   DSECT=YES                                              08620000
         IKJTCB  DSECT=YES,LIST=NO                                      08630000
         IHAACEE                                                        08640000
ACEEL    EQU   *-ACEE                                                   08650000
         END                                                            08660000
++SRC(IGC00130) DISTLIB(ASRCLIB)  SYSLIB(SRCLIB).
         TITLE 'ESG-SECURITY  SVC 130'                                  00010000
* CPARM='XREF(SHORT),OBJ,NODECK,RENT',LPARM='RENT'            RAKF0130  00020000
*      DD DSN=SYS1.MACLIB,DISP=SHR                                      00030000
*      DD DSN=SYS1.MODGEN,DISP=SHR                                      00040000
         EJECT                                                          00050000
RAKF0130 CSECT                                                          00060000
*                                                                       00070000
**********************************************************************  00080000
*                                                                    *  00090000
*    COPYRIGHT (C) 1991 BY CRAIG J. YASUNA.  ALL RIGHTS RESERVED.    *  00100000
*                                                                    *  00110000
*    THIS SOFTWARE PRODUCT OR ANY OF ITS COMPONENTS MUST NOT BE      *  00120000
*    SOLD, GIVEN, OR OTHERWISE DISTRIBUTED TO ANY OTHER COMPANY      *  00130000
*    WITHOUT THE PRIOR WRITTEN PERMISSION OF:                        *  00140000
*                                                                    *  00150000
*                                  CRAIG J. YASUNA, PRESIDENT        *  00160000
*                                  ENTERPRISE SYSTEMS GROUP          *  00170000
*                                  2 MARC COURT                      *  00180000
*                                  EDISON, NEW JERSEY 08820          *  00190000
*                                                                    *  00200000
**********************************************************************  00210000
*                                                                       00220000
         USING RAKF0130,R6         SVC ENTRY R6 = EP.                   00230000
         LR    R9,R1               SAVE ENTRY REG.                      00240000
         LR    R8,R14              SAVE RETURN REG.                     00250000
         GETMAIN RC,LV=GMLEN       GET STORAGE                          00260000
         LR    R13,R1              LOAD REGISTER 13 WITH GETMAINED PTR  00270000
         USING WORKAREA,R13        SET UP DATA DSECT ADDR.              00280000
         MVC   RACRGM,RACRREAL     MOVE RACROUTE                        00290000
*                                                                       00300000
         L     R15,PSATOLD-PSA(0)  ADDRESS TCB                          00310000
         L     R15,TCBRBP-TCB(R15) ADDRESS RB                           00320000
         XR    R14,R14             CLEAR R14                            00330000
         ICM   R14,B'0111',RBLINKB-RBSECT(R15) PRIOR RB                 00340000
*                                                                       00350000
         LR    R10,R9              SAVE ENTRY REG.                      00360000
         ICM   R10,B'1000',=X'00'  CLEAR HIGH ORDER OF PARM ADDR.       00370000
         MVC   RACRGM+RACRLEN-RACCLEN(RACCLEN),0(R10) MOVE PARM         00380000
         NC    RACRGM+RACRLEN-RACCLEN+16(40),=10X'00FFFFFF'             00390000
STRTSAFR LA    R5,SAFWORK          FOR SAF                              00400000
         RACROUTE REQUEST=AUTH,WORKA=(R5),MF=(E,RACRGM)                 00410000
         L     R3,RACRGM           RACHECK RETURN CODE                  00420000
         L     R4,RACRGM+4         RACHECK REASON CODE                  00430000
*                                                                       00440000
RTRNFREE FREEMAIN RC,LV=GMLEN,A=(R13)                                   00450000
         LR    R15,R3              RETURN CODE                          00460000
         LR    R0,R4               REASON CODE                          00470000
         LR    R1,R9               RETURN R1                            00480000
         USING ACHKLIST,R9         RACHECK parameter list               00490000
         TM    ACHKFLG1,ACHKCSA    profile to be returned in CSA?       00500000
         BNO   RETURN              no  --> exit                         00510000
         XR    R1,R1               yes --> return zero address to       00520000
*                                          signal unsupported function  00530000
RETURN   BR    R8                  RETURN TO ORIG. R14                  00540000
         LTORG                                                          00550000
RACRREAL RACROUTE REQUEST=AUTH,MF=L                                     00560000
RACRLEN  EQU   *-RACRREAL                                               00570000
RACHECK  RACHECK MF=L              THIS IS JUST FOR THE LENGTH          00580000
RACCLEN  EQU   *-RACHECK                                                00590000
*                                                                       00600000
WORKAREA DSECT                                                          00610000
SAVEAREA DS     18F                                                     00620000
SAFWORK  DS     XL512                                                   00630000
RACRGM   DS     XL(RACRLEN)                                             00640000
GMLEN    EQU    *-WORKAREA                                              00650000
         PRINT  NOGEN                                                   00660000
         YREGS                                                          00670000
         CVT     DSECT=YES,LIST=NO                                      00680000
         IHAPSA  DSECT=YES                                              00690000
         IKJRB   DSECT=YES                                              00700000
         IKJTCB  DSECT=YES,LIST=NO                                      00710000
         ICHACHKL                                                       00720000
         END                                                            00730000
++SRC(IGC0013A) DISTLIB(ASRCLIB)  SYSLIB(SRCLIB).
         TITLE 'ESG-SECURITY  SVC 131'                                  00010000
* CPARM='XREF(SHORT),OBJ,NODECK,RENT',LPARM='RENT'            RAKF013A  00020000
*      DD DSN=SYS1.MACLIB,DISP=SHR                                      00030000
*      DD DSN=SYS1.MODGEN,DISP=SHR                                      00040000
         EJECT                                                          00050000
RAKF013A CSECT                                                          00060000
*                                                                       00070000
**********************************************************************  00080000
*                                                                    *  00090000
*    COPYRIGHT (C) 1991 BY CRAIG J. YASUNA.  ALL RIGHTS RESERVED.    *  00100000
*                                                                    *  00110000
*    THIS SOFTWARE PRODUCT OR ANY OF ITS COMPONENTS MUST NOT BE      *  00120000
*    SOLD, GIVEN, OR OTHERWISE DISTRIBUTED TO ANY OTHER COMPANY      *  00130000
*    WITHOUT THE PRIOR WRITTEN PERMISSION OF:                        *  00140000
*                                                                    *  00150000
*                                  CRAIG J. YASUNA, PRESIDENT        *  00160000
*                                  ENTERPRISE SYSTEMS GROUP          *  00170000
*                                  2 MARC COURT                      *  00180000
*                                  EDISON, NEW JERSEY 08820          *  00190000
*                                                                    *  00200000
**********************************************************************  00210000
*                                                                       00220000
         USING RAKF013A,R6         SVC ENTRY R6 = EP.                   00230000
         LR    R9,R1               SAVE ENTRY REG.                      00240000
         LR    R8,R14              SAVE RETURN REG.                     00250000
         GETMAIN RC,LV=GMLEN       GET STORAGE                          00260000
         LR    R13,R1              LOAD REGISTER 13 WITH GETMAINED PTR  00270000
         USING WORKAREA,R13        SET UP DATA DSECT ADDR.              00280000
         MVC   RACRGM,RACRREAL     MOVE RACROUTE                        00290000
*                                                                       00300000
         L     R15,PSATOLD-PSA(0)  ADDRESS TCB                          00310000
         L     R15,TCBRBP-TCB(R15) ADDRESS RB                           00320000
         XR    R14,R14             CLEAR R14                            00330000
         ICM   R14,B'0111',RBLINKB-RBSECT(R15) PRIOR RB                 00340000
*                                                                       00350000
         LR    R10,R9              SAVE ENTRY REG.                      00360000
         ICM   R10,B'1000',=X'00'  CLEAR HIGH ORDER OF PARM ADDR.       00370000
         MVC   RACRGM+RACRLEN-RACILEN(RACILEN),0(R10) MOVE PARM         00380000
         NC    RACRGM+RACRLEN-RACILEN+4(52),=13X'00FFFFFF'              00390000
STRTSAFR LA    R5,SAFWORK          FOR SAF                              00400000
         RACROUTE REQUEST=VERIFY,WORKA=(R5),MF=(E,RACRGM)               00410000
         L     R3,RACRGM           RACINIT RETURN CODE                  00420000
         L     R4,RACRGM+4         RACINIT REASON CODE                  00430000
*                                                                       00440000
RTRNFREE FREEMAIN RC,LV=GMLEN,A=(R13)                                   00450000
         LR    R15,R3              RETURN CODE                          00460000
         LR    R0,R4               REASON CODE                          00470000
         LR    R1,R9               RETURN R1                            00480000
         BR    R8                  RETURN TO ORIG. R14                  00490000
         LTORG                                                          00500000
*                                                                       00510000
RACRREAL RACROUTE REQUEST=VERIFY,MF=L                                   00520000
RACRLEN  EQU   *-RACRREAL                                               00530000
RACINIT  RACINIT MF=L              THIS IS JUST FOR THE LENGTH          00540000
RACILEN  EQU   *-RACINIT                                                00550000
*                                                                       00560000
WORKAREA DSECT                                                          00570000
SAVEAREA DS     18F                                                     00580000
SAFWORK  DS     XL512                                                   00590000
RACRGM   DS     XL(RACRLEN)                                             00600000
GMLEN    EQU    *-WORKAREA                                              00610000
         PRINT  NOGEN                                                   00620000
         YREGS                                                          00630000
         CVT     DSECT=YES,LIST=NO                                      00640000
         IHAPSA  DSECT=YES                                              00650000
         IKJRB   DSECT=YES                                              00660000
         IKJTCB  DSECT=YES,LIST=NO                                      00670000
         END                                                            00680000
++SRC(IGC0013C) DISTLIB(ASRCLIB)  SYSLIB(SRCLIB).
         TITLE 'ESG-SECURITY  SVC 133'                                  00010000
* CPARM='XREF(SHORT),OBJ,NODECK,RENT',LPARM='RENT'            RAKF013C  00020000
*      DD DSN=SYS1.MACLIB,DISP=SHR                                      00030000
*      DD DSN=SYS1.MODGEN,DISP=SHR                                      00040000
         EJECT                                                          00050000
RAKF013C CSECT                                                          00060000
*                                                                       00070000
**********************************************************************  00080000
*                                                                    *  00090000
*    COPYRIGHT (C) 1991 BY CRAIG J. YASUNA.  ALL RIGHTS RESERVED.    *  00100000
*                                                                    *  00110000
*    THIS SOFTWARE PRODUCT OR ANY OF ITS COMPONENTS MUST NOT BE      *  00120000
*    SOLD, GIVEN, OR OTHERWISE DISTRIBUTED TO ANY OTHER COMPANY      *  00130000
*    WITHOUT THE PRIOR WRITTEN PERMISSION OF:                        *  00140000
*                                                                    *  00150000
*                                  CRAIG J. YASUNA, PRESIDENT        *  00160000
*                                  ENTERPRISE SYSTEMS GROUP          *  00170000
*                                  2 MARC COURT                      *  00180000
*                                  EDISON, NEW JERSEY 08820          *  00190000
*                                                                    *  00200000
**********************************************************************  00210000
*                                                                       00220000
         USING RAKF013C,R6         SVC ENTRY R6 = EP.                   00230000
         LR    R9,R1               SAVE ENTRY REG.                      00240000
         LR    R8,R14              SAVE RETURN REG.                     00250000
         GETMAIN RC,LV=GMLEN       GET STORAGE                          00260000
         LR    R13,R1              LOAD REGISTER 13 WITH GETMAINED PTR  00270000
         USING WORKAREA,R13        SET UP DATA DSECT ADDR.              00280000
         MVC   RACRGM,RACRREAL     move RACROUTE                        00290000
*                                                                       00300000
         L     R15,PSATOLD-PSA(0)  ADDRESS TCB                          00310000
         L     R15,TCBRBP-TCB(R15) ADDRESS RB                           00320000
         XR    R14,R14             CLEAR R14                            00330000
         ICM   R14,B'0111',RBLINKB-RBSECT(R15) PRIOR RB                 00340000
*                                                                       00350000
         LR    R10,R9              SAVE ENTRY REG.                      00360000
         ICM   R10,B'1000',=X'00'  CLEAR HIGH ORDER OF PARM ADDR.       00370000
         MVC   RACRGM+RACRLEN-RACDLEN(RACDLEN-8),0(R10) MOVE PARM       00380000
         NC    RACRGM+RACRLEN-RACDLEN+8(28),=7X'00FFFFFF'               00390000
         NC    RACRGM+RACRLEN-RACDLEN+40(8),=7X'00FFFFFF'               00400000
*                                                                       00410000
STRTSAFR LA    R5,SAFWORK          FOR SAF                              00420000
         RACROUTE REQUEST=DEFINE,WORKA=(R5),MF=(E,RACRGM)               00430000
         L     R3,RACRGM           RACDEF RETURN CODE                   00440000
         L     R4,RACRGM+4         RACDEF REASON CODE                   00450000
*                                                                       00460000
RTRNFREE FREEMAIN RC,LV=GMLEN,A=(R13)                                   00470000
         LR    R15,R3              RETURN CODE                          00480000
         LR    R0,R4               REASON CODE                          00490000
         LR    R1,R9               RETURN R1                            00500000
         BR    R8                  RETURN TO ORIG. R14                  00510000
         LTORG                                                          00520000
RACRREAL RACROUTE REQUEST=DEFINE,MF=L                                   00530000
RACRLEN  EQU   *-RACRREAL                                               00540000
RACDEF   RACDEF MF=L               THIS IS JUST FOR THE LENGTH          00550000
         DC    A(0)       INSTALLATION DATA ADDRESS (missing in RACDEF) 00560000
         DC    A(0)       ENTITY NAME ADDR FIELD    (missing in RACDEF) 00570000
RACDLEN  EQU   *-RACDEF                                                 00580000
*                                                                       00590000
WORKAREA DSECT                                                          00600000
SAVEAREA DS     18F                                                     00610000
SAFWORK  DS     XL512                                                   00620000
RACRGM   DS     XL(RACRLEN)                                             00630000
GMLEN    EQU    *-WORKAREA                                              00640000
         PRINT  NOGEN                                                   00650000
         YREGS                                                          00660000
         CVT     DSECT=YES,LIST=NO                                      00670000
         IHAPSA  DSECT=YES                                              00680000
         IKJRB   DSECT=YES                                              00690000
         IKJTCB  DSECT=YES,LIST=NO                                      00700000
         END                                                            00710000
++SRC(RACIND) DISTLIB(ASRCLIB)  SYSLIB(SRCLIB).
         TITLE 'Set or Clear RACF Indicator of VSAM Catalog Entries'    00010000
RACIND   CSECT                                                          00020000
         PRINT NOGEN                                                    00030000
*                                                                       00040000
**********************************************************************  00050000
*                                                                    *  00060000
* NAME: RACIND                                                       *  00070000
*                                                                    *  00080000
* TYPE: Assembler Source                                             *  00090000
*                                                                    *  00100000
* DESC: Set or Clear RACF Indicator of VSAM Catalog Entries          *  00110000
*                                                                    *  00120000
* FUNTION: Act upon control statements read from SYSIN to set or     *  00130000
*          clear the RACF indicator of VSAM catalog entries. The     *  00140000
*          following control statements are valid:                   *  00150000
*                                                                    *  00160000
*          ----+----1----+----2----+----3----+----4----+----5----+   *  00170000
*          CATALOG   name of catalog to search for entries           *  00180000
*          RACON     name of entry to indicate                       *  00190000
*          RACOFF    name of entry to unindicate                     *  00200000
*          * Comment                                                 *  00210000
*                                                                    *  00220000
*          Any number of control statements is allowed. The first    *  00230000
*          none comment statement must be a CATALOG statement. A     *  00240000
*          CATALOG statement remains active until a new CATALOG      *  00250000
*          statement replaces it. The SYSIN dataset must have fixed  *  00260000
*          length records with LRECL=80, padded to the right with    *  00270000
*          blanks.                                                   *  00280000
*                                                                    *  00290000
* REQUIREMENTS: //SYSIN DD    defining the input control statements  *  00300000
*               //SYSPRINT DD defining the print output listing      *  00310000
*               The utility must be run from an authorized library   *  00320000
*                                                                    *  00330000
**********************************************************************  00340000
*                                                                       00350000
* initialize                                                            00360000
*                                                                       00370000
         SAVE  (14,12),,RACIND_&SYSDATE._&SYSTIME                       00380000
         USING RACIND,R15          establish => program EP              00390000
         ST    R13,SAVEAREA+4      save HSA                             00400000
         LA    R11,SAVEAREA        establish => savearea                00410000
         ST    R11,8(R13)          save LSA                             00420000
         LR    R13,R11             setup => our savearea                00430000
         USING SAVEAREA,R13        new addressability                   00440000
         DROP  R15                 program EP no longer needed          00450000
         B     CONTINUE            branch around savearea               00460000
SAVEAREA DS    18F                 savearea                             00470000
CONTINUE OPEN  (SYSIN,(INPUT))     open control statement dataset       00480000
         OPEN  (SYSPRINT,(OUTPUT)) open output dataset                  00490000
         RACHECK ENTITY=RAKFADM,CLASS='FACILITY',ATTR=READ authorize    00500000
         LTR    R15,R15            RAKFADM granted?                     00510000
         BNZ    NOAUTH              no, talk dirrty and exit            00520000
         MODESET MODE=SUP,KEY=ZERO authorize ourselves                  00530000
         LA    R12,PL              address CTGPL                        00540000
         USING CTGPL,R12           tell assembler                       00550000
         LA    R11,FL              address CTGFL                        00560000
         USING CTGFL,R11           tell assembler                       00570000
*                                                                       00580000
* setup catalog parameter and catalog field parameter lists             00590000
*                                                                       00600000
         MVI   CTGNOFLD,X'01'      number of fields in CTGPL            00610000
         LA    R1,ENTNAME          entry name address                   00620000
         ST    R1,CTGENT           store entry name address in CTGPL    00630000
         LA    R1,ENTCAT           catalog name address                 00640000
         ST    R1,CTGCAT           store catalog name address in CTGPL  00650000
         OI    CTGOPTN1,CTGNAME+CTGCNAME indicate names provided        00660000
         ST    R11,CTGFIELD        store field list address in CTGPL    00670000
         LA    R1,WA               CTGPL work area address              00680000
         ST    R1,CTGWKA           store work area address in CTGPL     00690000
         MVI   CTGFLDNO,X'01'      number of fields in CTGFL            00700000
         LA    R1,SECFLAGS         field name address                   00710000
         ST    R1,CTGFLDNM         store field name address in CTGFL    00720000
         LA    R1,FLDWA            CTGFL work area address              00730000
         ST    R1,CTGFLDWA         store work area address in CTGFL     00740000
*                                                                       00750000
* read and interpret control statement                                  00760000
*                                                                       00770000
NEXTENT  GET   SYSIN,INPUT         read control statement               00780000
         CLI   INPUT,C'*'          comment line?                        00790000
         BE    NEXTENT              yes, read next control statement    00800000
         CLC   INPUT(10),=C'CATALOG   ' CATALOG statement?              00810000
         BE    SETCAT               yes, replace current catalog name   00820000
         CLC   INPUT(10),=C'RACON     ' RACON statement?                00830000
         BE    SETENT               yes, replace current entry name     00840000
         CLC   INPUT(10),=C'RACOFF    ' RACOFF statement?               00850000
         BE    SETENT               yes, replace current entry name     00860000
         B     INVSTMT              alas master, what do you want?      00870000
*                                                                       00880000
* clear work areas                                                      00890000
*                                                                       00900000
ENTOK    LA    R4,WA               CTGPL work area address              00910000
         LA    R2,3(,R4)           MVCL target is second byte of WA     00920000
         LA    R4,2(,R4)           MVCL source is first byte of WA      00930000
         MVI   0(R4),X'00'         clear source                         00940000
         LA    R3,WAL-3            length of MVCL target                00950000
         LA    R5,1                length of MVCL source with X'00' pad 00960000
         MVCL  R2,R4               clear CTGPL work area                00970000
         LA    R4,FLDWA            CTGFL work area address              00980000
         LA    R2,3(,R4)           MVCL target is second byte of WA     00990000
         LA    R4,2(,R4)           MVCL source is first byte of WA      01000000
         MVI   0(R4),X'00'         clear source                         01010000
         LA    R3,FLDWAL-3         length of MVCL target                01020000
         LA    R5,1                length of MVCL source with X'00' pad 01030000
         MVCL  R2,R4               clear CTGFL work area                01040000
*                                                                       01050000
* locate entry                                                          01060000
*                                                                       01070000
         NI    CTGOPTN3,X'00'      clear function indicator             01080000
         OI    CTGOPTN3,CTGLOC+CTGAM0 indicate locate via CTGPL         01090000
         LR    R1,R12              CTGPL address                        01100000
         SVC   26                  locate entry                         01110000
         LTR   R15,R15             found?                               01120000
         BNZ   CATERR              no, issue catalog error message      01130000
*                                                                       01140000
* set or clear RACF indicator                                           01150000
*                                                                       01160000
         L     R5,CTGFLPT          get SECFLAGS address                 01170000
         CLC   INPUT(10),=C'RACOFF    ' request type?                   01180000
         BE    RACOFF              process RACOFF                       01190000
         OI    0(R5),B'10000000'   RACON:  set RACF indicator           01200000
         B     UPDATE              go update                            01210000
RACOFF   NI    0(R5),B'01111111'   RACOFF: clear RACF indicator         01220000
*                                                                       01230000
* update entry                                                          01240000
*                                                                       01250000
UPDATE   NI    CTGOPTN3,X'00'      clear function indicator             01260000
         OI    CTGOPTN3,CTGUPDAT+CTGAM0 indicate update via CTGPL       01270000
         LR    R1,R12              CTGPL address                        01280000
         SVC   26                  update entry                         01290000
         LTR   R15,R15             success?                             01300000
         BNZ   CATERR              no, issue catalog error message      01310000
         MVC   OUTPUT(132),BLANK   clear output record                  01320000
         MVC   OUTPUT(37),=C'IND003I RACF indicator turned xxx for'     01330000
         MVC   OUTPUT+30(3),INPUT+3             build ..                01340000
         MVC   OUTPUT+38(44),ENTNAME                .. message          01350000
         PUT   SYSPRINT,OUTPUT     print message                        01360000
         B     NEXTENT             process next entry                   01370000
*                                                                       01380000
* return                                                                01390000
*                                                                       01400000
RETURN   MODESET MODE=PROB,KEY=NZERO return to problem state            01410000
EXIT     CLOSE (SYSIN)             close control statement dataset      01420000
         CLOSE (SYSPRINT)          close output dataset                 01430000
         L     R15,MAXRC           get this run's maximum return code   01440000
         L     R13,SAVEAREA+4      get caller's savearea                01450000
         L     R14,12(,R13)        restore ..                           01460000
         LM    R0,R12,20(R13)                .. regs                    01470000
         BR    R14                 return                               01480000
*                                                                       01490000
* process control statements                                            01500000
*                                                                       01510000
SETCAT   CLI   INPUT+10,C' '       process CATALOG statement            01520000
         BE    INVCAT              first char blank, ignore statement   01530000
         MVC   ENTCAT(44),INPUT+10 set new catalog                      01540000
         MVC   OUTPUT(132),BLANK   clear output record                  01550000
         MVC   OUTPUT(21),=C'IND001I using catalog' build ..            01560000
         MVC   OUTPUT+22(44),ENTCAT                     .. message      01570000
         PUT   SYSPRINT,OUTPUT     print message                        01580000
         B     NEXTENT             process next entry                   01590000
SETENT   CLI   INPUT+10,C' '       process RACxxx statement             01600000
         BE    INVENT              first char blank, ignore statement   01610000
         CLI   ENTCAT,C' '         catalog defined?                     01620000
         BE    NOCAT                no, ignore statement                01630000
         MVC   ENTNAME(44),INPUT+10 set entry name                      01640000
         MVC   OUTPUT(132),BLANK   clear output record                  01650000
         MVC   OUTPUT(18),=C'IND002I processing'    build ..            01660000
         MVC   OUTPUT+19(44),ENTNAME                    .. message      01670000
         PUT   SYSPRINT,OUTPUT     print message                        01680000
         B     ENTOK               process entry                        01690000
*                                                                       01700000
* end of SYSIN dataset reached                                          01710000
*                                                                       01720000
DONE     MVC   OUTPUT(132),BLANK   clear output record                  01730000
         MVC   OUTPUT(31),=C'IND004I processing ended at EOD' message   01740000
         PUT   SYSPRINT,OUTPUT     print message                        01750000
         B     RETURN              exit                                 01760000
*                                                                       01770000
* error messages                                                        01780000
*                                                                       01790000
INVCAT   MVC   OUTPUT(132),BLANK   clear output record                  01800000
         MVC   OUTPUT(33),=C'IND005E CATALOG statement invalid' message 01810000
         PUT   SYSPRINT,OUTPUT     print message                        01820000
         B     NEXTENT             process next entry                   01830000
INVENT   MVC   OUTPUT(132),BLANK   clear output record                  01840000
         MVC   OUTPUT(32),=C'IND006E RACxxx statement invalid' build .. 01850000
         MVC   OUTPUT+11(3),INPUT+3                          .. message 01860000
         PUT   SYSPRINT,OUTPUT     print message                        01870000
         B     NEXTENT             process next entry                   01880000
NOCAT    MVC   OUTPUT(132),BLANK   clear output record                  01890000
         MVC   OUTPUT(45),=C'IND007E no catalog defined, statement ignoX01900000
               red'                build message                        01910000
         PUT   SYSPRINT,OUTPUT     print message                        01920000
         B     NEXTENT             process next entry                   01930000
INVSTMT  MVC   OUTPUT(132),BLANK   clear output record                  01940000
         MVC   OUTPUT(33),=C'IND008E invalid statement ignored' message 01950000
         PUT   SYSPRINT,OUTPUT     print message                        01960000
         B     NEXTENT             process next entry                   01970000
CATERR   C     R15,MAXRC           RC higher than MAXRC?                01980000
         BNH   CATLOW               no, don't update MAXRC              01990000
         ST    R15,MAXRC            yes, update MAXRC                   02000000
CATLOW   MVC   OUTPUT(132),BLANK   clear output record                  02010000
         MVC   OUTPUT(72),=C'IND009E ** VSAM catalog return code is rrrX02020000
                - reason code is IGG0CLxx-nnn' build message            02030000
         MVC   OUTPUT+66(2),CTGFDBK move module id into message         02040000
         CVD   R15,ERRDEC          convert return code to decimal       02050000
         L     R5,ERRDEC+4         get low order decimal word           02060000
         SLL   R5,4                separate last digit from sign        02070000
         ST    R5,ERRDEC           store in high order decimal word     02080000
         UNPK  ERREBC(7),ERRDEC(4) convert to EBCDIC                    02090000
         MVC   OUTPUT+39(3),ERREBC+3 move return code into message      02100000
         LH    R5,CTGREAS1         get reason code                      02110000
         CVD   R5,ERRDEC           convert reason code to decimal       02120000
         L     R5,ERRDEC+4         get low order decimal word           02130000
         SLL   R5,4                separate last digit from sign        02140000
         ST    R5,ERRDEC           store in high order decimal word     02150000
         UNPK  ERREBC(7),ERRDEC(4) convert to EBCDIC                    02160000
         MVC   OUTPUT+69(3),ERREBC+3 move reason code into message      02170000
         PUT   SYSPRINT,OUTPUT     print message                        02180000
         B     NEXTENT             process next entry                   02190000
NOAUTH   MVC   OUTPUT(132),BLANK   clear output record                  02200000
         MVC   OUTPUT(21),=C'IND010T access denied' build message       02210000
         PUT   SYSPRINT,OUTPUT     print message                        02220000
         LA    R15,12              set ..                               02230000
         ST    R15,MAXRC                 .. return code ..              02240000
         B     EXIT                                       .. and exit   02250000
*                                                                       02260000
* data area                                                             02270000
*                                                                       02280000
ENTNAME  DS    CL44                name of entry to modify              02290000
ENTCAT   DC    CL44' '             name of catalog to search for entry  02300000
SECFLAGS DC    CL8'SECFLAGS'       field name for CTGFL                 02310000
MAXRC    DC    F'0'                maximum catalog return code          02320000
PL       DC    8F'0'               catalog parameter list               02330000
FL       DC    6F'0'               catalog field list                   02340000
WA       DC    AL2(WAL)            CTGPL ..                             02350000
         DS    1022X                       .. work area                 02360000
WAL      EQU   *-WA                length of CTGPL work area            02370000
FLDWA    DC    AL2(FLDWAL)         CTGFL ..                             02380000
         DS    1022X                       .. work area                 02390000
FLDWAL   EQU   *-FLDWA             length of CTGFL work area            02400000
RAKFADM  DC    CL39'RAKFADM'       facility name to authorize           02410000
INPUT    DS    CL80                control statement                    02420000
BLANK    DC    C' '                blank to clear output record         02430000
OUTPUT   DS    CL132               output record                        02440000
ERRDEC   DS    D                   return/reason codes decimal          02450000
ERREBC   DS    CL8                 return/reason codes EBCDIC           02460000
SYSIN    DCB   DDNAME=SYSIN,MACRF=GM,DSORG=PS,EODAD=DONE,              X02470000
               LRECL=80,RECFM=FB,BLKSIZE=800      control statements    02480000
SYSPRINT DCB   DDNAME=SYSPRINT,MACRF=PM,DSORG=PS,                      X02490000
               LRECL=132,RECFM=FB,BLKSIZE=1320    messages              02500000
         LTORG ,                   all literals go here                 02510000
*                                                                       02520000
* equates                                                               02530000
*                                                                       02540000
         YREGS                     register equates                     02550000
*                                                                       02560000
* parameter list mappings                                               02570000
*                                                                       02580000
         IEZCTGPL                  catalog parameter list               02590000
         IEZCTGFL                  catalog field parameter list         02600000
         END   RACIND              end of program                       02610000
++SRC(RAKFPROF) DISTLIB(ASRCLIB)  SYSLIB(SRCLIB).
         TITLE 'ESG-SECURITY DATA PROFILE PROCESSOR'                    00010000
* CPARM='XREF(SHORT),OBJ,NODECK',LPARM='AC=1'                 CJYRPROF  00020000
*      DD DSN=SYS1.MACLIB,DISP=SHR                                      00030000
*      DD DSN=SYS1.MODGEN,DISP=SHR                                      00040000
*      DD DSN=SYS3.SAF.PARMLIB,DISP=SHR                                 00050000
         EJECT                                                          00060000
         MACRO                                                          00070000
&LABEL   BASMAC &R1,&S2                                                 00080000
&LABEL.  DC    0XL4'00',X'4D',AL.4(&R1.,0),S(&S2.)                      00090000
         MEND                                                           00100000
CJYRPROF CSECT                                                          00110000
*                                                                       00120000
**********************************************************************  00130000
*                                                                    *  00140000
*    COPYRIGHT (C) 1991 BY CRAIG J. YASUNA.  ALL RIGHTS RESERVED.    *  00150000
*                                                                    *  00160000
*    THIS SOFTWARE PRODUCT OR ANY OF ITS COMPONENTS MUST NOT BE      *  00170000
*    SOLD, GIVEN, OR OTHERWISE DISTRIBUTED TO ANY OTHER COMPANY      *  00180000
*    WITHOUT THE PRIOR WRITTEN PERMISSION OF:                        *  00190000
*                                                                    *  00200000
*                                  CRAIG J. YASUNA, PRESIDENT        *  00210000
*                                  ENTERPRISE SYSTEMS GROUP          *  00220000
*                                  2 MARC COURT                      *  00230000
*                                  EDISON, NEW JERSEY 08820          *  00240000
*                                                                    *  00250000
**********************************************************************  00260000
*                                                                   @01 00260301
*    Change History                                                 @01 00260601
*                                                                   @01 00260901
*    2011/04/03 TRKF120 base version                                @01 00261201
*    2011/04/18 RRKF001 introduce change history                    @01 00261501
*                       enable comment lines in UDATA and PDATA     @01 00261801
*    2011/04/29 RRKF003 if in-core PDATA table already exists check @03 00262101
*                       for READ access to profile RAKFADM in the   @03 00262401
*                       FACILITY class to ensure that only properly @03 00262703
*                       authorized users can replace the in-core    @03 00263003
*                       PDATA table                                 @03 00263303
*                                                                   @03 00263603
********************************************************************@03 00263903
*                                                                       00270000
         SAVE  (14,12),,CJYRPROF_&SYSDATE._&SYSTIME._                   00280000
         LR    R12,R15                 SETUP BASE REG.                  00290000
         USING CJYRPROF,R12            ESTABLISH ADDRESSABILITY         00300000
         LA    R11,SAVEAREA            OUR SAVEAREA                     00310000
         ST    R13,SAVEAREA+4          SAVE HIGH SAVEAREA               00320000
         ST    R11,8(R13)              SAVE LOW SAVEAREA                00330000
         LR    R13,R11                 POINT TO OUR SAVEAREA            00340000
         L     R8,FLCCVT-PSA(R0)       GET CVT POINTER                  00350000
         ICM   R8,B'1111',CVTRAC-CVTMAP(R8) IS RCVT POINTER ZERO?       00360000
         BZ    NOGOOD                       YES, ABEND                  00370000
         ICM   R8,B'1111',RCVTISTL-RCVT(R8) IS POINTER TO OUR AREA 0?   00380000
         BZ    NOGOOD                       YES, ABEND                  00390000
*                                                                   @03 00390303
         ICM    R5,B'0111',CJYPROFS-CJYRCVTD(R8) does PDATA exist?  @03 00390603
         BZ     OK2GO                   NO, go ahead                @03 00390903
         RACHECK ENTITY=RAKFADM,CLASS='FACILITY',ATTR=READ authorize@03 00391203
         LTR    R15,R15                RAKFADM granted?             @03 00391503
         BNZ    ABEND600                NO, abend                   @03 00391803
*                                                                       00400000
         USING CJYRCVTD,R8                                              00410000
OK2GO    DS    0H                      NO, CONTINUE                     00420000
         ENQ   (SECURITY,PROFS,E,,SYSTEM),RET=HAVE                      00430000
         OPEN  (CJYPDATA,(INPUT))      OPEN INPUT FILE                  00440000
         MODESET MODE=SUP,KEY=ZERO     authorize ourselves              00450000
*                                                                       00460000
READLOOP DS    0H                                                       00470000
         MVC   INRECOLD,INRECNEW       SAVE OLD I/P FOR COMPARE         00480000
         GET   CJYPDATA,INRECNEW                                        00490000
         CLI   CLASS,C'*'              Comment?                     @01 00493001
         BE    READLOOP                 read next record            @01 00497001
         CLC   INRECNEW(ACCAUTH-INREC),INRECOLD  NEXT REC. > PREV?      00500000
         BNH   OUTOFORD                          NO, NOT SORTED         00510000
         CLC   INRECNEW(GROUPID-INREC),INRECOLD  SAME CLASS/ENTITY?     00520000
         BE    PERMBLD                 YES, BUILD PERMIT RECORD         00530000
         CLC   =CL8' ',GROUPID         IS THIS A UNIVERSAL RECORD?      00540000
         BNE   NOUAR                   NO, A PERMIT WITHOUT UAR - ABEND 00550000
         GETMAIN RU,LV=RPEL,SP=241                                      00560000
         LR    R10,R1                  ESTABLISH ADDRESSABILITY         00570000
         USING RPECBLK,R10               TO RPE                         00580000
         XC    RPECBLK(RPEL),RPECBLK   INITIALIZE TO BINARY ZEROES      00590000
         MVC   RPENEXT,LASTRPE         POINT TO PREVIOUS RPE            00600000
         ST    R10,LASTRPE             SAVE -> THIS RPE                 00610000
         LA    R11,RPEACCPT            SET PERMIT ANCHOR                00620000
         LA    R1,L'RPECLASN           DEFAULT LENGTH IF NOTFOUND       00630000
         TRT   CLASS,TRTTEST           SEARCH CLASS - BLANKS FOUND?     00640000
         BZ    NOBLANKS                NO, LENGTH ALREADY IN R1         00650000
         S     R1,=A(CLASS)            CALCULATE LENGTH OF DATA         00660000
NOBLANKS DS    0H                                                       00670000
         STC   R1,RPECLASL             SAVE LENGTH                      00680000
         MVC   RPECLASN,CLASS            AND CLASS NAME                 00690000
         XR    R2,R2                   CLEAR R2 FOR NOTFOUND            00700000
         LA    R4,38                   PRESET LENGTH FOR FACILITY       00710000
         CLC   =CL8'FACILITY',RPECLASN IS IT FACILITY?                  00720000
         BE    TESTENT                 YES, TRT                         00730000
         LA    R4,5                    PRESET LENGTH FOR DASDVOL        00740000
         CLC   =CL8'DASDVOL',RPECLASN  IS IT DASDVOL?                   00750000
         BE    TESTENT                 YES, TRT                         00760000
         LA    R4,5                    PRESET LENGTH FOR TAPEVOL        00770000
         CLC   =CL8'TAPEVOL',RPECLASN  IS IT TAPEVOL?                   00780000
         BE    TESTENT                 YES, TRT                         00790000
         LA    R4,43                   PRESET LENGTH FOR DATASET        00800000
         CLC   =CL8'DATASET',RPECLASN  IS IT DATASET?                   00810000
         BE    TESTENT                 YES, TRT                         00820000
         LA    R4,7                    DEFAULT LENGTH IF NOTFOUND       00830000
TESTENT  DS    0H                                                       00840000
         LA    R1,1(R4)                DEFAULT LENGTH FOR SUBTRACTION   00850000
         EX    R4,TRT                  EXECUTE THE TRT                  00860000
         B     CONT                                                     00870000
TRT      TRT   ENTITY(*-*),TRTTEST     SEARCH ENTITY FOR BLANK/*        00880000
CONT     DS    0H                                                       00890000
         B     BRANCTBL(R2)            AND WE FOUND...                  00900000
BRANCTBL DS    0H                                                       00910000
         B     NOTFND                  ... NO BLANKS/NO ASTERISKS       00920000
         B     ASTRKFND                ... AN ASTERISK                  00930000
         B     BLANKFND                ... A BLANK                      00940000
BLANKFND DS    0H                                                       00950000
         LA    R1,1(R1)                ADD 1 TO INCLUDE BLANK IN LENGTH 00960000
ASTRKFND DS    0H                                                       00970000
         S     R1,=A(ENTITY)           CALCULATE LENGTH                 00980000
NOTFND   DS    0H                      IF NOTFOUND LENGTH ALREADY IN R1 00990000
         STC   R1,RPEENTYL             SAVE LENGTH                      01000000
         MVC   RPEENTYN,ENTITY           AND ENTITY NAME                01010000
*                                                                       01020000
         CLC   =CL8' ',ACCAUTH         IS THERE AN ACCESS AUTHORITY?    01030000
         BE    NOACCAUT                NO, BLANK ACCESS AUTH. - ABEND   01040000
*                                                                       01050000
         LA    R1,PATTERN              BEGINNING OF TABLE               01060000
         LA    R2,ROWLEN               LENGTH OF A ROW IN THE TABLE     01070000
         LA    R3,PATTERN+PATTERNL-1   LAST BYTE OF TABLE               01080000
AUTHLP1  DS    0H                                                       01090000
         CLC   ACCAUTH,0(R1)           MATCH ON ACCESS AUTHORITY?       01100000
         BE    MATCH1                  YES, MOVE IT IN                  01110000
         BXLE  R1,R2,AUTHLP1           CHECK NEXT                       01120000
MATCH1   MVC   RPEUACC,8(R1)           MOVE IN BIT PATTERN              01130000
         B     READLOOP                SEE IF ANY MORE I/P RECORDS      01140000
PERMBLD  DS    0H                                                       01150000
         CLC   =CL8' ',GROUPID         IS THERE A GROUP/USERID?         01160000
         BE    PERMINVL                NO, INVALID PERMIT RECORD- ABEND 01170000
*                                                                       01180000
         CLC   =CL8' ',ACCAUTH         IS THERE AN ACCESS AUTHORITY?    01190000
         BE    NOACCAUT                NO, BLANK ACCESS AUTH. - ABEND   01200000
*                                                                       01210000
         GETMAIN RU,LV=RPEACCLN,SP=241                                  01220000
         LR    R9,R1                   ESTABLISH ADDRESSABILITY         01230000
         USING RPEACCLE,R9               TO ACCESS RECORD               01240000
         XC    RPEACCLE(RPEACCLN),RPEACCLE INITIALIZE TO B'0'           01250000
         ST    R9,0(R11)               ESTABLISH POINTER TO HERE        01260000
         MVC   RPEAUSR,GROUPID         SAVE GROUP/USERID OF PERMIT      01270000
*                                                                       01280000
         LA    R1,PATTERN              BEGINNING OF TABLE               01290000
         LA    R2,ROWLEN               LENGTH OF A ROW IN THE TABLE     01300000
         LA    R3,PATTERN+PATTERNL-1   LAST BYTE OF TABLE               01310000
AUTHLP2  DS    0H                                                       01320000
         CLC   ACCAUTH,0(R1)           MATCH ON ACCESS AUTHORITY?       01330000
         BE    MATCH2                  YES, MOVE IT IN                  01340000
         BXLE  R1,R2,AUTHLP2           CHECK NEXT                       01350000
MATCH2   MVC   RPEACS,8(R1)            MOVE IN BIT PATTERN              01360000
         LA    R11,RPEANEXT            PROVIDE ANCHOR FOR NEXT PERMIT   01370000
         B     READLOOP                SEE IF ANY MORE I/P RECORDS      01380000
*                                                                       01390000
EOFILE   DS    0H                                                       01400000
         OC    LASTRPE,LASTRPE         WERE THERE ANY RECORDS I/P?      01410000
         BZ    NOREC                   NO, ABEND                        01420000
         L     R11,CJYPROFS            SAVE ADDRESS OF OLD CHAIN        01430000
         MVC   CJYPROFS,LASTRPE        ACTIVATE NEW CHAIN               01440000
         DROP  R8                                                       01450000
         BASMAC R14,MORERPE            START TO FREE RPE'S              01460000
*                                                                       01470000
         DEQ   (SECURITY,PROFS,,SYSTEM)    RELEASE ENQ                  01480000
         MODESET MODE=PROB,KEY=NZERO   return to problem state          01490000
         CLOSE (CJYPDATA)              CLOSE I/P DATASET                01500000
         WTO   'RAKFPROF7  RAKF PROFILES UPDATED'                       01510000
         L     R13,4(R13)              RESTORE HSA                      01520000
         RETURN (14,12),,RC=0          END O.K.                         01530000
FREERPE  DS    0H                                                       01540000
         L     R8,RPEACCPT             GET ACCESS LIST POINTER          01550000
         B     MOREACC                 SEE IF ANY PERMITS               01560000
FREEACC  DS    0H                                                       01570000
         L     R8,RPEANEXT             GET -> NEXT ACC                  01580000
         FREEMAIN RU,LV=RPEACCLN,A=(R9),SP=241    FREE CURRENT RPE      01590000
MOREACC  DS    0H                                                       01600000
         LTR   R9,R8                   ANY MORE PERMITS?                01610000
         BNZ   FREEACC                 YES, DELETE THEM                 01620000
*                                                                       01630000
         L     R11,RPENEXT             GET -> NEXT RPE                  01640000
         FREEMAIN RU,LV=RPEL,A=(R10),SP=241   FREE CURRENT RPE          01650000
MORERPE  DS    0H                                                       01660000
         LTR   R10,R11                 ANYMORE RPE'S?                   01670000
         BNZ   FREERPE                 YES, FREE THEM                   01680000
         BR    R14                     RETURN TO END RUN                01690000
*                                                                       01700000
NOGOOD   DS    0H                                                       01710000
         WTO   'RAKFPROF1  RCVT NOT PROPERLY INITIALIZED'               01720000
         WTO   'RAKFPROF1  **  PROGRAM TERMINATED  **'                  01730000
         ABEND 100,,STEP                                                01740000
*                                                                       01750000
NOREC    DS    0H                                                       01760000
         WTO   'RAKFPROF2  NO RECORDS INPUT FROM CJYPDATA'              01770000
         WTO   'RAKFPROF2  **  PROGRAM TERMINATED  **'                  01780000
         ABEND 200,,STEP                                                01790000
*                                                                       01800000
OUTOFORD DS    0H                                                       01810000
         L     R11,LASTRPE             SETUP FOR RPE CLEANUP            01820000
         BASMAC R14,MORERPE            GO CLEANUP AND RETURN HERE       01830000
         BASMAC R14,BADMSG             WRITE BADMSG                     01840000
         WTO   'RAKFPROF3  I/P RECORDS OUT OF ORDER'                    01850000
         WTO   'RAKFPROF3  **  PROGRAM TERMINATED  **'                  01860000
         ABEND 300,,STEP                                                01870000
*                                                                       01880000
NOUAR    DS    0H                                                       01890000
         L     R11,LASTRPE             SETUP FOR RPE CLEANUP            01900000
         BASMAC R14,MORERPE            GO CLEANUP AND RETURN HERE       01910000
         BASMAC R14,BADMSG             WRITE BADMSG                     01920000
         WTO   'RAKFPROF4  PERMIT RECORD WITHOUT UNIVERSAL ACCESS RECOR*01930000
               D'                                                       01940000
         WTO   'RAKFPROF4  **  PROGRAM TERMINATED  **'                  01950000
         ABEND 400,,STEP                                                01960000
*                                                                       01970000
PERMINVL DS    0H                                                       01980000
         L     R11,LASTRPE             SETUP FOR RPE CLEANUP            01990000
         BASMAC R14,MORERPE            GO CLEANUP AND RETURN HERE       02000000
         BASMAC R14,BADMSG             WRITE BADMSG                     02010000
         WTO   'RAKFPROF5  PERMIT RECORD WITHOUT GROUPID/USERID'        02020000
         WTO   'RAKFPROF5  **  PROGRAM TERMINATED  **'                  02030000
         ABEND 500,,STEP                                                02040000
*                                                                       02050000
NOACCAUT DS    0H                                                       02060000
         L     R11,LASTRPE             SETUP FOR RPE CLEANUP            02070000
         BASMAC R14,MORERPE            GO CLEANUP AND RETURN HERE       02080000
         BASMAC R14,BADMSG             WRITE BADMSG                     02090000
         WTO   'RAKFPROF6  ENTITY RECORD WITH NO ACCESS AUTHORITY'      02100000
         WTO   'RAKFPROF6  **  PROGRAM TERMINATED  **'                  02110000
         ABEND 500,,STEP                                                02120000
*                                                                   @03 02122003
ABEND600 WTO    'RAKF008W illegal operation -- access denied'       @03 02124003
         WTO    'RAKF008W   ** program terminated **'               @03 02126003
         ABEND  600,,STEP                                           @03 02128003
*                                                                       02130000
BADMSG   DS    0H                                                       02140000
         MVC   BADMSGN+18(80),INRECNEW                                  02150000
BADMSGN  WTO   'RAKFPROF8                                              X02160000
                                                  '                     02170000
         BR    R14                                                      02180000
**********************                                                  02190000
         PRINT NOGEN                                                    02200000
SAVEAREA DS    18F                                                      02210000
LASTRPE  DC    F'0'                                                     02220000
LASTACC  DC    F'0'                                                     02230000
SECURITY DC    CL8'CJYRCVT'                                             02240000
PROFS    DC    CL8'CJYPROFS'                                            02250000
INREC    DS    0CL80                                                    02260000
INRECNEW DS    0CL80                                                    02270000
CLASS    DC    XL8'00'                                                  02280000
ENTITY   DC    XL44'00'                                                 02290000
GROUPID  DC    XL8'00'                                                  02300000
ACCAUTH  DC    XL8'00'                                                  02310000
         DC    XL(80-(*-INREC))'00'                                     02320000
INRECOLD DC    XL80'00'                                                 02330000
PATTERN  EQU   *                                                        02340000
         DC    CL8'ALTER   ',AL1(RACFALTR)                              02350000
ROWLEN   EQU   *-PATTERN                                                02360000
         DC    CL8'CONTROL ',AL1(RACFCNTL)                              02370000
         DC    CL8'UPDATE  ',AL1(RACFUPDT)                              02380000
         DC    CL8'READ    ',AL1(RACFREAD)                              02390000
         DC    CL8'EXEC    ',AL1(RACFEXEC)                              02400000
         DC    CL8'NONE    ',AL1(RACFNONE)                              02410000
PATTERNL EQU   *-PATTERN                                                02420000
TRTTEST  DC    XL256'00'               TABLE TO FIND BLANK/ASTERISK     02430000
         ORG   TRTTEST+C' '            SETUP FLAG FOR BLANK             02440000
         DC    X'08'                                                    02450000
         ORG   TRTTEST+C'*'            SETUP FLAG FOR ASTERISK          02460000
         DC    X'04'                                                    02470000
         ORG                                                            02480000
CJYPDATA DCB   MACRF=GM,EODAD=EOFILE,DDNAME=RAKFPROF,DSORG=PS           02490000
*                                                                   @03 02491003
RAKFADM  DC     CL39'RAKFADM'      facility name to authorize       @03 02495003
*                                                                   @03 02499003
         COPY  CJYRCVTD                                                 02500000
         COPY  CJYPCBLK                                                 02510000
         IHAPSA DSECT=YES                                               02520000
         CVT    DSECT=YES                                               02530000
         ICHPRCVT                                                       02540000
R0       EQU   00                                                       02550000
R1       EQU   01                                                       02560000
R2       EQU   02                                                       02570000
R3       EQU   03                                                       02580000
R4       EQU   04                                                       02590000
R5       EQU   05                                                       02600000
R6       EQU   06                                                       02610000
R7       EQU   07                                                       02620000
R8       EQU   08                                                       02630000
R9       EQU   09                                                       02640000
R10      EQU   10                                                       02650000
R11      EQU   11                                                       02660000
R12      EQU   12                                                       02670000
R13      EQU   13                                                       02680000
R14      EQU   14                                                       02690000
R15      EQU   15                                                       02700000
         END   CJYRPROF                                                 02710000
++SRC(RAKFPSAV) DISTLIB(ASRCLIB)  SYSLIB(SRCLIB).
         TITLE 'Save Password Changes in RAKF Users Table'              00010000
RAKFPSAV CSECT                                                          00020000
         PRINT NOGEN                                                    00030000
*                                                                       00040000
**********************************************************************  00050000
*                                                                    *  00060000
* NAME: RAKFPSAV                                                     *  00070000
*                                                                    *  00080000
* TYPE: Assembler Source                                             *  00090000
*                                                                    *  00100000
* DESC: Save Password Changes in RAKF Users Table                    *  00110000
*                                                                    *  00120000
* FUNCTION: - read the password change queue created by RAKFPWUP     *  00130000
*             into memory                                            *  00140000
*           - loop through the RAKF users table and check each line  *  00150000
*             for a password change queue entry. If an entry exists  *  00160000
*             update that line in place                              *  00170000
*           - clear the password change queue                        *  00180000
*                                                                    *  00190000
* REQUIREMENTS: - RAKF users table pointed to by ddname RAKFUSER     *  00200000
*               - RAKF password change queue pointed to by ddname    *  00210000
*                 RAKFPWUP, a sequential dataset with LRECL=18,      *  00220000
*                 RECFM=F and one line per password change in the    *  00230000
*                 following format:                                  *  00240000
*                                                                    *  00250000
*                 ----+----1----+---                                 *  00260000
*                 uuuuuuuu pppppppp                                  *  00270000
*                                                                    *  00280000
*                 where uuuuuuuu is the username and pppppppp is     *  00290000
*                 the new password, each padded to the right with    *  00300000
*                 blanks to 8 characters.                            *  00310000
*                                                                    *  00320000
**********************************************************************  00330000
*                                                                       00340000
* initialize                                                            00350000
*                                                                       00360000
         SAVE  (14,12),,RAKFPSAV_&SYSDATE._&SYSTIME                     00370000
         USING RAKFPSAV,R15        establish => program EP              00380000
         ST    R13,SAVEAREA+4      save HSA                             00390000
         LA    R11,SAVEAREA        establish => savearea                00400000
         ST    R11,8(R13)          save LSA                             00410000
         LR    R13,R11             setup => our savearea                00420000
         USING SAVEAREA,R13        new addressability                   00430000
         DROP  R15                 program EP no longer needed          00440000
         B     CONTINUE            branch around savearea               00450000
SAVEAREA DS    18F                 savearea                             00460000
*                                                                       00470000
* first read of password change queue to determine number of changes    00480000
*                                                                       00490000
CONTINUE XR    R5,R5               initialize changes counter           00500000
         XR    R6,R6               initialize changes buffer size       00510000
         OPEN  (RAKFPWUP,(INPUT))  open password change queue           00520000
SIZELOOP GET   RAKFPWUP,PWUP       get change record                    00530000
         LA    R5,1(,R5)           increment changes counter            00540000
         A     R6,CHGRECL          increment buffer size                00550000
         B     SIZELOOP            read next change record              00560000
ENDPWUP  CLOSE (RAKFPWUP)          close password change queue          00570000
         LTR   R5,R5               no password changes queued?          00580000
         BZ    RETURN               exit                                00590000
*                                                                       00600000
* second read of password change queue loads all changes in storage     00610000
*                                                                       00620000
         ST    R5,NCHANGES         remember number of changes           00630000
         ST    R6,CHGBSIZE         remember changes buffer size         00640000
         GETMAIN RU,LV=CHGBSIZE    get storage for changes buffer       00650000
         ST    R1,ACHGBUF          remember changes buffer address      00660000
         OPEN  (RAKFPWUP,(INPUT))  reopen password change queue         00670000
         A     R6,ACHGBUF          one byte beyond change buffer        00680000
         USING PWUP,R6             establish changes addressability     00690000
CHGREAD  S     R6,CHGRECL          address record for backward read     00700000
         GET   RAKFPWUP,PWUP       get change record                    00710000
         BCT   R5,CHGREAD          read next change record              00720000
         CLOSE (RAKFPWUP)          close password change queue          00730000
*                                                                       00740000
* update RAKF users table                                               00750000
*                                                                       00760000
         RDJFCB (RAKFUSER)         find out user table DSN for ENQ      00770000
         MODESET MODE=SUP,KEY=ZERO authorize ourselves for SYSDSN ENQ   00780000
         ENQ   (SYSDSN,JFCBDSNM,E,44,SYSTEM),RET=HAVE   serialization   00790000
         ENQ   (SPFEDIT,JFCBDSNM,E,52,SYSTEMS),RET=HAVE serialization   00800000
         MODESET MODE=PROB,KEY=NZERO return to problem state            00810000
         OPEN  (RAKFUSER,(UPDAT))  open users table                     00820000
         USING USERREC,R1          addressability of users table record 00830000
USERLOOP GET   RAKFUSER            get record from users table          00840000
         CLI   USER,C'*'           is it a comment?                     00850000
         BE    USERLOOP             process next record                 00860000
         L     R5,NCHANGES         get number of changes                00870000
         L     R6,ACHGBUF          get changes buffer address           00880000
PWDLOOP  CLC   USER(8),PWUPUSER    do we have an update?                00890000
         BE    UPDATEPW             go process it                       00900000
         A     R6,CHGRECL          address next entry                   00910000
         BCT   R5,PWDLOOP          check next password change record    00920000
         B     USERLOOP            no update found, process next user   00930000
UPDATEPW MVC   PASSWORD(8),PWUPPSWD update password                     00940000
         MVC   SEQNO(8),PWCHANGE   flag sequence number                 00950000
         PUTX  RAKFUSER            update users table record in place   00960000
         B     USERLOOP            process next user                    00970000
ENDUSER  CLOSE (RAKFUSER)          close users table                    00980000
         DROP  R1                  users table record no longer needed  00990000
         MODESET MODE=SUP,KEY=ZERO authorize ourselves for SYSDSN DEQ   01000000
         DEQ   (SPFEDIT,JFCBDSNM,52,SYSTEMS),RET=HAVE release ENQ       01010000
         DEQ   (SYSDSN,JFCBDSNM,44,SYSTEM),RET=HAVE   release ENQ       01020000
         MODESET MODE=PROB,KEY=NZERO return to problem state            01030000
         OPEN  (RAKFPWUP,(OUTPUT)) clear ..                             01040000
         CLOSE (RAKFPWUP)                 .. password change queue      01050000
*                                                                       01060000
* clear and free change queue buffer                                    01070000
*                                                                       01080000
         L     R5,NCHANGES         get number of changes                01090000
         L     R6,ACHGBUF          get changes buffer address           01100000
CLRLOOP  XC    PWUP(CHGLRECL),PWUP clear record                         01110000
         A     R6,CHGRECL          address next entry                   01120000
         BCT   R5,CLRLOOP          clear next change record             01130000
         DROP  R6                  changes buffer no longer needed      01140000
         FREEMAIN RU,LV=CHGBSIZE,A=ACHGBUF free changes buffer          01150000
*                                                                       01160000
* cleanup and return                                                    01170000
*                                                                       01180000
         XC    PWUP(CHGLRECL),PWUP clear local changes queue record     01190000
RETURN   L     R13,SAVEAREA+4      get caller's savearea                01200000
         RETURN (14,12),,RC=0      return                               01210000
*                                                                       01220000
* data area                                                             01230000
*                                                                       01240000
CHGBSIZE DS    F                   size of changes queue buffer         01250000
ACHGBUF  DS    F                   address of changes queue buffer      01260000
NCHANGES DS    F                   number of changes in queue           01270000
CHGRECL  DC    A(CHGLRECL)         record length of changes queue       01280000
PWCHANGE DC    CL8'PWCHANGE'       flag for sequence number field       01290000
SYSDSN   DC    CL8'SYSDSN'         resource name for enqueue            01300000
SPFEDIT  DC    CL8'SPFEDIT'        resource name for enqueue            01310000
RAKFPWUP DCB   DDNAME=RAKFPWUP,MACRF=(GM,PM),EODAD=ENDPWUP,DSORG=PS     01320000
RAKFUSER DCB   DDNAME=RAKFUSER,MACRF=(GL,PM),EODAD=ENDUSER,DSORG=PS,   X01330000
               EXLST=JFCB          DCB with exit list for RDJFCB        01340000
JFCB     DS    0F                  the exit list contains only ..       01350000
         DC    X'87'                .. the target JFCB address ..       01360000
         DC    AL3(INFMJFCB)        .. for RDJFCB                       01370000
         IEFJFCBN                  RDJFCB target                        01380000
         LTORG ,                   all literals go here                 01390000
PWUP     DS    0C                  changes queue record                 01400000
PWUPUSER DC    CL8' '              userid                               01410000
         DC    C' '                filler                               01420000
PWUPPSWD DC    CL8' '              new password                         01430000
         DC    C' '                filler                               01440000
CHGLRECL EQU   *-PWUP              record length of changes queue       01450000
*                                                                       01460000
* equates                                                               01470000
*                                                                       01480000
         YREGS                     register equates                     01490000
*                                                                       01500000
* RAKF users table                                                      01510000
*                                                                       01520000
USERREC  DSECT                     record from users table              01530000
USER     DS    CL8                 user                                 01540000
         DS    CL10                group, filler, flag                  01550000
PASSWORD DS    CL8                 password                             01560000
         ORG   USER+72             any other stuff                      01570000
SEQNO    DS    CL8                 sequence number                      01580000
         END   RAKFPSAV            end of program                       01590000
++SRC(RAKFPWUP) DISTLIB(ASRCLIB)  SYSLIB(SRCLIB).
         TITLE 'RAKF Password Update Program'                           00010000
RAKFPWUP CSECT                                                          00020000
         PRINT NOGEN                                                    00030000
*                                                                       00040000
**********************************************************************  00050000
*                                                                    *  00060000
* NAME: RAKFPWUP                                                     *  00070000
*                                                                    *  00080000
* TYPE: Assembler Source                                             *  00090000
*                                                                    *  00100000
* DESC: Process Password Update Requests                             *  00110000
*                                                                    *  00120000
* FUNCTION: - retrieve username and new password from CSA area       *  00130000
*             allocated by ICHSFR00 in subpool 227 (fetch protected) *  00140000
*           - clear and free CSA area                                *  00150000
*           - append username and new password to the RAKF password  *  00160000
*             change queue, a sequential dataset with LRECL=18,      *  00170000
*             RECFM=F containing one line per password change in the *  00180000
*             following format:                                      *  00190000
*                                                                    *  00200000
*             ----+----1----+---                                     *  00210000
*             uuuuuuuu pppppppp                                      *  00220000
*                                                                    *  00230000
*             where uuuuuuuu is the username and pppppppp is the new *  00240000
*             password, each padded to the right with blanks to 8    *  00250000
*             characters.                                            *  00260000
*                                                                    *  00270000
* REQUIREMENTS: - RAKF password change queue pointed to by ddname    *  00280000
*                 RAKFPWUP using DISP=MOD in the DD statement.       *  00290000
*                                                                    *  00300000
**********************************************************************  00310000
*                                                                       00320000
* initialize                                                            00330000
*                                                                       00340000
         SAVE  (14,12),,RAKFPWUP_&SYSDATE._&SYSTIME                     00350000
         USING RAKFPWUP,R15        establish => program EP              00360000
         ST    R13,SAVEAREA+4      save HSA                             00370000
         LA    R11,SAVEAREA        establish => savearea                00380000
         ST    R11,8(R13)          save LSA                             00390000
         LR    R13,R11             setup => our savearea                00400000
         USING SAVEAREA,R13        new addressability                   00410000
         DROP  R15                 program EP no longer needed          00420000
         B     CONTINUE            branch around savearea               00430000
SAVEAREA DS    18F                 savearea                             00440000
*                                                                       00450000
* Begin of code                                                         00460000
*                                                                       00470000
CONTINUE LR    R5,R1               remember PARM plist address          00480000
         MODESET MODE=SUP,KEY=ZERO authorize ourselves                  00490000
         L     R1,0(,R5)           address of PARM field plist          00500000
         LH    R5,0(,R1)           length of PARM field                 00510000
         CH    R5,=H'8'            is PARM field length 8 characters?   00520000
         BNE   INVPARM              talk dirrty and exit if not         00530000
         MVC   ADDRHEX,2(R1)       get PARM field in check plist        00540000
         MVC   ADDRUNPK,2(R1)      get PARM field for translate         00550000
         TR    ADDRUNPK,HEXTBL     translate PARM field to zoned        00560000
         PACK  ADDRESS(5),ADDRUNPK(9) pack PARM field                   00570000
         L     R1,ADDRESS          address storage pointed to by PARM   00580000
         CLC   0(LPWUPCMD,R1),STRTPWUP parmlist from ICHSFR00 found?    00590000
         BNE   INVPARM              talk dirrty and exit if not         00600000
         MVC   PWUPUSER(8),PWUPUSER-STRTPWUP(R1) get user               00610000
         MVC   PWUPPSWD(8),PWUPPSWD-STRTPWUP(R1) get new password       00620000
         XC    0(LPWUP,R1),0(R1)   clear ICHSFR00 parmlist storage      00630000
         FREEMAIN RU,LV=LPWUP,A=ADDRESS,SP=227 free parmlist storage    00640000
         ENQ    (SECURITY,USERS,E,,SYSTEM),RET=HAVE serialization       00650000
         OPEN  (QUEUE,(OUTPUT))    open password change queue           00660000
         PUT   QUEUE,PWUP          write entry                          00670000
         CLOSE (QUEUE)             close password change queue          00680000
         DEQ   (SECURITY,USERS,,SYSTEM) release ENQ                     00690000
         MVC   SUCCESS+38(8),PWUPUSER move user into success message    00700000
         WTO   MF=(E,SUCCESS)      tell operator                        00710000
*                                                                       00720000
* return                                                                00730000
*                                                                       00740000
RETURN   MODESET MODE=PROB,KEY=NZERO return to problem state            00750000
         L     R13,SAVEAREA+4      get caller's savearea                00760000
         RETURN (14,12),,RC=0      return                               00770000
INVPARM  WTO   'RAKF006W invalid password update request ignored'       00780000
         B     RETURN                                                   00790000
*                                                                       00800000
* data area                                                             00810000
*                                                                       00820000
STRTPWUP DC    AL2(LPWUPCMD)       parameter list that must have been.. 00830000
         DC    X'0000'               .. used to start this RAKFPWUP ..  00840000
         DC    C'S RAKFPWUP,PARM=''' .. run. This is used to perform .. 00850000
ADDRHEX  DS    CL8                   .. a validity check of the CSA ..  00860000
         DC    C''''                 .. storage addressed through ..    00870000
LPWUPCMD EQU   *-STRTPWUP            .. the PARM field                  00880000
PWUP     DS    0C                  changes queue record                 00890000
PWUPUSER DC    CL8' '              userid                               00900000
         DC    C' '                filler                               00910000
PWUPPSWD DC    CL8' '              new password                         00920000
         DC    C' '                filler                               00930000
CHGLRECL EQU   *-PWUP              record length of changes queue       00940000
LPWUP    EQU   *-STRTPWUP          total length of CSA area             00950000
ADDRUNPK DS    CL8                 unpacked address                     00960000
         DC    X'C0'               sign and dummy digit                 00970000
ADDRESS  DS    F                   packed address                       00980000
         DS    X                   dummy digit and sign after pack      00990000
SECURITY DC     CL8'CJYRCVT'       resource name for ENQ                01000000
USERS    DC     CL8'CJYUSRS'       resource name for ENQ                01010000
QUEUE    DCB   DDNAME=RAKFPWUP,MACRF=PM,DSORG=PS password change queue  01020000
SUCCESS  WTO   'RAKF007I password update for user UUUUUUUU queued',MF=L 01030000
*                 0 1 2 3 4 5 6 7 8 9 A B C D E F                       01040000
HEXTBL   DC    X'00000000000000000000000000000000' 0                    01050000
         DC    X'00000000000000000000000000000000' 1                    01060000
         DC    X'00000000000000000000000000000000' 2                    01070000
         DC    X'00000000000000000000000000000000' 3                    01080000
         DC    X'00000000000000000000000000000000' 4                    01090000
         DC    X'00000000000000000000000000000000' 5 translate table    01100000
         DC    X'00000000000000000000000000000000' 6 to convert CSA     01110000
         DC    X'00000000000000000000000000000000' 7 address from PARM  01120000
         DC    X'00000000000000000000000000000000' 8 field to zoned     01130000
         DC    X'00000000000000000000000000000000' 9 format             01140000
         DC    X'00000000000000000000000000000000' A                    01150000
         DC    X'00000000000000000000000000000000' B                    01160000
         DC    X'00FAFBFCFDFEFF000000000000000000' C                    01170000
         DC    X'00000000000000000000000000000000' D                    01180000
         DC    X'00000000000000000000000000000000' E                    01190000
         DC    X'F0F1F2F3F4F5F6F7F8F9000000000000' F                    01200000
*                                                                       01210000
* equates                                                               01220000
*                                                                       01230000
         YREGS                     register equates                     01240000
         END   RAKFPWUP            end of program                       01250000
++SRC(RAKFUSER) DISTLIB(ASRCLIB)  SYSLIB(SRCLIB).
         TITLE 'ESG-SECURITY USERID TABLE LOADER'                       00010000
* CPARM='XREF(SHORT)',LPARM='AC=1'                            CJYRUIDS  00020000
*      DD DSN=SYS1.MACLIB,DISP=SHR                                      00030000
*      DD DSN=SYS1.MODGEN,DISP=SHR                                      00040000
*      DD DSN=SYS3.SAF.PARMLIB,DISP=SHR                                 00050000
         EJECT                                                          00060000
CJYRUIDS CSECT                                                          00070000
*                                                                       00080000
**********************************************************************  00090000
*                                                                    *  00100000
*    COPYRIGHT (C) 1991 BY CRAIG J. YASUNA.  ALL RIGHTS RESERVED.    *  00110000
*                                                                    *  00120000
*    THIS SOFTWARE PRODUCT OR ANY OF ITS COMPONENTS MUST NOT BE      *  00130000
*    SOLD, GIVEN, OR OTHERWISE DISTRIBUTED TO ANY OTHER COMPANY      *  00140000
*    WITHOUT THE PRIOR WRITTEN PERMISSION OF:                        *  00150000
*                                                                    *  00160000
*                                  CRAIG J. YASUNA, PRESIDENT        *  00170000
*                                  ENTERPRISE SYSTEMS GROUP          *  00180000
*                                  2 MARC COURT                      *  00190000
*                                  EDISON, NEW JERSEY 08820          *  00200000
*                                                                    *  00210000
**********************************************************************  00220000
*                                                                   @01 00220301
*    Change History                                                 @01 00220601
*                                                                   @01 00220901
*    2011/04/03 TRKF120 base version                                @01 00221201
*    2011/04/18 RRKF001 introduce change history                    @01 00221501
*                       enable comment lines in UDATA and PDATA     @01 00221801
*                       consistently don't specify msg descriptor   @01 00222101
*                       fix S378 after error in first line of UDATA @01 00222401
*                       add missing DEQ for SECURITY,USERS          @01 00222701
*    2011/04/26 RRKF002 enable end users to change their passwords  @02 00223001
*                       permanently: Before updating the incore     @02 00223301
*                       users table RAKFUSER calls RAKFPSAV to      @02 00223602
*                       update UDATA with the temporary password    @02 00223902
*                       changes queued since the previous execution @02 00224202
*    2011/04/29 RRKF003 if in-core UDATA table already exists check @03 00224502
*                       for READ access to profile RAKFADM in the   @03 00224802
*                       FACILITY class to ensure that only properly @03 00225103
*                       authorized users can replace the in-core    @03 00225403
*                       UDATA table                                 @03 00225703
*                                                                   @03 00226003
********************************************************************@03 00226303
*                                                                       00230000
         SAVE   (14,12),,CJYRUIDS_&SYSDATE._&SYSTIME                    00240000
         LR     R12,R15             USE ENTRY AS BASE                   00250000
         USING  CJYRUIDS,R12        " " "                               00260000
         ST     R13,SAVEAREA+4      SAVE CALLER'S SAVE AREA ADDR        00270000
         LA     R2,SAVEAREA         OUR SAVEAREA                        00280000
         ST     R2,8(R13)           SAVE OUR AREA                       00290000
         LR     R13,R2              LOAD REGISTER 13 WITH SAVEAREA      00300000
*                                                                       00310000
         L      R8,FLCCVT-PSA(0)    GET CVT ADDR                        00320000
         ICM    R8,B'1111',CVTRAC-CVTMAP(R8) GET RCVT ADDR, IS IT 0?    00330000
         BZ     ABEND100             Y - CJYRCVT FAILED, GO TO ABEND    00340000
         ICM    R8,B'1111',RCVTISTL-RCVT(R8) GET OUR POINTER, IS IT 0?  00350000
         BZ     ABEND100             YES, ABEND                         00360000
*                                                                   @03 00360303
         ICM    R5,B'0111',CJYUSERS-CJYRCVTD(R8) does UDATA exist?  @03 00360603
         BZ     OK2GO                NO, go ahead                   @03 00360903
         RACHECK ENTITY=RAKFADM,CLASS='FACILITY',ATTR=READ authorize@03 00361203
         LTR    R15,R15             RAKFADM granted?                @03 00361503
         BNZ    ABEND600             NO, abend                      @03 00361803
*                                                                   @03 00362103
OK2GO    ENQ    (SECURITY,USERS,E,,SYSTEM),RET=HAVE serialization   @03 00370000
*                                                                   @02 00372002
         L      R15,RAKFPSAV        get password changer address    @02 00374002
         BALR   R14,R15             call it                         @02 00376002
*                                                                   @02 00378002
         OPEN   (CJYUDATA,(INPUT))  OPEN INPUT FILE                     00380000
         MODESET MODE=SUP,KEY=ZERO  authorize ourselves                 00390000
         XR     R5,R5               initialize GM chain             @01 00395001
*                                                                       00400000
READLOOP GET    CJYUDATA,RECORD     GET RECORD                          00410000
         CLI    USERID,C'*'         Comment?                        @01 00413001
         BE     READLOOP             read next record               @01 00417001
         CLC    USERID,OLDUSER      IS INPUT IN SORT SEQUENCE?          00420000
         BE     NEWGROUP             same USER, check for new group @01 00430000
         BNH    ABEND2               not in sort seq, tell about it @01 00435001
         CLI    USERID,C' '         UID?                                00440000
         BE     ABEND2               NO ---                             00450000
         CLI    PASSWORD,C' '       PSWD?                               00460000
         BE     ABEND2               NO ---                             00470000
         CLI    GROUP,C' '          GROUP.                              00480000
         BE     ABEND2               NO ---                             00490000
         CLI    OPERFLAG,C'Y'       FLAG ??                             00500000
         BE     RECOK                RECORD OK                          00510000
         CLI    OPERFLAG,C'N'       FLAG ??                             00520000
         BNE    ABEND2               TOUGH                              00530000
*                                                                       00540000
RECOK    MVC    OLDREC,RECORD       REFRESH LAST USER ID READ           00550000
         GETMAIN RU,LV=CBLKL,SP=241 GET AREA IN CSA                     00560000
         LR     R5,R1               GM AREA ADDR TO WORK REG.           00570000
         USING  CBLK,R5             HELLO BALL ---                      00580000
         XC     CBLK(CBLKL),CBLK    CLEAR TO B'0'                       00590000
         MVC    CBLKNEXT,LASTGM     GET PREV GETMAINED ADDR.            00600000
         ST     R5,LASTGM           SAVE ADDR OF GM AREA.               00610000
         LA     R7,CBLKGRPS        POINTER FOR GROUP                    00620000
*                                                                       00630000
         LA     R1,8               MAX                                  00640000
         TRT    USERID,TRTBLANK    FIND BLANK                           00650000
         BZ     MOVEUID            GO MOVE USERID                       00660000
         S      R1,=A(USERID)      SUBTRACT BEGINNING                   00670000
MOVEUID  STC    R1,CBLKUSRL        SAVE LENGTH                          00680000
         MVC    CBLKUSRI,USERID    MOVE USERID                          00690000
*                                                                       00700000
         LA     R1,8               MAX                                  00710000
         TRT    GROUP,TRTBLANK     FIND BLANK                           00720000
         BZ     MOVEGRP            GO MOVE GROUP                        00730000
         S      R1,=A(GROUP)       SUBTRACT BEGINNING                   00740000
MOVEGRP  STC    R1,CBLKGRPL        SAVE LENGTH                          00750000
         MVC    CBLKGRPN,GROUP     MOVE GROUP                           00760000
*                                                                       00770000
         LA     R1,8               MAX                                  00780000
         TRT    PASSWORD,TRTBLANK  FIND BLANK                           00790000
         BZ     MOVEPSW            GO MOVE PASSWORD                     00800000
         S      R1,=A(PASSWORD)    SUBTRACT BEGINNING                   00810000
MOVEPSW  STC    R1,CBLKPWDL        SAVE LENGTH                          00820000
         MVC    CBLKPWDE,PASSWORD  MOVE USERID                          00830000
         BCTR   R1,0               LENGTH OF PSWD - 1 FOR EX.           00840000
         EX     R1,SCRAMBLE        SCRABLE PSWD                         00850000
         MVC    CBLKFLG1,OPERFLAG  MOVE OPER FLAG TO ENTRY.             00860000
*                                                                       00870000
         B      ADDGROUP           ADD CONNECTS                         00880000
*                                                                       00890000
SCRAMBLE XC     CBLKPWDE(0),=C'SECURITY'  SCRAMBLE PSWD                 00900000
*                                                                       00910000
NEWGROUP CLC    GROUP,OLDGROUP      IS INPUT IN SORT SEQUENCE?          00920000
         BNH    ABEND2                 N - TELL ABOUT IT                00930000
         MVC    OLDREC,RECORD       REFRESH LAST USER ID READ           00940000
ADDGROUP GETMAIN RU,LV=CONNL,SP=241 GET AREA IN CSA                     00950000
         LR     R6,R1              GM AREA ADDR TO WORK REG.            00960000
         USING  CONNGRUP,R6        HELLO BALL ---                       00970000
         XC     CONNGRUP(CONNL),CONNGRUP CLEAR AREA                     00980000
         ST     R6,0(R7)           SAVE IN LAST POINTER                 00990000
         LA     R7,CONNNEXT        SAVE FOR NEXT TIME                   01000000
*                                                                       01010000
         LA     R1,8               MAX                                  01020000
         TRT    GROUP,TRTBLANK     FIND BLANK                           01030000
         BZ     MOVECONN           GO MOVE GROUP                        01040000
         S      R1,=A(GROUP)       SUBTRACT BEGINNING                   01050000
MOVECONN STC    R1,CONNGRPL        SAVE LENGTH                          01060000
         MVC    CONNGRPN,GROUP     MOVE GROUP                           01070000
*                                                                       01080000
         CLI    DFLTGRPF,C'*'      DEFAULT FLAG SET?                    01090000
         BNE    READLOOP           NO, DON'T MOVE                       01100000
         STC    R1,CBLKGRPL        SAVE LENGTH                          01110000
         MVC    CBLKGRPN,GROUP     MOVE GROUP                           01120000
         B      READLOOP           READ NEXT RECORD.                    01130000
*                                                                       01140000
ENDDATA  OC     OLDUSER,OLDUSER    EMPTY INPUT FILE?                    01150000
         BZ     ABEND300              Y - TELL ABOUT IT AND TERMINATE.  01160000
         L      R4,CJYUSERS-CJYRCVTD(R8)       GET OLD TABLE PTR        01170000
         MVC    CJYUSERS-CJYRCVTD(,R8),LASTGM  UPDATE PTR TO NEW TABLE  01180000
         B      ANYMORE                        GO CHK FOR ANY ENTRIES.  01190000
*                                      N - FALL THROUGH                 01200000
FREELOOP L      R4,CBLKNEXT        LOOP THRU OLD TABLE                  01210000
         L      R7,CBLKGRPS-CBLK(R5)  GROUP LOOP                        01220000
         B      FREECON2                                                01230000
FREECONN L      R7,CONNNEXT        LOOP THRU OLD TABLE                  01240000
         FREEMAIN RU,LV=CONNL,A=(R6),SP=241 FREE CURRENT ENTRY.         01250000
FREECON2 LTR    R6,R7              MORE ENTRIES?                        01260000
         BNZ    FREECONN               Y - NEXT ENTRY                   01270000
         FREEMAIN RU,LV=CBLKL,A=(R5),SP=241 FREE CURRENT ENTRY.         01280000
ANYMORE  LTR    R5,R4              MORE ENTRIES?                        01290000
         BNZ    FREELOOP               Y - NEXT ENTRY                   01300000
*                                                                       01310000
         TM     TERMFLAG,TERMABND  WAS AN ABEND REQUESTED?              01320000
         BO     ABEND200               Y-ABEND                          01330000
BYEBYE   MODESET MODE=PROB,KEY=NZERO   return to problem state          01340000
         CLOSE (CJYUDATA)              close input dataset              01350000
         DEQ   (SECURITY,USERS,,SYSTEM) release ENQ                 @01 01355001
         WTO    'RAKFUIDS4  USER TABLE UPDATED'                         01360000
         L      R13,SAVEAREA+4     LOAD CALLER'S SAVE AREA ADDR         01370000
         RETURN (14,12),RC=0                                            01380000
*                                                                       01390000
ABEND100 WTO    'RAKFUIDS1  RCVT NOT PROPERLY INITIALIZED'          @01 01400000
         WTO    'RAKFUIDSX  ** PROGRAM TERMINATED **',DESC=(2)          01410000
         ABEND  100,,STEP                                               01420000
*                                                                       01430000
ABEND2   WTO    'RAKFUIDS2  INPUT DATA INVALID OR OUT OF SEQ.',DESC=(2) 01440000
         MVC    PASSWORD,=C'********'                                   01450000
         MVC    BADMSG+8(80),RECORD                                     01460000
BADMSG   WTO    '                                                      X01470000
                                           ',DESC=(2)                   01480000
         WTO    'RAKFUIDSX  ** PROGRAM TERMINATED **',DESC=(2)          01490000
         OI     TERMFLAG,TERMABND          SET TO ABEND @ TERMINATION   01500000
         LR     R4,R5                      GET LAST GM AREA ADDR AND    01510000
         B      ANYMORE                    GO CLEAN THE GM CHAIN.       01520000
*                                                                       01530000
ABEND200 ABEND  200,,STEP              Y - ABEND !                      01540000
*                                                                       01550000
ABEND300 WTO    'RAKFUIDS3  EMPTY INPUT FILE ?!?!'                  @01 01560000
         WTO    'RAKFUIDSX  ** PROGRAM TERMINATED **'               @01 01570000
         ABEND  300,,STEP                                               01580000
*                                                                   @03 01582003
ABEND600 WTO    'RAKF008W illegal operation -- access denied'       @03 01584003
         WTO    'RAKF008W   ** program terminated **'               @03 01586003
         ABEND  600,,STEP                                           @03 01588003
*                                                                       01590000
TRTBLANK DC     XL256'00'          TABLE FOR TRT                        01600000
         ORG    TRTBLANK+C' '      BLANK                                01610000
         DC     X'04'                                                   01620000
         ORG                                                            01630000
*                                                                       01640000
         PRINT  NOGEN                                                   01650000
CJYUDATA DCB    DDNAME=RAKFUSER,MACRF=GM,EODAD=ENDDATA,DSORG=PS         01660000
*                                                                       01670000
RAKFPSAV DC     V(RAKFPSAV)        password change utility          @02 01673002
RAKFADM  DC     CL39'RAKFADM'      facility name to authorize       @03 01675003
*                                                                   @02 01677002
SAVEAREA DS     18F                                                     01680000
LASTGM   DC     F'0'                                                    01690000
SECURITY DC     CL8'CJYRCVT'                                            01700000
USERS    DC     CL8'CJYUSRS'                                            01710000
TERMFLAG DC     X'00'                                                   01720000
TERMABND EQU    X'01'                                                   01730000
RECORD   DS     0XL80                                                   01740000
USERID   DS     CL8                                                     01750000
DFLTGRPF DS     X                  DEFAULT GROUP FLAG                   01760000
GROUP    DS     CL8                                                     01770000
         DS     X                                                       01780000
PASSWORD DS     CL8                                                     01790000
         DS     X                                                       01800000
*                                                                       01810000
OPERFLAG DS     X                                                       01820000
         DS     XL(80-(*-RECORD))                                       01830000
OLDREC   DS     0XL80                                                   01840000
OLDUSER  DS     CL8                                                     01850000
         DS     X                                                       01860000
OLDGROUP DS     CL8                                                     01870000
         DS     XL(80-(*-OLDREC))                                       01880000
*                                                                       01890000
R0       EQU    00                                                      01900000
R1       EQU    01                                                      01910000
R2       EQU    02                                                      01920000
R3       EQU    03                                                      01930000
R4       EQU    04                                                      01940000
R5       EQU    05                                                      01950000
R6       EQU    06                                                      01960000
R7       EQU    07                                                      01970000
R8       EQU    08                                                      01980000
R9       EQU    09                                                      01990000
R10      EQU    10                                                      02000000
R11      EQU    11                                                      02010000
R12      EQU    12                                                      02020000
R13      EQU    13                                                      02030000
R14      EQU    14                                                      02040000
R15      EQU    15                                                      02050000
*                                                                       02060000
         COPY    CJYRCVTD                                               02070000
         COPY    CJYUCBLK                                               02080000
         ICHPRCVT                                                       02090000
         CVT     DSECT=YES                                              02100000
         IHAPSA  DSECT=YES                                              02110000
*                                                                       02120000
         END                                                            02130000
++MAC(RACIND) DISTLIB(APROCLIB) SYSLIB(PROCLIB).
//RACIND   JOB
//********************************************************************
//*
//* Name: RACIND
//*
//* Desc: Run RACIND Utility
//*
//* FUNTION: Act upon control statements read from SYSIN to set or
//*          clear the RACF indicator of VSAM catalog entries. The
//*          following control statements are valid:
//*
//*          ----+----1----+----2----+----3----+----4----+----5----+
//*          CATALOG   name of catalog to search for entries
//*          RACON     name of entry to indicate
//*          RACOFF    name of entry to unindicate
//*          * Comment
//*
//*          Any number of control statements is allowed. The first
//*          none comment statement must be a CATALOG statement. A
//*          CATALOG statement remains active until a new CATALOG
//*          statement replaces it.
//*
//********************************************************************
//RACIND  EXEC PGM=RACIND
//SYSPRINT DD  SYSOUT=*
//SYSIN    DD  *
**********************************************************************
*
* Example: Switch on the RACF indicator for a VSAM catalog
*          and a cluster contained in that catalog.
*
* Note:    - The data and index components of a VSAM catalog
*            MUST NOT be RACF indicated.
*
*          - All other entry types MUST have either all of their
*            components RACF indicated or all components not
*            indicated.
*
*          For that reason in the example only one RACON statement
*          is coded for the catalog, but three for the cluster.
*
**********************************************************************
CATALOG   SYS1.UCAT.TST
RACON     SYS1.UCAT.TST
RACON     TSTCAT.CLUSTER
RACON     TSTCAT.CLUSTER.INDEX
RACON     TSTCAT.CLUSTER.DATA
/*
//
++MAC(RAKFPROF) DISTLIB(APROCLIB) SYSLIB(PROCLIB).
//RAKFPROF EXEC PGM=RAKFPROF                                            00010000
//RAKFPROF DD DSN=SYS1.SECURE.CNTL(PROFILES),DISP=SHR                   00020000
++MAC(RAKFPWUP) DISTLIB(APROCLIB) SYSLIB(PROCLIB).
//RAKFPWUP EXEC PGM=RAKFPWUP                                            00010000
//RAKFPWUP  DD  DSN=SYS1.SECURE.PWUP,DISP=MOD                       @02 00020002
++MAC(RAKF) DISTLIB(APROCLIB) SYSLIB(PROCLIB).
//RAKF     EXEC PGM=ICHSEC00                                            00010000
//IEFPARM  DD DCB=(LRECL=80,BLKSIZE=80,DSORG=PO),DISP=(,DELETE),        00020000
//            UNIT=SYSDA,SPACE=(80,(1,0,1))                             00030000
//RAKFPROF DD DSN=SYS1.SECURE.CNTL(PROFILES),DISP=SHR                   00040000
//RAKFUSER DD DSN=SYS1.SECURE.CNTL(USERS),DISP=SHR                      00050000
//RAKFPWUP DD DSN=SYS1.SECURE.PWUP,DISP=SHR                         @06 00060006
++MAC(RAKFUSER) DISTLIB(APROCLIB) SYSLIB(PROCLIB).
//RAKFUSER EXEC PGM=RAKFUSER                                            00010000
//RAKFUSER DD DSN=SYS1.SECURE.CNTL(USERS),DISP=SHR                      00020000
//RAKFPWUP DD DSN=SYS1.SECURE.PWUP,DISP=SHR                         @02 00030002
++MAC(RAKFINIT) DISTLIB(APARMLIB) SYSLIB(PARMLIB).
NO
/* control RAKF initialization:                                      */
/* YES -- initialize RAKF                                            */
/* NO  -- don't initialize RAKF                                      */
/* ASK -- ask operator                                               */
??
/*
//SMPCNTL  DD  *
 REJECT SELECT(TRKF126) .
 RESETRC.
 RECEIVE SELECT(TRKF126) .
/*
//*
//* ------------------------------------------------------------------*
//* SMP apply of RAKF 1.2.6                                           *
//* Expected APPLY step return code: 04 or lower                      *
//*                                   |                               *
//*                                    --> resulting from RC=8 in SMP *
//*                                        generated LINKEDITs.       *
//*                                                                   *
//* After successful APPLY continue as outlined in member $$$$INST    *
//* of the installation JCL-Library, step 5 (Run Job D_ACCPT) to      *
//* SMP ACCEPT RAKF or step 6 (IPL CLPA) if you want to skip the      *
//* SMP ACCEPT for now.                                               *
//* ------------------------------------------------------------------*
//APPLYR  EXEC SMPAPP
//SYSLIB   DD
//         DD
//         DD
//         DD
//         DD
//         DD
//         DD
//         DD  DISP=SHR,DSN=SYSGEN.RAKF.V1R2M0.MACLIB
//MACLIB   DD  DISP=SHR,DSN=SYSGEN.RAKF.V1R2M0.MACLIB
//SAMPLIB  DD  DISP=SHR,DSN=SYSGEN.RAKF.V1R2M0.SAMPLIB
//SRCLIB   DD  DISP=SHR,DSN=SYSGEN.RAKF.V1R2M0.SRCLIB
//SMPCNTL  DD  *
 APPLY S(TRKF126) DIS(WRITE) .
/*
//* ------------------------------------------------------------------*
//* SMP accept of RAKF 1.2.6                                          *
//* Expected return code: 00                                          *
//* ------------------------------------------------------------------*
//ACCEPTR EXEC SMPAPP
//SYSLIB   DD
//         DD
//         DD
//         DD
//         DD
//         DD
//         DD
//         DD  DISP=SHR,DSN=SYSGEN.RAKF.V1R2M0.AMACLIB
//AMACLIB  DD  DISP=SHR,DSN=SYSGEN.RAKF.V1R2M0.AMACLIB
//APARMLIB DD  DISP=SHR,DSN=SYSGEN.RAKF.V1R2M0.APARMLIB
//APROCLIB DD  DISP=SHR,DSN=SYSGEN.RAKF.V1R2M0.APROCLIB
//ASAMPLIB DD  DISP=SHR,DSN=SYSGEN.RAKF.V1R2M0.ASAMPLIB
//MACLIB   DD  DISP=SHR,DSN=SYSGEN.RAKF.V1R2M0.MACLIB
//SAMPLIB  DD  DISP=SHR,DSN=SYSGEN.RAKF.V1R2M0.SAMPLIB
//ASRCLIB  DD  DISP=SHR,DSN=SYSGEN.RAKF.V1R2M0.ASRCLIB
//SRCLIB   DD  DISP=SHR,DSN=SYSGEN.RAKF.V1R2M0.SRCLIB
//SMPCNTL  DD  *
 ACCEPT S(TRKF126) DIS(WRITE) .
/*
//
