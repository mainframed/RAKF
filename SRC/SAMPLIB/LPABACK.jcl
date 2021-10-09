//LPABACK  JOB (RAKF),
//             'ZAP MVS for RAKF',
//             CLASS=A,
//             MSGCLASS=X,
//             REGION=2048K,
//             MSGLEVEL=(1,1)
//* ------------------------------------------------------------------*
//*                                                                   *
//* Backup LPA modules to be ZAPed for RAKF installation              *
//*                                                                   *
//* !!! Caution: Refer to member $$$$M38J before running this job !!! *
//*                                                                   *
//* ------------------------------------------------------------------*
//BACKUP  EXEC PGM=IEBCOPY
//LPALIB   DD  DISP=SHR,DSN=SYS1.LPALIB  <== LPA modules to be backuped
//LPABKUP  DD  DISP=(,CATLG),SPACE=(TRK,(3,1,5)),
//             UNIT=SYSDA,DCB=*.LPALIB,
//             DSN=RAKF.LPALIB.BACKUP    <== backup copy of LPA modules
//SYSPRINT DD  SYSOUT=*
//SYSIN    DD  *
 COPY INDD=((LPALIB,R)),OUTDD=LPABKUP
 SELECT MEMBER=(IGC0002I,IGG0290A,IGG0300F,IGC00030,IGC0003{,          -
               IGG03001,IGG03002,IGG0553A,IGG0553F,IFG0194A,IFG0194F,  -
               IFG0194J,IFG0195G,IFG0204A,IFG0204J,IFG0554A,IFG0554C,  -
               IFG0554F,IFG0554J,IGG0190A,IGG0550P)
/*
//
