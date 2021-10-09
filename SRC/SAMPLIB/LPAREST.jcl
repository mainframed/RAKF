//LPAREST  JOB (RAKF),
//             'ZAP MVS for RAKF',
//             CLASS=A,
//             MSGCLASS=X,
//             REGION=2048K,
//             MSGLEVEL=(1,1)
//* ------------------------------------------------------------------*
//*                                                                   *
//* Restore LPA modules ZAPed for RAKF installation                   *
//*                                                                   *
//* !!! Caution: Refer to member $$$$M38J before running this job !!! *
//*                                                                   *
//* ------------------------------------------------------------------*
//RESTORE EXEC PGM=IEBCOPY
//LPABKUP  DD  DISP=SHR,
//             DSN=RAKF.LPALIB.BACKUP    <== Backup copy of LPA modules
//LPALIB   DD  DISP=SHR,DSN=SYS1.LPALIB  <== Modules are restored here
//SYSPRINT DD  SYSOUT=*
//SYSIN    DD  *
 COPY INDD=((LPABKUP,R)),OUTDD=LPALIB
/*
//
