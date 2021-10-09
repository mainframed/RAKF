//ZJW0003  JOB (RAKF),
//             'Modify MSTRJCL',
//             CLASS=A,
//             MSGCLASS=X,
//             REGION=8192K,
//             MSGLEVEL=(1,1)
//*-------------------------------------------------------------------*
//*                                                                   *
//* Name: ZJW0003                                                     *
//*                                                                   *
//* DESC: Install USERMOD ZJW0003 to modify generation of MSTRJCL     *
//*       RAKF DD statements added for early initialization           *
//*                                                                   *
//*-------------------------------------------------------------------*
//RECEIVE EXEC SMPREC
//SMPPTFIN DD  *
++USERMOD (ZJW0003).
++VER (Z038) FMID(EBB1102).
++MACUPD(SGIEE0MS).
./ CHANGE NAME=SGIEE0MS
         DC    CL80'//RAKFPROF DD DSN=SYS1.SECURE.CNTL(PROFILES),'      04870010
         DC    CL80'//            DISP=SHR'                             04870011
         DC    CL80'//RAKFUSER DD DSN=SYS1.SECURE.CNTL(USERS),'         04870012
         DC    CL80'//            DISP=SHR'                             04870013
/*
//SMPCNTL  DD  *
 REJECT  SELECT(ZJW0003)
 .
 RESETRC
 .
 RECEIVE SELECT(ZJW0003)
 .
/*
//APPLY   EXEC SMPAPP
//AMODGEN  DD  DISP=SHR,DSN=SYS1.AMODGEN
//SMPCNTL  DD  *
 APPLY   SELECT(ZJW0003)
         DIS(WRITE)
 .
/*
//
