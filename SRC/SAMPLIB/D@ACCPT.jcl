//RAKFACPT JOB (RAKF),
//             'RAKF Installation',
//             CLASS=A,
//             MSGCLASS=X,
//             REGION=8192K,
//             MSGLEVEL=(1,1)
//* ------------------------------------------------------------------*
//* SMP accept of RAKF 1.2.0                                          *
//* Expected return code: 00                                          *
//* ------------------------------------------------------------------*
//ACCEPT  EXEC SMPACC
//SYSLIB   DD
//         DD
//         DD
//         DD
//         DD  DISP=SHR,DSN=HLQ.AMACLIB
//AMACLIB  DD  DISP=SHR,DSN=HLQ.AMACLIB
//APARMLIB DD  DISP=SHR,DSN=HLQ.APARMLIB
//APROCLIB DD  DISP=SHR,DSN=HLQ.APROCLIB
//ASAMPLIB DD  DISP=SHR,DSN=HLQ.ASAMPLIB
//MACLIB   DD  DISP=SHR,DSN=HLQ.MACLIB
//SAMPLIB  DD  DISP=SHR,DSN=HLQ.SAMPLIB
//ASRCLIB  DD  DISP=SHR,DSN=HLQ.ASRCLIB
//SRCLIB   DD  DISP=SHR,DSN=HLQ.SRCLIB
//SMPCNTL  DD  *
 ACCEPT S(TRKF120) DIS(WRITE) .
/*
//
