#!/binbash

# Bash script to create the SMP4 job stream for RAKF
# This will install RAKF only

cat << 'END'
//RAKFPREP JOB (RAKF),
//             'RAKF Installation',
//             CLASS=A,
//             MSGCLASS=A,
//             REGION=8192K,
//             MSGLEVEL=(1,1)
//* *******************************************************************
//* Installs RAKF 1.2.6
//*
//* This JCL was generated from makesmp.sh
//*
//* ------------------------------------------------------------------*
//* Prepare system for RAKF 1.2.6 installation                        *
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
/*
//* ------------------------------------------------------------------*
//* SMP receive of RAKF 1.2.6                                         *
//* Expected return code: 00                                          *
//* ------------------------------------------------------------------*
//RECEIVER EXEC SMPREC
//SMPPTFIN DD DATA,DLM='??'
END

echo "++FUNCTION(TRKF126)."
echo "++VER(Z038) SUP(RRKF006)."
echo "++JCLIN."
cat JCLIN/*.jcl

for i in MACLIB/*; do
    #++MAC(CJYPCBLK) DISTLIB(AMACLIB)  SYSLIB(MACLIB)
    filename=$(basename -- "$i")
    filename="${filename%.*}"
    echo "++MAC($filename) DISTLIB(AMACLIB)  SYSLIB(MACLIB)."
    cat $i
done

for i in SRCLIB/*; do
    filename=$(basename -- "$i")
    filename="${filename%.*}"
    echo "++SRC($filename) DISTLIB(ASRCLIB)  SYSLIB(SRCLIB)."
    cat $i
done

for i in PROCLIB/*; do
    filename=$(basename -- "$i")
    filename="${filename%.*}"
    echo "++MAC($filename) DISTLIB(APROCLIB) SYSLIB(PROCLIB)."
    cat $i
done

for i in PARMLIB/*; do
    filename=$(basename -- "$i")
    filename="${filename%.*}"
    echo "++MAC($filename) DISTLIB(APARMLIB) SYSLIB(PARMLIB)."
    cat $i
done

cat << 'END'
??
/*
//SMPCNTL  DD  *
 /* REJECT SELECT(TRKF126) . */
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
//         DD  DISP=SHR,DSN=SYSGEN.RAKF.V1R2M6.AMACLIB
//AMACLIB  DD  DISP=SHR,DSN=SYSGEN.RAKF.V1R2M6.AMACLIB
//APARMLIB DD  DISP=SHR,DSN=SYSGEN.RAKF.V1R2M6.APARMLIB
//APROCLIB DD  DISP=SHR,DSN=SYSGEN.RAKF.V1R2M6.APROCLIB
//ASAMPLIB DD  DISP=SHR,DSN=SYSGEN.RAKF.V1R2M6.ASAMPLIB
//MACLIB   DD  DISP=SHR,DSN=SYSGEN.RAKF.V1R2M6.MACLIB
//SAMPLIB  DD  DISP=SHR,DSN=SYSGEN.RAKF.V1R2M6.SAMPLIB
//ASRCLIB  DD  DISP=SHR,DSN=SYSGEN.RAKF.V1R2M6.ASRCLIB
//SRCLIB   DD  DISP=SHR,DSN=SYSGEN.RAKF.V1R2M6.SRCLIB
//SMPCNTL  DD  *
 ACCEPT S(TRKF126) DIS(WRITE) .
/*
//
END