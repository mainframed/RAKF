//ZAPMVS38 JOB (RAKF),
//             'ZAP MVS for RAKF',
//             CLASS=A,
//             MSGCLASS=X,
//             REGION=2048K,
//             MSGLEVEL=(1,1)
//*-------------------------------------------------------------------*
//*                                                                   *
//* Name: ZAPMVS38                                                    *
//*                                                                   *
//* Desc: ZAP MVS 3.8 OPEN, RENAME and SCRATCH processing for RAKF    *
//*                                                                   *
//* !!! Caution: Refer to member $$$$M38J before running this job !!! *
//*                                                                   *
//*-------------------------------------------------------------------*
//ZAP     EXEC PGM=AMASPZAP
//SYSLIB   DD DISP=SHR,DSN=SYS1.LPALIB      <== LPA modules to be ZAPed
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
*
* original ZAP to OPEN processing from
* Phil D's RAKF installation instructions
*
* ===> comment out the following 6 statements  <===
* ===> if you've already applied Phil D's ZAP  <===
*
  NAME IFG0194A IFG0194C
   IDRDATA PD050823
   VER 1392 47E0
   REP 1392 4700
   VER 13FA 47E0
   REP 13FA 4700
*
* fix additional security wholes in OPEN processing
*
  NAME IFG0194A IFG0194C
   IDRDATA JW110302
   VER 13EE 4780
   REP 13EE 4700
   VER 1426 47E0
   REP 1426 4700
*
* fix security wholes in DADSM RENAME processing
*
  NAME IGC00030 IGG03001
   IDRDATA JW110302
   VER 02E8 4710
   REP 02E8 47F0
   VER 02E0 4780
   REP 02E0 4700
   VER 032A 4710
   REP 032A 4700
   VER 036A 47E0
   REP 036A 47F0
   VER 0372 4710
   REP 0372 4700
*
* fix security wholes in DADSM SCRATCH processing
*
  NAME IGC0002I IGG0290A
   IDRDATA JW110302
   VER 0298 47E0
   REP 0298 47F0
   VER 02BA 4780
   REP 02BA 4700
*
* fix security wholes in DADSM SPACE ALLOCATION processing
*
  NAME IGG0553A IGG0553A
   IDRDATA JW110302
   VER 014A 4780
   REP 014A 4700
   VER 015C 4780
   REP 015C 4700
   VER 016E 4780
   REP 016E 4700
*
  NAME IGG0553A IGG0553E
   IDRDATA JW110302
   VER 0234 4780
   REP 0234 4700
/*
//
