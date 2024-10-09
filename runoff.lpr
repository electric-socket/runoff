 // Original options for Stanford Pascal

 (* $D-              NO DEBUG *)
 (* $K-              NO EXECUTION PROFILE *)
 (* $F-              DO NOT SAVE FLOATING POINT REGS *)
 (* $Q+              ACCEPT ALL FORMS OF SQUARE BRACKET, (UNL ONLY) *)




(*                                   RNF
     RNF is a text formatting program similar but not identical to Runoff
 (Runoff is a registered trademark of the digital Equipment Corporation).
     RNF was originally written, implemented and maintained by Robert Foster
 and later maintained by Richard Chilausky, both of the Computing Services
 Office of the University of Illinois.
     It was converted from Pascal 6000 (for CDC machines) to Stanford
 Pascal (for IBM machines) by Curt Hill of University Of Nebraska at
 Lincoln. This version should be portable to any Pascal running on machines
 that use the ASCII or EBCDIC character sets.
     Our thanks to Richard Chilausky, for his help in the original
 conversion to Stanford Pascal.
     We appreciate and acknowledge George F. Badger, Jr., Directer Of Computing
 Services Office at the University of Illinois at Urbana-Champaign, for
 graciously allowing free distribution of this program.
      Distribution of this program is primarily tied to distribution
  of the Stanford Pascal compiler. Revisions (if any should occur), will
  be distributed in the same way. If you require a CDC compatible
  implementation, contact the Office of Computer Services, University of
  Illinois, Urbana, Illinois. If you desire a copy of this program for
  an implementation that uses the ASCII or EBCDIC character set
  that is not based on a CDC or Ibm machine then contact me. Bug reports
  and general comments should be addressed to myself or Sassan Hazeghi,
  who will relay them to me.
      There are no warranties, expressed or implied, concerning this program
  by anyone mentioned herein.

                                              Curt Hill
                                              225 Nebraska Hall
                                              University Of Nebraska - Lincoln
                                              Lincoln, Nebraska
                                              (402) 472-3701

      If the address changes, check Pascal News for an update.

               *)

  (* Change history:
    MODIFY     4/21 Make lines <= 72 chars so modify can work. BF
   NONMP       4/21 Symptom: .nonmp causes pages not to be counted.
               FIx: rewrite macro. BF
   PERIOD      5/12 Command with no prefix turns off period spacing.
               Fix: use true instead of yes.BF
   HL          5/12 HL not reset by .CH. Fix: add to macro. BF
   PUSHSYL     4/23 Symptom: using .AP, a forced .PP or the entire
               line it began sometimes vanished.  Cause: forced
               commands were not saved properly over forced page
               ejects.  Fix: save the pending symbol when forcing;
               for this special case, push it so will come back when
               .frcpage concludes.  (Introduces alloc to handle
               macro free list, which is needed for future
               enhancements.) BF
  GETCUR       5/17 Restructure code to allow for future enhancements.
               Move input line reading to getcur; move asis
               handling to command handler.  Change fixes bug where
               blank line in asis text caused autoparagraph after asis
               concluded. BF
  UNDL         5/18 Make underlining part of environment so undone
               by pops and not done in page footers and headers. BF
  DOLLAR       5/18 Don't think $BLANK or $NUMBER is a variable. BF
  BR           5/23 When .SP >1, A .BR after the first line on a page
               left one or more blank lines.  Problem is hokey code
               around page throws.  Fix it so every pushed line forces
               a .CR And saves symbol for next time around.  Much
               cleaner now. BF
 USB           5/25 Significant blank (SB) bugs: a) SB at end of
               justified line screwed up justification, b) SB "pushed"
               to new line were lost, c) controversy over whether to
               underline SB. Solution: add .USB command (underline
               significant blanks) to turn on or off. W, BF
 ARGS          5/27 SIgnificant change to macro handling: macro
               "parms" (macros defined by macro invocation) are
               initially defined as null and remain well-defined
               until redefined by another invocation.  Permits
               macros such as ".MACRO CHAP * = .CH .CHAP1", as
               .CHAP1 retains its meaning until .CHAP called again.
               Note that if this had been available before, the
               ..TTL circumlocution would not have been needed. BF
 RIGHT         6/6 Alternating fill broken by forcing .CR after
               each line.  Reset alternating fill only on .PP
               command. BF
 DOMID         6/17 Needed some more clrlines in domid.  BF
 EMPTY         7/11 If the first symbol in a line exceeded the
               right margin, addword would never put it on a
               line, but pushsyl'd it infinitely.  Symptom:
               stack overflow in procedure alloc. Fix: don't
               push first symbol on an empty line. BF
 ADDSYL        7/13 Change "right" was wrong; broke .X at
               line overflow.  Now need something to hold
               symbol in both in and out formats; added
               addsyl for this purpose. BF
 VAR           7/13 Allow .VAR $X = expression where both $X and
               expression can be macro arguments (or anything). BF
OLNO           7/16 OLNO change to predefined variable $$OLNO so
               that output line number available at runtime. RC
 PARAGR        7/22 If paragraph's test page did a .FRCPAGE, the
               paragraph indent was lost.  Showed up in manual.
               Reported by rick cichelli. Fix is to call pushsyl. BF
 PS            9/14 .PS changed right margin as well as page # right
               margin if issued after a .RM. Corrected to work as
               documented ie change page # right margin only.
               Implemented change by adding RNF variable $$PRM. RC
 ASIS          9/19 Asis mode allows spurious underlining to happen.
               Fix: reinitialize entire line before processing asis
               text. RC
 WHEN          9/20 Two commands added allow date(.DATE) and time
               (.TIME) to be accessed. Date as DD/MM/YY and time as
               HH:MM. RC
 DELMAC        9/20 Command added (.DELMAC) to allow deletion of user
               defined macros from macro list. RC
 NONMP         9/23 .FRCPAGE forced 6 lines for non page numbered
               output and 7 lines for page numbered output. Force
               an extra line when page not numbered via system
               macro ..NMP RC
 ELIST         9/26 Elist does not reset the left margin unless
               explicitly forced to. Fix add a clrline after
               popping the environment(twice) to reset the
               left margin. RC
 ASIS1         9/27 Asis mode does not process the end of asis
               mode flag(×) so that the remainder of the line
               will be processed. Fix: set the character
               pointer(cup) to the character following the flag
               character. RC
 MACDEF        10/3 Macro definition not allowed on same line as
               variable definition. Fix change pushsyl to
               backupsyl. RC
 NOFILL        10/3 Nofill text no longer needs an explicit break
               to force a page with single line of text. Accomplished
               by checking defrsym in the main procedure and getting
               a new line when there are no deferred commands. RC
 CENTER        10/5 .C does an extra cr. Affects the spacing when
               .SP is greater than 1. Fix remove the extra .CR. RC
 CENTER2       10/7 Correction in center would work only in text
               was filled. Corrected to work for both filled and
               nofill test.  RC
 SIGCOM        10/7 When using .SIG, all commands which required
               arguments were picking up blanks as arguments.
               Added case statement to turn .SIG off when processing
               these commands. RC
  AUTOCAP       A new facility has been added. Letters that begin a sentence,
                paragraph, chapter, or sub-chapter are automatically
                capitalized. To maintain compatibility with previous versions
                this feature is only done following the .AUTOCAP command,
                and .NOAUTOCAP is the default. Further, no auto capitalization
                is done when in .ASCII mode.   CH
 QRR           This is to print the input file that is being processed.
               This file is usually ignored. CH
 DEBUG1        This is the old debug plus debug2. CH
 DEBUG2        Print on the right margin (linlen) the number of the
               input line that generated this output line. This aids
               finding the correct input line that needs changing. CH

Changes by Paul Robinson (not affiliated with any of the above.
 2024-October-9   Disable compiler $ switches as they were for
                  Stanford Pascal. Changed all comments above from
                  all caps to mixed case. Converted program to work
                  with the Free Pascal compiler.
 *)

PROGRAM RUNOFF (INPUT,OUTPUT,QRR);

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes
  { you can add units after this };

CONST
 VPAGE = 1; VCH = 2; VHL =3; VLIST = 9; (* NEXT IS 20 *)
 VLM = 20; VRM = 21; VSP = 22; VNMP = 23; VOLNO = 24; (* NEXT IS 25 *)
 VPRM = 25; (* NEXT IS 26 *)
 VARMAX = 100;
 MACCHR = '.';
 VARCHR = '$';
 CMDCHR = '.';
 TABMAX = 16;
 LINLEN = 132;
 MAXENP = 20;
 HLMAX = 5;
 MAXLIST = 10;
 FIGMAX = 10;
 MACMAX = 10;
 VHLMAX = 5;
 CASEDIFF = 64; (* DIFFERENCE BETWEEN UPPER AND LOWER CASE, WITH UPPER>LOWER
                   ASCII = 32,     EBCDIC = 64;         *)
 CHARSIZE = 256;    (* NUMBER OF CHARS IN CHARACTER SET FOR EBCDIC *)

TYPE
 VARTYP = (VITEM,VARRAY);
 RELOPR = (EQ,GT,LT,NE,GE,LE);
 ENRANGE = 0..MAXENP;
 SIGN = (PLUS,MINUS,UNSIGNED,INVALID);
 SYMTYP = (WORD,COMMAND,VARS,NONE);
 LLEN = 0..LINLEN;
 LALEN = 1..LINLEN;
 JUSLIN = RECORD
  NDX: LLEN;
  POS: ARRAY (/LALEN/) OF INTEGER
 END;
 LINE = RECORD
  CENTER,BBAR: BOOLEAN;
  VLEN,LEN: LLEN;
  LIN: ARRAY (/LALEN/) OF CHAR;    (* HOLDS THE LINE TO BE PROCESSEED *)
  LINT : ARRAY(/LALEN/) OF RECORD
                           LINC : ARRAY (/1..4/) OF CHAR;
         (* EACH ELEMENT OF LINT CORRESPONDS TO A LETTER OF LIN.
            THE FIRST THREE CHARACTERS ARE POSSIBLE OVERSTRIKE CHARACTERS
            AND THE FOURTH CHARACTER IS A INDEX AS TO THE NUMBER OF OVERSTRIKES
           *)              END;
 END;
 CMDTYP = (CBLANK,CCR,CBREAK,CRESPAG,CRES,
           CESCCHR,CASCII,CCENTER,CJUST,(*CUL,*)CLMAR,CRMAR,
           CFILL,CSIG,CPAGE,CSUP,CLPT,CSTD,CPS,CSAV,CP,
           CPP,CAP,CI,CSP,CS,CTP,CCH,CHL,CNMP,CPNO,CTITLE,CST,
           CATITLE,CLIST,CLE,CELIST,CFIG,CBAR,CBB,CEB,CU,
           CT,CTAB,CTABS,CRT,CRIGHT,CLINES,CMACRO,CX,
           CVAR,CINC,CASIS,CDEC,CDATE,CTIME,CDELMAC,
           (* END OF CMDTYP SET *)
           CFLAG,CFLAGCAPS,CFLAGOVER,CFLAGSIG,CLOWER,CUPPER,CPERIOD,
           CSAVPAG,CTOP,CMID,CBOT,CARRAY,
           CFMT,CIF,CDOT,CREM,CUPP,CUSB,CADD,CDEBUG1,CDEBUG2,CAUTOCAP,
           NOTCMD);
 ENVIRON = RECORD
            J,F,PR,SG,UN:   BOOLEAN;
            PM,SP:  INTEGER;
            LM,RM,PS,PT,PRM: LLEN;
            TB:  ARRAY (/1..TABMAX/) OF LLEN
          END;
MACTYP = (HEADER,PARM);
MACLEN = 1..MACMAX;
MACSYM =  ARRAY(/MACLEN/) OF CHAR;
ALINE =  ARRAY(/LALEN/) OF CHAR;
MAC = RECORD
      ON: BOOLEAN;
      CA,MA: @MAC;
      CU,BE,EN: LLEN;
      MT: MACTYP;
      NP: 0..8;
      LI: @ALINE;
      NM: MACSYM
     END;
PMAC = @MAC;
 SETOFCHAR = ARRAY (/0..255/) OF BOOLEAN;
VAR
 SYMTYPE: SYMTYP;
 SYL,OTL,TMPL: LINE;    (* THE LINE ENTERS THROUGH SYL AND THEN IS TRANSFERRED
                           A WORD AT A TIME INTO TMPL, WHERE THE CASE IS
                           ADJUSTED AND OVERSTRIKES ADDED AND THEN IS MOVED
                           OTL WHERE IT IS JUSTIFIED, CENTERED AND WRITTEN *)
 FRCSYL,ADDSYL: LINE;
 FREEMACP: PMAC;
 CMDS: ARRAY (/CMDTYP/) OF  ARRAY (/1..10/) OF CHAR;
 CMDTYPE: CMDTYP;
 LN: LLEN;
 FLAG,LOWER,FLAGCAPS,FLAGOVER,FLAGSIG: BOOLEAN;
 PQEND,PERIOD,ESCCHR: BOOLEAN;
 YES,PAGE: BOOLEAN;
 JUSTIT,RIGHT: BOOLEAN;
 (*UL,*) LPT: BOOLEAN;
 FILL,SIGBL: BOOLEAN;
 SUP: BOOLEAN;
 PARA,AP,PREL,ATITLE:  BOOLEAN;
 ILNO: INTEGER;
 OPNO,OBTXT,OETXT,OVETXT,OEPAG,OVBTXT: INTEGER;
 TMAR: LLEN;
 PMAR: INTEGER;
 JUST,SAVJUST: JUSLIN;
 ENSTK: ARRAY (/ENRANGE/) OF ENVIRON; ENP: ENRANGE;
 PARSPACE,PARTEST,SPACING: INTEGER;
 PAGENV: ENVIRON;
 DEFRB: INTEGER;
 FORCE: BOOLEAN;
 FIRSTCH: BOOLEAN;
 FIGP: 0..FIGMAX; FIGN: ARRAY (/1..FIGMAX/) OF INTEGER;
  UNDL: BOOLEAN;
 BAR,BB,HOLDBB:  BOOLEAN;
 RIGHTSPACE: 0..136;
 TABS: ARRAY(/1..TABMAX/) OF LLEN;
 TAB,DOTN: LLEN;
 DEFRSYM: BOOLEAN;
 RT,T,DOT:   BOOLEAN;
 BREAKSET,OPTBRKSET,CRSET: SET OF CBLANK..CDEC;
 EMPTY: BOOLEAN;
 ACTP,MACLSTP,BOTMAC,TTLMACP,STLMACP,CHTMACP: PMAC;
 NOTMACRO: BOOLEAN;
 CUR: ARRAY(/LALEN/) OF CHAR;
 BEG,ENL,CUP,LASTCUP: LLEN;
 XTEND: BOOLEAN;
 LASTLEN,LASTVLEN,LASTSLEN,LASTSVLEN: LLEN;
 VID: ARRAY (/1..VARMAX/) OF ALFA;
 VAL: ARRAY (/1..VARMAX/) OF INTEGER;
 VTY: ARRAY (/1..VARMAX/) OF VARTYP;
 VUP: ARRAY (/1..VARMAX/) OF 1..VARMAX;
 VARNDX,TV: 1..VARMAX;
 PUSHED: BOOLEAN;
 PAGSAV,PAGSAVS,TMPLSAV: LINE;
 PAGOTL,PAGSYL: BOOLEAN;
 CH: CHAR;
 EP: LLEN;
 ARELOPR: ARRAY (/RELOPR/) OF ALFA;
 EXPRERR,SHOWEXPR: BOOLEAN;
  ITEMSET, TERMSET : SETOFCHAR;
  SUB, ERRORS : INTEGER;
  LITERAL1 : ARRAY (/1..60/) OF CHAR;
  LITERAL2 : ARRAY (/1..57/) OF CHAR;
  LITERAL3 : ARRAY (/1..16/) OF CHAR;
  LITERAL4 : ARRAY (/1..38/) OF CHAR;
  LITERAL5 : ARRAY (/1..27/) OF CHAR;
  BLANKLINC : ARRAY (/1..4/) OF CHAR;
 ROMLC,LASTFILD: BOOLEAN;
 EOFINPUT: BOOLEAN;
 XTRABL,USB: BOOLEAN;
 CMMD: BOOLEAN;
 LETTERS : ARRAY (/0..25/) OF CHAR; (* EASES RELIANCE ON UNDERLYING CHAR SET *)
 DEBUG1, DEBUG2 : BOOLEAN;
 CAPNEXT : BOOLEAN;
 AUTOCAP : BOOLEAN;
   (*$E        *)
   FUNCTION INLALPHA (CH:CHAR):BOOLEAN;
     (* THIS FUNCTION IS EXPRESSLY FOR THE PURPOSE OF THE
        FOLLOWING TEST :
                CH IN (×'a'..'z'×)
                               *)
    BEGIN
      INLALPHA := FALSE;
      IF (CH >= 'a') AND (CH <= 'i')
         THEN INLALPHA := TRUE
         ELSE IF (CH>='j') AND (CH<='r')
                 THEN INLALPHA := TRUE
                 ELSE IF (CH>='s') AND (CH<='z')
                         THEN INLALPHA := TRUE
                         ELSE;
       END;  (* SO MUCH FOR INLALPHA *)



   FUNCTION INALPHA (CH:CHAR):BOOLEAN;
     (* THIS FUNCTION IS EXPRESSLY FOR THE PURPOSE OF THE
        FOLLOWING TEST :
                CH IN (×'A'..'Z'×)
                               *)
    BEGIN
      INALPHA := FALSE;
      IF (CH >= 'A') AND (CH <= 'I')
         THEN INALPHA := TRUE
         ELSE IF (CH>='J') AND (CH<='R')
                 THEN INALPHA := TRUE
                 ELSE IF (CH>='S') AND (CH<='Z')
                         THEN INALPHA := TRUE
                         ELSE;
       END;  (* SO MUCH FOR INALPHA *)

   FUNCTION INDIGIT (CH : CHAR) : BOOLEAN;
     (* THIS MODULE PERFORMS THE TEST: CH IN (×0..9×) WHICH IS
        SOMEWHAT DEFICIENT USING EBCDIC.
                              *)
     BEGIN
       INDIGIT := ((CH >= '0') AND (CH <= '9'));
       END;  (* INDIGIT *)




   FUNCTION INSETOFCHAR (S : SETOFCHAR; CH : CHAR) : BOOLEAN;
     (* THIS MODULE PERFORMS THE TEST: CH IN S , WHERE
        S IS DEFINED AS SET OF CHAR.
                              *)
     BEGIN
       INSETOFCHAR := S(/ ORD (CH) /) = TRUE
       END;  (* INSETOFCHAR *)                            (*$E  *)
   PROCEDURE ERR;
     BEGIN
       WRITE(' ERROR:  ');
       ERRORS := ERRORS + 1;
     END;    (* END OF ERR *)

  PROCEDURE ERRE;
    VAR T: PMAC;
    BEGIN
     WRITELN(' ON INPUT LINE ',ILNO:4);
     T := ACTP;
     WHILE T@.CA<>NIL DO
        BEGIN
        WRITELN('       WITHIN MACRO ',T@.NM:10);
        T := T@.CA;
        END;
    END;     (* END OF ERRE *)



  PROCEDURE SYLERR;
    BEGIN
    WRITE(' ''');
    FOR LN:=1 TO SYL.LEN DO WRITE(SYL.LIN(/LN/));
    WRITE('''');
    ERRE
    END;     (* END OF SYLERR *)

  PROCEDURE UPCASESYL;
    VAR I: LLEN;
    BEGIN
    I := 0;
    WITH SYL DO
      FOR LN:=1 TO LEN DO
          IF INLALPHA (LIN(/LN/) )
             THEN LIN (/LN/) := CHR( ORD( LIN(/LN/) ) + CASEDIFF )
             ELSE
    END;     (* END OF UPCASESYL *)
   FUNCTION  LOWERTHECASE (C:CHAR):CHAR;
      (* LOWER THE CASE IF ADVISABLE  *)
     BEGIN
       IF INALPHA(C)
          THEN LOWERTHECASE := CHR( ORD(C) - CASEDIFF)
          ELSE LOWERTHECASE := C;
     END;



  PROCEDURE NEXTCH;
    BEGIN
    IF EP<SYL.LEN THEN BEGIN EP := EP + 1; CH := SYL.LIN(/EP/) END
           ELSE IF EP=SYL.LEN THEN BEGIN EP := EP + 1; CH := ' ' END
       ELSE BEGIN EXPRERR := TRUE; CH := ' ' END;
    END;     (* END OF NEXTCH *)

FUNCTION TERM:INTEGER; FORWARD;
FUNCTION VARIABLE: INTEGER;
VAR V: ALFA; I: INTEGER; VNDX1,VNDX2: 0..VARMAX; VAR1: INTEGER;
BEGIN
 NEXTCH;
 V := '          ';
 I := 0;
 WHILE (INALPHA(CH) OR INDIGIT(CH) OR (CH = '$')) DO
  BEGIN I := I + 1;
   IF I<=10 THEN V(/I/) := CH;
   NEXTCH;
  END;
 VAR1 := 0;
 IF I=0 THEN EXPRERR := TRUE
 ELSE
  BEGIN
   VID(/TV/) := V;
   VNDX1 := 1;
   VNDX2 := 0;
   WHILE VID(/VNDX1/)<>V DO VNDX1 := VNDX1 + 1;
   IF VNDX1<>TV THEN
    BEGIN
     IF (VTY(/VNDX1/)=VARRAY) AND (CH='[') THEN
      BEGIN
       NEXTCH;
       VNDX2 := TERM;
       IF CH<>']' THEN EXPRERR := TRUE ELSE NEXTCH;
       IF (VNDX2<0) OR (VNDX2>VUP(/VNDX1/)) THEN
        BEGIN ERR; WRITE('ARRAY INDEX OUT OF BOUNDS'); SYLERR;
              VNDX2 := 0 END;
      END;
     IF CH='=' THEN
      BEGIN
       NEXTCH;
       VAL(/VNDX1+VNDX2/) := TERM;
      END;
     VAR1 := VAL(/VNDX1+VNDX2/);
    END
   ELSE BEGIN ERR; WRITE('UNDEFINED VARIABLE: $',V); ERRE END;
  END;
VARIABLE := VAR1;
END;     (* END OF VARIABLE *)

FUNCTION ITEM: INTEGER;
VAR SIGN,ITEM1: INTEGER;
BEGIN
 SIGN := 1;
 IF CH='-' THEN BEGIN SIGN := 2; NEXTCH END
 ELSE IF CH='+' THEN NEXTCH
 ELSE IF CH='#' THEN BEGIN SIGN := 3; NEXTCH END;
 ITEM1 := 0;
 IF CH='$' THEN ITEM1 := VARIABLE
 ELSE IF INDIGIT(CH) THEN
  REPEAT
   ITEM1 := ITEM1*10 + (ORD(CH)-ORD('0'));
   NEXTCH
  UNTIL NOT (INDIGIT(CH))
 ELSE EXPRERR := TRUE;
 CASE SIGN OF
  2: ITEM1 := - ITEM1;
  3: IF ITEM1=0 THEN ITEM1 := 1 ELSE ITEM1 := 0;
  1:
 END;
 ITEM := ITEM1;
END;     (* END OF ITEM *)

FUNCTION TERM;
VAR TERM1,TERM2: INTEGER; TCH: CHAR;
BEGIN
 TERM1 := 0;
 IF CH='(' THEN
  BEGIN
   NEXTCH;
   TERM1 := TERM;
   IF CH<>')' THEN EXPRERR := TRUE ELSE NEXTCH;
  END
 ELSE IF INSETOFCHAR(ITEMSET,CH) THEN
  BEGIN
   TERM1 := ITEM;
   WHILE (CH = '+') OR (CH = '-') DO
    BEGIN
     TCH := CH;
     NEXTCH;
     TERM2 := 0;
     IF INSETOFCHAR(ITEMSET,CH) THEN TERM2 := ITEM
     ELSE IF CH='(' THEN TERM2 := TERM;
     IF TCH='+' THEN TERM1 := TERM1 + TERM2
     ELSE TERM1 := TERM1 - TERM2;
    END;
  END;
 TERM := TERM1;
END;     (* END OF TERM *)

FUNCTION RELOP: RELOPR;
VAR OP: ALFA; ROP: RELOPR;
BEGIN
 NEXTCH;
 OP := '          ';
 OP(/1/) := CH; NEXTCH;
 OP(/2/) := CH; NEXTCH;
 RELOP := EQ;
 IF CH='.' THEN NEXTCH;
 FOR ROP:=EQ TO LE DO IF ARELOPR(/ROP/)=OP THEN RELOP := ROP;
END;     (* END OF RELOP *)

FUNCTION EXPR: INTEGER;
VAR EXPR1,EXPR2,EXPR3: INTEGER; EXPROP: RELOPR;
BEGIN
 WITH SYL DO
  BEGIN
   UPCASESYL;
   IF (LIN(/EP/)='$') AND (LIN(/EP+1/)='(') THEN EP := EP + 1;
   CH := LIN(/EP/);
   EXPR1 := 0;
   EXPRERR := FALSE;
   IF INSETOFCHAR(TERMSET,CH) THEN
    BEGIN
     EXPR1 := TERM;
     IF CH='.' THEN
      BEGIN
       EXPROP := RELOP;
       EXPR2 := 0;
       IF INSETOFCHAR(TERMSET,CH) THEN EXPR2 := TERM;
       EXPR3 := 0;
       CASE EXPROP OF
        EQ: IF EXPR1=EXPR2 THEN EXPR3 := 1;
        GT: IF EXPR1>EXPR2 THEN EXPR3 := 1;
        LT: IF EXPR1<EXPR2 THEN EXPR3 := 1;
        NE: IF EXPR1<>EXPR2 THEN EXPR3 := 1;
        GE: IF EXPR1>=EXPR2 THEN EXPR3 := 1;
        LE: IF EXPR1<=EXPR2 THEN EXPR3 := 1
        END;
       EXPR1 := EXPR3;
      END
    END;
   EXPR := EXPR1;
   SHOWEXPR := TRUE;
   IF (EP<=LEN) AND (CH=';') THEN BEGIN NEXTCH; SHOWEXPR := FALSE; END;
   IF EP<=LEN THEN EXPRERR := TRUE;
   IF EXPRERR THEN
    BEGIN ERR; WRITE('ERROR IN EXPRESSION:'); SYLERR END;
  END;
END;     (* END OF EXPR *)

PROCEDURE GETNUM(VAR S: SIGN; VAR N: INTEGER);
BEGIN
 WITH SYL DO
  BEGIN
   N := 0;
   LN := 2;
   IF LIN(/1/)='+' THEN S := PLUS
   ELSE IF LIN(/1/)='-' THEN S := MINUS
   ELSE BEGIN S := UNSIGNED; LN := 1 END;
   EP := LN;
   N := EXPR;
   IF EXPRERR THEN S := INVALID;
  END;
END;     (* END OF GETNUM *)

PROCEDURE CLRTAB;
BEGIN
 FOR LN:=1 TO TABMAX DO TABS(/LN/) := 0;
END;     (* END OF CLRTAB *)

FUNCTION GETTAB(X: INTEGER): INTEGER;
BEGIN
 LN := 1;
 TABS(/TABMAX/) := X;
 WHILE TABS(/LN/)<X DO LN := LN + 1;
 JUST.NDX := 0;
 RT := FALSE;
 T := FALSE;
 GETTAB := TABS(/LN/);
END;     (* END OF GETTAB *)

PROCEDURE SAVENV(VAR E: ENVIRON);
BEGIN
 WITH E DO
  BEGIN
   LM := VAL(/VLM/); RM := VAL(/VRM/); PM := PMAR;
   PS := PARSPACE; PT := PARTEST; PR := PREL;
   J := JUSTIT; F := FILL;
   SP := VAL (/VSP/);
   TB := TABS;
   SG := SIGBL; UN := UNDL;
   PRM:=VAL(/VPRM/);
  END
END;     (* END OF SAVENV *)

PROCEDURE RESENV(VAR E: ENVIRON);
BEGIN
 WITH E DO BEGIN
  VAL(/VLM/) := LM; VAL(/VRM/) := RM; PMAR := PM;
  PARSPACE := PS; PARTEST := PT; PREL := PR;
  JUSTIT := J; FILL := F;
  VAL(/VSP/) := SP;
  TABS := TB;
  SIGBL := SG; UNDL := UN;
  VAL(/VPRM/):=PRM;
 END
END;     (* END OF RESENV *)

PROCEDURE PSHENV;
BEGIN
 SAVENV(ENSTK(/ENP/));
 IF ENP=MAXENP THEN
  BEGIN ERR; WRITE('TOO MANY P OR LIST LEVELS'); ERRE END
 ELSE ENP := ENP + 1;
END;     (* END OF PSHENV *)

PROCEDURE POPENV;
BEGIN
 IF ENP=0 THEN
  BEGIN ERR; WRITE('TOO MANY POPS'); ERRE END
 ELSE ENP := ENP - 1;
 RESENV(ENSTK(/ENP/));
END;     (* END OF POPENV *)


   PROCEDURE UPCASE(VAR L: LINE);
     VAR I,J: LLEN;
     BEGIN
      WITH L DO
          FOR I := 1 TO LEN DO
              IF INLALPHA (LIN(/I/) )
                 THEN LIN(/I/) := CHR (ORD(LIN(/I/) ) + CASEDIFF)
                 ELSE
      END;      (* UPCASE *)





PROCEDURE FLAGERR(C1,C2: CHAR);
BEGIN
 WRITELN;
 WRITELN('FLAG ERROR IN INPUT LINE ',ILNO:5,':');
 WRITELN('  ''',C1,C2,''' SEQUENCE NOT ALLOWED -- ''',
  C1,''' DELETED')
END;     (* END OF FLAGERR *)

PROCEDURE DOJUST(VAR L: LINE; VAR F: JUSLIN; RIGHT: BOOLEAN);
VAR
 I,J,K,N,M: LLEN;
BEGIN        (* DOJUST *)
 WITH L,F DO
  BEGIN
   IF LEN>2 THEN
    IF XTRABL THEN BEGIN LEN := LEN - 1 END;
   IF (NOT CENTER) AND (NDX>1) AND (LEN<=VAL(/VRM/)+1) THEN
    BEGIN
     I := NDX; J := VAL(/VRM/);
     N := (VAL(/VRM/)-LEN+1) DIV (NDX-1);
     M := (VAL(/VRM/)-LEN+1) MOD (NDX-1);
     LEN := J + 1;
     FOR K:=NDX DOWNTO 2 DO
      BEGIN
       FOR LN:=POS(/K/) DOWNTO POS(/K-1/)+1 DO
        BEGIN LINT(/J/) := LINT(/LN/); LIN(/J/) := LIN(/LN/); J := J - 1 END;
       FOR LN:=1 TO N DO
        BEGIN LINT(/J/).LINC := BLANKLINC; LIN(/J/) := ' ';
                 J := J - 1 END;
       IF RIGHT THEN
        BEGIN IF (NDX-K)<=M THEN
         BEGIN LINT(/J/).LINC := BLANKLINC; LIN(/J/) := ' ';
                   J := J - 1 END
        END
       ELSE IF (K-2)<=M THEN
        BEGIN LINT(/J/).LINC := BLANKLINC;  LIN(/J/) := ' ';
                         J := J - 1  END
      END
    END
  END;
END;     (* END OF DOJUST *)

PROCEDURE SAVOTL;
BEGIN
 TMPLSAV := TMPL;
 PAGSAV := OTL;
 PAGOTL := TRUE;
END;     (* END OF SAVOTL *)

PROCEDURE SAVSYL;
BEGIN
 PAGSAVS := OTL;
 PAGSYL := TRUE;
 SAVJUST := JUST;
END;     (* END OF SAVSYL *)

PROCEDURE CLRLINE;
BEGIN
 WITH OTL DO
  BEGIN
   FOR LN:=1 TO VAL(/VLM/) DO BEGIN LIN(/LN/) := ' ';
                                    LINT(/LN/).LINC := BLANKLINC END;
   VLEN := VAL(/VLM/);
   LEN  := VAL(/VLM/);
   JUST.NDX := 0;
   SUP := FALSE;
   DEFRB := 0;
   TAB := 0;
   EMPTY := TRUE;
   CENTER := FALSE;
   FORCE := FALSE;
   OTL.BBAR := BB;
  END;
END;     (* END OF CLRLINE *)

PROCEDURE WRITEOTL; FORWARD;
PROCEDURE PUTBLANK; FORWARD;
PROCEDURE MIDRESTORE;
VAR LN: INTEGER;
BEGIN
 CLRLINE;
 IF PAGOTL THEN
  BEGIN
   TMPL := TMPLSAV;
   OTL := PAGSAV;
   WRITEOTL;
   VAL(/VOLNO/) := VAL(/VOLNO/) + 1;
   CLRLINE;
  END;
 PAGOTL := FALSE;
 IF PAGSYL THEN
  BEGIN
   OTL := PAGSAVS;
   JUST := SAVJUST;
   EMPTY := FALSE;
   CLRLINE;
  END;
 PAGSYL := FALSE;
 BB := HOLDBB; HOLDBB := FALSE;
END;     (* END OF MIDRESTORE *)

PROCEDURE STARTLINE(C: CHAR);
BEGIN
 IF DEBUG1                      (* DEBUG1STUFF    *)
    THEN
         WRITE(VAL(/VOLNO/):3,' ');
 IF LPT THEN WRITE(C);
 IF RIGHTSPACE>0 THEN
  FOR LN:=1 TO RIGHTSPACE DO WRITE(' ');
 IF BAR THEN
  BEGIN
   IF OTL.BBAR THEN WRITE('×')
   ELSE WRITE(' ');
   WRITE('  ');
  END;
END;     (* END OF STARTLINE *)

PROCEDURE DOTOP;
BEGIN
 IF NOT LPT THEN
   FOR SUB := VAL (/VOLNO/) TO OEPAG DO WRITELN;
 VAL(/VOLNO/) := 1;
 STARTLINE('1');
 OVETXT := OETXT - 1;
 OVBTXT := 0;
 IF NOT HOLDBB THEN BEGIN HOLDBB := BB; BB := FALSE; END;
END;     (* END OF DOTOP *)

PROCEDURE FORCECMD(A:ALFA; B:LLEN);
BEGIN
 FRCSYL := SYL;
 FOR SUB := 1 TO 10 DO BEGIN SYL.LIN (/SUB/) := A(/SUB/);
                             SYL.LINT(/SUB/).LINC := BLANKLINC; END;
 SYL.LIN(/11/) := ' ';
 SYL.LEN := B;
 SYL.VLEN := B;
 DEFRSYM := TRUE;
END;     (* END OF FORCECMD *)

PROCEDURE DOMID;
VAR DOFIG: BOOLEAN;
BEGIN
 OVBTXT := VAL(/VOLNO/);
 DOFIG := TRUE;
 IF FIGP>0 THEN
  WHILE DOFIG DO
   IF FIGN(/FIGP/)<=OVETXT-OVBTXT+1 THEN
    BEGIN
     FOR LN:=1 TO FIGN(/FIGP/) DO
      BEGIN WRITELN; VAL(/VOLNO/) := VAL(/VOLNO/) + 1 END;
     FIGP := FIGP - 1;
     IF FIGP=0 THEN DOFIG := FALSE;
    END
   ELSE DOFIG := FALSE;
 MIDRESTORE;
END;     (* END OF DOMID *)

PROCEDURE DOBOT;
BEGIN
 FOR LN:=VAL(/VOLNO/) TO OETXT DO WRITELN;
 VAL(/VOLNO/) := OETXT + 1;
 OVETXT := 100000;
 HOLDBB := BB;
END;     (* END OF DOBOT *)

PROCEDURE PUTBLANK;
BEGIN
 IF VAL(/VOLNO/)>OVBTXT THEN
  IF VAL(/VOLNO/)<=OVETXT+1 THEN
   BEGIN
    VAL(/VOLNO/) := VAL(/VOLNO/) + 1;
    STARTLINE(' ');
    WRITELN;
   END;
END;     (* END OF PUTBLANK *)
  (*$E                  *)
PROCEDURE WRITEOTL;
VAR I,J,K,CENTS: INTEGER;

PROCEDURE DOCENTER;
VAR I: INTEGER;
BEGIN
 CENTS := ((VAL(/VRM/)-VAL(/VLM/)) DIV 2) - ((OTL.LEN-VAL(/VLM/)) DIV 2) ;
 IF OTL.CENTER THEN FOR I := 1 TO CENTS DO WRITE(' ')
 ELSE CENTS := 0;
END;     (* END OF DOCENTER *)

BEGIN
 WITH OTL DO
  BEGIN
   IF LEN>0 THEN LEN := LEN - 1;
(* IF NOT UL THEN (* CONVERT TO UC *)
    BEGIN
(*   FOR LN:=1 TO LEN DO
      IF LINT(/LN/)<0 THEN
       BEGIN
(*      IF (LINC(/LN,0/)='@') AND (LINC(/LN,9/)<='D') THEN      *)
         CASE LINC(/LN,9/) OF
          'A': LINC(/LN,9/) := '@';
          'B': LINC(/LN,9/) := '^';
          'D': LINC(/LN,9/) := ':';
          ':','C':
         END;
        LINC(/LN,0/) := ':';
       END      *)
    END;           *)
   I := 0;
   STARTLINE(' ');
   DOCENTER;
   K := 0;
   FOR LN:=1 TO LEN DO (* WRITE THE LINE *)
    BEGIN
(*   IF LINT(/LN/)<0 THEN BEGIN K := K + 1; WRITE(LINC(/LN,0/)) END;    *)
     K := K + 1; WRITE(LIN(/LN/));
     IF ORD(LINT(/LN/).LINC(/4/))>I THEN I := ORD(LINT(/LN/).LINC(/4/));
    END;
   IF I>0 THEN (* COMPUTE LINE LEN *)
    BEGIN
     K := RIGHTSPACE + CENTS + K;
     IF LPT THEN K := K + 1;
     IF BAR THEN K := K + 3;
    END;
   FOR J:=1 TO I DO
    BEGIN
     IF LPT THEN BEGIN WRITELN; STARTLINE('+') END
     ELSE
      BEGIN
(*     IF ODD(K) THEN WRITE(' ');
       WRITELN(':K');
       WRITE('@ @5@5@5@5@5');
                               *)
      WRITELN; WRITELN; WRITELN;
       STARTLINE(' ');
      END;
     DOCENTER;
     FOR LN:=1 TO LEN DO
      IF ORD(LINT(/LN/).LINC(/4/))>=J THEN WRITE(LINT(/LN/).LINC(/J/))
      ELSE WRITE(' ');
     K := LEN
    END;
    IF DEBUG2
       THEN WRITELN(OUTPUT,' ':(LINLEN-LEN-CENTS-10),ILNO:5)
       ELSE WRITELN(OUTPUT)
  END;
END;     (* END OF WRITEOTL *)
   (*$E                 *)
PROCEDURE PUTLINE;
BEGIN
 IF (NOT SUP) AND (NOT EMPTY) THEN
  BEGIN
   IF (VAL(/VOLNO/)>OVETXT+1) OR (PUSHED AND (VAL(/VOLNO/)>OVETXT)) THEN
    BEGIN
     IF VAL(/VOLNO/)>OVETXT+1 THEN LASTFILD := TRUE;
     SAVOTL;
     FORCECMD('.FRCPAGE  ',8);
    END
   ELSE
    BEGIN
     PUSHED := FALSE; (* NO PAGE THROW *)
     VAL(/VOLNO/) := VAL(/VOLNO/) + 1;
     WITH OTL DO
      BEGIN
       RIGHT := NOT RIGHT;
       WRITEOTL;
      END
    END
  END;
 FOR LN:=1 TO DEFRB DO PUTBLANK;
 CLRLINE;
END;     (* END OF PUTLINE *)

PROCEDURE TESTPAGE(N: INTEGER);
BEGIN
 IF (N*VAL(/VSP/))-1>(OVETXT-VAL(/VOLNO/)+1) THEN
 BEGIN SAVSYL; FORCECMD('.FRCPAGE  ',8) END
END;     (* END OF TESTPAGE *)

PROCEDURE PUSHSYL(S: LINE); FORWARD;
PROCEDURE PARAGRAPH;
BEGIN
 RIGHT := TRUE; (* RESET ALTERNATING FILL *)
 FOR LN:=1 TO PARSPACE*VAL(/VSP/) DO PUTBLANK;
 WITH OTL DO BEGIN
  IF PREL THEN
   IF VAL(/VLM/)+PMAR>0 THEN LN := VAL(/VLM/) + PMAR
   ELSE LN := 1
  ELSE LN := PMAR;
  VLEN := LN; LEN := LN;
  FOR LN:=1 TO LEN DO BEGIN LIN(/LN/) := ' ';
                      LINT(/LN/).LINC := BLANKLINC; END;
 END;
 RIGHT := TRUE;
 TESTPAGE(PARTEST);
 IF DEFRSYM THEN PUSHSYL(FRCSYL);
 CAPNEXT := AUTOCAP AND LOWER;
END;     (* PARAGRAPH *)
PROCEDURE MARKJUST(N: LLEN);
BEGIN
 WITH JUST DO
  BEGIN NDX := NDX + 1; POS(/NDX/) := N END
END;     (* END OF MARKJUST *)
   (*$E             *)
PROCEDURE ADDWORD;
VAR I,J,LN: INTEGER;
BEGIN
 WITH OTL DO
  BEGIN
   IF (XTEND) AND (JUST.NDX>0) THEN
    BEGIN
     JUST.NDX := JUST.NDX -1;
     FOR LN:=TMPL.LEN DOWNTO 1 DO BEGIN TMPL.LIN(/LN+LASTSLEN/):=TMPL.LIN(/LN/);
      TMPL.LINT(/LN+LASTSLEN/) := TMPL.LINT(/LN/);   END;
     FOR LN:=1 TO LASTSLEN DO BEGIN TMPL.LINT(/LN/) := LINT(/LN+LASTLEN-1/);
                                 TMPL.LIN(/LN/) := LIN(/LN+LASTLEN-1/) END;
     TMPL.LEN := TMPL.LEN + LASTSLEN;
     LEN := LASTLEN;
(*   FOR LN:=1 TO SYL.LEN DO
      ADDSYL.LIN(/LN+ADDSYL.LEN/) := SYL.LIN(/LN/);
     ADDSYL.LEN := ADDSYL.LEN + SYL.LEN *)
    END;
(* ELSE ADDSYL := SYL; *)
   XTEND := FALSE;
   TAB := 0; DOTN := 0;
   IF RT THEN TAB := GETTAB(LEN+TMPL.LEN-1) - TMPL.LEN + 1
   ELSE IF T THEN TAB := GETTAB(LEN);
   WHILE LEN<TAB DO
    BEGIN
     IF DOT AND (NOT (LEN=TAB-1)) THEN LIN(/LEN/) := '.'
     ELSE LIN(/LEN/) := ' ';
     LINT(/LEN/).LINC := BLANKLINC;
     LEN := LEN + 1; DOTN := DOTN + 1;
    END;
   IF (LEN+TMPL.LEN-1>VAL(/VRM/)) AND (NOT EMPTY) THEN
     BEGIN
      IF JUSTIT THEN DOJUST(OTL,JUST,RIGHT);
      PUSHED := TRUE;
      FORCECMD('.CR       ',3);  (* FORCE THE END OF LINE *)
      PUSHSYL(ADDSYL);         (* AND SAVE THE CURRENT SYMBOL *)
      PUTLINE;
(*    IF NOT XTEND THEN PUSHSYL(ADDSYL) (* SAVE CURRENT SYMBOL *)
      ELSE
       BEGIN
        XTEND := FALSE;
        ADDWORD
      END; *)
      IF PUSHED THEN           (* RETURN FLAG MEANS PAGE THROWN *)
       PUSHSYL(FRCSYL);        (* SAVE THE .CR *)
      PUSHED := FALSE;         (* TURN IT OFF NOW *)
     END
   ELSE
    BEGIN
     EMPTY := FALSE;
     (*DEBUG*)
     IF LEN=0 THEN
      BEGIN ERR; WRITE('LEN=0;');FOR LN:=1 TO 10 DO WRITE(LIN(/LN/));
         WRITE(' -- ');
         FOR LN:=1 TO 10 DO WRITE(TMPL.LIN(/LN/));
        ERRE END;
     FOR LN:=1 TO TMPL.LEN DO BEGIN LINT(/LEN+LN-1/) := TMPL.LINT(/LN/);
                                    LIN(/LEN+LN-1/) := TMPL.LIN(/LN/) END;
     LASTLEN := LEN; LASTSLEN := TMPL.LEN;
     LEN := LEN + TMPL.LEN;
     MARKJUST(LEN-1);
     IF NOT SIGBL THEN
      BEGIN
       LIN(/LEN/) := ' ';
       LINT(/LEN/).LINC := BLANKLINC; LEN := LEN + 1;
       IF PQEND THEN
        BEGIN LIN(/LEN/):=' ';LINT(/LEN/).LINC:=BLANKLINC; LEN := LEN+1  END;
       XTRABL := PQEND
      END;
    END;
  END;
 PUSHED := FALSE;
END;     (* END OF ADDWORD *)

PROCEDURE ADDN(N: INTEGER; VAR OTL: LINE);
BEGIN
 IF N>=10 THEN ADDN(N DIV 10,OTL);
 WITH OTL DO BEGIN
  LIN(/LEN/) := CHR((N MOD 10) + ORD('0'));
  LINT(/LEN/).LINC := BLANKLINC;
  LEN := LEN + 1;
  VLEN := VLEN + 1;
 END
END;     (* END OF ADDN *)


PROCEDURE ADDNUM(N: INTEGER; VAR OTL: LINE);
BEGIN
 IF N<0 THEN
  BEGIN
   WITH OTL DO BEGIN LIN(/LEN/) := '-'; LEN := LEN + 1;
                     VLEN := VLEN + 1;LINT(/LEN/).LINC := BLANKLINC END;
   ADDN(-N,OTL)
  END
 ELSE ADDN(N,OTL);
END;     (* END OF ADDNUM *)

PROCEDURE ADDCHR(C: CHAR);
BEGIN
 WITH OTL DO BEGIN
  LIN(/LEN/) := C;
  LINT(/LEN/).LINC := BLANKLINC;
  LEN := LEN + 1; VLEN := VLEN + 1;
 END;
END;     (* END OF ADDCHR *)
    (*$E           *)
PROCEDURE UNFLAG(VAR L: LINE; LOWER: BOOLEAN);
  (* LOWERS THE CASE ADDS OVERSTRIKES AND TRANSFERS STUFF FROM L TO TMPL *)
VAR
 FUP : 0..3;
 VCHN,RCHN: LLEN;
 SKIP,OVER: BOOLEAN;
 OUTF: CHAR;

 PROCEDURE OUT(C: CHAR);
 BEGIN
  RCHN := RCHN + 1;
  TMPL.LINT(/RCHN/).LINC := BLANKLINC;
  IF INLALPHA(C) OR INALPHA(C)
     THEN BEGIN    (*  ONLY INVERT LETTERS *)
          IF (OUTF <> ':') AND (NOT CAPNEXT) (* INVERT? *)
             THEN TMPL.LIN(/RCHN/) := LOWERTHECASE(C)
             ELSE TMPL.LIN(/RCHN/) := C;
          CAPNEXT := FALSE; (* RESET WHETHER USED OR NOT *)
          END
     ELSE TMPL.LIN(/RCHN/) := C;
  OUTF := ':';
 END;     (* END OF OUT *)

PROCEDURE OVERS(C: CHAR);
VAR Z: INTEGER;
BEGIN
 WITH TMPL DO
  BEGIN
   Z := ORD(LINT(/RCHN/).LINC(/4/));
   IF (Z<3) AND (RCHN>0) AND (Z>-1) THEN
    BEGIN
     Z := Z + 1;
     LINT(/RCHN/).LINC(/4/) := CHR(Z);
     LINT(/RCHN/).LINC(/Z/) := C;
    END
  END;
END;     (* END OF OVERS *)

BEGIN     (* UNFLAG *)
 WITH L DO
  BEGIN
     FUP := 0; (* NO CASE FORCING *)
     VCHN := 0;
     RCHN := 0;
     LN := 1;
     OUTF := ':';
     PQEND := FALSE;
     WHILE LN<=LEN DO
      BEGIN
       OVER := FALSE;
      IF NOT ( (LIN(/LN/) = '''' )  OR
               (LIN(/LN/) = '"'  )  OR
               (LIN(/LN/) = ')'  )       ) THEN PQEND := FALSE;
      SKIP := FALSE;
       CASE LIN(/LN/) OF
        '^':
          BEGIN
           LN := LN + 1;
           CASE FUP OF
            2: OUTF := '^';
            0:
             IF FLAG THEN
              BEGIN
               IF NOT(LOWER AND INALPHA( LIN(/LN/) ) ) THEN
                OUTF := '^';
              END
             ELSE  OUT('^');
            1:
           END;
          END;
        'A','B','C','D','E','F','G',
        'H','I','J','K','L','M','N',
        'O','P','Q','R','S','T','U',
        'V','W','X','Y','Z':
          CASE FUP OF
           0: IF LOWER AND FLAG THEN OUTF := '^';
           2: OUTF := '^';
           1:
          END;
        '@':
          BEGIN
           LN := LN + 1;
           IF FLAG  THEN BEGIN
                         CASE LIN(/LN/) OF
                               'A':LIN(/LN/):='@';
                               'B':LIN(/LN/):='^'
                              END;
                         OUTF:=':';
                         OUT(LIN(/LN/))
                         END;
          END;
        '<':
          IF FLAGCAPS THEN
           BEGIN FUP := FUP + 1; IF FUP=3 THEN FUP := 1; SKIP := TRUE;
           END;
        '.','?','!':
         BEGIN
         CAPNEXT := AUTOCAP AND LOWER;
         IF PERIOD THEN PQEND := TRUE;
         END;
        '_':
          IF ESCCHR THEN LN := LN + 1;
        '#':
         IF FLAGSIG THEN
          BEGIN
           OUT(' ');
           SKIP := TRUE;
           IF UNDL AND USB THEN OVERS('_');
          END;
        '\':
          IF FLAGOVER THEN
           BEGIN LN := LN + 1; OVER := TRUE; SKIP := TRUE END;
         '0'..'9' : CAPNEXT := FALSE;
        '+','-','*','/',
        '(',')','$','=',' ',',',
        '[',']',   '%','"',
        '&','''','>',';',':' :
       END;
       IF NOT SKIP THEN
        BEGIN
         OUT(LIN(/LN/)); VCHN := VCHN + 1;
         IF UNDL THEN OVERS('_');
        END
       ELSE IF OVER THEN OVERS(LIN(/LN/));
       LN := LN + 1
      END;
   WITH TMPL DO BEGIN LEN := RCHN; VLEN := RCHN END;
  END;
END;     (*$END OF UNFLAG *)
PROCEDURE ADRC(C: CHAR);
BEGIN
 WITH SYL DO
  BEGIN
   LEN := LEN + 1;
   IF ROMLC
      THEN LIN(/LEN/) := LOWERTHECASE(C)
      ELSE LIN(/LEN/) := C;
   LINT(/LEN/).LINC := BLANKLINC;
  END;
END;     (* END OF ADRC *)

PROCEDURE ROMAN(N: INTEGER);
BEGIN
    WHILE N > 1000 DO
       BEGIN     ADRC('M');    N := N - 1000;       END;
    IF N > 500 THEN
       BEGIN     ADRC('D');    N := N - 500;        END;
    WHILE N > 100 DO
       BEGIN     ADRC('C');    N := N - 100;        END;
    IF N > 50 THEN
       BEGIN     ADRC('L');    N := N - 50;         END;
    WHILE N > 10 DO
       BEGIN     ADRC('X');    N := N - 10;         END;
    IF N > 5 THEN
       BEGIN     ADRC('V');    N := N - 5;          END;
    WHILE N > 0 DO
       BEGIN     ADRC('I');    N := N - 1;          END;
END;     (* END OF ROMAN *)

PROCEDURE DOFMT(F,N: INTEGER);
BEGIN
SYL.LEN := 0;
IF (F>=0) AND (F<=5) THEN
 CASE F OF
  0: BEGIN SYL.LEN := 1; ADDNUM(N,SYL); SYL.LEN := SYL.LEN - 1; END;
  1: BEGIN
      SYL.LIN(/1/) := LETTERS(/ (N-1) MOD 26 /);
      SYL.LINT(/1/).LINC := BLANKLINC;
      SYL.LEN := 1;
     END;
  2: BEGIN
      SYL.LIN(/1/) := LOWERTHECASE(LETTERS(/ (N-1) MOD 26 /) );
      SYL.LINT(/1/).LINC := BLANKLINC;
      SYL.LEN := 1;
     END;
  3: BEGIN ROMLC := FALSE; ROMAN(N); END;
  4: BEGIN ROMLC := TRUE; ROMAN(N); END;
  5: BEGIN
      SYL.LIN(/1/) := CHR( N MOD CHARSIZE);
      SYL.LINT(/1/).LINC := BLANKLINC;
      SYL.LEN := 1;
     END
 END;
IF SYL.LEN>0 THEN BEGIN UNFLAG(SYL,FALSE); ADDWORD; END;
END;     (* END OF DOFMT *)

PROCEDURE BREAK;
BEGIN
 PUTLINE;
END;     (* END OF BREAK *)

PROCEDURE CR;
BEGIN
 FOR LN:=2 TO VAL(/VSP/) DO PUTBLANK;
END;     (* END OF CR *)

PROCEDURE ENDPARA;
BEGIN
 BREAK; CR;
END;     (* END OF ENDPARA *)

PROCEDURE BLANKLINE;
BEGIN
 IF (NOT AP) THEN
  BEGIN
   ENDPARA;
   PUTBLANK
  END
 ELSE BEGIN FORCECMD('.PP       ',3); SYMTYPE := COMMAND END;
END;     (* END OF BLANKLINE *)

PROCEDURE ENDLINE;
BEGIN
 IF SUP THEN CLRLINE;
 IF FORCE OR (NOT FILL) OR OTL.CENTER THEN BREAK;
 IF FORCE OR (NOT FILL) THEN CR;
END;      (* END OF ENDLINE *)

PROCEDURE FIN;
BEGIN
 PUTLINE;
 DOTOP;
  WRITELN;
  IF ERRORS > 0 THEN WRITELN(ERRORS:9,' ERRORS ENCOUNTERED.');
END;     (* END OF FIN *)
(*
PROCEDURE DBUGWORD;
BEGIN
 WITH SYL DO
  BEGIN
   WRITE(LEN,' ');
   FOR LN:=1 TO LEN DO WRITE(LIN(/LN/));
   WRITELN(' (',LEN:3,',',VLEN:3,')');
  END
END;     (* END OF DBUGWORD *)             *)

PROCEDURE PUTWORD;
BEGIN
 UNFLAG(SYL,LOWER);
 ADDWORD;
END;     (* END OF PUTWORD *)

PROCEDURE PUTVAR;
VAR N: INTEGER; S: SIGN;
BEGIN
 GETNUM(S,N);
 IF S<>INVALID THEN
  BEGIN
   IF SHOWEXPR THEN
    BEGIN
     SYL.LEN := 1;
     SYL.VLEN := 1;
     ADDNUM(N,SYL);
     SYL.LEN := SYL.LEN - 1;
     SYL.VLEN := SYL.VLEN - 1;
     PUTWORD;
    END
  END
 ELSE PUTWORD;
END;     (* END OF PUTVAR *)
 (*$E           *)
PROCEDURE INISTDMACS;
VAR P: PMAC;
BEGIN
 (* .FRCPAGE *)
 MACLSTP := NIL;
 NEW(P);
 WITH P@ DO
  BEGIN
   ON := FALSE; MA := MACLSTP; CA := NIL; CU := 1; BE := 1;
   EN := 60; MT := HEADER; NP := 0;
   NEW(LI);
   FOR SUB := 1 TO 60 DO LI@(/SUB/) := LITERAL1(/SUB/);

   NM := 'FRCPAGE   ';
  END;
 MACLSTP := P; NEW(P);
 (* ..NMP *)
 WITH P@ DO
  BEGIN
   ON := FALSE; MA := MACLSTP; CA := NIL; CU := 1; BE := 1;
   EN := 57; MT := HEADER; NP := 0;
   NEW(LI);
   FOR SUB := 1 TO 57 DO LI@(/SUB/) := LITERAL2 (/SUB/);

   NM := '.NMP      ';
  END;
 MACLSTP := P; NEW(P);
 (* ..NONMP *)
 WITH P@ DO
  BEGIN
   ON := FALSE; MA := MACLSTP; CA := NIL; CU := 1; BE := 1;
   EN := 16; MT := HEADER; NP := 0;
   NEW(LI);
   FOR SUB := 1 TO 16 DO LI@ (/SUB/) := LITERAL3 (/SUB/);
   NM := '.NONMP    ';
  END;
 MACLSTP := P; NEW(P);
 (* ..TTL *)
 WITH P@ DO
  BEGIN
   ON := FALSE; MA := MACLSTP; CA := NIL; CU := 1; BE := 1;
   EN := 1; MT := HEADER; NP := 0;
   NEW(LI); NM := '.TTL      ';
  END;
 MACLSTP := P; TTLMACP := P; NEW(P);
 (* ..STL *)
 WITH P@ DO
  BEGIN
   ON := FALSE; MA := MACLSTP; CA := NIL; CU := 1; BE := 1;
   EN := 1; MT := HEADER; NP := 0;
   NEW(LI); NM := '.STL      ';
  END;
 MACLSTP := P; STLMACP := P; NEW(P);
 (* ..CH *)
 WITH P@ DO
  BEGIN
   ON := FALSE; MA := MACLSTP; CA := NIL; CU := 1; BE := 1;
   EN := 65; MT := HEADER; NP := 0;
   NEW(LI);
   FOR SUB := 1 TO 38 DO LI@ (/SUB/) := LITERAL4(/SUB/);
   FOR SUB := 1 TO 27 DO LI@ (/SUB+38/) := LITERAL5(/SUB/);
   NM := '.CH       ';
  END;
 MACLSTP := P; NEW(P);
 (* ..CHT *)
 WITH P@ DO
  BEGIN
   ON := FALSE; MA := MACLSTP; CA := NIL; CU := 1; BE := 1;
   EN := 1; MT := HEADER; NP := 0;
   NEW(LI); NM := '.CHT      ';
  END;
 MACLSTP := P; CHTMACP := P;
END;     (* END OF INISTDMACS *)
 (*$E *)
PROCEDURE INIRELS;
BEGIN
 ARELOPR(/EQ/)       := 'EQ        ';
 ARELOPR(/GT/)       := 'GT        ';
 ARELOPR(/LT/)       := 'LT        ';
 ARELOPR(/NE/)       := 'NE        ';
 ARELOPR(/GE/)       := 'GE        ';
 ARELOPR(/LE/)       := 'LE        ';
END;     (* END OF INIRELS *)

PROCEDURE INIVARS;
BEGIN
 VID(/VPAGE/)        := '$PAGE     ';  VTY(/VPAGE/) := VITEM;
 VID(/VCH/)          := '$CH       ';  VTY(/VCH/) := VITEM;
 VID(/VHL/)          := '$HL       ';  VTY(/VHL/) := VARRAY;
         VUP(/VHL/) :=5;
 VID(/VLIST/)        := '$LIST     ';  VTY(/VLIST/) := VARRAY;
         VUP(/VLIST/) := 5;
 VID(/VLM/)          := '$LM       ';  VTY(/VLM/) := VITEM;
 VID(/VRM/)          := '$RM       ';  VTY(/VRM/) := VITEM;
 VID(/VSP/)          := '$SP       ';  VTY(/VSP/) := VITEM;
 VID(/VNMP/)         := '$NMP      ';  VTY(/VNMP/) := VITEM;
 VID(/VOLNO/)        := '$OLNO     ';  VTY(/VOLNO/) := VITEM;
 VID(/VPRM/)         := '$PRM      ';  VTY(/VPRM/) := VITEM;
 TV := 26;
 FOR LN := 1 TO VARMAX DO VAL(/LN/) := 0;
END;     (* END OF INIVARS *)

PROCEDURE INICMDS;
BEGIN
 CMDS(/CBLANK/)      := 'B         ';
 CMDS(/CFLAG/)       := 'FLAG      ';
 CMDS(/CFLAGCAPS/)   := 'FLAGCAPS  ';
 CMDS(/CFLAGOVER/)   := 'FLAGOVER  ';
 CMDS(/CFLAGSIG/)    := 'FLAGSIG   ';
 CMDS(/CLOWER/)      := 'LOWER     ';
 CMDS(/CUPPER/)      := 'UPPER     ';
 CMDS(/CPERIOD/)     := 'PERIOD    ';
 CMDS(/CBREAK/)      := 'BR        ';
 CMDS(/CCR/)         := 'CR        ';
 CMDS(/CESCCHR/)     := 'ESC       ';
 CMDS(/CASCII   /)   := 'ASCII     ';
 CMDS(/CCENTER/)     := 'C         ';
 CMDS(/CJUST/)       := 'J         ';
(*MDS(/CUL/)         := 'UL        ';
       *)
 CMDS(/CLMAR/)       := 'LM        ';
 CMDS(/CRMAR/)       := 'RM        ';
 CMDS(/CSUP/)        := 'SUP       ';
 CMDS(/CSTD/)        := 'STD       ';
 CMDS(/CPS/)         := 'PS        ';
 CMDS(/CLPT/)        := 'LPT       ';
 CMDS(/CSAV/)        := 'SAV       ';
 CMDS(/CP/)          := 'P         ';
 CMDS(/CRES/)        := 'RES       ';
 CMDS(/CPP/)         := 'PP        ';
 CMDS(/CSP/)         := 'SP        ';
 CMDS(/CS/)          := 'S         ';
 CMDS(/CTP/)         := 'TP        ';
 CMDS(/CNMP/)        := 'NMP       ';
 CMDS(/CPNO/)        := 'PNO       ';
 CMDS(/CTITLE/)      := 'TITLE     ';
 CMDS(/CST/)         := 'ST        ';
 CMDS(/CATITLE/)     := 'ATITLE    ';
 CMDS(/CLIST/)       := 'LIST      ';
 CMDS(/CLE/)         := 'LE        ';
 CMDS(/CELIST/)      := 'ELIST     ';
 CMDS(/CFIG/)        := 'FIG       ';
 CMDS(/CBAR/)        := 'BAR       ';
 CMDS(/CBB/)         := 'BB        ';
 CMDS(/CEB/)         := 'EB        ';
 CMDS(/CU/)          := 'U         ';
 CMDS(/CT/)          := 'T         ';
 CMDS(/CTAB/)        := 'TAB       ';
 CMDS(/CTABS/)       := 'TABS      ';
 CMDS(/CRT/)         := 'RT        ';
 CMDS(/CCH/)         := 'CH        ';
 CMDS(/CAP/)         := 'AP        ';
 CMDS(/CI/)          := 'I         ';
 CMDS(/CFILL/)       := 'F         ';
 CMDS(/CSIG/)        := 'SIG       ';
 CMDS(/CPAGE/)       := 'PAGE      ';
 CMDS(/CTOP/)        := 'TOP       ';
 CMDS(/CMID/)        := 'MID       ';
 CMDS(/CBOT/)        := 'BOT       ';
 CMDS(/CARRAY/)      := 'ARRAY     ';
 CMDS(/CFMT/)        := 'FMT       ';
 CMDS(/CIF/)         := 'IF        ';
 CMDS(/CASIS/)       := 'ASIS      ';
 CMDS(/CDOT/)        := 'DOT       ';
 CMDS(/CREM/)        := 'REM       ';
 CMDS(/CUPP/)        := 'UP        ';
 CMDS(/CUSB/)        := 'USB       ';
 CMDS(/CADD/)        := 'ADD       ';
 CMDS(/CHL/)         := 'HL        ';
 CMDS(/CRIGHT/)      := 'RIGHT     ';
 CMDS(/CLINES/)      := 'LINES     ';
 CMDS(/CMACRO/)      := 'MACRO     ';
 CMDS(/CX/)          := 'X         ';
 CMDS(/CVAR/)        := 'VAR       ';
 CMDS(/CINC/)        := 'INC       ';
 CMDS(/CDEC/)        := 'DEC       ';
 CMDS(/CSAVPAG/)     := 'SAVPAG    ';
 CMDS(/CRESPAG/)     := 'RESPAG    ';
 CMDS(/CDATE/)       := 'DATE      ';
 CMDS(/CTIME/)       := 'TIME      ';
 CMDS(/CDELMAC/)     := 'DELMAC    ';
 CMDS(/CAUTOCAP/)    := 'AUTOCAP   ';
 CMDS(/CDEBUG1/)     := 'DEBUG1    ';
 CMDS(/CDEBUG2/)     := 'DEBUG2    ';
 BREAKSET := (/CCENTER,CTITLE,CST,CI,CCH,CLE,CLIST,CELIST,CHL,CCR,
              CPP,CPAGE,CFIG,CS,CTP,CBLANK,CASIS,CBREAK,CRES,CRESPAG/);
 CRSET := BREAKSET - (/CBREAK,CBLANK,CRES,CRESPAG/);
END;     (*$END OF INICMDS *)
PROCEDURE BACKUPSYL;
BEGIN CUP := LASTCUP END;
     (* END OF BACKUPSYL *)

PROCEDURE POPMACROS; (* CLEAR ALL ACTIVE MACROS *)
BEGIN
 WHILE ACTP@.CA<>NIL DO
  BEGIN ACTP@.ON := FALSE; ACTP := ACTP@.CA; END
END;     (* END OF POPMACROS *)

PROCEDURE GETCUR;
   VAR I : INTEGER;
BEGIN
 IF NOT EOF(INPUT) THEN
  BEGIN
   ILNO := ILNO + 1;
   BEG := 1; CUP := 1; ENL := 1;
   WHILE NOT EOLN DO
    BEGIN CUR(/ENL/) := INPUT@; GET(INPUT); ENL := ENL + 1 END;
   CUR(/ENL/) := ' ';
   WITH ACTP@ DO
    BEGIN LI@ := CUR; BE := BEG; EN := ENL END;
   READLN(INPUT);
   WRITE (QRR,' ',ILNO:6,'  ');
   FOR I := 1 TO ENL DO WRITE (QRR,CUR(/I/) );
   WRITELN(QRR);
  END
 ELSE EOFINPUT := TRUE
END;     (* END OF GETCUR *)

PROCEDURE GETTOKEN;
BEGIN        (* GETTOKEN *)
 IF DEFRSYM THEN DEFRSYM := FALSE
 ELSE
  WITH SYL DO
   BEGIN
    FOR LN:=1 TO MACMAX+1 DO BEGIN LIN(/LN/) := ' ';
                                   LINT(/LN/).LINC := BLANKLINC    END;
    LEN := 0;
    VLEN := 0;
    LASTCUP := CUP;
    IF (CUR(/CUP/)=' ') AND SIGBL AND NOT CMMD THEN
     WHILE (CUP<ENL) AND (CUR(/CUP/)=' ') DO
      BEGIN LEN := LEN + 1; LIN(/LEN/) := CUR(/CUP/); CUP := CUP + 1;
                  LINT(/LEN/).LINC := BLANKLINC   END
    ELSE
     BEGIN
      WHILE (CUP<ENL) AND (CUR(/CUP/)=' ') DO CUP := CUP + 1;
      CUR(/ENL/) := ' ';
      WHILE (CUR(/CUP/) <> ' ') AND (CUP <= ENL) DO
       BEGIN LEN := LEN + 1; LIN(/LEN/) := CUR(/CUP/); CUP := CUP + 1;
                    LINT(/LEN/).LINC := BLANKLINC; END
     END
   END;
END;     (* END OF GETTOKEN *)

PROCEDURE GETMACSYM;
VAR EXIT: BOOLEAN;
BEGIN      (*GETMACSYM *)
 REPEAT
  GETTOKEN;
  EXIT := TRUE;
  IF SYL.LEN=0 THEN
   BEGIN
    ACTP@.ON := FALSE;
    IF ACTP@.CA<>NIL THEN
     BEGIN
      EXIT := FALSE;
      ACTP := ACTP@.CA;
      WITH ACTP@ DO
       BEGIN
        CUR := LI@;
        CUP := CU; BEG := BE; ENL := EN;
       END
     END
   END
 UNTIL EXIT
END;     (* END OF GETMACSYM *)

PROCEDURE EXPMACRO(T: PMAC);
VAR N: 0..8;
BEGIN  (* EXPMACRO *)
 T@.CA := ACTP;                                  (* CHAIN NEW MACRO  *)
 ACTP := T;                                      (* NEW CURRENT MACRO*)
 IF T@.NP<8 THEN                                 (* SCAN PARMS       *)
  FOR N:=T@.NP DOWNTO 1 DO
   BEGIN
    T := T@.MA;                                  (* PARM MACRO       *)
    WITH T@ DO
     BEGIN
      BE := CUP;
      GETTOKEN;
      EN := CUP;
      LI@ := ACTP@.CA@.LI@;
     END
   END
 ELSE
  BEGIN                                    (* REST OF LINE IS PARM *)
   T := T@.MA;
   WITH T@ DO
    BEGIN BE := CUP; EN := ENL; LI@ := ACTP@.CA@.LI@ END;
   CUP := ENL
  END;
 ACTP@.CA@.CU := CUP;
 WITH ACTP@ DO                                   (* NOW FILL CUR   *)
  BEGIN
   ON := TRUE;
   CUP := BE; BEG := BE; ENL := EN;
   CUR := LI@;


  END;
END;      (* END OF EXPMACRO *)

PROCEDURE LOOKUPVAR;
VAR T: ALFA;
BEGIN  (* LOOKUPVAR *)
 UPCASESYL;
 FOR SUB := 1 TO 10 DO T(/SUB/) := SYL.LIN(/SUB+1/);
 VID(/TV/) := T;
 VARNDX := 1;
 WHILE VID(/VARNDX/)<>T DO VARNDX := VARNDX + 1;
END;     (* END OF LOOKUPVAR *)

PROCEDURE LOOKUPMAC;
VAR L: MACSYM;
 T: PMAC;
 EXIT: BOOLEAN;
BEGIN
 UPCASESYL;
 FOR SUB := 1 TO 10 DO L(/SUB/) := SYL.LIN (/SUB + 1/);
 T := MACLSTP;
 EXIT := FALSE;
 REPEAT
  IF T=NIL THEN BEGIN EXIT := TRUE; NOTMACRO := TRUE END
  ELSE
   IF L=T@.NM THEN
    BEGIN EXIT := TRUE;
     IF T@.ON THEN
      BEGIN
       ERR; WRITE('RECURSIVE MACRO CALL TO'); SYLERR;
       NOTMACRO := FALSE;
      END
     ELSE
      BEGIN
       EXPMACRO(T);
       NOTMACRO := FALSE;
      END
    END
  ELSE T := T@.MA
 UNTIL EXIT;
END;     (* END OF LOOKUPMAC *)


PROCEDURE GETSYM;
VAR
 START: 2..4;
 I,J: INTEGER;
 PIN:  ARRAY (/1..10/) OF CHAR;
BEGIN   (* GETSYM *)
  WITH SYL DO
   BEGIN
    REPEAT
     GETMACSYM;
     NOTMACRO := TRUE;
     IF LIN(/1/)=MACCHR THEN LOOKUPMAC
    UNTIL NOTMACRO;
    IF LEN=0 THEN SYMTYPE := NONE
    ELSE IF (NOT (INDIGIT ( LIN(/2/) ))) AND (LIN(/1/)=CMDCHR) THEN
     BEGIN
      SYMTYPE := COMMAND;
      START := 2;
      YES := TRUE;
      IF (LEN>3) AND (LIN(/2/)='N') AND (LIN(/3/)='O') THEN
       BEGIN YES := FALSE; START := 4 END;
      FOR LN:=LEN+1 TO START+9 DO BEGIN LIN(/LN/) := ' ';
                                  LINT(/LN/).LINC := BLANKLINC END;
      FOR SUB := 1 TO 10 DO PIN(/SUB/) := LIN(/SUB + START -1/);
      CMDS(/NOTCMD/) := PIN;
      CMDTYPE := CBLANK;
      WHILE PIN<>CMDS(/CMDTYPE/) DO CMDTYPE := SUCC(CMDTYPE);
      IF CMDTYPE=NOTCMD THEN
       BEGIN ERR; WRITE('UNKNOWN COMMAND'); SYLERR END;
     END
    ELSE IF (SYL.LIN(/1/)=VARCHR)
            AND  NOT (INDIGIT(SYL.LIN(/2/)) OR (SYL.LIN(/2/)=' ') )  THEN
              SYMTYPE := VARS
    ELSE SYMTYPE := WORD
   END;
END;     (* END OF GETSYM *)

PROCEDURE ALLOC(VAR T: PMAC);
LABEL 1;
VAR P: PMAC;
BEGIN           (* ALLOC *)
 P := FREEMACP;
 1:
  IF P<>NIL THEN
   IF P@.ON THEN BEGIN P := P@.MA; GOTO 1 END;
 IF P=NIL THEN
  BEGIN
   NEW(T);
   NEW(T@.LI);
   T@.MA := FREEMACP;
   FREEMACP := T;
  END
 ELSE T := P;
END;     (* END OF ALLOC *)

PROCEDURE PUSHSYL (* (S:LINE) ; FROM THE FORWARD DECL *)  ;
VAR M: PMAC;
BEGIN
 ALLOC(M);
 WITH M@ DO
  BEGIN
   ON := FALSE;
   CU := 1; BE := 1; EN := S.LEN + 1;
   MT := HEADER; NP := 0;
   LI@ := S.LIN;
   NM := '* DEFER * ';
  END;
 EXPMACRO(M);
END;     (* END OF PUSHSYL *)

FUNCTION GETEXP: INTEGER;
BEGIN
 GETSYM;
 EP := 0;
 NEXTCH;
 GETEXP := EXPR;
END;     (* END OF GETEXP *)


PROCEDURE SETSTD;
BEGIN
 FLAG := YES; FLAGCAPS := YES; LOWER := YES;
 ESCCHR := YES; PERIOD := YES; JUSTIT := YES;
 LOWER := YES; FILL := YES; SIGBL := NOT YES;
 IF YES THEN OPTBRKSET := BREAKSET
 ELSE OPTBRKSET := (//);
END;     (* END OF SETSTD *)
 (*$E *)
PROCEDURE DOCOMMAND;
VAR
 S: SIGN;
 I,N,LN: INTEGER;
 N8: 0..8;
 L: LLEN;
 TMACP,MACP: PMAC;
 EXIT: BOOLEAN;
 TL: LINE;
 VTEMP: 1..VARMAX;
 V: ALFA;

PROCEDURE STUFFMAC(P: PMAC);
BEGIN     (* STUFFMAC *)
 WITH P@ DO
  IF YES THEN
   BEGIN
    LI@ := CUR;
    BE := CUP; CU := CUP; EN := ENL;
    CUP := ENL;
   END
  ELSE CU := EN;
END;    (* END OF STUFFMAC *)

PROCEDURE SYLTOMAC;
BEGIN
 WITH TMACP@ DO
  BEGIN
   FOR SUB := 1 TO MACMAX DO NM(/SUB/) := SYL.LIN(/SUB/);
   CU := 0; BE := 0; EN := 0; NP := 0;
   CA := NIL; LI := ACTP@.LI; ON := FALSE;
  END;
END;     (* END OF SYLTOMAC *)

PROCEDURE MAKPARM;
BEGIN
 SYL := TL;
 ADDNUM(N-N8+1,SYL);
 NEW(TMACP); SYLTOMAC;
 WITH TMACP@ DO
  BEGIN
   MA := MACP@.MA; MACP@.MA := TMACP;
   NP := 0; MT := PARM;
   NEW(LI);
   MACP := TMACP;
  END;
END;     (* END OF MAKPARM *)

PROCEDURE CLRLISTS;
BEGIN
 ENP := 0;
 VAL(/VLIST/) := 0;
END;     (* END OF CLRLISTS *)

PROCEDURE GETOPTN;
BEGIN
 GETSYM;
 IF ((INDIGIT( SYL.LIN(/1/))) OR
     (SYL.LIN (/1/) = '$')    OR
     (SYL.LIN (/1/) = '(')        )      THEN
  BEGIN
   GETNUM(S,N);
   IF (S=INVALID) OR (N<=0) THEN
    BEGIN
     ERR; WRITE('EXPECTING POSITIVE NUMBER; GOT'); SYLERR;
     S := INVALID;
    END;
  END
 ELSE BEGIN BACKUPSYL; S := INVALID; END;
END;     (* END OF GETOPTN *)

PROCEDURE GETSN;
BEGIN GETSYM; GETNUM(S,N) END;
     (* END OF GETSN *)                                   (*$E *)
    PROCEDURE CPPROC;  (* MOVED HERE FOR ADDRESABILITY REASONS *)
       BEGIN          (* CPPROC *)
        GETSN;
        IF (S<>INVALID) THEN
         BEGIN
          CASE S OF
           UNSIGNED: BEGIN PREL := FALSE; PMAR := N END;
           MINUS:    BEGIN PREL := TRUE;  PMAR := -N END;
           PLUS:     BEGIN PREL := TRUE;  PMAR := N END
          END;
          GETSN;
          IF (S=UNSIGNED) AND (N>=0) AND (N<=5) THEN
           BEGIN
            PARSPACE := N;
            GETSN;
            IF (S=UNSIGNED) THEN
             PARTEST := N
            ELSE BEGIN ERR; WRITE('PARAGRAPH TP'); SYLERR END
           END
          ELSE BEGIN ERR; WRITE('PARAGRAPH SPACE'); SYLERR END
         END
        ELSE BEGIN ERR; WRITE('PARAGRAPH INDENT'); SYLERR END
       END;       (* CPPROC *)




     PROCEDURE CHLPROC;   (* MOVED HERE FOR ADDRESSABILITY REASONS *)
     BEGIN     (* CHLPROC *)
       IF (OVETXT-VAL(/VOLNO/)+1)>8 THEN
        BEGIN
         CLRLISTS;
         GETSN;
         IF (S=UNSIGNED) AND (N>0) AND (N<=VHLMAX) AND
         (N<=VAL(/VHL/)+1) THEN
          BEGIN
           IF FIRSTCH THEN BEGIN SAVENV(PAGENV); FIRSTCH := FALSE END;
           RESENV(PAGENV); ENP := 0; CLRLINE;
           FOR LN:=1 TO 3 DO PUTBLANK;
           IF N>VAL(/VHL/) THEN VAL(/VHL+N/) := 0;
           VAL(/VHL/) := N;
           N := VHL + N;
           VAL(/N/) := VAL(/N/) + 1;
           IF VAL(/VCH/)>0 THEN BEGIN LN := VHL+1; ADDNUM(VAL(/VCH/),OTL) END
              ELSE BEGIN LN := VHL+2; ADDNUM(VAL(/VHL+1/),OTL) END;
           FOR LN:=LN TO N DO BEGIN ADDCHR('.'); ADDNUM(VAL(/LN/),OTL) END;
           ADDCHR(' '); ADDCHR(' ');
           DEFRB := 1; FORCE := TRUE;
          END
         ELSE BEGIN ERR; WRITE('BAD HEADER LEVEL'); SYLERR END;
        END
       ELSE BEGIN BACKUPSYL; FORCECMD('.FRCPAGE  ',8) END;
       CAPNEXT := AUTOCAP AND LOWER;
    END;        (* CHLPROC *)                                    (*$E *)



    PROCEDURE CMACROPROC;    (* MOVED HERE FOR ADDRESSABILITY REASONS *)
     BEGIN    (* CMACROPROC *)
       IF ACTP@.CA=NIL THEN
        BEGIN
         GETTOKEN;
         IF SYL.LEN<>0 THEN
          BEGIN
           NEW(TMACP);
           UPCASESYL;
           SYLTOMAC;
           NEW(TMACP@.LI);
           TMACP@.MA := MACLSTP;
           TMACP@.MT := HEADER;
           TL := SYL;                              (* SAVE MACRO NAME  *)
           TL.LEN := TL.LEN + 1;                   (* FOR ADDNUM       *)
           IF TL.LEN>10 THEN TL.LEN := 10;
           GETTOKEN;
           IF NOT ((SYL.LIN(/1/) = '*' ) OR
                   (SYL.LIN(/1/) = '=' ) ) THEN GETNUM (S,N)
               ELSE S := INVALID;
           IF S=UNSIGNED THEN GETTOKEN
              ELSE IF (SYL.LIN(/1/)='*') AND (SYL.LEN=1) THEN
                      BEGIN N := 8; GETTOKEN END
                      ELSE N := 0;
           IF (SYL.LIN(/1/)='=') AND (SYL.LEN=1) THEN
              BEGIN
              MACLSTP := TMACP;
              WITH TMACP@ DO
                BEGIN
                FOR SUB:=1 TO (LINLEN-CUP+1) DO LI@ (/SUB/):=CUR(/SUB-1+CUP/);
                BE := 1;
                EN := ENL-CUP+1;
                CU := 1
                END;
              IF N<8 THEN
               BEGIN
                TMACP@.NP := N;
                MACP := MACLSTP;
                FOR N8:=N DOWNTO 1 DO MAKPARM
                END
             ELSE
              BEGIN
               TMACP@.NP := 8; N8 := N;
               MACP := MACLSTP;
               MAKPARM
              END;
             CUP := ENL;
            END
           ELSE BEGIN ERR; WRITE('MISSING = IN MACRO DEF'); ERRE END
          END
         ELSE BEGIN ERR; WRITE('NO MACRO NAME'); ERRE END;
        END
       ELSE BEGIN ERR; WRITE('NESTED MACRO DEFS'); ERRE END;
      END;    (* CMACROPROC *)




     PROCEDURE CVARPROC;   (* MOVED HERE FOR ADDRESSABILITY REASONS *)
       BEGIN
        GETSYM;
        IF SYMTYPE=VARS THEN
         BEGIN
          LOOKUPVAR;
          IF VARNDX<VARMAX THEN
           BEGIN
            IF VARNDX=TV THEN
             BEGIN TV := TV + 1; VTY(/VARNDX/) := VITEM END;
            GETMACSYM;
            IF (SYL.LEN=1) AND (SYL.LIN(/1/)='=') THEN
             BEGIN
              GETSN;
              IF S<>INVALID THEN
               BEGIN
                IF S=MINUS THEN N := -N;
                VAL(/VARNDX/) := N
               END
             END
            ELSE BACKUPSYL
           END
          ELSE BEGIN ERR; WRITE('TOO MANY VARIABLES'); ERRE END
         END
        ELSE BEGIN ERR; WRITE('NEED VAR NAME; GOT'); SYLERR END
     END;        (* CVARPROC *)



     PROCEDURE CARRAYPROC;  (* MOVED HERE FOR ADDRESABILITY REASONS *)
       BEGIN
        GETSYM;
        IF SYMTYPE=VARS THEN
         BEGIN
          LOOKUPVAR;
          IF VARNDX=TV THEN
            BEGIN
            N := GETEXP;
            IF (N>0) AND (NOT EXPRERR) THEN
             IF N+TV<VARMAX THEN
              BEGIN
               TV := TV + N + 1;
               VUP(/VARNDX/) := N;
               VTY(/VARNDX/) := VARRAY;
              END
             ELSE BEGIN ERR; WRITE('NO ROOM FOR ARRAY ',VID(/VARNDX/)); ERRE
                  END
            ELSE BEGIN ERR; WRITE('BAD ARRAY SIZE'); SYLERR END
           END
          ELSE BEGIN ERR; WRITE('ALREADY DECLARED'); SYLERR END
         END
        ELSE BEGIN ERR; WRITE('NOT A VARIABLE SYMBOL'); SYLERR END
       END;        (* CARRAYPROC *)



     PROCEDURE CASISPROC;    (* MOVED HERE FOR ADDRESABILITY REASONS *)
       BEGIN
        POPMACROS;
        FOR I:=1 TO LINLEN DO BEGIN OTL.LIN(/I/) := ' ';
                                    OTL.LINT(/I/).LINC := BLANKLINC  END;
        IF NOT EOF(INPUT) THEN
         REPEAT
          GETCUR;
          IF CUR(/1/)<>'!' THEN
           BEGIN
            LN := VAL(/VLM/); CUP := 1;
            WHILE CUP < ENL DO (* EACH ITERATION ADDS 1 CHAR TO OTL *)
             BEGIN
              OTL.LIN(/LN/) := CUR(/CUP/);
              OTL.LINT(/LN/).LINC := BLANKLINC;
              CUP := CUP + 1; LN := LN + 1;
             END;
            OTL.LEN := LN; CUP := ENL;
            EMPTY := FALSE;
            PUTLINE;
           END;
         UNTIL (CUR(/1/)='!') OR DEFRSYM OR EOFINPUT;
        CUP := CUP+1;
        IF DEFRSYM THEN PUSHSYL(FRCSYL);
       END;     (* CASISPROC *)
  (*$E    *)
BEGIN       (* DOCOMMAND *)
 IF CMDTYPE<=CDEC THEN BEGIN          (* PROTECT AGAINST SET OUT OF RANGE *)
                       IF CMDTYPE IN OPTBRKSET THEN BREAK;
                       IF CMDTYPE IN CRSET THEN CR;           END;
 (* ABOVE BREAK MAY FORCE PAGE EJECT;
    USE DEFRSYM FLAG TO TEST FOR THIS
    AND RE-DO COMMAND AFTER EJECT *)
 IF DEFRSYM THEN PUSHSYL(FRCSYL)
 ELSE
 WITH SYL DO
  BEGIN
   CASE CMDTYPE OF

    CBLANK,CLMAR,CRMAR,CPS,CP,CI,CSP,
    CS,CTP,CHL,CPNO,CLIST,CFIG,CTAB,
    CTABS,CRIGHT,CLINES,CMACRO,CVAR,
    CINC,CDEC,CARRAY,CIF,CUPP:

     CMMD := TRUE;

    CCR,CBREAK,CRESPAG,CRES,CESCCHR,
    CASCII,CCENTER,CJUST,(*CUL,*)CFILL,
    CSIG,CPAGE,CSUP,CLPT,CSTD,CSAV,
    CPP,CAP,CCH,CNMP,CTITLE,CST,
    CATITLE,CLE,CELIST,CBAR,CBB,CEB,
    CU,CT,CRT,CX,CASIS,CDATE,CTIME,
    CFLAG,CFLAGCAPS,
    CFLAGOVER,CFLAGSIG,CLOWER,CUPPER,
    CPERIOD,CSAVPAG,CTOP,
    CMID,CBOT,CFMT,CDOT,CREM,CUSB,CADD,CAUTOCAP,CDEBUG1,CDEBUG2,NOTCMD:

   END;
   CASE CMDTYPE OF
    CBLANK:
     (* BREAK FOLLOWED BY N EXTRA BLANK LINES *)
     BEGIN
      GETOPTN;
      IF S<>INVALID THEN FOR LN:=1 TO N DO PUTBLANK
      ELSE PUTBLANK;
     END;
    CFLAG:
     FLAG := YES;
    CFLAGCAPS:
     FLAGCAPS := YES;
    CLOWER:
     LOWER := YES;
    CUPPER:
     LOWER := NOT YES;
    CESCCHR:
     ESCCHR := YES;
    CPERIOD:
     PERIOD := YES;
    CBREAK:
     (* DOESN'T DO ANYTHING *)
     BEGIN END;
    CCR:
     (* DOESN'T DO ANYTHING *)
     BEGIN END;
    CASCII:
     LOWER := NOT YES;
    CCENTER:
     OTL.CENTER := TRUE;
    CJUST:
     JUSTIT := YES;
(*  CUL:
     UL := YES;              *)
    CLMAR:
     BEGIN GETSN;
      IF OTL.LEN=VAL(/VLM/) THEN OTL.LEN := 0;
      CASE S OF
       MINUS:    IF (VAL(/VLM/)-N)<0 THEN
                  BEGIN ERR; WRITE('LM<0'); ERRE END
                 ELSE VAL(/VLM/) := VAL(/VLM/) - N;
       PLUS:     IF (VAL(/VLM/)+N)>136 THEN
                  BEGIN ERR; WRITE('LM>136'); ERRE END
                 ELSE VAL(/VLM/) := VAL(/VLM/) + N;
       UNSIGNED: IF (N<1) OR (N>136) THEN
                  BEGIN ERR; WRITE('LM OUT OF RANGE'); ERRE END
                 ELSE VAL(/VLM/) := N;
       INVALID:  BEGIN ERR; WRITE('LM FOLLOWED BY'); SYLERR END
      END;
      IF OTL.LEN=0 THEN
       BEGIN OTL.LEN := VAL(/VLM/); OTL.VLEN := VAL(/VLM/);
        FOR LN:=1 TO VAL(/VLM/) DO BEGIN OTL.LIN(/LN/) := ' ';
                                        OTL.LINT(/LN/).LINC := BLANKLINC  END;
       END
     END;
    CRMAR:
     BEGIN GETSN;
      CASE S OF
       PLUS:     IF (VAL(/VRM/)+N)>136 THEN
                  BEGIN ERR; WRITE('RM>136'); ERRE END
                 ELSE VAL(/VRM/) := VAL(/VRM/) + N;
       MINUS:    IF (VAL(/VRM/)-N)<1 THEN
                  BEGIN ERR; WRITE('RM<1'); ERRE END
                 ELSE VAL(/VRM/) := VAL(/VRM/) - N;
       UNSIGNED: IF (N<1) OR (N>136) THEN
                  BEGIN ERR; WRITE('RM OUT OF RANGE'); ERRE END
                 ELSE VAL(/VRM/) := N;
       INVALID:  BEGIN ERR; WRITE('RM FOLLOWED BY'); SYLERR END
      END;
     END;
    CFILL:
     FILL := YES;
    CSIG:
     SIGBL := YES;
    CPAGE:  BEGIN
            FORCECMD('.FRCPAGE  ',8);
            CAPNEXT := AUTOCAP AND LOWER;
            END;
    CTOP:
     DOTOP;
    CMID:
     DOMID;
    CBOT:
     DOBOT;
    CSUP:
     SUP := YES;
    CLPT:
     LPT := YES;
    CSTD:
     SETSTD;
    CPS:
     BEGIN
      GETSN;
      IF (S=UNSIGNED) AND (N>10) THEN
       BEGIN
        OETXT := N;
        OVETXT := N - 1;
        GETSN;
        IF (S=UNSIGNED) AND (N>0) THEN
         BEGIN
          VAL(/VPRM/) := N;
          PAGENV.PRM := N;
         END
        ELSE BEGIN ERR; WRITE('INVALID PS COLUMN'); SYLERR END
       END
      ELSE BEGIN ERR; WRITE('INVALID PS LINES'); SYLERR END
     END;
    CSAV:
     PSHENV;
    CP: CPPROC;
    CRES:
     BEGIN POPENV; CLRLINE END;
    CPP:
     PARAGRAPH;
    CAP:
     AP := YES;
    CI:
     BEGIN
      GETSN;
      IF S<>INVALID THEN
       BEGIN
        CASE S OF
         PLUS:     N := N + VAL(/VLM/);
         MINUS:    N := VAL(/VLM/) - N;
         UNSIGNED:
        END;
        IF N>0 THEN
         BEGIN
          WITH OTL DO BEGIN
           LEN := N; VLEN := N;
           FOR LN:=1 TO N DO BEGIN LIN(/LN/) := ' ';
                                  LINT(/LN/).LINC := BLANKLINC    END;
          END
         END
        ELSE BEGIN ERR; WRITE('INDENT LESS THAN 0'); ERRE END
       END
      ELSE BEGIN ERR; WRITE('I FOLLOWED BY'); SYLERR END
     END;
    CSP:
     BEGIN
      GETSN;
      IF (S<>INVALID) AND (N>=1) AND (N<=5) THEN VAL(/VSP/) := N
      ELSE BEGIN ERR; WRITE('S FOLLOWED BY'); SYLERR END
     END;
    CS:
     (* RETURN PLUS N EXTRA CARRIAGE RETURNS *)
     BEGIN
      GETOPTN;
      IF S<>INVALID THEN FOR LN:=1 TO N*VAL(/VSP/) DO PUTBLANK
      ELSE FOR LN:=1 TO VAL(/VSP/) DO PUTBLANK;
     END;
    CTP:
     BEGIN
      GETSN;
      IF S=UNSIGNED THEN TESTPAGE(N)
      ELSE BEGIN ERR; WRITE('TP FOLLOWED BY'); SYLERR END
     END;
    CCH:
     BEGIN
      YES := TRUE;
      LN := CUP; (*SAVE THIS*)
      STUFFMAC(CHTMACP);
      IF ATITLE THEN
       BEGIN
        CUP := LN; (*SAME AS ABOVE*)
        STUFFMAC(TTLMACP);
        STUFFMAC(STLMACP);
       END;
      FORCECMD('..CH      ',4);    CAPNEXT := AUTOCAP AND LOWER;
     END;
    CHL: CHLPROC;
    CNMP:
     IF YES THEN VAL(/VNMP/) := 1 ELSE VAL(/VNMP/) := 0;
    CPNO:
     BEGIN
      GETSN;
      IF (S=UNSIGNED) AND (N<1000) THEN VAL(/VPAGE/) := N - 1
      ELSE BEGIN ERR; WRITE('PAGE NUMBER WAS'); SYLERR END
     END;
    CTITLE:
     BEGIN STUFFMAC(TTLMACP); CAPNEXT := AUTOCAP AND LOWER; END;
    CST:
     BEGIN STUFFMAC(STLMACP); CAPNEXT := AUTOCAP AND LOWER; END;
    CATITLE:
     ATITLE := YES;
    CLIST:
     BEGIN
      GETSN;
      IF (S=UNSIGNED) AND (N>0) AND (N<6) THEN
       BEGIN
        PSHENV;
        IF VAL(/VLIST/)=0 THEN VAL(/VLM/) := VAL(/VLM/) + 9
        ELSE VAL(/VLM/) := VAL(/VLM/) + 4;
        VAL(/VLIST/) := VAL(/VLIST/) + 1;
        VAL(/VLIST+VAL(/VLIST/)/) := 0;
        VAL(/VSP/) := N;
        PARSPACE := N + 1;
        PMAR := 0;
        PREL := TRUE;
        PSHENV;
       END
      ELSE BEGIN ERR; WRITE('LIST SPACING'); SYLERR END;
     END;
    CLE:
     IF VAL(/VLIST/)>0 THEN
      BEGIN
       FOR LN:=1 TO VAL(/VSP/) DO PUTBLANK;
       RESENV(ENSTK(/ENP-1/));
       CLRLINE;
       VAL(/VLIST+VAL(/VLIST/)/) := VAL(/VLIST+VAL(/VLIST/)/) + 1;
       OTL.LEN := VAL(/VLM/) - 4;
       ADDNUM(VAL(/VLIST+VAL(/VLIST/)/),OTL);
       ADDCHR('.');
       OTL.LEN := VAL(/VLM/);
       OTL.VLEN := VAL(/VLM/);
      END
     ELSE BEGIN ERR; WRITE('NO ACTIVE LIST'); ERRE END;
    CELIST:
     IF VAL(/VLIST/)>0 THEN
      BEGIN
       VAL(/VLIST/) := VAL(/VLIST/) - 1;
       POPENV; POPENV; CLRLINE;
      END
     ELSE BEGIN ERR; WRITE('NO ACTIVE LIST'); ERRE END;
    CFIG:
     BEGIN
      GETSN;
      IF (S=UNSIGNED) THEN
       BEGIN
        IF N<=OVETXT-VAL(/VOLNO/)+1 THEN
         BEGIN
          OVBTXT := 0; (* SO PUTBLANK WORKS *)
          FOR LN:=1 TO N DO PUTBLANK
         END
        ELSE
         BEGIN
          IF FIGP<FIGMAX THEN
           BEGIN
            FIGP := FIGP + 1;
            FIGN(/FIGP/) := N;
           END
          ELSE BEGIN ERR; WRITE('TOO MANY PENDING FIGS'); ERRE END
         END
       END
      ELSE BEGIN ERR; WRITE('FIG FOLLOWED BY'); SYLERR END
     END;
    CBAR:
     BAR := YES;
    CBB:
     BEGIN BB := TRUE; OTL.BBAR := TRUE END;
    CEB:
       BEGIN
      IF EMPTY THEN OTL.BBAR := FALSE;
      BB := FALSE;
     END;
    CU:
     UNDL := YES;
    CT:
     T := TRUE;
    CTAB,CTABS:
     BEGIN
      L := 1;
      CLRTAB;
      REPEAT
       GETOPTN;
       IF S<>INVALID THEN
        IF L<=TABMAX THEN BEGIN TABS(/L/) := N; L := L + 1 END
      UNTIL S=INVALID;
      BACKUPSYL;
     END;
    CRT:
     RT := TRUE;
    CDOT:
     DOT := YES;
    CRIGHT:
     BEGIN
      GETSN;
      IF (S=UNSIGNED) AND (N<=136) THEN RIGHTSPACE := N
      ELSE BEGIN ERR; WRITE('RIGHT SPACE '); SYLERR END
     END;
    CLINES:
     BEGIN
      GETSN;
      IF (S=UNSIGNED) THEN
       OEPAG := N
      ELSE BEGIN ERR; WRITE('LINES FOL BY'); SYLERR END
     END;
    CMACRO: CMACROPROC;
    CX:
     BEGIN XTEND := YES; CAPNEXT := FALSE END;
    CVAR: CVARPROC;
    CINC:
     BEGIN
      GETSYM;
      IF SYMTYPE=VARS THEN
       IF VARNDX<TV THEN VAL(/VARNDX/) := VAL(/VARNDX/) + 1
       ELSE BEGIN ERR; WRITE('UNDEDECLARED VAR'); SYLERR END
      ELSE BEGIN ERR; WRITE('INC FOLLOWED BY'); SYLERR END
     END;
    CDEC:
     BEGIN
      GETSYM;
      IF SYMTYPE=VARS THEN
       IF VARNDX<TV THEN VAL(/VARNDX/) := VAL(/VARNDX/) - 1
       ELSE BEGIN ERR; WRITE('UNDECLARED VAR'); SYLERR END
      ELSE BEGIN ERR; WRITE('DEC FOLLOWED BY'); SYLERR END
     END;
    CSAVPAG:
     BEGIN SAVENV(PAGENV); FIRSTCH := FALSE END;
    CRESPAG:
     RESENV(PAGENV);
    CARRAY: CARRAYPROC;
    CFMT:
     BEGIN N := GETEXP; DOFMT(N,GETEXP) END;
    CIF:
     IF GETEXP=0 THEN CUP := ENL;
    CASIS: CASISPROC;
    CFLAGOVER:
     FLAGOVER := YES;
    CFLAGSIG:
     FLAGSIG := YES;
    CREM:
     CUP := ENL; (* MAKES REST OF LINE A COMMENT *)
    CUPP:
     (* FORCE NEXT SYMBOL UPPER *)
     BEGIN GETSYM; UPCASESYL; ADDWORD; END;
    CUSB:
     USB := YES;
    CADD:
     ADDWORD;
    CDATE:
     BEGIN
   (* DATE(V); *)    V:='          ';
      WITH SYL DO
       BEGIN
        FOR I:=3 TO 6 DO BEGIN LIN(/I/) := V(/I+1/);
                         LINT(/I/).LINC := BLANKLINC END;
        LIN(/1/) := V(/8/); LIN(/2/) := V(/9/);
        LIN(/7/) := V(/2/); LIN(/8/) := V(/3/);
        LEN := 8;
       END;
      PUTWORD;
     END;
    CTIME:
     BEGIN
    (*TIME(V); *)     V:='          ';
      WITH SYL DO
       BEGIN
        FOR I:=1 TO 5 DO BEGIN LIN(/I/) := V(/I+1/);
                         LINT(/I/).LINC := BLANKLINC    END;
        LIN(/3/) := ':';
       END;
      PUTWORD;
     END;
   CDELMAC:
    BEGIN
     TMACP := MACLSTP;
     GETTOKEN;
     IF SYL.LEN<>0 THEN
      BEGIN
       UPCASESYL;
       FOR SUB := 1 TO 10 DO V(/SUB/) := SYL.LIN(/SUB/);
       WHILE (TMACP@.NM<>V) AND (TMACP<>CHTMACP) DO
        BEGIN MACP := TMACP; TMACP := TMACP@.MA; END;
       IF TMACP=CHTMACP THEN
        BEGIN ERR; WRITE('NO MACRO ',V,' DEFINED'); ERRE END
       ELSE IF TMACP=MACLSTP THEN MACLSTP := TMACP@.MA
        ELSE MACP@.MA := TMACP@.MA;
      END
     ELSE BEGIN ERR; WRITE('NO MACRO NAME'); ERRE END;
    END;
    CDEBUG1 : BEGIN  DEBUG1 := YES; DEBUG2 := YES; END;
    CDEBUG2 : DEBUG2 := YES;
    CAUTOCAP : BEGIN AUTOCAP := YES; CAPNEXT := AUTOCAP AND LOWER; END;
    NOTCMD: BEGIN END
   END
  END;
 CMMD := FALSE;
END;     (*$END OF DOCOMMAND *)

PROCEDURE INI;
BEGIN
 LITERAL1 := '.TOP .SAV .RESPAG .B 3 ..TTL ..NMP .BR ..STL .B 2 .RES .MID ';
 LITERAL2 := '$$PAGE=$$PAGE+1; ..NONMP .IF $$NMP .TAB $$PRM .RT $$PAGE ';
 LITERAL3 := '.IF #$$NMP .B 1 ';
 LITERAL4 := '.PAGE .FIG 12 .C <CHAPTER $$CH=$$CH+1 ';
 LITERAL5 := '$$HL=0; .B 2 .C ..CHT .B 3 ';
  BLANKLINC := '    ';
  BLANKLINC(/4/) := CHR(0);
 VAL(/VLM/) := 1;
 CMMD := FALSE;
 USB := TRUE;
 UNDL := FALSE;
 ILNO := 0; EOFINPUT := FALSE;
 FREEMACP := NIL;
 INISTDMACS;
 FLAGOVER := TRUE; FLAGSIG := TRUE;
 T := FALSE; RT := FALSE; DOT := FALSE;
  FOR ERRORS := 0 TO (CHARSIZE - 1) DO BEGIN
      ITEMSET(/ERRORS/) := FALSE;
      TERMSET(/ERRORS/) := FALSE;
      END;
  ERRORS := 0;
  ITEMSET (/ ORD('$') /) := TRUE; ITEMSET (/ ORD('+') /) := TRUE;
  ITEMSET (/ ORD('-') /) := TRUE; ITEMSET (/ ORD('#') /) := TRUE;
  FOR CH := '0' TO '9' DO  ITEMSET (/ORD(CH)/) := TRUE;
  TERMSET := ITEMSET;
  TERMSET (/ ORD('(') /) := TRUE;
 INIRELS;
 PUSHED := FALSE;
 PMAR := 0;
 AP := FALSE;
 PARA := FALSE;
 OTL.CENTER := FALSE;
 RIGHT := TRUE;
 OPNO := 5;
 OBTXT := 7;
 OVETXT := 58; OETXT := 58;
 OEPAG := 66;
 LPT := TRUE;                      (* THIS IS CHANGED FROM ORIGINAL SPECS *)
 ENP := 0;
 SUP := FALSE;
 PAGE := FALSE;
 YES := TRUE;
 ENP := 0;
 PARSPACE := 1;
 PARTEST := 3;
 DEFRB := 0;
 PREL := TRUE;
 FIRSTCH := TRUE;
 FORCE := FALSE;
 ATITLE := FALSE;
 FIGP := 0;
 BAR := FALSE; BB := FALSE;
(* LINELIMIT(OUTPUT,50000); *)
 CLRTAB;
 TAB := 0;
 DEFRSYM := FALSE;
 RT := FALSE;
 CLRLINE;
 NEW(ACTP);
 WITH ACTP@ DO
  BEGIN NP := 0; CA := NIL; ON := FALSE; NEW(LI) END;
 BOTMAC := ACTP;
 XTEND := FALSE;
 INICMDS;
 SETSTD;
 INIVARS;
 VAL(/VNMP/) := 1;
 VAL(/VLM/) := 1;
 VAL(/VRM/) := 72;
 VAL(/VPRM/) := 72;
 OTL.LEN := VAL(/VLM/); OTL.VLEN := VAL(/VLM/);
 VAL(/VSP/) := 1;
 VAL(/VOLNO/) := 1000000;
 SAVENV(PAGENV);
 WITH ADDSYL DO
  BEGIN LIN(/1/) := '.'; LIN (/2/) := 'A'; LIN(/3/) := 'D'; LIN(/4/):='D';
        LEN := 4; END;
 LETTERS := 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';
 DEBUG1 := FALSE;
 DEBUG2 := FALSE;
 RIGHTSPACE := 0;
  AUTOCAP := FALSE;
  CAPNEXT := FALSE;
END;     (*$END OF INI *)
          (* MAIN *)

BEGIN             REWRITE(QRR);
 INI;
 GETCUR;
 WHILE NOT EOFINPUT DO
  BEGIN
   IF (CUR(/1/)=' ') AND AP THEN FORCECMD('.PP       ',3);
   GETSYM;
   IF SYMTYPE=NONE THEN BLANKLINE
   ELSE
    BEGIN
     WHILE SYMTYPE<>NONE DO
      BEGIN
       CASE SYMTYPE OF
        WORD: PUTWORD;
        VARS: PUTVAR;
        COMMAND: DOCOMMAND
       END;
       GETSYM
      END;
    END;
   ENDLINE;
   IF NOT DEFRSYM THEN GETCUR;
  END;
 FIN
END.
