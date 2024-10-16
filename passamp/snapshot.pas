(*$D-,X+*)
(*******************************************************************
*
*             SNAPSHOT AND POST MORTEM DUMP UTILITY
*
*     THIS PROGRAM PRINTS OUT THE CURRENT STACK ENVIRONMENT FOR A
*     PASCAL PROGRAM RUNNING ON THE S.L.A.C. TRIPLEX SYSTEM (IBM 370)
*     SNAPSHOT TRACES BACK THROUGH THE STACK FRAMES OF THE PROGRAM
*     AND FOR EACH STACK FRAME PRINTS 1) THE NAME OF THE PROCEDURE
*     WHICH 'OWNS' THAT FRAME, 2) THE NAME OF THE PROCEDURE WHICH
*     CALLED THE OWNING PROCEDURE AND THE SOURCE LINE NUMBER FROM
*     WHICH THE CALL WAS MADE, 3) THE VALUES OF THE PROCEDURE'S
*     PARAMETERS AND LOCAL VARIABLES.
*
*     THIS PROGRAM IS CLOSELY AWARE OF THE RUN-TIME ENVIRONMENT OF
*     'PASCAL' PROGRAMS AND USES 'CASE VARIANT' RECORDS TO INTERPRET
*     THE CONTENTS OF MEMORY LOCATIONS DIFFERENTLY AS NEEDED. FOR
*     THIS REASON, IT SHOULD NOT BE COMPILED WITH THE 'D+' OPTION.
*
*  AUTHOR:  EARL WALDIN
*
*           COMPUTATION RESEARCH GROUP
*           STANFORD LINEAR ACCELERATOR CENTER
*           STANFORD, CA. 94305
*
*
*                                          UPDATED: 12-05-78  (SZH)
*                                          UPDATED: 08-09-79  (SZH)
*
*
******************************************************************)


PROGRAM DUMP(QRD,OUTPUT);



PROCEDURE SNAPSHOT(LEVEL,DMPKIND:INTEGER);


   (* VALUES FOR DMPKIND WHEN CALLED BY A USER PROCEDURE ARE:

      0 - FULL LISTING EXCEPT THAT FOR ARRAYS, ONLY THE FIRST
           AND LAST FEW  ENTRIES ARE PRINTED. (DEFAULT ON ABEND)

      1 - TRACE OF PROCEDURE CALLS ONLY (NO VARIABLES).

     10 - FULL LISTING INCLUDING ALL ENTRIES IN ARRAYS.

   -----------------------------------------------------------*)


   CONST
      HALFW = 65536;   (*2EXP16*)
      ADDRC = 16777216;    (*2EXP24*)
      PROCSIZE = 8192;
      VARLN = 8;
      PNAMLN = 12;
      BYL_INT  = 4;  MAL_INT  = 4;
      BYL_PTR  = 4;  MAL_PTR  = 4;
      BYL_REAL = 8;  MAL_REAL = 8;
      BYL_BOOL = 1;  MAL_BOOL = 1;
      BYL_CHAR = 1;  MAL_CHAR = 1;
      BYL_SET  = 8;  MAL_SET  = 4;
      IBFLN    = 300;
      DEFLEV   = 20;  (* DEFAULT MAX LEVEL OF STACK TO TRACE *)
#     ARRLMT   = 3 ;  (* Default # of array elements printed *)
#     STRLMT   = 140; (* LIMIT ON STRING (ARRAY OF CHAR) SIZE *)
      UNDFINT  = -2122219135 ;    (* HEX '81818181' *)
      UNDFBYTE = 129 ;            (* HEX '81'       *)

   TYPE

      ABNDP = @ABNDAREA;

      ABNDAREA = RECORD
                  ABCODE:INTEGER;
                  ABADDR:INTEGER;
                  ABSPC1:INTEGER;
                  ABLOWR:INTEGER;
                  ABUPPR:INTEGER;
                  ABCVAL:INTEGER;
#                 ABEMSG:@ MSGTYPE;
                  ABMSGL: 1..100;
                  ABREGS: ARRAY[5..12] OF INTEGER;
               END;

      MSGTYPE = ARRAY[1..100] OF CHAR;

      FUNCC = RECORD CASE INTEGER OF
               1:(C:(FULL,STKTRACE));
               2:(P:ABNDP);
               3:(I:INTEGER);
            END;

      MXSET  = SET OF 0..63 ;
      PROCNAME = ARRAY(/1..PNAMLN/) OF CHAR;

      PROC_PTR = @PROC_HEAD;    PROCTPTR = @PROC_TAIL;
      FRM_PTR = @STK_FRM;
      STK_FRM = RECORD
           CASE INTEGER OF

                  (* STACK FRAME MARK AREA *)
              1:(SPC1:INTEGER;
                 BAK_LNK:FRM_PTR;
                 FOR_LNK:FRM_PTR;
                 RET:INTEGER;
                 EPA:PROC_PTR;
                 GPR0_12:ARRAY(/0..12/) OF INTEGER;
                 SPC2,SPC3:INTEGER;
                 FPRS:ARRAY(/1..4/) OF REAL);

                  (* STACK FRAME VARIABLE OVERLAYS *)
              2:(CASE INTEGER OF
                    1:(INT_VA:ARRAY(/0..10000/) OF INTEGER);
                    2:(BOOL_VA:ARRAY(/0..10000/) OF BOOLEAN);
                    3:(CHR_VA:ARRAY(/0..10000/) OF CHAR);
                    4:(REAL_VA:ARRAY(/0..10000/) OF REAL);
                    5:(SETE:ARRAY(/0..10000/) OF MXSET);
                    6:(SPCI:INTEGER;
                           SETO:ARRAY(/0..10000/) OF MXSET) );

                  (* GLOBAL STACK AREA *)
              3:(SPCB:ARRAY(/1..18/) OF INTEGER;
                  CURNTNP:INTEGER;
                  ENDNP0:INTEGER)
           END;

      PROC_HEAD  = RECORD
            SPC1:INTEGER;
            SPNAME:ARRAY(/0..5/) OF CHAR;
            PROCLN:ARRAY(/0..1/) OF CHAR;
            END;

      PROC_TAIL = RECORD
            PROCID:ARRAY(/0..1/) OF CHAR;
            SLINNO:ARRAY(/0..1/) OF CHAR;
            FPNAME:ARRAY(/1..PNAMLN/) OF CHAR;
            LINARY:ARRAY(/1..100000/) OF CHAR;
            END;

      VARTYPE = (INT,RL,BOL,CHA,PTR,REC,PSET,ARY,SCL,FIL,UNK);


   VAR
      DMARRAY:ARRAY(/0..1/) OF FRM_PTR;
      VARTARY:ARRAY(/0..40/) OF VARTYPE;
      DEPTH,MAXDEPTH,I,J,UID,PLEN:INTEGER;
      CH:CHAR ;
      DONE: BOOLEAN ;
      TOPSTK,BOTSTK,TMPSTK:FRM_PTR;
      CPROCN:PROCNAME;
      IBF:ARRAY(/1..IBFLN/) OF CHAR;
      PAR2:FUNCC;
      ARRYLSTF:BOOLEAN;


   PROCEDURE $WRADDR(X:INTEGER);

      VAR   Z0,Z9,ZA:INTEGER; H:ARRAY(/1..6/) OF CHAR;

      BEGIN
      Z0 := ORD('0');  Z9 := ORD('9');  ZA := ORD('A');
       IF (BOTSTK@.CURNTNP <= X) AND (X <= BOTSTK@.ENDNP0)
         THEN BEGIN
            I := 6;
#              REPEAT
#              J := X MOD 16 ;  X := X DIV 16;
#              IF J >= 10 THEN   H(/I/) := CHR(ZA+(J-10))
#              ELSE  H(/I/) := CHR(Z0+J) ;
#              I := I-1 ;
#              UNTIL I = 0;
#
#           WRITE(OUTPUT, H, 'H') ;

            END

         ELSE IF X=-1 THEN WRITE(OUTPUT, 'NIL')
            ELSE WRITE(OUTPUT, 'UNDEF. (pointer)');

      END; (*WRADDR*)


#  FUNCTION $IDLEN(NAME : PROCNAME): INTEGER ;
#     VAR I : INTEGER ;
#
#     BEGIN  I := 0 ;
#
#     REPEAT I := I+1 UNTIL  (NAME(/I/) = ' ') OR ( I >= PNAMLN) ;
#
#     $IDLEN := I-1 ;
#     END ;


   PROCEDURE $GETPNAME(P:PROC_PTR; VAR PID,LEN:INTEGER;
                       VAR NAME:PROCNAME);

      VAR
        TP:RECORD  CASE INTEGER OF
           1:(I:INTEGER);
           2:(P:PROCTPTR)
           END;
        I:INTEGER;

      BEGIN
      LEN := ORD(P@.PROCLN(/0/))*256 + ORD(P@.PROCLN(/1/));
     "FOR I := 1 TO PNAMLN DO NAME(/I/) := ' ';"
      NAME := '            ' ;
      PID := 0;

      IF (LEN = 0) OR (LEN > PROCSIZE)
         THEN FOR I := 1 TO 7 DO
            NAME(/I/) := P@.SPNAME(/I/)

         ELSE BEGIN
         TP.I := ORD(P) + LEN;
         FOR I := 1 TO PNAMLN DO
            NAME(/I/) := TP.P@.FPNAME(/I/);
         PID := ORD(TP.P@.PROCID(/0/))*256 + ORD(TP.P@.PROCID(/1/));
         END;
      END;  (* $GETPNAME *)




   PROCEDURE $PRNTVAR(STKP:FRM_PTR; PNAME:PROCNAME; PID:INTEGER);

      CONST   INDENT6=6;

      TYPE   CMPCODE = (PROCESS, SUCC, TYPERR, IDERR, SYNERR,
                        NUMERR, EOFERR, BUFERR, ADDERR);

         CNTRL = (YES,NO);

      VAR
#        MAXI,I,VADDR,ALNFCT: INTEGER;  TPROCN: PROCNAME;  ERR: CMPCODE;
         INDRCT:BOOLEAN;

         SB:RECORD CASE INTEGER OF
               1:(I:INTEGER);
               2:(P:FRM_PTR)
               END;

#
#     PROCEDURE $ALIGN(VAR OFFSET: INTEGER; ALN: INTEGER);
#
#        (* $ALIGNS POSITIVE 'OFFSET' ON AN 'ALN' BYTE BOUNDARY *)
#
#        BEGIN
#        OFFSET := ((OFFSET+ALN-1) DIV ALN)*ALN;
#        END (*ALIGN*);
#
      PROCEDURE $ERRMSG(ECODE:CMPCODE; INDX:INTEGER);

         VAR   J,K:INTEGER;

         BEGIN
         WRITELN(OUTPUT) ;
         WRITE(OUTPUT, '0    ****  ERROR IN SYMBOL TABLE  ****');
         CASE ECODE OF
            TYPERR:  WRITELN(OUTPUT, '    ILLEGAL TYPE.');
            IDERR:   WRITELN(OUTPUT, '    ILLEGAL IDENTIFIER.');
            SYNERR:  WRITELN(OUTPUT, '    SYNTAX ERROR.');
            NUMERR:  WRITELN(OUTPUT, '    IMPROPER NUMBER.');
            EOFERR:  WRITELN(OUTPUT, '    PREMATURE END OF FILE.');
            BUFERR:  WRITELN(OUTPUT, '    INTERNAL BUFFER EXCEEDED.');
            ADDERR:  WRITELN(OUTPUT, '    ADDRESS EXPECTED.');
            END;

         J := 1;
         WHILE J <= INDX DO BEGIN
            WRITELN(OUTPUT);   WRITE(OUTPUT, ' ');
            IF (INDX-J+1) < 80 THEN K := INDX-J+1 ELSE K := 80;
            WHILE K > 0 DO BEGIN WRITE(OUTPUT, IBF(/J/)); J := J+1;
                  K := K-1; END;
            END;
         WRITELN(OUTPUT);
         END;  (* $ERRMSG *)


      FUNCTION $TRANSVAR(CH:CHAR):VARTYPE;

         VAR  I:INTEGER;

         BEGIN
         I := ORD(CH) - ORD('A');
         IF (0 <= I) AND (I <= (ORD('Z')-ORD('A')))
            THEN $TRANSVAR := VARTARY(/I/)
            ELSE $TRANSVAR := UNK;
         END;  (* $TRANSVAR *)



      PROCEDURE $PRNT(INDNT:INTEGER; VAR INDX,OFFSET:INTEGER;
#                       BASE:FRM_PTR; IDREQ(*,LIST*):CNTRL);

#        VAR   M,L,K,J,I,SAVALN:INTEGER;
               ubnd, lmt, lmt2, lmt3, lmt4: integer;

               I_R_S :RECORD
                        CASE INTEGER OF
                        1: (R : REAL) ;
                        2: (S : MXSET) ;
                        3: (I1: INTEGER; I2: INTEGER) ;
                      END;

               TBOL:BOOLEAN;   TCH:CHAR;

         FUNCTION $CNVTNUM(VAR J:INTEGER): INTEGER;

            VAR  ZERO,NUM:INTEGER;  SIGN:BOOLEAN;

            BEGIN
            NUM := 0;  ZERO := ORD('0');  SIGN := TRUE;
            WHILE IBF(/J/) = ' ' DO J := J+1;
            IF IBF(/J/) = '-' THEN BEGIN SIGN := FALSE; J := J+1; END;
            WHILE IBF(/J/) <> ' ' DO
               BEGIN
               NUM := 10*NUM + ORD(IBF(/J/)) - ZERO;
               J := J+1;
               END;
            J := J+1;
            IF SIGN THEN $CNVTNUM := NUM ELSE $CNVTNUM := -NUM;
            END;


         BEGIN
         IF IDREQ=YES
            THEN BEGIN   I := INDX ;
            WHILE IBF(/INDX/) <> ' ' DO
               BEGIN
              "IF LIST=YES THEN"WRITE(OUTPUT, IBF(/INDX/));
               INDX := INDX+1
               END;
            I := INDX-I ;
#           IF (I > 0) "AND (LIST=YES)"  THEN
#              IF I <= 6 THEN  WRITE(OUTPUT, ' ':6-I)
#              ELSE  BEGIN  WRITE(OUTPUT, ' ':12-I); INDNT := INDNT+INDENT6 END;
            INDX := INDX+1;
            END;

         INDX := INDX+2;

         CASE $TRANSVAR(IBF(/INDX-2/)) OF

            INT,SCL:  BEGIN "  I := OFFSET + BYL_INT -1;
                  OFFSET := I - (I MOD BYL_INT);"
#                 $ALIGN(OFFSET, BYL_INT);  $ALIGN(ALNFCT, BYL_INT);
                  I := BASE@.INT_VA(/OFFSET DIV BYL_INT/) ;
#                "IF LIST=YES THEN"
                     IF I = UNDFINT
                             THEN WRITELN(OUTPUT, ' = UNDEF. (integer/scaler)' )
                             ELSE  WRITELN(OUTPUT, ' = ', I:1);
                  OFFSET := OFFSET + BYL_INT;
                  END;

             RL:  BEGIN    "I := OFFSET + BYL_REAL -1;
                  OFFSET := I - (I MOD BYL_REAL);"
#                 $ALIGN(OFFSET, BYL_REAL);  $ALIGN(ALNFCT, BYL_REAL);
                  I_R_S.R := BASE@.REAL_VA(/OFFSET DIV BYL_REAL/) ;
#                "IF LIST=YES THEN"
                     IF (I_R_S.I1 = UNDFINT) AND (I_R_S.I2 = UNDFINT)
                            THEN WRITELN(OUTPUT, ' = UNDEF. (real)')
                            ELSE  WRITELN(OUTPUT, ' =', I_R_S.R:10);
                  OFFSET := OFFSET + BYL_REAL;
                  END;

            CHA:  BEGIN     "IF LIST=YES THEN" WRITE(OUTPUT, ' = ''');
                  (* LENGTH=1 --> NO ALIGNMENT HERE *)
                  TCH := BASE@.CHR_VA(/OFFSET/);
#                "IF LIST=YES THEN"
                     IF (64 <= ORD(TCH)) AND (ORD(TCH) <= 250)
                            THEN WRITELN(OUTPUT, TCH,'''')
                            ELSE WRITELN(OUTPUT, '#''');
                  OFFSET := OFFSET + BYL_CHAR;
                  END;

            BOL:  BEGIN
                  (* LENGTH=1 --> NO ALIGNMENT HERE *)
                  TBOL := BASE@.BOOL_VA(/OFFSET/) ;

                  "IF LIST=YES THEN"
                     IF ORD(TBOL) = UNDFBYTE
                             THEN WRITELN(OUTPUT, ' = UNDEF. (boolean)')
                             ELSE  IF TBOL THEN  WRITELN(OUTPUT, ' = TRUE')
                                ELSE  WRITELN(OUTPUT, ' = FALSE');
                  OFFSET := OFFSET + BYL_BOOL;
                  END;

            PTR:  BEGIN    "I := OFFSET + BYL_INT -1;
                  OFFSET := I - (I MOD BYL_INT);"
#                 $ALIGN(OFFSET, BYL_PTR);  $ALIGN(ALNFCT, BYL_PTR);
                 "IF LIST=YES THEN"
                     BEGIN
                     WRITE(OUTPUT, ' = ');
                     $WRADDR(BASE@.INT_VA(/ OFFSET DIV BYL_INT/));
                     WRITELN(OUTPUT);
                     END;
                  OFFSET := OFFSET + BYL_INT;
                  END;

           PSET:  BEGIN  "  I := OFFSET + MAL_SET -1;
                  OFFSET := I - (I MOD MAL_SET);"
#                 $ALIGN(OFFSET, MAL_SET);  $ALIGN(ALNFCT, MAL_SET);
                  I := OFFSET DIV BYL_SET;
                  M := OFFSET MOD BYL_SET;
                  L := $CNVTNUM(INDX);    L := $CNVTNUM(INDX);
                  J := 0;

                  IF M=0 THEN  I_R_S.S := BASE@.SETE(/I/)
                     ELSE  I_R_S.S := BASE@.SETO(/I/);

                  "IF LIST=YES THEN"
                     BEGIN
   ##                WRITELN(OUTPUT, ' = (/0..', L:1, '/)');
                     WRITE(OUTPUT, ' ':INDNT+INDENT6+3);
                     IF (I_R_S.I1 = UNDFINT) AND (I_R_S.I2 = UNDFINT)
                        THEN   WRITE(OUTPUT, ' UNDEF. (set)')

                        ELSE  REPEAT
                        IF (J MOD 10) = 0 THEN  WRITE(OUTPUT, ' ');
                        WRITE(OUTPUT, ORD(J IN I_R_S.S):1) ;
                        J := J+1;
                        UNTIL J > L ;


                     WRITELN(OUTPUT);   WRITELN(OUTPUT);
                     END;
#                 OFFSET := OFFSET + BYL_SET;
                  END;

#           REC:  BEGIN   "IF LIST=YES THEN" WRITELN(OUTPUT);
                  INDNT := INDNT+INDENT6;
                  WHILE IBF(/INDX/) <> ';' DO
                     BEGIN
#                   "IF LIST=YES THEN" WRITE(OUTPUT, ' ': INDNT+1);
#                    $PRNT(INDNT,INDX,OFFSET,BASE,YES(*,LIST*));
                     END;
                  WRITELN(OUTPUT);
                  END;

            ARY:  BEGIN   K := $CNVTNUM(INDX); M := $CNVTNUM(INDX);
                  UBND := M-K;

                  IF ($TRANSVAR(IBF(/INDX/)) = CHA) AND (UBND <= STRLMT)
                     THEN BEGIN

#                   "IF LIST=YES THEN"
                        BEGIN  WRITE(OUTPUT, ' = ''');
                        FOR I:=0 TO UBND DO
                           BEGIN
                           TCH := BASE@.CHR_VA(/OFFSET+I/);
                           IF (64 <= ORD(TCH)) AND (ORD(TCH) <= 250) THEN
                              WRITE(OUTPUT, TCH)
                           ELSE  WRITE(OUTPUT, '#');
                           END;

                        WRITELN(OUTPUT, '''');
                        END ;

                     INDX := INDX + 2;
                     OFFSET := OFFSET + BYL_CHAR*(1+UBND);
                     END

                  ELSE BEGIN
                     (* find the range of indices to be printed *)
#                    lmt := arrlmt;
#                    if (ubnd >= 500*arrlmt) or (ubnd <= 3*arrlmt) then
                        lmt := arrlmt*2 ;
#                       (* short arrays, print the whole thing,  *)
#                       (* very long arrays, print more elements *)
#                    lmt4 := ubnd-lmt;  lmt2 := lmt4 div 2;  lmt3 := lmt2+lmt+1;
                    "IF LIST=YES THEN"  WRITELN(OUTPUT);
#                    savaln := alnfct;  alnfct := BYL_CHAR;  (*=1*)

                     FOR i := 0 to ubnd DO

                        IF (i < lmt) OR ((lmt2 < i) and (i < lmt3)) OR
                           (i > lmt4) OR arrylstf  THEN
                           BEGIN
#                          J := INDX;  l := offset;
#                         "IF LIST=YES THEN"
#                             WRITE(OUTPUT, ' ':INDNT, K+I:INDENT6, ']');
#                          $PRNT(INDNT+"INDENT6"4,J,OFFSET,BASE,NO(*,LIST*));
#                          $align(offset, alnfct);
#                          l := offset-l;  (* length of array element *)
#                          END

                        ELSE
                           "IF (ARRLMT <= I) AND (I <= M-K-ARRLMT) THEN "
                           BEGIN
                           "IF LIST=YES THEN"
                              IF (i = lmt) or (i = lmt3) THEN WRITELN(OUTPUT);
                           J := INDX;
#                          offset := offset+l;  (* skip over other elements *)
                           (*PRNT(INDNT+4,J,OFFSET,BASE,NO,"no");*)
                           END ;


                     INDX := J-2;
#                    $align(alnfct, savaln);  (* update alignment factor *)
                    "IF LIST=YES THEN" WRITELN(OUTPUT) ;
                     END;
                  END;

            FIL:  "IF LIST=YES THEN"
                     BEGIN
                     WRITE(OUTPUT, ' @ = ''');
                     IF OFFSET = 0 THEN WRITE(OUTPUT, BASE@.CHR_VA(/0/))
                           ELSE WRITE(OUTPUT, BOTSTK@.CHR_VA(/OFFSET/));
                     WRITELN(OUTPUT, '''');
                     END;

            END;

         INDX := INDX+2;
         END;  (* $PRNT *)



      FUNCTION $IVSCAN(VAR INDX:INTEGER; VAR F:TEXT;
                        IDREQ:CNTRL): CMPCODE;

         TYPE STATVAR = (GTYP,GNAME,PTYP,COPY,GNUM,GTERM);

         VAR  CONT:CMPCODE; VTYPE:VARTYPE;  STATE:STATVAR;
              FIR,LAS:INTEGER;   SWITCH:BOOLEAN;

         BEGIN
         CONT := PROCESS;      SWITCH := FALSE;
         IF IDREQ=YES THEN STATE := GNAME ELSE STATE := GTYP;

         WHILE (NOT EOF(F)) AND (INDX+1<IBFLN) AND (CONT=PROCESS) DO
            BEGIN
            WHILE (F@=' ') AND (NOT EOF(F)) DO GET(F);

            FIR := INDX+1;
            IF NOT (EOF(F) OR (STATE=PTYP))
               THEN IF F@ = ';'
                      THEN BEGIN  READ(F,IBF(/FIR/));  LAS := FIR;
                      INDX := LAS+1; IBF(/INDX/) := ' '; END

                      ELSE BEGIN
                      WHILE NOT(EOF(F) OR (F@ = ';') OR (F@ = ' '))
                            AND (INDX+1 < IBFLN)
                        DO BEGIN
                        INDX := INDX+1; READ(F,IBF(/INDX/));
                        END;

                      IF INDX < IBFLN
                        THEN BEGIN    LAS := INDX;
                        INDX := INDX+1;
                        IBF(/INDX/) := ' '; END

                        ELSE CONT := BUFERR;
                      END;

            IF (NOT EOF(F)) AND (CONT = PROCESS)
               THEN BEGIN

                CASE STATE OF

                  GNAME:   IF (IBF(/FIR/) <> ';')
                              AND ((LAS-FIR)>=0)

                              THEN STATE := GTYP
                              ELSE CONT := IDERR;


                  GTYP:   BEGIN    VTYPE := $TRANSVAR(IBF(/FIR/));
                          IF (VTYPE < UNK) AND ((LAS-FIR)=0)
                            THEN  STATE := PTYP
                            ELSE  CONT := TYPERR;
                          END;

                  PTYP:  IF (VTYPE <= REC) AND (F@ = ';')

                            THEN BEGIN     CONT := SUCC;
                            READ(F,IBF(/INDX+1/)); INDX := INDX+2;
                            IBF(/INDX/) := ' ';  END

                            ELSE IF VTYPE <= PTR
                                  THEN CONT := SYNERR

                            ELSE  CASE VTYPE OF

                                  SCL,FIL: STATE := COPY;


                                  REC: BEGIN
                                       CONT := $IVSCAN(INDX,F,YES);
                                       IF CONT=SUCC THEN CONT:=PROCESS;
                                       END;

                                  PSET,ARY: STATE := GNUM;
                                END;

                  COPY:  IF IBF(/FIR/) = ';' THEN CONT:=SUCC;

                  GNUM:  BEGIN
                         IF IBF(/FIR/) = '-' THEN FIR := FIR+1;
                         FOR FIR := FIR TO LAS DO
                           IF NOT ( (ORD(IBF(/FIR/)) - ORD('0'))
                                     IN (/0..9/) )
                              THEN CONT := NUMERR;

                         IF CONT=PROCESS
                            THEN IF SWITCH
                                   THEN IF VTYPE = ARY
                                      THEN CONT := $IVSCAN(INDX,F,NO)
                                      ELSE STATE := GTERM
                                    ELSE SWITCH := TRUE;
                         END;

                  GTERM: IF IBF(/FIR/) = ';' THEN CONT := SUCC
                            ELSE CONT := SYNERR;


                  END;  (* CASE STATE *)
               END;
            END;
         IF EOF(F) AND (CONT=PROCESS) THEN CONT := EOFERR;
         IF (INDX+2 > IBFLN) AND (CONT=PROCESS) THEN CONT := BUFERR;
         $IVSCAN := CONT;

         END;(*IVSCAN*)


      BEGIN     (* $PRNTVAR *)
#     WRITELN(OUTPUT) ;
#     WRITELN(OUTPUT, '0    ****  Variables for ''', PNAME: $IDLEN(PNAME),
                      ''' are:');
      WRITELN(OUTPUT) ;
      RESET(QRD);
      TPROCN := '            ';   VADDR := -1;

      WHILE ((PNAME <> TPROCN) OR (PID<>VADDR))   AND (NOT EOF(QRD)) DO
         BEGIN
         WHILE (QRD@ <> '%') AND (NOT EOF(QRD)) DO READLN(QRD);

         IF NOT EOF(QRD)
            THEN BEGIN
            GET(QRD);
            WHILE (QRD@=' ') DO GET(QRD);

            I := 1;  TPROCN := '            ';
            WHILE (QRD@ <> ' ') DO IF I <= PNAMLN
                  THEN BEGIN READ(QRD,TPROCN(/I/));  I:=I+1 END
                  ELSE GET(QRD);

            READ(QRD, VADDR);
            READLN(QRD);
            END;


         END;

      IF (PNAME = TPROCN) AND (PID = VADDR)

         THEN BEGIN    ERR := SUCC;
         WHILE (QRD@ = ' ') AND (NOT EOF(QRD)) DO GET(QRD);
         WHILE    (NOT EOF(QRD)) AND (QRD@<>'%') AND (ERR =SUCC)
               AND (QRD@ <> '#') DO
            BEGIN
            INDRCT := QRD@ = '@';
            IF INDRCT
               THEN BEGIN    GET(QRD);
               WHILE (QRD@=' ') AND (NOT EOF(QRD)) DO GET(QRD);
               END;

            IF (ORD(QRD@) - ORD('0')) IN (/0..9/)
               THEN BEGIN
               READ(QRD,VADDR);  MAXI := 0;
               ERR := $IVSCAN(MAXI,QRD,YES);

               IF ERR = SUCC
                  THEN BEGIN "FOR I := 0 TO INDENT6 DO WRITE(OUTPUT, ' ');"
#                 WRITE(OUTPUT, ' ':(INDENT6+1)+4) ;
                  I := 1;

                  IF INDRCT
                     THEN BEGIN
                     SB.I := STKP@.INT_VA(/VADDR DIV BYL_INT/);
                     VADDR := 0;
                     END

                     ELSE SB.P := STKP;

#                 alnfct := mal_char;   (* initialize alignment factor *)
#                 $PRNT(INDENT6+4,I,VADDR,SB.P,YES(*,YES*));
                  WHILE (NOT EOF(QRD)) AND (QRD@=' ') DO GET(QRD);
                  END;

               END

               ELSE BEGIN ERR := ADDERR; MAXI := 0; END;
            END;

         IF ERR <> SUCC THEN $ERRMSG(ERR, MAXI);
         END


         ELSE WRITELN(OUTPUT, ' procedure ',PNAME,' not found in symbol_table');
      END; (*PRNTVAR*)



   FUNCTION $FROMLIN(P:PROC_PTR; RTNADD:INTEGER):INTEGER;

      CONST      ESCAPE = 254; (* HEX 'FE' *)
         ESEND = 255;  (* HEX 'FF' *)

      VAR
         NADDR,LIN,I,INDX:INTEGER;
         TL:RECORD CASE INTEGER OF
            1:(I:INTEGER);
            2:(P:PROCTPTR)
           END;

      BEGIN
      I :=  ORD(P@.PROCLN(/0/))*256 + ORD(P@.PROCLN(/1/));

      IF (I = 0) OR (I > PROCSIZE)  THEN $FROMLIN := 0
         ELSE BEGIN
         NADDR := ORD(P);
         TL.I := ORD(P) + I;
         LIN := ORD(TL.P@.SLINNO(/0/))*256 + ORD(TL.P@.SLINNO(/1/)) -1;
         INDX := 1;

         WITH TL.P@ DO
            WHILE (RTNADD > NADDR) AND (ORD(LINARY(/INDX/)) <> ESEND) DO
               BEGIN
               I := ORD(LINARY(/INDX/));
               INDX := INDX+1;
               LIN := LIN+1;
               IF I <> ESCAPE THEN NADDR := NADDR + 2*I
                  ELSE BEGIN
                  NADDR := NADDR + ORD(LINARY(/INDX/))*512
                           + ORD(LINARY(/INDX+1/))*2;
                  INDX := INDX+2;
                  END;
            END;

         $FROMLIN := LIN;
         END;

      END;  (* $FROMLIN *)


   PROCEDURE $PRNTLNK(CURSTK,PRESTK:FRM_PTR; VAR PNAME:PROCNAME;
                      VAR PID,LNGTH:INTEGER);

      VAR
         TPROCN:PROCNAME;
         I,J:INTEGER;



      BEGIN
      $GETPNAME(CURSTK@.EPA, PID, LNGTH, PNAME);
      $GETPNAME(PRESTK@.EPA,J,I,TPROCN);
      WRITELN(OUTPUT);
      WRITE(OUTPUT, '0    ****  procedure ''',PNAME: $IDLEN(PNAME),
             ''' was called by --> ''',TPROCN: $IDLEN(TPROCN), '''');

      WRITELN(OUTPUT, '  from line: ',
            $FROMLIN(PRESTK@.EPA,(CURSTK@.RET MOD ADDRC)):1);

     END; (*PRNTLNK*)


   PROCEDURE $PRNTSYSD(P:ABNDP; S:PROC_PTR);

      VAR     I,J:INTEGER;  TPROCN:PROCNAME;    CODE: 0..21;

      BEGIN
      WITH P@ DO BEGIN
         WRITE(OUTPUT, '0    ****  Run Error: ',ABCODE:4);
         $GETPNAME(S,I,J,TPROCN);
         IF (0 < J) AND (J < PROCSIZE)
            THEN WRITE(OUTPUT, '  from line: ',$FROMLIN(S,ABADDR MOD ADDRC):1)
            ELSE WRITE(OUTPUT, ' at location: ',(ABADDR MOD ADDRC)-
                                        (ABREGS[10] MOD ADDRC):1);
         WRITELN(OUTPUT, '  of procedure: ''', TPROCN: $IDLEN(TPROCN), '''');

         WRITE(OUTPUT, '0    ****  ');
         IF ABCODE < 2000
            THEN BEGIN
            CODE := ABCODE-1000;
            CASE CODE OF
               1: WRITE(OUTPUT, 'INDEX VALUE ');
               2: WRITE(OUTPUT, 'SUBRANGE VALUE ');
               3: WRITE(OUTPUT, 'ACTUAL PARAMETER ');
               4: WRITE(OUTPUT, 'SET ELEMENT/MEMBER ');
               5: WRITE(OUTPUT, 'POINTER VALUE ');
               6: WRITELN(OUTPUT, 'STACK/HEAP COLLISION.');
               7: WRITELN(OUTPUT, 'ILLEGAL INPUT/RESET OPERATION.');
               8: WRITELN(OUTPUT, 'ILLEGAL OUTPUT/REWRITE OPERATION.');
#              9: WRITELN(OUTPUT, 'SYN. I/O ERROR: ', ABEMSG@:ABMSGL) ;
              10: WRITELN(OUTPUT, 'PROGRAM RUNNING OUT OF TIME.');
              11: WRITELN(OUTPUT, 'FILE DEFINITION ERROR.');
              12: WRITELN(OUTPUT, 'PROGRAM OUT OF STACK SPACE.');
              13: WRITELN(OUTPUT, 'CALL TO UNDEFINED STNDARD PROC.');
              14: WRITELN(OUTPUT, 'FILE LINE-LIMIT EXCEEDED.');
              20: WRITELN(OUTPUT, 'ATTEMPT TO READ PAST END OF FILE.');
              21: WRITELN(OUTPUT, 'ATTEMPT TO READ BAD BOOLEAN.');
              22: WRITELN(OUTPUT, 'ATTEMPT TO READ BAD INTEGER.');
              23: WRITELN(OUTPUT, 'ATTEMPT TO READ REAL.');
              END;

            IF CODE <= 5
               THEN BEGIN
               WRITELN(OUTPUT, 'OUT OF RANGE.');
               WRITELN(OUTPUT, '0    ****  The offending value:  ',ABCVAL:1,
                 '  is not in the range:  ', ABLOWR:1, '..', ABUPPR:1) ;
               END;
            END

            ELSE IF ABCODE < 3000
               THEN BEGIN
               CODE := ABCODE-2000;
               CASE CODE OF
                  0: WRITE(OUTPUT, 'IMPRECISE');
                  1: WRITE(OUTPUT, 'OPERATION');
                  2: WRITE(OUTPUT, 'PRIVILEGED OPERATION');
                  3: WRITE(OUTPUT, 'EXECUTE');
                  4: WRITE(OUTPUT, 'PROTECTION');
                  5: WRITE(OUTPUT, 'ADDRESSING');
                  8: WRITE(OUTPUT, 'FIXED-POINT OVERFLOW');
                  9: WRITE(OUTPUT, 'FIXED-POINT DIVIDE');
                 12: WRITE(OUTPUT, 'EXPONENT OVERFLOW');
                 13: WRITE(OUTPUT, 'EXPONENT UNDERFLOW');
                 14: WRITE(OUTPUT, 'SIGNIFICANCE');
                 15: WRITE(OUTPUT, 'FLOATING-POINT DIVIDE');
                 END;

               WRITELN(OUTPUT, ' EXCEPTION.');
               END

            ELSE IF ABCODE = 3001
#              THEN WRITELN(OUTPUT, 'EXTERNAL ERROR: ', ABEMSG@:ABMSGL);

         END;   (* WITH P@ DO *)
      END;  (* $PRNTSYSD *)


   BEGIN   (* SNAPSHOT *)
  "PAR2.I := DMPKIND MOD 10;"
   I := ORD('A');
   VARTARY(/0/) := ARY;       VARTARY(/ORD('B')-I/) := BOL;
   VARTARY(/ORD('C')-I/) := CHA;
   VARTARY(/ORD('D')-I/) := REC;
   VARTARY(/ORD('I')-I/) := INT;
   VARTARY(/ORD('R')-I/) := RL;
   VARTARY(/ORD('P')-I/) := PTR;
   VARTARY(/ORD('S')-I/) := PSET;
   VARTARY(/ORD('L')-I/) := SCL;
   VARTARY(/ORD('F')-I/) := FIL;

   TOPSTK := DMARRAY(/-16/);  (* SAVED R1 IN RUNTIME STACK *)
   BOTSTK := DMARRAY(/-5/);   (* SAVED R12 IN RUNTIME STACK *)
   TMPSTK := TOPSTK@.BAK_LNK;
   REWRITE(OUTPUT) ;    (* IN CASE "OUTPUT" IS NOT OPEN *)
   WRITELN(OUTPUT, '     ****  SNAPSHOT DUMP OF PROGRAM  ****');
   WRITELN(OUTPUT) ;
   WRITE(OUTPUT, '0    ****  ''SNAPSHOT'' was called by --> ');

   IF LEVEL < 0
      THEN BEGIN
      WRITELN(OUTPUT, '''PASCAL_MONITOR''');
      PAR2.I := DMPKIND ;
      ARRYLSTF := FALSE;
      $PRNTSYSD(PAR2.P,TMPSTK@.EPA);
      LEVEL := -1;  PAR2.C := FULL;
      END

   ELSE BEGIN
      PAR2.I := DMPKIND MOD 10 ;
      ARRYLSTF := ((DMPKIND DIV 10) MOD 10) = 1;
      $GETPNAME(TMPSTK@.EPA,UID,PLEN,CPROCN);
      WRITELN(OUTPUT, '''', CPROCN: $IDLEN(CPROCN), '''  from line: ',
            $FROMLIN(TMPSTK@.EPA,TOPSTK@.RET MOD ADDRC):1);
      END;


   DEPTH := 1;
   IF LEVEL = -1 THEN MAXDEPTH := DEFLEV
      ELSE IF LEVEL = 0  THEN MAXDEPTH := ADDRC
              ELSE MAXDEPTH := LEVEL;


#  IF (PAR2.C = FULL) OR (PAR2.C =STKTRACE) THEN
#
#     REPEAT
#     DONE :=  (TMPSTK = BOTSTK) OR (DEPTH > MAXDEPTH) ;
#
#     IF PAR2.C = FULL THEN    (* DUMP VARIABLES AS WELL AS CALL TRACE *)
#        BEGIN
#        $GETPNAME(TMPSTK@.EPA, UID, PLEN, CPROCN);
#        IF (PLEN > 0) AND (PLEN < PROCSIZE)
#           THEN  $PRNTVAR(TMPSTK, CPROCN, UID);
#        END ; (* FULL DUMP *)
#
#     IF TMPSTK <> BOTSTK THEN
#        $PRNTLNK(TMPSTK, TMPSTK@.BAK_LNK, CPROCN, UID, PLEN);
#     TMPSTK := TMPSTK@.BAK_LNK;
#     DEPTH := DEPTH+1;
#     UNTIL DONE ;
#
#  WRITELN(OUTPUT, '0    ****  END OF DUMP  ****') ;
   PAGE(OUTPUT);

   END; (* SNAPSHOT *)

BEGIN
END.
