(*$M-,D-*)
(* PASCALS.P                                                                  *)
PROGRAM PASCALS(INPUT,OUTPUT);     (*1.7.75*)
(*$M-, T-,P-  N.WIRTH,  E.T.H.
      CLAUSIUSSTR.55   CH-8006 ZURICH    *)
(* #INCLUDE "GLOBALS.I"                                                       *)
(* GLOBALS.I                                                                  *)
LABEL 99;
CONST NKW = 27;     (*NO. OF KEY WORDS*)
## TABCH = ' ' "TAB";
    ALNG =  10;     (*NO. OF SIGNIFICANT CHARS IN IDENTIFIERS*)
    LLNG = 120;     (*INPUT LINE LENGTH*)
    EMAX = 322;     (*MAX EXPONENT OF REAL NUMBERS*)
    EMIN =-292;     (*MIN EXPONENT*)
    KMAX =  15;     (*MAX NO. OF SIGNIFICANT DIGITS*)
    TMAX = 100;     (*SIZE OF TABLE*)
    BMAX =  20;     (*SIZE OF BLOCK-TABLE*)
    AMAX =  30;     (*SIZE OF ARRAY-TABLE*)
    C2MAX = 20;     (*SIZE OF REAL CONSTANT TABLE*)
    CSMAX = 30;     (*MAX NO. OF CASES*)
    CMAX = 500;     (*SIZE OF CODE*)
    LMAX =   7;     (*MAXIMUM LEVEL*)
    SMAX = 400;     (*SIZE OF STRING-TABLE*)
    ERMAX = 58;     (*MAX ERROR NO.*)
    OMAX =  66;     (*HIGHEST ORDER CODE*)
(* { SHIT, LOOK AT THIS NUMBER×××××                                           *)
(*  NMAX = 281474976710655;     2**48-1*)
(* }                                                                          *)
##  NMAX = "MAXINT" 32767;
    XMAX = NMAX;    (*131071 = 2**17 - 1*)
    LINELENG = 136; (*OUTPUT LINE LENGTH*)
    LINELIMIT = 200;
    STACKSIZE = 200;

TYPE SYMBOL = (INTCON,REALCON,CHARCON,STRING,
          NOTSY,PLUS,MINUS,TIMES,IDIV,RDIV,IMOD,ANDSY,ORSY,
          EQL,NEQ,GTR,GEQ,LSS,LEQ,
          LPARENT,RPARENT,LBRACK,RBRACK,COMMA,SEMICOLON,PERIOD,
          COLON,BECOMES,CONSTSY,TYPESY,VARSY,FUNCSY,
          PROCSY,ARRAYSY,RECORDSY,PROGRAMSY,IDENT,
          BEGINSY,IFSY,CASESY,REPEATSY,WHILESY,FORSY,
          ENDSY,ELSESY,UNTILSY,OFSY,DOSY,TOSY,DOWNTOSY,THENSY);

#   INDEX  = 0(*-XMAX*) .. +XMAX;
    ALFA = PACKED ARRAY [1..ALNG] OF CHAR;
    OBJECT = (KONSTANT,VARIABLE,TYPE1,PROZEDURE,FUNKTION);
    TYPES  = (NOTYP,INTS,REALS,BOOLS,CHARS,ARRAYS,RECORDS);
    SYMSET = SET OF SYMBOL;
    TYPSET = SET OF TYPES;
    ITEM   = RECORD
          TYP: TYPES; REF: INDEX;
        END ;
    ORDER  = PACKED RECORD
          F: -OMAX..+OMAX;
          X: -LMAX..+LMAX;
#         Y: 0(*-NMAX*)..+NMAX;
        END ;

VAR SY: SYMBOL;          (*LAST SYMBOL READ BY INSYMBOL*)
    ID: ALFA;            (*IDENTIFIER FROM INSYMBOL*)
    INUM: INTEGER;       (*INTEGER FROM INSYMBOL*)
    RNUM: REAL;          (*REAL NUMBER FROM INSYMBOL*)
    SLENG: INTEGER;      (*STRING LENGTH*)
    CH: CHAR;            (*LAST CHARACTER READ FROM SOURCE PROGRAM*)
    LINE: ARRAY [1..LLNG] OF CHAR;
    CC: INTEGER;         (*CHARACTER COUNTER*)
    LC: INTEGER;         (*PROGRAM LOCATION COUNTER*)
    LL: INTEGER;         (*LENGTH OF CURRENT LINE*)
    ERRS: SET OF 0..ERMAX;
    ERRPOS: INTEGER;
    PROGNAME: ALFA;
    IFLAG, OFLAG: BOOLEAN;
    CONSTBEGSYS,TYPEBEGSYS,BLOCKBEGSYS,FACBEGSYS,STATBEGSYS: SYMSET;
    KEY: ARRAY [1..NKW] OF ALFA;
    KSY: ARRAY [1..NKW] OF SYMBOL;
    SPS: ARRAY [CHAR] OF SYMBOL;  (*SPECIAL SYMBOLS*)

    T,A,B,SX,C1,C2: INTEGER;  (*INDICES TO TABLES*)
    STANTYPS: TYPSET;
    DISPLAY: ARRAY [0 .. LMAX] OF INTEGER;

    TAB:     ARRAY [0 .. TMAX] OF     (*IDENTIFIER TABLE*)
          PACKED RECORD
       NAME: ALFA;  LINK: INDEX;
       OBJ: OBJECT; TYP: TYPES;
       REF: INDEX;  NORMAL: BOOLEAN;
       LEV: 0 .. LMAX; ADR: INTEGER;
          END ;
    ATAB:    ARRAY [1 .. AMAX] OF     (*ARRAY-TABLE*)
          PACKED RECORD
       INXTYP, ELTYP: TYPES;
       ELREF, LOW, HIGH, ELSIZE, SIZE: INDEX;
          END ;
    BTAB:    ARRAY [1 .. BMAX] OF    (*BLOCK-TABLE*)
          PACKED RECORD
        LAST, LASTPAR, PSIZE, VSIZE: INDEX
          END ;
    STAB:    PACKED ARRAY [0..SMAX] OF CHAR;  (*STRING TABLE*)
    RCONST:  ARRAY [1 .. C2MAX] OF REAL;
    CODE:    ARRAY [0 .. CMAX] OF ORDER;
(* EOF                                                                        *)
(* #INCLUDE "ERROR.I"                                                         *)
(* ERROR.I                                                                    *)

PROCEDURE ERRORMSG;
   VAR K: INTEGER;
       MSG: ARRAY [0..ERMAX] OF ALFA;
BEGIN
  MSG[ 0] := 'UNDEF ID  '; MSG[ 1] := 'MULTI DEF ';
  MSG[ 2] := 'IDENTIFIER'; MSG[ 3] := 'PROGRAM   ';
  MSG[ 4] := ')         '; MSG[ 5] := ':         ';
  MSG[ 6] := 'SYNTAX    '; MSG[ 7] := 'IDENT, VAR';
  MSG[ 8] := 'OF        '; MSG[ 9] := '(         ';
  MSG[10] := 'ID, ARRAY '; MSG[11] := '[         ';
  MSG[12] := ']         '; MSG[13] := '..        ';
  MSG[14] := ';         '; MSG[15] := 'FUNC. TYPE';
  MSG[16] := '=         '; MSG[17] := 'BOOLEAN   ';
  MSG[18] := 'CONVAR TYP'; MSG[19] := 'TYPE      ';
  MSG[20] := 'PROG.PARAM'; MSG[21] := 'TOO BIG   ';
  MSG[22] := '.         '; MSG[23] := 'TYP (CASE)';
  MSG[24] := 'CHARACTER '; MSG[25] := 'CONST ID  ';
  MSG[26] := 'INDEX TYPE'; MSG[27] := 'INDEXBOUND';
  MSG[28] := 'NO ARRAY  '; MSG[29] := 'TYPE ID   ';
  MSG[30] := 'UNDEF TYPE'; MSG[31] := 'NO RECORD ';
  MSG[32] := 'BOOLE TYPE'; MSG[33] := 'ARITH TYPE';
  MSG[34] := 'INTEGER   '; MSG[35] := 'TYPES     ';
  MSG[36] := 'PARAM TYPE'; MSG[37] := 'VARIAB ID ';
  MSG[38] := 'STRING    '; MSG[39] := 'NO.OF PARS';
  MSG[40] := 'TYPE      '; MSG[41] := 'TYPE      ';
  MSG[42] := 'REAL TYPE '; MSG[43] := 'INTEGER   ';
  MSG[44] := 'VAR, CONST'; MSG[45] := 'VAR, PROC ';
  MSG[46] := 'TYPES (:=)'; MSG[47] := 'TYP (CASE)';
  MSG[48] := 'TYPE      '; MSG[49] := 'STORE OVFL';
  MSG[50] := 'CONSTANT  '; MSG[51] := ':=        ';
  MSG[52] := 'THEN      '; MSG[53] := 'UNTIL     ';
  MSG[54] := 'DO        '; MSG[55] := 'TO DOWNTO ';
  MSG[56] := 'BEGIN     '; MSG[57] := 'END       ';
  MSG[58] := 'FACTOR    ';
  K := 0; WRITELN; WRITELN(' KEY WORDS');
  WHILE ERRS <> [] DO
  BEGIN WHILE NOT (K IN ERRS) DO K := K+1;
        WRITELN(K,'  ',MSG[K]); ERRS := ERRS - [K]
  END
END (*ERRORMSG*) ;


PROCEDURE ERROR(N: INTEGER);
BEGIN IF ERRPOS = 0 THEN WRITE(' ****');
   IF CC > ERRPOS THEN
      BEGIN WRITE(' ': CC-ERRPOS, '@', N:2);
         ERRPOS := CC+3; ERRS := ERRS + [N]
      END
END (*ERROR*) ;

PROCEDURE FATAL(N: INTEGER);
   VAR MSG: ARRAY [1..7] OF ALFA;
BEGIN WRITELN; ERRORMSG;
   MSG[ 1] := 'IDENTIFIER'; MSG[ 2] := 'PROCEDURES';
   MSG[ 3] := 'REALS     '; MSG[ 4] := 'ARRAYS    ';
   MSG[ 5] := 'LEVELS    '; MSG[ 6] := 'CODE      ';
   MSG[ 7] := 'STRINGS   ';
   WRITELN(' COMPILER TABLE FOR ', MSG[N], ' IS TOO SMALL');
   (*GOTO 99*)  EXIT(99)    (* TERMINATE COMPILATION*)
END (*FATAL*) ;

(* EOF                                                                        *)
(* #INCLUDE "SCANNER.I"                                                       *)
(* SCANNER.I                                                                  *)
PROCEDURE NEXTCH;   (*READ NEXT CHARACTER; PROCESS LINE END*)
BEGIN IF CC = LL THEN
      BEGIN IF EOF(INPUT) THEN
            BEGIN WRITELN;
               WRITELN(' PROGRAM INCOMPLETE');
               ERRORMSG; (*GOTO 99*)  EXIT(99)
            END ;
         IF ERRPOS <> 0 THEN
            BEGIN WRITELN; ERRPOS := 0
            END ;
         WRITE(LC:6, '  ');
         LL := 0; CC := 0;
         WHILE NOT EOLN(INPUT) DO
            BEGIN LL := LL+1; READ(CH); WRITE(CH); LINE[LL] := CH
            END ;
         WRITELN; LL := LL+1; READ(LINE[LL])
      END ;
   CC := CC+1; CH := LINE[CC];
END (*NEXTCH*) ;

PROCEDURE INSYMBOL;           (*READS NEXT SYMBOL*)
   LABEL 1,2,3;
   VAR I,J,K,E: INTEGER;

   PROCEDURE READSCALE;
      VAR S, SIGN: INTEGER;
   BEGIN NEXTCH; SIGN := 1; S := 0;
      IF CH = '+' THEN NEXTCH ELSE
      IF CH = '-' THEN BEGIN NEXTCH; SIGN := -1 END ;
      WHILE CH IN ['0'..'9'] DO
         BEGIN S := 10*S + ORD(CH) - ORD('0'); NEXTCH
         END ;
      E := S*SIGN + E
   END (*READSCALE*) ;

   PROCEDURE ADJUSTSCALE;
      VAR S: INTEGER; D,T: REAL;
   BEGIN IF K+E > EMAX THEN ERROR(21) ELSE
         IF K+E < EMIN THEN RNUM := 0 ELSE
     BEGIN S := ABS(E); T := 1.0; D := 10.0;
       REPEAT
         WHILE NOT ODD(S) DO
#           BEGIN S := S DIV 2; D := D*D (*SQR(D)*)
            END ;
         S := S-1; T := D*T
       UNTIL S = 0;
       IF E >= 0 THEN RNUM := RNUM*T ELSE RNUM := RNUM/T
     END
   END (*ADJUSTSCALE*) ;

BEGIN (*INSYMBOL*)
1: WHILE (CH = ' ') OR (CH = TABCH) DO NEXTCH;
   IF CH IN ['A'..'Z'] THEN
   BEGIN (*IDENTIFIER OR WORDSYMBOL*)  K := 0; ID := '          ';
      REPEAT IF K < ALNG THEN
             BEGIN K := K+1; ID[K] := CH
             END ;
         NEXTCH
      UNTIL NOT (CH IN ['A'..'Z','0'..'9']);
      I := 1; J := NKW;   (*BINARY SEARCH*)
      REPEAT K := (I+J) DIV 2;
         IF ID <= KEY[K] THEN J := K-1;
         IF ID >= KEY[K] THEN I := K+1
      UNTIL I > J;
      IF I-1 > J THEN SY := KSY[K] ELSE SY := IDENT
   END ELSE
   IF CH IN ['0'..'9'] THEN
   BEGIN (*NUMBER*) K := 0; INUM := 0; SY := INTCON;
      REPEAT INUM := INUM*10 + ORD(CH) - ORD('0');
         K := K+1; NEXTCH
      UNTIL NOT (CH IN ['0'..'9']);
      IF (K > KMAX) OR (INUM > NMAX) THEN
        BEGIN ERROR(21); INUM := 0; K := 0
        END ;
      IF CH = '.' THEN
      BEGIN NEXTCH;
         IF CH = '.' THEN CH := ':' ELSE
            BEGIN SY := REALCON; RNUM := INUM; E := 0;
               WHILE CH IN ['0'..'9'] DO
               BEGIN E := E-1;
                  RNUM := 10.0*RNUM + (ORD(CH)-ORD('0')); NEXTCH
               END ;
               IF CH = 'E' THEN READSCALE;
               IF E <> 0 THEN ADJUSTSCALE
            END
      END ELSE
      IF CH = 'E' THEN
      BEGIN SY := REALCON; RNUM := INUM; E := 0;
         READSCALE; IF E <> 0 THEN ADJUSTSCALE
      END ;
   END ELSE
   CASE CH OF
':' : BEGIN NEXTCH;
          IF CH = '=' THEN
            BEGIN SY := BECOMES; NEXTCH
            END  ELSE SY := COLON
      END ;
'<' : BEGIN NEXTCH;
         IF CH = '=' THEN BEGIN SY := LEQ; NEXTCH END ELSE
         IF CH = '>' THEN BEGIN SY := NEQ; NEXTCH END ELSE SY := LSS
      END ;
'>' : BEGIN NEXTCH;
         IF CH = '=' THEN BEGIN SY := GEQ; NEXTCH END ELSE SY := GTR
      END ;
'.' : BEGIN NEXTCH;
         IF CH = '.' THEN
            BEGIN SY := COLON; NEXTCH
            END  ELSE SY := PERIOD
      END ;
'''': BEGIN K := 0;
    2:  NEXTCH;
        IF CH = '''' THEN
          BEGIN NEXTCH; IF CH <> '''' THEN GOTO 3
          END ;
        IF SX+K = SMAX THEN FATAL(7);
        STAB[SX+K] := CH; K := K+1;
        IF CC = 1 THEN
          BEGIN (*END OF LINE*) K := 0;
          END
        ELSE GOTO 2;
    3:  IF K = 1 THEN
           BEGIN SY := CHARCON; INUM := ORD(STAB[SX])
           END ELSE
        IF K = 0 THEN
           BEGIN ERROR(38); SY := CHARCON; INUM := 0
           END ELSE
           BEGIN SY := STRING; INUM := SX; SLENG := K; SX := SX+K
           END
      END ;
'(' : BEGIN NEXTCH;
         IF CH <> '*' THEN SY := LPARENT ELSE
         BEGIN (*COMMENT*) NEXTCH;
            REPEAT
               WHILE CH <> '*' DO NEXTCH;
               NEXTCH
            UNTIL CH = ')';
            NEXTCH; GOTO 1
         END
      END ;
'+', '-', '*', '/', ')', '=', ',', '[', ']', '#', '&', ';' :
      BEGIN SY := SPS[CH]; NEXTCH
      END ;
'$', (*'\',*) '×', (*'?',*) '@', '_', '"', '^' :
      BEGIN ERROR(24); NEXTCH; GOTO 1
      END
   END;
END (*INSYMBOL*) ;

(* EOF                                                                        *)
(* #INCLUDE "TABLES.I"                                                        *)
(* TABLES.I                                                                   *)
PROCEDURE ENTER(X0: ALFA; X1: OBJECT;
                X2: TYPES; X3: INTEGER);
BEGIN T := T+1;   (*ENTER STANDARD IDENTIFIER*)
   WITH TAB[T] DO
   BEGIN NAME := X0; LINK := T-1; OBJ := X1;
      TYP := X2; REF := 0; NORMAL := TRUE;
      LEV := 0; ADR := X3
   END
END (*ENTER*) ;

PROCEDURE ENTERARRAY(TP: TYPES; L,H: INTEGER);
BEGIN IF L > H THEN ERROR(27);
   IF (ABS(L)>XMAX) OR (ABS(H)>XMAX) THEN
      BEGIN ERROR(27); L := 0; H := 0;
      END ;
   IF A = AMAX THEN FATAL(4) ELSE
      BEGIN A := A+1;
        WITH ATAB[A] DO
            BEGIN INXTYP := TP; LOW := L; HIGH := H
            END
      END
END (*ENTERARRAY*) ;

PROCEDURE ENTERBLOCK;
BEGIN IF B = BMAX THEN FATAL(2) ELSE
      BEGIN B := B+1; BTAB[B].LAST := 0; BTAB[B].LASTPAR := 0
      END
END (*ENTERBLOCK*) ;

PROCEDURE ENTERREAL(X: REAL);
BEGIN IF C2 = C2MAX-1 THEN FATAL(3) ELSE
      BEGIN RCONST[C2+1] := X; C1 := 1;
         WHILE RCONST[C1] <> X DO  C1 := C1+1;
         IF C1 > C2 THEN C2 := C1
      END
END (*ENTERREAL*) ;

PROCEDURE EMIT(FCT: INTEGER);
BEGIN IF LC = CMAX THEN FATAL(6);
   CODE[LC].F := FCT; LC := LC+1
END (*EMIT*) ;

PROCEDURE EMIT1(FCT,B: INTEGER);
BEGIN IF LC = CMAX THEN FATAL(6);
   WITH CODE[LC] DO
      BEGIN F := FCT; Y := B END ;
   LC := LC+1
END (*EMIT1*) ;

PROCEDURE EMIT2(FCT,A,B: INTEGER);
BEGIN IF LC = CMAX THEN FATAL(6);
   WITH CODE[LC] DO
     BEGIN F := FCT; X := A; Y := B END ;
   LC := LC+1
END (*EMIT2*) ;

PROCEDURE PRINTTABLES;
   VAR I: INTEGER; O: ORDER;
BEGIN
   WRITELN('0IDENTIFIERS          LINK', '  OBJ  TYP  REF  NRM  LEV  ADR');
   FOR I := BTAB[1].LAST +1 TO T DO
      WITH TAB[I] DO
      WRITELN(I,' ',NAME,LINK:5, ORD(OBJ):5, ORD(TYP):5, REF:5,
            ORD(NORMAL):5, LEV:5, ADR:5);
   WRITELN('0BLOCKS    LAST LPAR PSZE VSZE');
   FOR I := 1 TO B DO
      WITH BTAB[I] DO
      WRITELN(I, LAST:5, LASTPAR:5, PSIZE:5, VSIZE:5);
   WRITELN('0ARRAYS    XTYP ETYP EREF  LOW HIGH ELSZ SIZE');
   FOR I := 1 TO A DO
      WITH ATAB[I] DO
      WRITELN(I, ORD(INXTYP):5, ORD(ELTYP):5,
              ELREF:5, LOW:5, HIGH:5, ELSIZE:5, SIZE:5);
   WRITELN('0CODE:');
   FOR I := 0 TO LC-1 DO
   BEGIN IF I MOD 5 = 0 THEN
         BEGIN WRITELN; WRITE(I:5)
         END ;
      O := CODE[I]; WRITE(O.F:5);
      IF O.F < 31 THEN
        IF O.F < 4 THEN WRITE(O.X:2, O.Y:5)
                    ELSE WRITE(O.Y:7)
      ELSE WRITE('       ');
      WRITE(',')
   END ;
   WRITELN;
END (*PRINTTABLES*) ;

(* EOF                                                                        *)
(* #INCLUDE "BLOCK.I"                                                         *)
(* BLOCK.I                                                                    *)
PROCEDURE BLOCK(FSYS: SYMSET; ISFUN: BOOLEAN; LEVEL: INTEGER);

   TYPE CONREC =
      RECORD CASE TP: TYPES OF
         INTS,CHARS,BOOLS: (I: INTEGER);
         REALS: (R: REAL)
      END ;

   VAR DX: INTEGER;    (*DATA ALLOCATION INDEX*)
       PRT: INTEGER;   (*T-INDEX OF THIS PROCEDURE*)
       PRB: INTEGER;   (*B-INDEX OF THIS PROCEDURE*)
       X: INTEGER;
   PROCEDURE SKIP(FSYS: SYMSET; N: INTEGER);
   BEGIN ERROR(N);
      WHILE NOT (SY IN FSYS) DO INSYMBOL
   END (*SKIP*) ;

   PROCEDURE TEST(S1,S2: SYMSET; N: INTEGER);
   BEGIN IF NOT (SY IN S1) THEN
         SKIP(S1+S2,N)
   END (*TEST*) ;

   PROCEDURE TESTSEMICOLON;
   BEGIN
     IF SY = SEMICOLON THEN INSYMBOL ELSE
     BEGIN ERROR(14);
       IF SY IN [COMMA,COLON] THEN INSYMBOL
     END ;
     TEST([IDENT]+BLOCKBEGSYS, FSYS, 6)
   END (*TESTSEMICOLON*) ;

   PROCEDURE ENTER(ID: ALFA; K: OBJECT);
      VAR J,L: INTEGER;
   BEGIN IF T = TMAX THEN FATAL(1) ELSE
         BEGIN TAB[0].NAME := ID;
            J := BTAB[DISPLAY[LEVEL]].LAST;  L := J;
            WHILE TAB[J].NAME <> ID DO  J := TAB[J].LINK;
            IF J <> 0 THEN ERROR(1) ELSE
            BEGIN T := T+1;
              WITH TAB[T] DO
              BEGIN NAME := ID; LINK := L;
               OBJ := K; TYP := NOTYP; REF := 0; LEV := LEVEL; ADR := 0
              END ;
              BTAB[DISPLAY[LEVEL]].LAST := T
            END
         END
   END (*ENTER*) ;

   FUNCTION LOC(ID: ALFA): INTEGER;
      VAR I,J: INTEGER;     (*LOCATE ID IN TABLE*)
   BEGIN I := LEVEL; TAB[0].NAME := ID;   (*SENTINEL*)
      REPEAT J := BTAB[DISPLAY[I]].LAST;
         WHILE TAB[J].NAME <> ID DO  J := TAB[J].LINK;
         I := I-1;
      UNTIL (I<0) OR (J<>0);
      IF J = 0 THEN ERROR(0);  LOC := J
   END (*LOC*) ;

  PROCEDURE ENTERVARIABLE;
  BEGIN IF SY = IDENT THEN
          BEGIN ENTER(ID,VARIABLE); INSYMBOL
          END
        ELSE ERROR(2)
  END (*ENTERVARIABLE*) ;

   PROCEDURE CONSTANT(FSYS: SYMSET; VAR C: CONREC);
     VAR X, SIGN: INTEGER;
   BEGIN C.TP := NOTYP; C.I := 0;
     TEST(CONSTBEGSYS, FSYS, 50);
     IF SY IN CONSTBEGSYS THEN
     BEGIN
         IF SY = CHARCON THEN
           BEGIN C.TP := CHARS; C.I := INUM; INSYMBOL
           END
         ELSE
           BEGIN SIGN := 1;
             IF SY IN [PLUS,MINUS] THEN
               BEGIN IF SY = MINUS THEN SIGN := -1;
                 INSYMBOL
               END ;
             IF SY = IDENT THEN
               BEGIN X := LOC(ID);
                 IF X <> 0 THEN
                   IF TAB[X].OBJ <> KONSTANT THEN ERROR(25) ELSE
                   BEGIN C.TP := TAB[X].TYP;
                     IF C.TP = REALS THEN C.R := SIGN*RCONST[TAB[X].ADR]
                                     ELSE C.I := SIGN*TAB[X].ADR
                   END ;
                 INSYMBOL
               END
             ELSE
             IF SY = INTCON THEN
               BEGIN C.TP := INTS; C.I := SIGN*INUM; INSYMBOL
               END ELSE
             IF SY = REALCON THEN
               BEGIN C.TP := REALS; C.R := SIGN*RNUM; INSYMBOL
               END ELSE SKIP(FSYS,50)
           END;
         TEST(FSYS, [], 6)
       END
   END (*CONSTANT*) ;
   PROCEDURE TYP(FSYS: SYMSET; VAR TP: TYPES; VAR RF, SZ: INTEGER);
     VAR X: INTEGER;
         ELTP: TYPES; ELRF: INTEGER;
         ELSZ, OFFSET, T0,T1: INTEGER;

     PROCEDURE ARRAYTYP(VAR AREF,ARSZ: INTEGER);
        VAR ELTP: TYPES;
           LOW, HIGH: CONREC;
           ELRF, ELSZ: INTEGER;
     BEGIN CONSTANT([COLON,RBRACK,RPARENT,OFSY]+FSYS, LOW);
        IF LOW.TP = REALS THEN
           BEGIN ERROR(27); LOW.TP := INTS; LOW.I := 0
           END ;
        IF SY = COLON THEN INSYMBOL ELSE ERROR(13);
        CONSTANT([RBRACK,COMMA,RPARENT,OFSY]+FSYS, HIGH);
        IF HIGH.TP <> LOW.TP THEN
           BEGIN ERROR(27); HIGH.I := LOW.I
           END ;
        ENTERARRAY(LOW.TP, LOW.I, HIGH.I); AREF := A;
        IF SY = COMMA THEN
           BEGIN INSYMBOL; ELTP := ARRAYS; ARRAYTYP(ELRF,ELSZ)
           END ELSE
        BEGIN
           IF SY = RBRACK THEN INSYMBOL ELSE
              BEGIN ERROR(12);
                 IF SY = RPARENT THEN INSYMBOL
              END ;
           IF SY = OFSY THEN INSYMBOL ELSE ERROR(8);
           TYP(FSYS,ELTP,ELRF,ELSZ)
        END ;
        WITH ATAB[AREF] DO
        BEGIN ARSZ := (HIGH-LOW+1)*ELSZ; SIZE := ARSZ;
           ELTYP := ELTP; ELREF := ELRF; ELSIZE := ELSZ
        END ;
     END (*ARRAYTYP*) ;

   BEGIN (*TYP*) TP := NOTYP; RF := 0; SZ := 0;
     TEST(TYPEBEGSYS, FSYS, 10);
     IF SY IN TYPEBEGSYS THEN
       BEGIN
         IF SY = IDENT THEN
         BEGIN X := LOC(ID);
           IF X <> 0 THEN
           WITH TAB[X] DO
             IF OBJ <> TYPE1 THEN ERROR(29) ELSE
             BEGIN TP := TYP; RF := REF; SZ := ADR;
               IF TP = NOTYP THEN ERROR(30)
             END ;
           INSYMBOL
         END ELSE
         IF SY = ARRAYSY THEN
         BEGIN INSYMBOL;
             IF SY = LBRACK THEN INSYMBOL ELSE
                BEGIN ERROR(11);
                   IF SY = LPARENT THEN INSYMBOL
                END ;
             TP := ARRAYS; ARRAYTYP(RF,SZ)
         END ELSE
         BEGIN (*RECORDS*) INSYMBOL;
           ENTERBLOCK; TP := RECORDS; RF := B;
           IF LEVEL = LMAX THEN FATAL(5);
           LEVEL := LEVEL+1; DISPLAY[LEVEL] := B; OFFSET := 0;
           WHILE SY <> ENDSY DO
           BEGIN (*FIELD SECTION*)
             IF SY = IDENT THEN
             BEGIN T0 := T; ENTERVARIABLE;
               WHILE SY = COMMA DO
                 BEGIN INSYMBOL; ENTERVARIABLE
                 END ;
               IF SY = COLON THEN INSYMBOL ELSE ERROR(5);
               T1 := T;
               TYP(FSYS+[SEMICOLON,ENDSY,COMMA,IDENT],ELTP,ELRF,ELSZ);
               WHILE T0 < T1 DO
               BEGIN T0 := T0+1;
                 WITH TAB[T0] DO
                 BEGIN TYP := ELTP; REF := ELRF; NORMAL := TRUE;
                   ADR := OFFSET; OFFSET := OFFSET + ELSZ
                 END
               END
             END ;
             IF SY <> ENDSY THEN
             BEGIN IF SY = SEMICOLON THEN INSYMBOL ELSE
                   BEGIN ERROR(14);
                     IF SY = COMMA THEN INSYMBOL
                   END ;
                TEST([IDENT,ENDSY,SEMICOLON], FSYS, 6)
             END
           END ;
           BTAB[RF].VSIZE := OFFSET; SZ := OFFSET; BTAB[RF].PSIZE := 0;
           INSYMBOL; LEVEL := LEVEL-1
         END ;
         TEST(FSYS, [], 6)
       END
   END (*TYP*) ;

   PROCEDURE PARAMETERLIST;     (*FORMAL PARAMETER LIST*)
      VAR TP: TYPES;
          RF, SZ, X, T0: INTEGER;
          VALPAR: BOOLEAN;
   BEGIN INSYMBOL; TP := NOTYP; RF := 0; SZ := 0;
     TEST([IDENT, VARSY], FSYS+[RPARENT], 7);
     WHILE SY IN [IDENT,VARSY] DO
       BEGIN IF SY <> VARSY THEN VALPAR := TRUE ELSE
               BEGIN INSYMBOL; VALPAR := FALSE
               END ;
         T0 := T; ENTERVARIABLE;
         WHILE SY = COMMA DO
            BEGIN INSYMBOL; ENTERVARIABLE;
            END ;
         IF SY = COLON THEN
           BEGIN INSYMBOL;
             IF SY <> IDENT THEN ERROR(2) ELSE
             BEGIN X := LOC(ID); INSYMBOL;
               IF X <> 0 THEN
               WITH TAB[X] DO
                 IF OBJ <> TYPE1 THEN ERROR(29) ELSE
                   BEGIN TP := TYP; RF := REF;
                     IF VALPAR THEN SZ := ADR ELSE SZ := 1
                   END ;
             END ;
             TEST([SEMICOLON,RPARENT], [COMMA,IDENT]+FSYS, 14)
           END
         ELSE ERROR(5);
         WHILE T0 < T DO
         BEGIN T0 := T0+1;
           WITH TAB[T0] DO
           BEGIN TYP := TP; REF := RF;
               NORMAL := VALPAR; ADR := DX; LEV := LEVEL;
               DX := DX + SZ
           END
         END ;
         IF SY <> RPARENT THEN
         BEGIN IF SY = SEMICOLON THEN INSYMBOL ELSE
               BEGIN ERROR(14);
                 IF SY = COMMA THEN INSYMBOL
               END ;
            TEST([IDENT,VARSY], [RPARENT]+FSYS, 6)
         END
       END (*WHILE*) ;
     IF SY = RPARENT THEN
       BEGIN INSYMBOL;
         TEST([SEMICOLON,COLON], FSYS, 6)
       END
     ELSE ERROR(4)
   END (*PARAMETERLIST*) ;

   PROCEDURE CONSDECLARATION;
     VAR C: CONREC;
   BEGIN INSYMBOL;
     TEST([IDENT], BLOCKBEGSYS, 2);
     WHILE SY = IDENT DO
       BEGIN ENTER(ID,KONSTANT); INSYMBOL;
         IF SY = EQL THEN INSYMBOL ELSE
            BEGIN ERROR(16);
               IF SY = BECOMES THEN INSYMBOL
            END ;
         CONSTANT([SEMICOLON,COMMA,IDENT]+FSYS,C);
         TAB[T].TYP := C.TP; TAB[T].REF := 0;
         IF C.TP = REALS THEN
           BEGIN ENTERREAL(C.R); TAB[T].ADR := C1 END
         ELSE TAB[T].ADR := C.I;
         TESTSEMICOLON
       END
   END (*CONSDECLARATION*) ;

   PROCEDURE TYPEDECLARATION;
     VAR TP: TYPES; RF, SZ, T1: INTEGER;
   BEGIN INSYMBOL;
     TEST([IDENT], BLOCKBEGSYS, 2);
     WHILE SY = IDENT DO
       BEGIN ENTER(ID,TYPE1); T1 := T; INSYMBOL;
         IF SY = EQL THEN INSYMBOL ELSE
            BEGIN ERROR(16);
               IF SY = BECOMES THEN INSYMBOL
            END ;
         TYP([SEMICOLON,COMMA,IDENT]+FSYS, TP, RF, SZ);
         WITH TAB[T1] DO
           BEGIN TYP := TP; REF := RF; ADR := SZ
           END ;
         TESTSEMICOLON
       END
   END (*TYPEDECLARATION*) ;

   PROCEDURE VARDECLARATION;
     VAR T0, T1, RF, SZ: INTEGER;
         TP: TYPES;
   BEGIN INSYMBOL;
     WHILE SY = IDENT DO
     BEGIN T0 := T; ENTERVARIABLE;
       WHILE SY = COMMA DO
         BEGIN INSYMBOL; ENTERVARIABLE;
         END ;
       IF SY = COLON THEN INSYMBOL ELSE ERROR(5);
       T1 := T;
       TYP([SEMICOLON,COMMA,IDENT]+FSYS, TP, RF, SZ);
       WHILE T0 < T1 DO
       BEGIN T0 := T0+1;
         WITH TAB[T0] DO
         BEGIN TYP := TP; REF := RF;
           LEV := LEVEL; ADR := DX; NORMAL := TRUE;
           DX := DX + SZ
         END
       END ;
       TESTSEMICOLON
     END
   END (*VARDECLARATION*) ;

   PROCEDURE PROCDECLARATION;
      VAR ISFUN: BOOLEAN;
   BEGIN ISFUN := SY = FUNCSY; INSYMBOL;
     IF SY <> IDENT THEN
        BEGIN  ERROR(2); ID := '          '
        END ;
     IF ISFUN THEN ENTER(ID,FUNKTION) ELSE ENTER(ID,PROZEDURE);
     TAB[T].NORMAL := TRUE;
     INSYMBOL; BLOCK([SEMICOLON]+FSYS, ISFUN, LEVEL+1);
     IF SY = SEMICOLON THEN INSYMBOL ELSE ERROR(14);
     EMIT(32+ORD(ISFUN))    (*EXIT*)
   END (*PROCEDUREDECLARATION*) ;

(*---------------------------------------------------------STATEMENT--*)

   PROCEDURE STATEMENT(FSYS: SYMSET);
      VAR I: INTEGER;
      PROCEDURE EXPRESSION(FSYS: SYMSET; VAR X: ITEM); FORWARD;

      PROCEDURE SELECTOR(FSYS: SYMSET; VAR V:ITEM);
         VAR X: ITEM; A,J: INTEGER;
      BEGIN (*SY IN [LPARENT, LBRACK, PERIOD]*)
        REPEAT
          IF SY = PERIOD THEN
          BEGIN INSYMBOL;  (*FIELD SELECTOR*)
            IF SY <> IDENT THEN ERROR(2) ELSE
            BEGIN
              IF V.TYP <> RECORDS THEN ERROR(31) ELSE
              BEGIN (*SEARCH FIELD IDENTIFIER*)
                J := BTAB[V.REF] .LAST; TAB[0].NAME := ID;
                WHILE TAB[J].NAME <> ID DO J := TAB[J].LINK;
                IF J = 0 THEN ERROR(0);
                V.TYP := TAB[J].TYP; V.REF := TAB[J].REF;
                A := TAB[J].ADR; IF A <> 0 THEN EMIT1(9,A)
              END ;
              INSYMBOL
            END
          END ELSE
          BEGIN (*ARRAY SELECTOR*)
            IF SY <> LBRACK THEN ERROR(11);
            REPEAT INSYMBOL;
              EXPRESSION(FSYS+[COMMA,RBRACK], X);
              IF V.TYP <> ARRAYS THEN ERROR(28) ELSE
                BEGIN A := V.REF;
                  IF ATAB[A].INXTYP <> X.TYP THEN ERROR(26) ELSE
                IF ATAB[A].ELSIZE = 1 THEN EMIT1(20,A) ELSE EMIT1(21,A);
                  V.TYP := ATAB[A].ELTYP; V.REF := ATAB[A].ELREF
                END
            UNTIL SY <> COMMA;
            IF SY = RBRACK THEN INSYMBOL ELSE
              BEGIN ERROR(12); IF SY = RPARENT THEN INSYMBOL
              END
          END
        UNTIL NOT (SY IN [LBRACK,LPARENT,PERIOD]);
        TEST(FSYS, [], 6)
      END (*SELECTOR*) ;

      PROCEDURE CALL(FSYS: SYMSET; I: INTEGER);
         VAR X: ITEM;
             LASTP, CP, K: INTEGER;
      BEGIN EMIT1(18,I);  (*MARK STACK*)
        LASTP := BTAB[TAB[I].REF].LASTPAR; CP := I;
        IF SY = LPARENT THEN
        BEGIN (*ACTUAL PARAMETER LIST*)
          REPEAT INSYMBOL;
            IF CP >= LASTP THEN ERROR(39) ELSE
            BEGIN CP := CP+1;
              IF TAB[CP].NORMAL THEN
              BEGIN (*VALUE PARAMETER*)
                EXPRESSION(FSYS+[COMMA,COLON,RPARENT], X);
                IF X.TYP=TAB[CP].TYP THEN
                  BEGIN
                    IF X.REF <> TAB[CP].REF THEN ERROR(36) ELSE
                  IF X.TYP = ARRAYS THEN EMIT1(22,ATAB[X.REF].SIZE) ELSE
                    IF X.TYP = RECORDS THEN EMIT1(22,BTAB[X.REF].VSIZE)
                  END ELSE
                IF (X.TYP=INTS) AND (TAB[CP].TYP=REALS) THEN
                   EMIT1(26,0) ELSE
                   IF X.TYP<>NOTYP THEN ERROR(36);
              END ELSE
              BEGIN (*VARIABLE PARAMETER*)
                IF SY <> IDENT THEN ERROR(2) ELSE
                BEGIN K := LOC(ID); INSYMBOL;
                  IF K <> 0 THEN
                  BEGIN IF TAB[K].OBJ <> VARIABLE THEN ERROR(37);
                    X.TYP := TAB[K].TYP; X.REF := TAB[K].REF;
                    IF TAB[K].NORMAL THEN EMIT2(0,TAB[K].LEV,TAB[K].ADR)
                       ELSE EMIT2(1,TAB[K].LEV,TAB[K].ADR);
                    IF SY IN [LBRACK,LPARENT,PERIOD] THEN
                       SELECTOR(FSYS+[COMMA,COLON,RPARENT], X);
                    IF (X.TYP<>TAB[CP].TYP) OR (X.REF<>TAB[CP].REF) THEN
                       ERROR(36)
                  END
                END
              END
            END ;
            TEST([COMMA,RPARENT], FSYS, 6)
          UNTIL SY <> COMMA;
          IF SY = RPARENT THEN INSYMBOL ELSE ERROR(4)
        END ;
        IF CP < LASTP THEN ERROR(39); (*TOO FEW ACTUAL PARAMETERS*)
        EMIT1(19, BTAB[TAB[I].REF].PSIZE-1);
        IF TAB[I].LEV < LEVEL THEN EMIT2(3, TAB[I].LEV, LEVEL)
      END (*CALL*) ;

      FUNCTION RESULTTYPE(A,B: TYPES): TYPES;
      BEGIN
        IF (A>REALS) OR (B>REALS) THEN
          BEGIN ERROR(33); RESULTTYPE := NOTYP
          END ELSE
        IF (A=NOTYP) OR (B=NOTYP) THEN RESULTTYPE := NOTYP ELSE
        IF A=INTS THEN
          IF B=INTS THEN RESULTTYPE := INTS ELSE
            BEGIN RESULTTYPE := REALS; EMIT1(26,1)
            END
        ELSE
          BEGIN RESULTTYPE := REALS;
            IF B=INTS THEN EMIT1(26,0)
          END
      END (*RESULTTYPE*) ;

      PROCEDURE EXPRESSION;
        VAR Y:ITEM; OP:SYMBOL;

        PROCEDURE SIMPLEEXPRESSION(FSYS:SYMSET; VAR X:ITEM);
          VAR Y:ITEM; OP:SYMBOL;

          PROCEDURE TERM(FSYS:SYMSET; VAR X:ITEM);
            VAR Y:ITEM; OP:SYMBOL;

            PROCEDURE FACTOR(FSYS:SYMSET; VAR X:ITEM);
              VAR I,F: INTEGER;

              PROCEDURE STANDFCT(N: INTEGER);
                 VAR TS: TYPSET;
              BEGIN (*STANDARD FUNCTION NO. N*)
                IF SY = LPARENT THEN INSYMBOL ELSE ERROR(9);
                IF N < 17 THEN
                  BEGIN EXPRESSION(FSYS+[RPARENT],X);
                    CASE N OF
(*ABS,SQR*)      0,2:  BEGIN TS := [INTS,REALS]; TAB[I].TYP := X.TYP;
                         IF X.TYP = REALS THEN N := N+1
                       END ;
(*ODD,CHR*)      4,5:  TS := [INTS];
(*ORD*)          6: BEGIN
            IF X.TYP = INTS THEN N := 19;
            TS := [INTS,BOOLS,CHARS];
         END;
(*SUCC,PRED*)    7,8:  TS := [CHARS];
(*ROUND,TRUNC*)  9,10,11,12,13,14,15,16:
(*SIN,COS,...*)        BEGIN TS := [INTS,REALS];
                         IF X.TYP = INTS THEN EMIT1(26,0)
                       END ;
                    END ;
                    IF X.TYP IN TS THEN EMIT1(8,N) ELSE
                    IF X.TYP <> NOTYP THEN ERROR(48);
                  END ELSE
(*EOF,EOLN*)      BEGIN (*N IN [17,18]*)
                    IF SY <> IDENT THEN ERROR(2) ELSE
                    IF ID <> 'INPUT     ' THEN ERROR(0) ELSE INSYMBOL;
                    EMIT1(8,N);
                  END ;
                X.TYP := TAB[I].TYP;
                IF SY = RPARENT THEN INSYMBOL ELSE ERROR(4)
              END (*STANDFCT*) ;

            BEGIN (*FACTOR*) X.TYP := NOTYP; X.REF := 0;
              TEST(FACBEGSYS, FSYS, 58);
              WHILE SY IN FACBEGSYS DO
                BEGIN
                  IF SY = IDENT THEN
                  BEGIN I := LOC(ID); INSYMBOL;
                    WITH TAB[I] DO
                    CASE OBJ OF
              KONSTANT: BEGIN X.TYP := TYP; X.REF := 0;
         CASE X.TYP OF
            INTS: EMIT1(24,ADR);
            REALS: EMIT1(25,ADR);
            BOOLS: EMIT1(64,ADR);
            CHARS: EMIT1(65,ADR);
         END;
                        END ;
              VARIABLE: BEGIN X.TYP := TYP; X.REF := REF;
                          IF SY IN [LBRACK,LPARENT,PERIOD] THEN
                            BEGIN IF NORMAL THEN F := 0 ELSE F := 1;
                              EMIT2(F, LEV, ADR);
                              SELECTOR(FSYS,X);
                              IF X.TYP IN STANTYPS THEN EMIT(34)
                            END ELSE
                            BEGIN
                              IF X.TYP IN STANTYPS THEN
                                IF NORMAL THEN F := 1 ELSE F := 2
                              ELSE
                                IF NORMAL THEN F := 0 ELSE F := 1;
                              EMIT2(F, LEV, ADR)
                            END
                        END ;
              TYPE1, PROZEDURE:    ERROR(44);
              FUNKTION :BEGIN X.TYP := TYP;
                          IF LEV <> 0 THEN CALL(FSYS, I)
                                ELSE STANDFCT(ADR)
                        END
                    END (*CASE,WITH*)
                  END ELSE
                  IF SY IN [CHARCON,INTCON,REALCON] THEN
                   BEGIN
                     IF SY = REALCON THEN
                     BEGIN X.TYP := REALS; ENTERREAL(RNUM);
                       EMIT1(25, C1)
                     END ELSE
         BEGIN
         IF SY = CHARCON THEN BEGIN
            X.TYP := CHARS;
            EMIT1(65, INUM);
         END ELSE BEGIN
            X.TYP := INTS;
            EMIT1(24, INUM);
         END;
                     END ;
                     X.REF := 0; INSYMBOL
                   END ELSE
                  IF SY = LPARENT THEN
                   BEGIN INSYMBOL; EXPRESSION(FSYS+[RPARENT], X);
                     IF SY = RPARENT THEN INSYMBOL ELSE ERROR(4)
                   END ELSE
                  IF SY = NOTSY THEN
                   BEGIN INSYMBOL; FACTOR(FSYS,X);
                     IF X.TYP=BOOLS THEN EMIT(35) ELSE
                       IF X.TYP<>NOTYP THEN ERROR(32)
                   END ;
                  TEST(FSYS, FACBEGSYS, 6)
                END (*WHILE*)
            END (*FACTOR*) ;

          BEGIN (*TERM*)
            FACTOR(FSYS+[TIMES,RDIV,IDIV,IMOD,ANDSY], X);
            WHILE SY IN [TIMES,RDIV,IDIV,IMOD,ANDSY] DO
              BEGIN OP := SY; INSYMBOL;
                FACTOR(FSYS+[TIMES,RDIV,IDIV,IMOD,ANDSY], Y);
                IF OP = TIMES THEN
                BEGIN X.TYP := RESULTTYPE(X.TYP, Y.TYP);
                  CASE X.TYP OF
                    NOTYP: ;
                    INTS : EMIT(57);
                    REALS: EMIT(60);
                  END
                END ELSE
                IF OP = RDIV THEN
                BEGIN
                  IF X.TYP = INTS THEN
                    BEGIN EMIT1(26,1); X.TYP := REALS
                    END ;
                  IF Y.TYP = INTS THEN
                    BEGIN EMIT1(26,0); Y.TYP := REALS
                    END ;
                  IF (X.TYP=REALS) AND (Y.TYP=REALS) THEN EMIT(61) ELSE
                    BEGIN IF (X.TYP<>NOTYP) AND (Y.TYP<>NOTYP) THEN
                            ERROR(33);
                          X.TYP := NOTYP
                    END
                END ELSE
                IF OP = ANDSY THEN
                BEGIN IF (X.TYP=BOOLS) AND (Y.TYP=BOOLS) THEN
                         EMIT(56) ELSE
                      BEGIN IF (X.TYP<>NOTYP) AND (Y.TYP<>NOTYP) THEN
                               ERROR(32);
                         X.TYP := NOTYP
                      END
                END ELSE
                BEGIN (*OP IN [IDIV,IMOD]*)
                  IF (X.TYP=INTS) AND (Y.TYP=INTS) THEN
                    IF OP=IDIV THEN EMIT(58)
                               ELSE EMIT(59) ELSE
                    BEGIN IF (X.TYP<>NOTYP) AND (Y.TYP<>NOTYP) THEN
                             ERROR(34);
                          X.TYP := NOTYP
                    END
                END
              END
          END (*TERM*) ;

        BEGIN (*SIMPLEEXPRESSION*)
          IF SY IN [PLUS,MINUS] THEN
            BEGIN OP := SY; INSYMBOL;
              TERM(FSYS+[PLUS,MINUS], X);
              IF X.TYP > REALS THEN ERROR(33) ELSE
                IF OP = MINUS THEN
         IF X.TYP = REALS THEN
            EMIT(66)
         ELSE
            EMIT(36)
            END ELSE
          TERM(FSYS+[PLUS,MINUS,ORSY], X);
          WHILE SY IN [PLUS,MINUS,ORSY] DO
            BEGIN OP := SY; INSYMBOL;
               TERM(FSYS+[PLUS,MINUS,ORSY], Y);
               IF OP = ORSY THEN
               BEGIN
                 IF (X.TYP=BOOLS) AND (Y.TYP=BOOLS) THEN EMIT(51) ELSE
                   BEGIN IF (X.TYP<>NOTYP) AND (Y.TYP<>NOTYP) THEN
                            ERROR(32);
                         X.TYP := NOTYP
                   END
               END ELSE
               BEGIN X.TYP := RESULTTYPE(X.TYP, Y.TYP);
                 CASE X.TYP OF
                   NOTYP: ;
                   INTS : IF OP = PLUS THEN EMIT(52)
                                   ELSE EMIT(53);
                   REALS: IF OP = PLUS THEN EMIT(54)
                                   ELSE EMIT(55)
                 END
               END
            END
        END (*SIMPLEEXPRESSION*) ;

      BEGIN (*EXPRESSION*)
        SIMPLEEXPRESSION(FSYS+[EQL,NEQ,LSS,LEQ,GTR,GEQ], X);
        IF SY IN [EQL,NEQ,LSS,LEQ,GTR,GEQ] THEN
          BEGIN OP := SY; INSYMBOL;
             SIMPLEEXPRESSION(FSYS, Y);
             IF (X.TYP IN [ NOTYP,INTS,BOOLS,CHARS]) AND (X.TYP = Y.TYP) THEN
               CASE OP OF
                 EQL: EMIT(45);
                 NEQ: EMIT(46);
                 LSS: EMIT(47);
                 LEQ: EMIT(48);
                 GTR: EMIT(49);
                 GEQ: EMIT(50);
               END ELSE
             BEGIN IF X.TYP = INTS THEN
                     BEGIN X.TYP := REALS; EMIT1(26,1)
                     END ELSE
                   IF Y.TYP = INTS THEN
                     BEGIN Y.TYP := REALS; EMIT1(26,0)
                     END ;
               IF (X.TYP=REALS) AND (Y.TYP=REALS) THEN
                 CASE OP OF
                   EQL: EMIT(39);
                   NEQ: EMIT(40);
                   LSS: EMIT(41);
                   LEQ: EMIT(42);
                   GTR: EMIT(43);
                   GEQ: EMIT(44);
                 END
               ELSE ERROR(35)
             END ;
             X.TYP := BOOLS
          END
      END (*EXPRESSION*) ;

      PROCEDURE ASSIGNMENT(LV,AD: INTEGER);
         VAR X,Y: ITEM; F: INTEGER;
         (*TAB[I].OBJ IN [VARIABLE,PROZEDURE]*)
      BEGIN X.TYP := TAB[I].TYP; X.REF := TAB[I].REF;
        IF TAB[I].NORMAL THEN F := 0 ELSE F := 1;
        EMIT2(F, LV, AD);
        IF SY IN [LBRACK,LPARENT,PERIOD] THEN
           SELECTOR([BECOMES,EQL]+FSYS, X);
        IF SY = BECOMES THEN INSYMBOL ELSE
          BEGIN ERROR(51); IF SY = EQL THEN INSYMBOL
          END ;
        EXPRESSION(FSYS, Y);
        IF X.TYP = Y.TYP THEN
          IF X.TYP IN STANTYPS THEN EMIT(38) ELSE
          IF X.REF <> Y.REF THEN ERROR(46) ELSE
          IF X.TYP = ARRAYS THEN EMIT1(23, ATAB[X.REF].SIZE)
                            ELSE EMIT1(23, BTAB[X.REF].VSIZE)
        ELSE
        IF (X.TYP=REALS) AND (Y.TYP=INTS) THEN
          BEGIN EMIT1(26,0); EMIT(38)
          END ELSE
          IF (X.TYP<>NOTYP) AND (Y.TYP<>NOTYP) THEN ERROR(46)
      END (*ASSIGNMENT*) ;

      PROCEDURE COMPOUNDSTATEMENT;
      BEGIN INSYMBOL;
        STATEMENT([SEMICOLON,ENDSY]+FSYS);
        WHILE SY IN [SEMICOLON]+STATBEGSYS DO
        BEGIN IF SY = SEMICOLON THEN INSYMBOL ELSE ERROR(14);
          STATEMENT([SEMICOLON,ENDSY]+FSYS)
        END ;
        IF SY = ENDSY THEN INSYMBOL ELSE ERROR(57)
      END (*COMPOUNDSTATEMENET*) ;

      PROCEDURE IFSTATEMENT;
         VAR X: ITEM; LC1,LC2: INTEGER;
      BEGIN INSYMBOL;
        EXPRESSION(FSYS+[THENSY,DOSY], X);
        IF NOT (X.TYP IN [BOOLS,NOTYP]) THEN ERROR(17);
        LC1 := LC; EMIT(11);  (*JMPC*)
        IF SY = THENSY THEN INSYMBOL ELSE
          BEGIN ERROR(52); IF SY = DOSY THEN INSYMBOL
          END ;
        STATEMENT(FSYS+[ELSESY]);
        IF SY = ELSESY THEN
          BEGIN INSYMBOL; LC2 := LC; EMIT(10);
            CODE[LC1].Y := LC; STATEMENT(FSYS); CODE[LC2].Y := LC
          END
        ELSE CODE[LC1].Y := LC
      END (*IFSTATEMENT*) ;

      PROCEDURE CASESTATEMENT;
        VAR X: ITEM;
            I,J,K,LC1: INTEGER;
            CASETAB: ARRAY [1..CSMAX] OF
                       PACKED RECORD VAL, LC: INDEX END ;
            EXITTAB: ARRAY [1..CSMAX] OF INTEGER;

        PROCEDURE CASELABEL;
          VAR LAB: CONREC; K: INTEGER;
        BEGIN CONSTANT(FSYS+[COMMA,COLON], LAB);
          IF LAB.TP <> X.TYP THEN ERROR(47) ELSE
          IF I = CSMAX THEN FATAL(6) ELSE
            BEGIN I := I+1; K := 0;
              CASETAB[I].VAL := LAB.I; CASETAB[I].LC := LC;
              REPEAT K := K+1 UNTIL CASETAB[K].VAL = LAB.I;
              IF K < I THEN ERROR(1);   (*MULTIPLE DEFINITION*)
            END
        END (*CASELABEL*) ;

        PROCEDURE ONECASE;
        BEGIN IF SY IN CONSTBEGSYS THEN
          BEGIN CASELABEL;
            WHILE SY = COMMA DO
              BEGIN INSYMBOL; CASELABEL
              END ;
            IF SY = COLON THEN INSYMBOL ELSE ERROR(5);
            STATEMENT([SEMICOLON,ENDSY]+FSYS);
            J := J+1; EXITTAB[J] := LC; EMIT(10)
          END
        END (*ONECASE*) ;

      BEGIN INSYMBOL; I := 0; J := 0;
        EXPRESSION(FSYS+[OFSY,COMMA,COLON], X);
        IF NOT (X.TYP IN [INTS,BOOLS,CHARS,NOTYP]) THEN ERROR(23);
        LC1 := LC; EMIT(12);  (*JMPX*)
        IF SY = OFSY THEN INSYMBOL ELSE ERROR(8);
        ONECASE;
        WHILE SY = SEMICOLON DO
          BEGIN INSYMBOL; ONECASE
          END ;
        CODE[LC1].Y := LC;
        FOR K := 1 TO I DO
          BEGIN EMIT1(13,CASETAB[K].VAL); EMIT1(13,CASETAB[K].LC)
          END ;
        EMIT1(10,0);
        FOR K := 1 TO J DO CODE[EXITTAB[K]].Y := LC;
        IF SY = ENDSY THEN INSYMBOL ELSE ERROR(57)
      END (*CASESTATEMENT*) ;

      PROCEDURE REPEATSTATEMENT;
         VAR X: ITEM; LC1: INTEGER;
      BEGIN LC1 := LC;
        INSYMBOL; STATEMENT([SEMICOLON,UNTILSY]+FSYS);
        WHILE SY IN [SEMICOLON]+STATBEGSYS DO
        BEGIN IF SY = SEMICOLON THEN INSYMBOL ELSE ERROR(14);
          STATEMENT([SEMICOLON,UNTILSY]+FSYS)
        END ;
        IF SY = UNTILSY THEN
          BEGIN INSYMBOL; EXPRESSION(FSYS, X);
            IF NOT (X.TYP IN [BOOLS,NOTYP]) THEN ERROR(17);
            EMIT1(11,LC1)
          END
        ELSE ERROR(53)
      END (*REPEATSTATEMENT*) ;

      PROCEDURE WHILESTATEMENT;
         VAR X: ITEM; LC1,LC2: INTEGER;
      BEGIN INSYMBOL; LC1 := LC;
        EXPRESSION(FSYS+[DOSY], X);
        IF NOT (X.TYP IN [BOOLS,NOTYP]) THEN ERROR(17);
        LC2 := LC; EMIT(11);
        IF SY = DOSY THEN INSYMBOL ELSE ERROR(54);
        STATEMENT(FSYS); EMIT1(10,LC1); CODE[LC2].Y := LC
      END (*WHILESTATEMENT*) ;

      PROCEDURE FORSTATEMENT;
         VAR CVT: TYPES; X: ITEM;
             I,F,LC1,LC2: INTEGER;
      BEGIN INSYMBOL;
        IF SY = IDENT THEN
          BEGIN I := LOC(ID); INSYMBOL;
            IF I = 0 THEN CVT := INTS ELSE
            IF TAB[I].OBJ = VARIABLE THEN
              BEGIN CVT := TAB[I].TYP;
                IF NOT TAB[I].NORMAL THEN ERROR(37) ELSE
                  EMIT2(0, TAB[I].LEV, TAB[I].ADR);
                IF NOT (CVT IN [NOTYP,INTS,BOOLS,CHARS]) THEN ERROR(18)
              END ELSE
              BEGIN ERROR(37); CVT := INTS
              END
          END ELSE SKIP([BECOMES,TOSY,DOWNTOSY,DOSY]+FSYS, 2);
        IF SY = BECOMES THEN
          BEGIN INSYMBOL; EXPRESSION([TOSY,DOWNTOSY,DOSY]+FSYS, X);
            IF X.TYP <> CVT THEN ERROR(19);
          END ELSE SKIP([TOSY,DOWNTOSY,DOSY]+FSYS, 51);
        F := 14;
        IF SY IN [TOSY, DOWNTOSY] THEN
          BEGIN IF SY = DOWNTOSY THEN F := 16;
            INSYMBOL; EXPRESSION([DOSY]+FSYS, X);
            IF X.TYP <> CVT THEN ERROR(19)
          END ELSE SKIP([DOSY]+FSYS, 55);
        LC1 := LC; EMIT(F);
        IF SY = DOSY THEN INSYMBOL ELSE ERROR(54);
        LC2 := LC; STATEMENT(FSYS);
        EMIT1(F+1,LC2); CODE[LC1].Y := LC
      END (*FORSTATEMENT*) ;

      PROCEDURE STANDPROC(N: INTEGER);
         VAR I,F: INTEGER;
             X,Y: ITEM;
      BEGIN
        CASE N OF
   1,2: BEGIN (*READ*)
          IF NOT IFLAG THEN
            BEGIN ERROR(20); IFLAG := TRUE
            END ;
          IF SY = LPARENT THEN
          BEGIN
            REPEAT INSYMBOL;
              IF SY <> IDENT THEN ERROR(2) ELSE
              BEGIN I := LOC(ID); INSYMBOL;
                IF I <> 0 THEN
                IF TAB[I].OBJ <> VARIABLE THEN ERROR(37) ELSE
                BEGIN X.TYP := TAB[I].TYP; X.REF := TAB[I].REF;
                  IF TAB[I].NORMAL THEN F := 0 ELSE F := 1;
                  EMIT2(F, TAB[I].LEV, TAB[I].ADR);
                  IF SY IN [LBRACK,LPARENT,PERIOD] THEN
                    SELECTOR(FSYS+[COMMA,RPARENT], X);
                  IF X.TYP IN [INTS,REALS,CHARS,NOTYP] THEN
                    EMIT1(27, ORD(X.TYP)) ELSE ERROR(40)
                END
              END ;
              TEST([COMMA,RPARENT], FSYS, 6);
            UNTIL SY <> COMMA;
            IF SY = RPARENT THEN INSYMBOL ELSE ERROR(4)
          END ;
          IF N = 2 THEN EMIT(62)
        END ;
   3,4: BEGIN (*WRITE*)
          IF SY = LPARENT THEN
          BEGIN
            REPEAT INSYMBOL;
              IF SY = STRING THEN
                BEGIN EMIT1(24,SLENG); EMIT1(28,INUM); INSYMBOL
                END ELSE
              BEGIN EXPRESSION(FSYS+[COMMA,COLON,RPARENT], X);
                IF NOT (X.TYP IN STANTYPS) THEN ERROR(41);
                IF SY = COLON THEN
                BEGIN INSYMBOL;
                  EXPRESSION(FSYS+[COMMA,COLON,RPARENT], Y);
                  IF Y.TYP <> INTS THEN ERROR(43);
                  IF SY = COLON THEN
                  BEGIN IF X.TYP <> REALS THEN ERROR(42);
                    INSYMBOL; EXPRESSION(FSYS+[COMMA,RPARENT], Y);
                    IF Y.TYP <> INTS THEN ERROR(43);
                    EMIT(37)
                  END
                  ELSE EMIT1(30, ORD(X.TYP))
                END
                ELSE EMIT1(29, ORD(X.TYP))
              END
            UNTIL SY <> COMMA;
            IF SY = RPARENT THEN INSYMBOL ELSE ERROR(4)
          END ;
          IF N = 4 THEN EMIT(63)
        END ;
        END (*CASE*)
      END (*STANDPROC*) ;

    BEGIN (*STATEMENT*)
      IF SY IN STATBEGSYS+[IDENT] THEN
          CASE SY OF
            IDENT:    BEGIN I := LOC(ID); INSYMBOL;
                        IF I <> 0 THEN
                        CASE TAB[I].OBJ OF
                          KONSTANT, TYPE1: ERROR(45);
                          VARIABLE: ASSIGNMENT(TAB[I].LEV, TAB[I].ADR);
                          PROZEDURE:
                            IF TAB[I].LEV <> 0 THEN CALL(FSYS, I)
                                    ELSE STANDPROC(TAB[I].ADR);
                          FUNKTION:
                            IF TAB[I].REF = DISPLAY[LEVEL] THEN
                              ASSIGNMENT(TAB[I].LEV+1, 0) ELSE ERROR(45)
                        END
                      END ;
            BEGINSY:  COMPOUNDSTATEMENT;
            IFSY:     IFSTATEMENT;
            CASESY:   CASESTATEMENT;
            WHILESY:  WHILESTATEMENT;
            REPEATSY: REPEATSTATEMENT;
            FORSY:    FORSTATEMENT;
          END;
        TEST(FSYS, [], 14)
    END (*STATEMENT*) ;

BEGIN (*BLOCK*) DX := 5; PRT := T;
  IF LEVEL > LMAX THEN FATAL(5);
  TEST([LPARENT,COLON,SEMICOLON], FSYS, 7);
  ENTERBLOCK; DISPLAY[LEVEL] := B; PRB := B;
  TAB[PRT].TYP := NOTYP; TAB[PRT].REF := PRB;
  IF SY = LPARENT THEN PARAMETERLIST;
  BTAB[PRB].LASTPAR := T; BTAB[PRB].PSIZE := DX;
  IF ISFUN THEN
    IF SY = COLON THEN
    BEGIN INSYMBOL;   (*FUNCTION TYPE*)
      IF SY = IDENT THEN
      BEGIN X := LOC(ID); INSYMBOL;
        IF X <> 0 THEN
          IF TAB[X].OBJ <> TYPE1 THEN ERROR(29) ELSE
            IF TAB[X].TYP IN STANTYPS THEN TAB[PRT].TYP := TAB[X].TYP
              ELSE ERROR(15)
      END ELSE SKIP([SEMICOLON]+FSYS, 2)
    END ELSE ERROR(5);
  IF SY = SEMICOLON THEN INSYMBOL ELSE ERROR(14);
  REPEAT
    IF SY = CONSTSY THEN CONSDECLARATION;
    IF SY = TYPESY THEN TYPEDECLARATION;
    IF SY = VARSY THEN VARDECLARATION;
    BTAB[PRB].VSIZE := DX;
    WHILE SY IN [PROCSY,FUNCSY] DO PROCDECLARATION;
    TEST([BEGINSY], BLOCKBEGSYS+STATBEGSYS, 56)
  UNTIL SY IN STATBEGSYS;
  TAB[PRT].ADR := LC;
  INSYMBOL; STATEMENT([SEMICOLON,ENDSY]+FSYS);
  WHILE SY IN [SEMICOLON]+STATBEGSYS DO
    BEGIN IF SY = SEMICOLON THEN INSYMBOL ELSE ERROR(14);
      STATEMENT([SEMICOLON,ENDSY]+FSYS)
    END ;
  IF SY = ENDSY THEN INSYMBOL ELSE ERROR(57);
  TEST(FSYS+[PERIOD], [], 6)
END (*BLOCK*) ;

(* EOF                                                                        *)
(* #INCLUDE "INTERPRET.I"                                                     *)
(* INTERPRET.I                                                                *)
PROCEDURE INTERPRET;
  (*GLOBAL CODE, TAB, BTAB*)
  VAR IR: ORDER;      (*INSTRUCTION BUFFER*)
      PC: INTEGER;    (*PROGRAM COUNTER*)
      PS: (RUN,FIN,CASCHK,DIVCHK,INXCHK,STKCHK,LINCHK,LNGCHK,REDCHK);
      T:  INTEGER;    (*TOP STACK INDEX*)
      B:  INTEGER;    (*BASE INDEX*)
      LNCNT, OCNT, BLKCNT, CHRCNT: INTEGER;     (*COUNTERS*)
      H1,H2,H3,H4: INTEGER;
      FLD: ARRAY [1..4] OF INTEGER;     (*DEFAULT FIELD WIDTHS*)

      DISPLAY: ARRAY [1..LMAX] OF INTEGER;
      S: ARRAY [1..STACKSIZE] OF          (*BLOCKMARK:              *)
         RECORD CASE TYPES OF             (*   S[B+0] = FCT RESULT  *)
           INTS:  (I: INTEGER);           (*   S[B+1] = RETURN ADR  *)
           REALS: (R: REAL);              (*   S[B+2] = STATIC LINK *)
           BOOLS: (B: BOOLEAN);           (*   S[B+3] = DYNAMIC LINK*)
           CHARS: (C: CHAR)               (*   S[B+4] = TABLE INDEX *)
         END ;

BEGIN (*INTERPRET*)
  S[1].I := 0; S[2].I := 0; S[3].I := -1; S[4].I := BTAB[1].LAST;
  B := 0; DISPLAY[1] := 0;
  T := BTAB[2].VSIZE - 1; PC := TAB[S[4].I].ADR;
  PS := RUN;
  LNCNT := 0; OCNT := 0; CHRCNT := 0;
  FLD[1] := 10; FLD[2] := 22; FLD[3] := 10; FLD[4] := 1;
  REPEAT IR := CODE[PC]; PC := PC+1; OCNT := OCNT + 1;
    CASE IR.F OF
  0: BEGIN (*LOAD ADDRESS*) T := T+1;
       IF T > STACKSIZE THEN PS := STKCHK
         ELSE S[T].I := DISPLAY[IR.X] + IR.Y
     END ;
  1: BEGIN (*LOAD VALUE*) T := T+1;
       IF T > STACKSIZE THEN PS := STKCHK
         ELSE S[T] := S[DISPLAY[IR.X] + IR.Y]
     END ;
  2: BEGIN (*LOAD INDIRECT*) T := T+1;
       IF T > STACKSIZE THEN PS := STKCHK
         ELSE S[T] := S[S[DISPLAY[IR.X] + IR.Y].I]
     END ;
  3: BEGIN (*UPDATE DISPLAY*)
       H1 := IR.Y; H2 := IR.X; H3 := B;
       REPEAT DISPLAY[H1] := H3; H1 := H1-1; H3 := S[H3+2].I
       UNTIL H1 = H2
     END ;
  8: CASE IR.Y OF
      0: S[T].I := ABS(S[T].I);
      1: S[T].R := ABS(S[T].R);
#     2: S[T].I := S[T].I*S[T].I (*SQR(S[T].I)*);
      3: S[T].R := S[T].R*S[T].R (*SQR(S[T].R)*);
      4: S[T].B := ODD(S[T].I);
      5: BEGIN
           IF (S[T].I < 0) OR (S[T].I > 127) THEN PS := INXCHK
   ELSE S[T].C := CHR(S[T].I)
         END ;
      6: S[T].I := ORD(S[T].C) ;
      7: S[T].C := SUCC(S[T].C);
      8: S[T].C := PRED(S[T].C);
##  (*9: S[T].I := ROUND(S[T].R);*)
     10: S[T].I := TRUNC(S[T].R);
     11: S[T].R := SIN(S[T].R);
     12: S[T].R := COS(S[T].R);
     13: S[T].R := EXP(S[T].R);
     14: S[T].R := LN(S[T].R);
     15: S[T].R := SQRT(S[T].R);
     16: S[T].R := ARCTAN(S[T].R);
     17: BEGIN T := T+1;
           IF T > STACKSIZE THEN PS := STKCHK ELSE S[T].B := EOF(INPUT)
         END ;
     18: BEGIN T := T+1;
           IF T > STACKSIZE THEN PS := STKCHK ELSE S[T].B := EOLN(INPUT)
         END ;
     19:
     END ;
  9: S[T].I := S[T].I + IR.Y;   (*OFFSET*)
 10: PC := IR.Y;  (*JUMP*)
 11: BEGIN (*CONDITIONAL JUMP*)
       IF NOT S[T].B THEN PC := IR.Y;  T := T-1
     END ;
 12: BEGIN (*SWITCH*) H1 := S[T].I; T := T-1;
       H2 := IR.Y; H3 := 0;
       REPEAT IF CODE[H2].F <> 13 THEN
                BEGIN H3 := 1; PS := CASCHK
                END ELSE
              IF CODE[H2].Y = H1 THEN
                BEGIN H3 := 1; PC := CODE[H2+1].Y
                END ELSE
              H2 := H2 + 2
       UNTIL H3 <> 0
     END ;
 14: BEGIN (*FOR1UP*) H1 := S[T-1].I;
       IF H1 <= S[T].I THEN S[S[T-2].I].I := H1 ELSE
          BEGIN T := T-3; PC := IR.Y
          END
     END ;
 15: BEGIN (*FOR2UP*) H2 := S[T-2].I; H1 := S[H2].I + 1;
       IF H1 <= S[T].I THEN
         BEGIN S[H2].I := H1; PC := IR.Y END
       ELSE T := T-3;
     END ;
 16: BEGIN (*FOR1DOWN*) H1 := S[T-1].I;
       IF H1 >= S[T].I THEN S[S[T-2].I].I := H1 ELSE
          BEGIN PC := IR.Y; T := T-3
          END
     END ;
 17: BEGIN (*FOR2DOWN*) H2 := S[T-2].I; H1 := S[H2].I - 1;
       IF H1 >= S[T].I THEN
         BEGIN S[H2].I := H1; PC := IR.Y END
       ELSE T := T-3;
     END ;
 18: BEGIN (*MARK STACK*)  H1 := BTAB[TAB[IR.Y].REF].VSIZE;
       IF T+H1 > STACKSIZE THEN PS := STKCHK ELSE
         BEGIN T := T+5; S[T-1].I := H1-1; S[T].I := IR.Y
         END
     END ;
 19: BEGIN (*CALL*) H1 := T - IR.Y;  (*H1 POINTS TO BASE*)
       H2 := S[H1+4].I;            (*H2 POINTS TO TAB*)
       H3 := TAB[H2].LEV; DISPLAY[H3+1] := H1;
       H4 := S[H1+3].I + H1;
       S[H1+1].I := PC; S[H1+2].I := DISPLAY[H3]; S[H1+3].I := B;
       FOR H3 := T+1 TO H4 DO S[H3].I := 0;
       B := H1; T := H4; PC := TAB[H2].ADR
     END ;
 20: BEGIN (*INDEX1*) H1 := IR.Y;      (*H1 POINTS TO ATAB*)
       H2 := ATAB[H1].LOW; H3 := S[T].I;
       IF H3 < H2 THEN PS := INXCHK ELSE
       IF H3 > ATAB[H1].HIGH THEN PS := INXCHK ELSE
         BEGIN T := T-1; S[T].I := S[T].I + (H3-H2)
         END
     END ;
 21: BEGIN (*INDEX*)  H1 := IR.Y;      (*H1 POINTS TO ATAB*)
       H2 := ATAB[H1].LOW; H3 := S[T].I;
       IF H3 < H2 THEN PS := INXCHK ELSE
       IF H3 > ATAB[H1].HIGH THEN PS := INXCHK ELSE
         BEGIN T := T-1; S[T].I := S[T].I + (H3-H2)*ATAB[H1].ELSIZE
         END
     END ;
 22: BEGIN (*LOAD BLOCK*) H1 := S[T].I; T := T-1;
       H2 := IR.Y + T; IF H2 > STACKSIZE THEN PS := STKCHK ELSE
       WHILE T < H2 DO
         BEGIN T := T+1; S[T] := S[H1]; H1 := H1+1
         END
     END ;
 23: BEGIN (*COPY BLOCK*) H1 := S[T-1].I;
       H2 := S[T].I; H3 := H1 + IR.Y;
       WHILE H1 < H3 DO
         BEGIN S[H1] := S[H2]; H1 := H1+1; H2 := H2+1
         END ;
       T := T-2
     END ;
 24: BEGIN (*LITERAL*) T := T+1;
       IF T > STACKSIZE THEN PS := STKCHK ELSE S[T].I := IR.Y
     END ;
 25: BEGIN (*LOAD REAL*) T := T+1;
       IF T > STACKSIZE THEN PS := STKCHK ELSE S[T].R := RCONST[IR.Y]
     END ;
  64,65: BEGIN T := T + 1; IF T > STACKSIZE THEN PS := STKCHK
   ELSE S[T].C := CHR(IR.Y) END;
 26: BEGIN (*FLOAT*) H1 := T - IR.Y; S[H1].R := S[H1].I
     END ;
 27: BEGIN (*READ*)
       IF EOF(INPUT) THEN PS := REDCHK ELSE
          CASE IR.Y OF
           1: READ(S[S[T].I].I);
           2: READ(S[S[T].I].R);
           4: READ(S[S[T].I].C);
          END ;
       T := T-1
     END ;
 28: BEGIN (*WRITE STRING*)
       H1 := S[T].I; H2 := IR.Y; T := T-1;
       CHRCNT := CHRCNT+H1; IF CHRCNT > LINELENG THEN PS := LNGCHK;
       REPEAT WRITE(STAB[H2]); H1 := H1-1; H2 := H2+1
       UNTIL H1 = 0
     END ;
 29: BEGIN (*WRIT1*)
       CHRCNT := CHRCNT + FLD[IR.Y];
       IF CHRCNT > LINELENG THEN PS := LNGCHK ELSE
       CASE IR.Y OF
        1: WRITE(S[T].I: FLD[1]);
        2: WRITE(S[T].R: FLD[2]);
        3: WRITE(S[T].B: FLD[3]);
   4: WRITE(S[T].C);
       END ;
       T := T-1
     END ;
 30: BEGIN (*WRITE2*)
       CHRCNT := CHRCNT + S[T].I;
       IF CHRCNT > LINELENG THEN PS := LNGCHK ELSE
       CASE IR.Y OF
        1: WRITE(S[T-1].I: S[T].I);
        2: WRITE(S[T-1].R: S[T].I);
        3: WRITE(S[T-1].B: S[T].I);
        4: WRITE(S[T-1].C: S[T].I);
       END ;
       T := T-2
     END ;
 31: PS := FIN;
 32: BEGIN (*EXIT PROCEDURE*)
       T := B-1; PC := S[B+1].I; B := S[B+3].I
     END ;
 33: BEGIN (*EXIT FUNCTION*)
       T := B; PC := S[B+1].I; B := S[B+3].I
     END ;
 34: S[T] := S[S[T].I];
 35: S[T].B := NOT S[T].B;
 36: S[T].I := - S[T].I;
 66: S[T].R := - S[T].R;
 37: BEGIN CHRCNT := CHRCNT + S[T-1].I;
       IF CHRCNT > LINELENG THEN PS := LNGCHK ELSE
##        WRITE(S[T-2].R: S[T-1].I ": S[T].I");
       T := T-3
     END ;
 38: BEGIN (*STORE*) S[S[T-1].I] := S[T]; T := T-2
     END ;
 39: BEGIN T := T-1; S[T].B := S[T].R = S[T+1].R
     END ;
 40: BEGIN T := T-1; S[T].B := S[T].R <> S[T+1].R
     END ;
 41: BEGIN T := T-1; S[T].B := S[T].R < S[T+1].R
     END ;
 42: BEGIN T := T-1; S[T].B := S[T].R <= S[T+1].R
     END ;
 43: BEGIN T := T-1; S[T].B := S[T].R > S[T+1].R
     END ;
 44: BEGIN T := T-1; S[T].B := S[T].R >= S[T+1].R
     END ;
 45: BEGIN T := T-1; S[T].B := S[T].I = S[T+1].I
     END ;
 46: BEGIN T := T-1; S[T].B := S[T].I <> S[T+1].I
     END ;
 47: BEGIN T := T-1; S[T].B := S[T].I < S[T+1].I
     END ;
 48: BEGIN T := T-1; S[T].B := S[T].I <= S[T+1].I
     END ;
 49: BEGIN T := T-1; S[T].B := S[T].I > S[T+1].I
     END ;
 50: BEGIN T := T-1; S[T].B := S[T].I >= S[T+1].I
     END ;
 51: BEGIN T := T-1; S[T].B := S[T].B OR S[T+1].B
     END ;
 52: BEGIN T := T-1; S[T].I := S[T].I + S[T+1].I
     END ;
 53: BEGIN T := T-1; S[T].I := S[T].I - S[T+1].I
     END ;
 54: BEGIN T := T-1; S[T].R := S[T].R + S[T+1].R;
     END ;
 55: BEGIN T := T-1; S[T].R := S[T].R - S[T+1].R;
     END ;
 56: BEGIN T := T-1; S[T].B := S[T].B AND S[T+1].B
     END ;
 57: BEGIN T := T-1; S[T].I := S[T].I * S[T+1].I
     END ;
 58: BEGIN T := T-1;
       IF S[T+1].I = 0 THEN PS := DIVCHK ELSE
         S[T].I := S[T].I DIV S[T+1].I
     END ;
 59: BEGIN T := T-1;
       IF S[T+1].I = 0 THEN PS := DIVCHK ELSE
         S[T].I := S[T].I MOD S[T+1].I
     END ;
 60: BEGIN T := T-1; S[T].R := S[T].R * S[T+1].R;
     END ;
 61: BEGIN T := T-1;
   IF S[T+1].R = 0.0 THEN PS := DIVCHK ELSE S[T].R := S[T].R / S[T+1].R;
     END ;
 62: IF EOF(INPUT) THEN PS := REDCHK ELSE READLN(INPUT);
 63: BEGIN WRITELN; LNCNT := LNCNT + 1; CHRCNT := 0;
        IF LNCNT > LINELIMIT THEN PS := LINCHK
     END
    END (*CASE*) ;
  UNTIL PS <> RUN;

  IF PS <> FIN THEN
  BEGIN WRITELN;
    WRITE('0HALT AT', PC:5, ' BECAUSE OF ');
    CASE PS OF
      CASCHK: WRITELN('UNDEFINED CASE');
      DIVCHK: WRITELN('DIVISION BY 0');
      INXCHK: WRITELN('INVALID INDEX');
      STKCHK: WRITELN('STORAGE OVERFLOW');
      LINCHK: WRITELN('TOO MUCH OUTPUT');
      LNGCHK: WRITELN('LINE TOO LONG');
      REDCHK: WRITELN('READING PAST END OF FILE');
    END ;
    H1 := B; BLKCNT := 10;   (*POST MORTEM DUMP*)
    REPEAT WRITELN; BLKCNT := BLKCNT - 1;
      IF BLKCNT = 0 THEN H1 := 0; H2 := S[H1+4].I;
      IF H1<>0 THEN
        WRITELN(' ', TAB[H2].NAME, ' CALLED AT', S[H1+1].I: 5);
      H2 := BTAB[TAB[H2].REF].LAST;
      WHILE H2 <> 0 DO
      WITH TAB[H2] DO
      BEGIN IF OBJ = VARIABLE THEN
            IF TYP IN STANTYPS THEN
            BEGIN WRITE('    ', NAME, ' = ');
              IF NORMAL THEN H3 := H1+ADR ELSE H3 := S[H1+ADR].I;
              CASE TYP OF
               INTS:  WRITELN(S[H3].I);
               REALS: WRITELN(S[H3].R);
               BOOLS: WRITELN(S[H3].B);
               CHARS: WRITELN(S[H3].C);
              END
            END ;
            H2 := LINK
      END ;
      H1 := S[H1+3].I
    UNTIL H1 < 0;
  END ;
  WRITELN; WRITELN(OCNT, ' STEPS')
END (*INTERPRET*) ;
(* EOF                                                                        *)

BEGIN WRITELN;
   KEY[ 1] := 'AND       '; KEY[ 2] := 'ARRAY     ';
   KEY[ 3] := 'BEGIN     '; KEY[ 4] := 'CASE      ';
   KEY[ 5] := 'CONST     '; KEY[ 6] := 'DIV       ';
   KEY[ 8] := 'DOWNTO    '; KEY[ 7] := 'DO        ';
   KEY[ 9] := 'ELSE      '; KEY[10] := 'END       ';
   KEY[11] := 'FOR       '; KEY[12] := 'FUNCTION  ';
   KEY[13] := 'IF        '; KEY[14] := 'MOD       ';
   KEY[15] := 'NOT       '; KEY[16] := 'OF        ';
   KEY[17] := 'OR        '; KEY[18] := 'PROCEDURE ';
   KEY[19] := 'PROGRAM   '; KEY[20] := 'RECORD    ';
   KEY[21] := 'REPEAT    '; KEY[22] := 'THEN      ';
   KEY[23] := 'TO        '; KEY[24] := 'TYPE      ';
   KEY[25] := 'UNTIL     '; KEY[26] := 'VAR       ';
   KEY[27] := 'WHILE     ';
   KSY[ 1] := ANDSY;        KSY[ 2] := ARRAYSY;
   KSY[ 3] := BEGINSY;      KSY[ 4] := CASESY;
   KSY[ 5] := CONSTSY;      KSY[ 6] := IDIV;
   KSY[ 8] := DOWNTOSY;     KSY[ 7] := DOSY;
   KSY[ 9] := ELSESY;       KSY[10] := ENDSY;
   KSY[11] := FORSY;        KSY[12] := FUNCSY;
   KSY[13] := IFSY;         KSY[14] := IMOD;
   KSY[15] := NOTSY;        KSY[16] := OFSY;
   KSY[17] := ORSY;         KSY[18] := PROCSY;
   KSY[19] := PROGRAMSY;    KSY[20] := RECORDSY;
   KSY[21] := REPEATSY;     KSY[22] := THENSY;
   KSY[23] := TOSY;         KSY[24] := TYPESY;
   KSY[25] := UNTILSY;      KSY[26] := VARSY;
   KSY[27] := WHILESY;
   SPS['+'] := PLUS;        SPS['-'] := MINUS;
   SPS['*'] := TIMES;       SPS['/'] := RDIV;
   SPS['('] := LPARENT;     SPS[')'] := RPARENT;
   SPS['='] := EQL;         SPS[','] := COMMA;
   SPS['['] := LBRACK;      SPS[']'] := RBRACK;
   SPS['#'] := NEQ;         SPS['&'] := ANDSY;
   SPS[';'] := SEMICOLON;
  CONSTBEGSYS := [PLUS,MINUS,INTCON,REALCON,CHARCON,IDENT];
  TYPEBEGSYS := [IDENT,ARRAYSY,RECORDSY];
  BLOCKBEGSYS := [CONSTSY,TYPESY,VARSY,PROCSY,FUNCSY,BEGINSY];
  FACBEGSYS := [INTCON,REALCON,CHARCON,IDENT,LPARENT,NOTSY];
  STATBEGSYS := [BEGINSY,IFSY,WHILESY,REPEATSY,FORSY,CASESY];
  STANTYPS := [NOTYP,INTS,REALS,BOOLS,CHARS];
  LC := 0; LL := 0; CC := 0; CH := ' ';
  ERRPOS := 0; ERRS := []; INSYMBOL;
  T := -1; A := 0; B := 1; SX := 0; C2 := 0;
  DISPLAY[0] := 1;
  IFLAG := FALSE; OFLAG := FALSE;
  IF SY <> PROGRAMSY THEN ERROR(3) ELSE
  BEGIN INSYMBOL;
    IF SY <> IDENT THEN ERROR(2) ELSE
    BEGIN PROGNAME := ID; INSYMBOL;
      IF SY <> LPARENT THEN ERROR(9) ELSE
      REPEAT INSYMBOL;
   IF SY <> IDENT THEN ERROR(2) ELSE
   BEGIN IF ID = 'INPUT     ' THEN IFLAG := TRUE ELSE
         IF ID = 'OUTPUT    ' THEN OFLAG := TRUE ELSE ERROR(0);
      INSYMBOL
   END
      UNTIL SY <> COMMA;
      IF SY = RPARENT THEN INSYMBOL ELSE ERROR(4);
      IF NOT OFLAG THEN ERROR(20)
    END
  END ;
  ENTER('          ', VARIABLE, NOTYP, 0);  (*SENTINEL*)
  ENTER('FALSE     ', KONSTANT, BOOLS, 0);
  ENTER('TRUE      ', KONSTANT, BOOLS, 1);
  ENTER('REAL      ', TYPE1, REALS, 1);
  ENTER('CHAR      ', TYPE1, CHARS, 1);
  ENTER('BOOLEAN   ', TYPE1, BOOLS, 1);
  ENTER('INTEGER   ', TYPE1, INTS , 1);
  ENTER('ABS       ', FUNKTION, REALS,0);
  ENTER('SQR       ', FUNKTION, REALS,2);
  ENTER('ODD       ', FUNKTION, BOOLS,4);
  ENTER('CHR       ', FUNKTION, CHARS,5);
  ENTER('ORD       ', FUNKTION, INTS, 6);
  ENTER('SUCC      ', FUNKTION, CHARS,7);
  ENTER('PRED      ', FUNKTION, CHARS,8);
  ENTER('ROUND     ', FUNKTION, INTS, 9);
  ENTER('TRUNC     ', FUNKTION, INTS, 10);
  ENTER('SIN       ', FUNKTION, REALS, 11);
  ENTER('COS       ', FUNKTION, REALS, 12);
  ENTER('EXP       ', FUNKTION, REALS, 13);
  ENTER('LN        ', FUNKTION, REALS, 14);
  ENTER('SQRT      ', FUNKTION, REALS, 15);
  ENTER('ARCTAN    ', FUNKTION, REALS, 16);
  ENTER('EOF       ', FUNKTION, BOOLS, 17);
  ENTER('EOLN      ', FUNKTION, BOOLS, 18);
  ENTER('READ      ', PROZEDURE, NOTYP, 1);
  ENTER('READLN    ', PROZEDURE, NOTYP, 2);
  ENTER('WRITE     ', PROZEDURE, NOTYP, 3);
  ENTER('WRITELN   ', PROZEDURE, NOTYP, 4);
  ENTER('          ', PROZEDURE, NOTYP, 0);
  WITH BTAB[1] DO
    BEGIN LAST := T; LASTPAR := 1; PSIZE := 0; VSIZE := 0
    END ;

  BLOCK(BLOCKBEGSYS+STATBEGSYS, FALSE, 1);
  IF SY <> PERIOD THEN ERROR(22);
  EMIT(31);  (*HALT*)
  IF BTAB[2].VSIZE > STACKSIZE THEN ERROR(49);
  IF PROGNAME = 'TEST0     ' THEN PRINTTABLES;

  IF ERRS = [] THEN
  BEGIN
(* { MUST BLOCK ALL OF THIS OUT FOR NOW.                                      *)
    IF IFLAG THEN
##  BEGIN  (* GETSEG(INPUT); *)
      IF EOF(INPUT) THEN WRITELN(' INPUT DATA MISSING') ELSE
      BEGIN WRITELN(' (EOR)'); (*COPY INPUT DATA*)
##(* WHILE NOT EOF(INPUT) DO
## BEGIN WRITE(' ');
##   WHILE NOT EOLN(INPUT) DO
##     BEGIN READ(CH); WRITE(CH)
##     END ;
##   WRITELN; READ(CH)
## END ;
## GETSEG(INPUT,0)  *)
      END
    END ;
(*  ALL THIS BECAUSE OF SEGMENTED FILE REPOSITIONING×× }                      *)
    WRITELN(' (EOF)');
    INTERPRET
  END
  ELSE ERRORMSG;
99:
END .
(* EOF                                                                        *)
