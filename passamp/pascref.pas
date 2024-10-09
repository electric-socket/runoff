PROGRAM PASCREF(INPUT,OUTPUT);      (*$D-,L+,M+   N.WIRTH  2.7.75  *)   00010000
(*CROSS REFERENCE GENERATOR FOR PASCAL PROGRAMS*)                       00020000
(*QUADRATIC QUOTIENT HASH METHOD*)                                      00030000
(*MODIFIED SLIGHTLY BY A. MICKEL 75/12/08 AND D. LALIBERTE              00040000
  78/03/15 TO PRODUCE PROCEDURE LIST AND SKIP COMPILER TITLE*)          00050000
(* PASCAL-6000 DEPENDENT CONSTRUCTS CHANGED. S.HAZEGHI,  02-15-79 *)    00060000
(* 6/86 - CORRECTIONS FOR COMPILER OUTPUT PROCESSING (LPP, LINE#)   *)  00070000
(*             INCREASED HASH TABLE SIZE; FIXED BRACED COMMENTS     *)  00080000
(*          G. POSTPISCHIL, EXPERT SYSTEM PROGRAMMING, INC. / 86180 *)  00090000
(*$D-  TESTS OFF*)                                                      00100000
LABEL 99;                                                               00110000
CONST P = 8999;     (*SIZE OF HASH TABLE*)                 (* 86180 *)  00120000
  NK = 33;          (*NO. OF KEYWORDS*)                                 00130000
  KLN = 10;         (*KEYLENGTH*)                                       00140000
  LPPG = 55;        (*NO. OF LINES PER PAGE*)                           00150000
  LLMAX = 132;      (*LINE LENGTH DEFAULT MAX*)                         00160000
  LLMIN = 72;       (*LINE LENGTH MINIMUM*)                             00170000
  MAXN = 10000;     (*MAX NO. OF LINES*)                                00180000
  DGPN =  6;        (*NO. OF DIGITS PER NUMBER*)                        00190000
  LITL = 3;         (*NUMBER OF LINES IN COMPILER TITLE*)               00200000
  ADDRWIDTH = 11;        (*NUMBER OF DIGITS IN CODE ADDRESS / 86180 *)  00210000
  EMPTY = '          ';                                                 00220000
  STARS = ' *****';                                                     00230000
TYPE INDEX = 0..P;                                                      00240000
  ALFA = PACKED ARRAY [1..KLN] OF CHAR;                                 00250000
  REF = @ITEM;                                                          00260000
  WORD = RECORD KEY: ALFA;                                              00270000
           FIRST: REF;                                                  00280000
         END ;                                                          00290000
  ITEM = PACKED RECORD                                                  00300000
           LNO: 0..MAXN;                                                00310000
           NEXT: REF                                                    00320000
         END ;                                                          00330000
  PROCREF = @PROC;  (*PROCEDURE OR FUNCTION REFERENCE*)                 00340000
  PROC = PACKED RECORD                                                  00350000
           NAME: ALFA;                                                  00360000
           LNO: 0..MAXN;                                                00370000
           NEXT: PROCREF                                                00380000
         END ;                                                          00390000
VAR I: INDEX;                                                           00400000
  K: INTEGER;                                                           00410000
  M: INTEGER;       (*NO. OF LINES ON PAGE*)                            00420000
  N: INTEGER;       (*NO. OF LINES INPUT*)                              00430000
  LN: INTEGER;      (*CURRENT LINE NUMBER*)                             00440000
  OLDLN: INTEGER;               (* LINE NUMBER OF LAST LINE / 86180 *)  00450000
  LLNGOUT: INTEGER; (*LINE LENGTH FOR OUTPUT*)                          00460000
  LLNGIN: INTEGER;  (*LINE LENGTH FOR INPUT*)                           00470000
  CCOUNT: INTEGER;  (*CHARACTER COUNT IN LINE*)                         00480000
  NOPL: INTEGER;    (*NO. OF LINE-NUMBERS PER LINE*)                    00490000
  ID: RECORD CASE BOOLEAN OF                                            00500000
             FALSE: (A: ALFA);                                          00510000
             TRUE:  (ORD: INTEGER)                                      00520000
      END ;                                                             00530000
  KEY: ARRAY [1..NK] OF ALFA;                                           00540000
  PROCORFUNC,                                                           00550000
  COMPILERLISTING,                                                      00560000
  LINENUMBERS: BOOLEAN;                                                 00570000
  C1, C2, C3 : INTEGER;    (* ID, OCCUR, PROC. COUNTERS *)              00580000
  FIRSTPROC,                                                            00590000
  PROCPTR: PROCREF; (*POINTERS TO CHAIN OF PROCEDURES*)                 00600000
  T: ARRAY [INDEX] OF WORD;    (*HASH TABLE*)                           00610000
                                                                        00620000
                                                                        00630000
   FUNCTION LETTER(C: CHAR): BOOLEAN;                                   00640000
                                                                        00650000
     BEGIN                                                              00660000
     LETTER := (('A' <= C) AND (C <= 'Z')) OR                           00670000
               (('a' <= C) AND (C <= 'i')) OR                           00680000
               (('j' <= C) AND (C <= 'r')) OR                           00690000
               (('s' <= C) AND (C <= 'z')) ;                            00700000
     END ;                                                              00710000
                                                                        00720000
   FUNCTION DIGIT(C: CHAR): BOOLEAN ;                                   00730000
                                                                        00740000
     BEGIN                                                              00750000
     DIGIT := ('0' <= C) AND (C <= '9') ;                               00760000
     END ;                                                              00770000
                                                                        00780000
   FUNCTION SPECIAL(C: CHAR): BOOLEAN;                                  00790000
     BEGIN  SPECIAL := (C = '$') OR (C = '_')  END ;                    00800000
                                                                        00810000
FUNCTION NOKEY: BOOLEAN;                                                00820000
   VAR I,J,K: INTEGER;                                                  00830000
BEGIN I := 1; J := NK;   (*BINARY SEARCH*)                              00840000
  REPEAT K := (I+J) DIV 2;                                              00850000
    IF KEY[K] <= ID.A THEN I := K+1 ELSE J := K-1                       00860000
  UNTIL I > J;                                                          00870000
  IF J = 0 THEN NOKEY := TRUE ELSE                                      00880000
    NOKEY := KEY[J] <> ID.A                                             00890000
END (*NOKEY*) ;                                                         00900000
                                                                        00910000
PROCEDURE COUNTLINE;                                                    00920000
BEGIN                                                                   00930000
  IF M >= LPPG THEN                                                     00940000
    BEGIN PAGE(OUTPUT); WRITELN(OUTPUT); """WRITELN(OUTPUT); """        00950000
      M := 0                                                            00960000
    END;                                                                00970000
  M := M + 1                                                            00980000
END (*COUNTLINE*) ;                                                     00990000
                                                                        01000000
PROCEDURE ADVANCE;                                                      01010000
BEGIN                                                                   01020000
  WRITE(OUTPUT,INPUT@); GET(INPUT);                                     01030000
  CCOUNT := CCOUNT + 1;                                                 01040000
  IF CCOUNT = LLNGIN THEN                                               01050000
    WHILE NOT EOLN(INPUT) DO                                            01060000
      BEGIN WRITE(OUTPUT,INPUT@); GET(INPUT);                           01070000
      END                                                               01080000
END (*ADVANCE*);                                                        01090000
                                                                        01100000
PROCEDURE SPACE(J: INTEGER);                                            01110000
BEGIN                                                                   01120000
  REPEAT J := J-1; WRITELN(OUTPUT); COUNTLINE                           01130000
  UNTIL J = 0                                                           01140000
END (*SPACE*) ;                                                         01150000
                                                                        01160000
PROCEDURE SKIPCOMPILERTITLE;                               (* 86180 *)  01170000
                                                           (* 86180 *)  01180000
  VAR I: INTEGER;                                          (* 86180 *)  01190000
BEGIN                                                      (* 86180 *)  01200000
  COMPILERLISTING := INPUT@ = '1';                         (* 86180 *)  01210000
  PAGE(OUTPUT);                                            (* 86180 *)  01220000
  IF COMPILERLISTING THEN                                  (* 86180 *)  01230000
  BEGIN I := 0; GET(INPUT);                                (* 86180 *)  01240000
    WHILE I < LITL DO                                      (* 86180 *)  01250000
      BEGIN I := I + 1;                                    (* 86180 *)  01260000
        WHILE NOT EOLN(INPUT) DO                           (* 86180 *)  01270000
          ADVANCE;                                         (* 86180 *)  01280000
        READLN; WRITELN(OUTPUT);                           (* 86180 *)  01290000
      END;                                                 (* 86180 *)  01300000
    """COUNTLINE;"""      (* GENERATES INCORRECT LINES/PAGE / 86180 *)  01310000
         M := 0;(* SET LINE COUNT CORRECTLY - WE JUST EJECTED/86180 *)  01320000
    LINENUMBERS := TRUE                                    (* 86180 *)  01330000
  END ELSE                                                 (* 86180 *)  01340000
  BEGIN WRITELN(OUTPUT); WRITELN(OUTPUT);                  (* 86180 *)  01350000
    LINENUMBERS := """INPUT@ IN ['0'..'9']""" DIGIT(INPUT@)(* 86180 *)  01360000
  END;                                                     (* 86180 *)  01370000
END (*SKIPCOMPILERTITLE*) ;                                (* 86180 *)  01380000
                                                                        01390000
PROCEDURE HEADSHOP;                                        (* 86180 *)  01400000
BEGIN                                                      (* 86180 *)  01410000
  IF COMPILERLISTING AND NOT EOLN AND (INPUT@ = '1')       (* 86180 *)  01420000
    THEN SKIPCOMPILERTITLE                                 (* 86180 *)  01430000
END; (* HEADSHOP *)                                        (* 86180 *)  01440000
                                                           (* 86180 *)  01450000
PROCEDURE FLUSHLINE;                                       (* 86180 *)  01460000
BEGIN                                                      (* 86180 *)  01470000
  WHILE NOT EOLN DO                                        (* 86180 *)  01480000
    ADVANCE;                                               (* 86180 *)  01490000
END; (* FLUSHLINE *)                                       (* 86180 *)  01500000
                                                                        01510000
PROCEDURE BLANKSKIP;                                       (* 86180 *)  01520000
BEGIN                                                      (* 86180 *)  01530000
  IF NOT EOLN AND (INPUT@ = ' ') THEN                      (* 86180 *)  01540000
    ADVANCE;                                               (* 86180 *)  01550000
END; (* BLANKSKIP *)                                       (* 86180 *)  01560000
                                                                        01570000
PROCEDURE NEWLINE;                                                      01580000
BEGIN CCOUNT := 0;                                                      01590000
  HEADSHOP;      (* CHECK FOR COMPILER TITLE *)            (* 86180 *)  01600000
  OLDLN := LN;                         (* SAVE PRIOR LINE # / 86180 *)  01610000
  LN := 0;                                                              01620000
  IF N < MAXN THEN                                                      01630000
  BEGIN COUNTLINE;  N := N + 1;                                         01640000
    IF COMPILERLISTING THEN                                             01650000
       BEGIN                                                            01660000
       WHILE NOT EOLN AND (INPUT@ <>  ')') DO              (* 86180 *)  01670000
         BEGIN                                             (* 86180 *)  01680000
         FOR I := 1 TO 9 DO                                (* 86180 *)  01690000
           BLANKSKIP;                                      (* 86180 *)  01700000
         IF NOT DIGIT(INPUT@) THEN FLUSHLINE;              (* 86180 *)  01710000
         WHILE DIGIT(INPUT@) DO                            (* 86180 *)  01720000
             BEGIN LN := 10*LN + ORD(INPUT@) - ORD('0');   (* 86180 *)  01730000
             ADVANCE;                                      (* 86180 *)  01740000
             END;                                          (* 86180 *)  01750000
         FOR I := 1 TO ADDRWIDTH DO                        (* 86180 *)  01760000
           IF NOT EOLN THEN ADVANCE;                       (* 86180 *)  01770000
       END;                                                (* 86180 *)  01780000
         IF NOT EOLN AND (INPUT@ <> ')') THEN FLUSHLINE    (* 86180 *)  01790000
                            ELSE ADVANCE;                  (* 86180 *)  01800000
       END                                                 (* 86180 *)  01810000
    ELSE WRITE(OUTPUT,' ');                                             01820000
    IF LINENUMBERS THEN                                                 01830000
      BEGIN                                                             01840000
      WHILE DIGIT(INPUT@) DO                                            01850000
"""   WHILE INPUT@ IN ['0'..'9'] DO   """                               01860000
        BEGIN LN := 10*LN + ORD(INPUT@) - ORD('0');                     01870000
          ADVANCE;                                                      01880000
        END;                                                            01890000
      END                                                               01900000
    ELSE BEGIN                                                          01910000
      LN := N;  WRITE(OUTPUT,LN:6, ') ')                                01920000
      END                                                               01930000
    END                                                                 01940000
  ELSE BEGIN                                                            01950000
    WRITELN(STARS, ' TEXT TOO LONG', STARS);                            01960000
    "GOTO 99"  EXIT(99);                                                01970000
    END;                                                                01980000
    IF LN = 0 THEN LN := OLDLN         (* REL. TO PRIOR LINE /86180 *)  01990000
END (*NEWLINE*) ;                                                       02000000
                                                                        02010000
PROCEDURE SEARCH;   (*MODULO P HASH SEARCH*)                            02020000
  VAR H,D: INDEX;                                                       02030000
      X: REF; F: BOOLEAN;                                               02040000
      K: INTEGER;                                                       02050000
BEGIN  I := ABS(ID.ORD);  H := I MOD P;                                 02060000
  F := FALSE; D := 1;                                                   02070000
  NEW(X); X@.LNO := LN;                                                 02080000
  REPEAT                                                                02090000
    IF T[H].KEY = ID.A THEN                                             02100000
    BEGIN (*FOUND*) F := TRUE;                                          02110000
      X@.NEXT := T[H].FIRST; T[H].FIRST := X;                           02120000
    END ELSE                                                            02130000
    IF T[H].KEY = EMPTY THEN                                            02140000
    BEGIN (*NEW ENTRY*) F := TRUE;                                      02150000
      T[H].KEY := ID.A;                                                 02160000
      T[H].FIRST := X; X@.NEXT := NIL                                   02170000
    END ELSE                                                            02180000
    BEGIN (*COLLISION*) H := H+D; D := D+2;                             02190000
      IF H >= P THEN H := H-P;                                          02200000
      IF D = P THEN                                                     02210000
        BEGIN WRITELN(OUTPUT); WRITELN(STARS,' TABLE FULL',STARS);      02220000
        """GOTO 99"""   EXIT(99);                                       02230000
        END                                                             02240000
    END                                                                 02250000
  UNTIL F                                                               02260000
END (*SEARCH*) ;                                                        02270000
                                                                        02280000
PROCEDURE SORT(MIN, MAX: INTEGER);                                      02290000
                                                                        02300000
(* QUICKSORT WITH BOUNDED RECURSION DEPTH *)                            02310000
(* REQUIRES MIN <= MAX *)                                               02320000
                                                                        02330000
   VAR                                                                  02340000
         LOW,                                                           02350000
        HIGH: INDEX;                                                    02360000
      MIDKEY: ALFA;                                                     02370000
        TEMP: WORD;                                                     02380000
                                                                        02390000
   BEGIN                                                                02400000
      REPEAT (*PICK SPLIT POINT*)                                       02410000
         MIDKEY := T[(MIN + MAX) DIV 2].KEY;                            02420000
         LOW := MIN;                                                    02430000
         HIGH := MAX;                                                   02440000
         REPEAT (*PARTITION*)                                           02450000
            WHILE T[LOW].KEY < MIDKEY DO                                02460000
               LOW := LOW + 1;                                          02470000
            WHILE T[HIGH].KEY > MIDKEY DO                               02480000
               HIGH := HIGH - 1;                                        02490000
            IF LOW <= HIGH THEN                                         02500000
               BEGIN                                                    02510000
                  TEMP := T[LOW];                                       02520000
                  T[LOW] := T[HIGH];                                    02530000
                  T[HIGH] := TEMP;                                      02540000
                  LOW := LOW + 1;                                       02550000
                  HIGH := HIGH - 1                                      02560000
               END;                                                     02570000
         UNTIL LOW > HIGH;                                              02580000
                                                                        02590000
         (*RECURSIVELY SORT SHORTER SUB-SEGMENT*)                       02600000
         IF HIGH - MIN < MAX - LOW                                      02610000
         THEN                                                           02620000
            BEGIN                                                       02630000
               IF MIN < HIGH THEN                                       02640000
                  SORT(MIN, HIGH);                                      02650000
               MIN := LOW                                               02660000
            END                                                         02670000
         ELSE                                                           02680000
            BEGIN                                                       02690000
               IF LOW < MAX THEN                                        02700000
                  SORT(LOW, MAX);                                       02710000
               MAX := HIGH                                              02720000
            END                                                         02730000
      UNTIL MAX <= MIN                                                  02740000
   END (*SORT*);                                                        02750000
                                                                        02760000
                                                                        02770000
PROCEDURE NOTEPROC;   (*NOTE INSTANCE OF PROCEDURE OR FUNCTION*)        02780000
  VAR P: PROCREF;                                                       02790000
BEGIN PROCORFUNC := FALSE;                                              02800000
  NEW(P); PROCPTR@.NEXT := P;                                           02810000
  P@.NAME := ID.A; P@.LNO := LN; P@.NEXT := NIL;                        02820000
  PROCPTR := P                                                          02830000
END (*NOTEPROC*) ;                                                      02840000
                                                                        02850000
PROCEDURE PRINTWORD(W: WORD);                                           02860000
  VAR L: INTEGER; X,Y,Z: REF;                                           02870000
BEGIN COUNTLINE; WRITE(OUTPUT,' ', W.KEY);                              02880000
  X := W.FIRST; Y := X@.NEXT; X@.NEXT := NIL;                           02890000
  WHILE Y <> NIL DO                                                     02900000
    BEGIN Z := Y@.NEXT; Y@.NEXT := X; X := Y; Y := Z                    02910000
    END ;                                                               02920000
  L := 0;                                                               02930000
  REPEAT                                                                02940000
    IF L = NOPL THEN                                                    02950000
      BEGIN L := 0; WRITELN(OUTPUT); COUNTLINE; WRITE(OUTPUT,' ', EMPTY)02960000
      END;                                                              02970000
    L := L+1; WRITE(OUTPUT,X@.LNO: DGPN); X := X@.NEXT; C2 := C2+1;     02980000
  UNTIL X = NIL;                                                        02990000
  WRITELN(OUTPUT);                                                      03000000
END (*PRINTWORD*) ;                                                     03010000
                                                                        03020000
PROCEDURE PRINTTABLE;                                                   03030000
  VAR I,N: INDEX;                                                       03040000
BEGIN N := 0;    (*COMPRESS TABLE*)                                     03050000
  FOR I := 0 TO P-1 DO                                                  03060000
    IF T[I].KEY <> EMPTY THEN                                           03070000
      BEGIN T[N] := T[I]; N := N+1                                      03080000
      END ;                                                             03090000
  IF N > 0 THEN SORT(0,N-1);                                            03100000
  NOPL := (LLNGOUT-KLN-1) DIV DGPN;                                     03110000
# M := LPPG; SPACE(1); WRITELN(' CROSS REFERENCE OF IDENTIFIERS,',      03120000
            ' LABEL DECLARATIONS AND GOTO STATEMENTS:');                03130000
# COUNTLINE; SPACE(1);  C1 := N;  C2 := 0;                              03140000
  FOR I := 0 TO N-1 DO PRINTWORD(T[I])                                  03150000
END (*PRINTTABLE*) ;                                                    03160000
                                                                        03170000
PROCEDURE PRINTPROCS;                                                   03180000
BEGIN SPACE(2); COUNTLINE;                                              03190000
  WRITELN(' LIST OF PROCEDURES AND FUNCTIONS:');                        03200000
# C3 := 0;  COUNTLINE;  SPACE(1);                                       03210000
  PROCPTR := FIRSTPROC@.NEXT;                                           03220000
  WHILE PROCPTR <> NIL DO                                               03230000
    BEGIN WITH PROCPTR@ DO WRITELN(NAME:24,LNO:10);                     03240000
#     C3 := C3+1;  COUNTLINE; PROCPTR := PROCPTR@.NEXT                  03250000
    END;                                                                03260000
# SPACE(2);                                                             03270000
# WRITELN(OUTPUT, ' # OF IDENTIFIERS: ', C1:1, ',  # OF OCCURENCES: ',  03280000
#                 C2:1, ',  # OF PROCEDURES: ', C3:1, '.');             03290000
END (*PRINTPROCS*) ;                                                    03300000
                                                                        03310000
PROCEDURE INITIALIZE;                                                   03320000
  TYPE SETTING = PACKED RECORD                                          03330000
                   CASE SWITCH: BOOLEAN OF                              03340000
                     TRUE: (ONOFF: CHAR);                               03350000
                     FALSE: (SIZE: 0..999999)                           03360000
                   END;                                                 03370000
  VAR S: SETTING;                                                       03380000
  FUNCTION OPTION(NAME: CHAR; VAR S: SETTING): BOOLEAN;                 03390000
    EXTERNAL;                                                           03400000
BEGIN N := 0; M := 0;                                                   03410000
  LLNGIN := LLMAX; LLNGOUT := LLMAX;                                    03420000
"""IF OPTION('U',S) THEN                                                03430000
    IF S.SWITCH AND (S.ONOFF = '+')                                     03440000
      THEN LLNGIN := LLMIN;                                             03450000
  IF OPTION('W',S) THEN                                                 03460000
    IF S.SWITCH AND (S.ONOFF = '+')                                     03470000
      THEN LLNGOUT := LLMIN;    """                                     03480000
  FOR I := 0 TO P-1 DO T[I].KEY := EMPTY;                               03490000
  NEW(PROCPTR); FIRSTPROC := PROCPTR; PROCPTR@.NEXT := NIL;             03500000
  PROCORFUNC := TRUE;   (*TO GET P R O G R A M NAME IN PROCEDURE INDEX*)03510000
  KEY[ 1] := 'AND       '; KEY[ 2] := 'ARRAY     ';                     03520000
  KEY[ 3] := 'BEGIN     '; KEY[ 4] := 'CASE      ';                     03530000
  KEY[ 5] := 'CONST     '; KEY[ 6] := 'DIV       ';                     03540000
  KEY[ 7] := 'DOWNTO    '; KEY[ 8] := 'DO        ';                     03550000
  KEY[ 9] := 'ELSE      '; KEY[10] := 'END       ';                     03560000
  KEY[11] := 'FILE      '; KEY[12] := 'FOR       ';                     03570000
  KEY[13] := 'FUNCTION  '; KEY[14] := 'IF        ';                     03580000
  KEY[15] := 'IN        '; KEY[16] := 'MOD       ';                     03590000
  KEY[17] := 'NIL       '; KEY[18] := 'NOT       ';                     03600000
  KEY[19] := 'OF        '; KEY[20] := 'OR        ';                     03610000
  KEY[21] := 'PACKED    '; KEY[22] := 'PROCEDURE ';                     03620000
  KEY[23] := 'PROGRAM   '; KEY[24] := 'RECORD    ';                     03630000
  KEY[25] := 'REPEAT    '; KEY[26] := 'SET       ';                     03640000
  KEY[27] := 'THEN      '; KEY[28] := 'TO        ';                     03650000
  KEY[29] := 'TYPE      '; KEY[30] := 'UNTIL     ';                     03660000
  KEY[31] := 'VAR       '; KEY[32] := 'WHILE     ';                     03670000
  KEY[33] := 'WITH      '                                               03680000
END (*INITIALIZE*) ;                                                    03690000
                                                                        03700000
PROCEDURE SCANANDLISTINPUT;                                             03710000
BEGIN                                                                   03720000
  WHILE NOT EOF(INPUT) DO                                               03730000
  BEGIN NEWLINE;                                                        03740000
    WHILE NOT EOLN(INPUT) DO                                            03750000
    CASE INPUT@ OF                                                      03760000
     'a','b','c','d','e','f','g','h','i','j','k','l','m',               03770000
     'n','o','p','q','r','s','t','u','v','w','x','y','z',               03780000
     'A','B','C','D','E','F','G','H','I','J','K','L','M',               03790000
     'N','O','P','Q','R','S','T','U','V','W','X','Y','Z':               03800000
      BEGIN K := 0; ID.A := EMPTY;                                      03810000
        REPEAT                                                          03820000
          IF K < KLN THEN                                               03830000
            BEGIN K := K+1; ID.A[K] := INPUT@                           03840000
            END;                                                        03850000
          ADVANCE                                                       03860000
"""     UNTIL NOT(INPUT@ IN ['A'..'Z', '0'..'9']);   """                03870000
        UNTIL NOT(LETTER(INPUT@) OR DIGIT(INPUT@) OR SPECIAL(INPUT@));  03880000
        IF NOKEY THEN                                                   03890000
        BEGIN SEARCH;                                                   03900000
          IF PROCORFUNC THEN NOTEPROC                                   03910000
        END ELSE                                                        03920000
        IF (ID.A = 'PROCEDURE ') OR (ID.A = 'FUNCTION  ') THEN          03930000
          PROCORFUNC := TRUE                                            03940000
      END;                                                              03950000
     '0','1','2','3','4','5','6','7','8','9':                           03960000
        REPEAT ADVANCE;                                                 03970000
"""     UNTIL NOT (INPUT@ IN ['B','E','0'..'9']);  """                  03980000
        UNTIL NOT DIGIT(INPUT@) ;                                       03990000
     '''':                                                              04000000
      BEGIN (*STRING*)                                                  04010000
        REPEAT ADVANCE;                                                 04020000
        UNTIL (INPUT@ = '''') OR EOLN(INPUT);                           04030000
        IF NOT EOLN(INPUT) THEN                                         04040000
          ADVANCE                                                       04050000
      END;                                                              04060000
#    '"':                                                               04070000
      BEGIN (*COMMENT*)                                                 04080000
        REPEAT ADVANCE;                                                 04090000
          WHILE EOLN(INPUT) DO                                          04100000
            BEGIN WRITELN(OUTPUT); GET(INPUT); NEWLINE                  04110000
            END                                                         04120000
        UNTIL INPUT@ = '"';                                             04130000
        ADVANCE                                                         04140000
      END;                                                              04150000
     '¯':                                                   (* 86180 *) 04160000
      BEGIN (*COMMENT*)                                     (* 86180 *) 04170000
        REPEAT ADVANCE;                                     (* 86180 *) 04180000
          WHILE EOLN(INPUT) DO                              (* 86180 *) 04190000
            BEGIN WRITELN(OUTPUT); GET(INPUT); NEWLINE      (* 86180 *) 04200000
            END                                             (* 86180 *) 04210000
        UNTIL INPUT@ = 'ò';                                 (* 86180 *) 04220000
        ADVANCE                                             (* 86180 *) 04230000
      END;                                                  (* 86180 *) 04240000
     '(':                                                               04250000
      BEGIN ADVANCE;                                                    04260000
        IF INPUT@ = '*' THEN                                            04270000
        BEGIN (*COMMENT*) ADVANCE;                                      04280000
          REPEAT                                                        04290000
            WHILE INPUT@ <> '*' DO                                      04300000
            BEGIN                                                       04310000
              IF EOLN(INPUT) THEN                                       04320000
                BEGIN GET(INPUT); WRITELN(OUTPUT); NEWLINE              04330000
                END ELSE                                                04340000
                ADVANCE                                                 04350000
            END ;                                                       04360000
            ADVANCE                                                     04370000
          UNTIL INPUT@ = ')';                                           04380000
          ADVANCE                                                       04390000
        END                                                             04400000
      END;                                                              04410000
     '+','-','*','/',')','$','=',' ',',','.','[',']',                   04420000
     ':','!','×','&','@','?','<','>','Ö','\','^',';','#','_','%','ò':   04430000
      ADVANCE                                                           04440000
    END (*CASE*) ;                                                      04450000
    WRITELN(OUTPUT); GET(INPUT)                                         04460000
  END ;                                                                 04470000
END (*SCANANDLISTINPUT*) ;                                              04480000
                                                                        04490000
BEGIN (*CROSSREF*)                                                      04500000
"""LINELIMIT(OUTPUT, MAXN);PAGE(OUTPUT)"""  ; INITIALIZE;               04510000
  IF NOT EOF(INPUT) THEN                                                04520000
  BEGIN SKIPCOMPILERTITLE;                                              04530000
    SCANANDLISTINPUT; """LINELIMIT(OUTPUT, MAXN); """                   04540000
    PRINTTABLE; PRINTPROCS                                              04550000
  END ELSE WRITELN(STARS,' NO PROGRAM FOUND TO CROSS REFERENCE',STARS); 04560000
99:END .                                                                04570000
