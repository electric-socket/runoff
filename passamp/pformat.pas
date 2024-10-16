 (*************************************************************
  *                                                           *
  *      P A S C A L   P R O G R A M   F O R M A T T E R      *
  *      -----------------------------------------------      *
  *                                                           *
  *             COPYRIGHT MICHAEL N. CONDICT 1975             *
  *                                                           *
  *************************************************************)
 PROGRAM FORMAT( (*PROG,*) OUTPUT, INPUT );

 CONST
    LASTPSYMBOLNAME = 40;
    BUFFERSIZE = 160;
    BUFFERSZP1 = 161;
    BUFFSZDIV10 = 16;
    ALFLEN = 10;

 TYPE
    chset = 0..63;
    STATMNTTYPES =
       (FORWITHWHILESTATEMENT, REPEATSTATEMENT, IFSTATEMENT,
        CASESTATEMENT, COMPOUNDSTATEMENT, OTHERSTATEMENT);
    SYMBOLS =
       (PROGRAMSYMBOL, COMMENT, BEGINSYMBOL, ENDSYMBOL,
        SEMICOLON, CONSTSYMBOL, TYPESYMBOL, RECORDSYMBOL,
        COLONSYMBOL, EQUALSYMBOL, PERIODSYMBOL, RANGE,
        CASESYMBOL, OTHERSYMBOL, IFSYMBOL, THENSYMBOL,
        ELSESYMBOL, DOSYMBOL, OFSYMBOL, FORSYMBOL, WITHSYMBOL,
        WHILESYMBOL, REPEATSYMBOL, UNTILSYMBOL, IDENTIFIER,
        VARSYMBOL, VALUESYMBOL, PROCEDSYMBOL, FUNCTIONSYMBOL,
        LEFTBRACKET, RIGHTBRACKET, COMMASYMBOL, LABELSYMBOL,
        LEFTPAREN, RIGHTPARENTH, UPARROW, ALPHAOPERATOR);
    WIDTH = 0 .. BUFFERSIZE;
    CHARTYPES =
       (ALPHANUMERIC, ENDOFLINE, BLANK, RIGHTARROW, SLASH,
        LFTPAREN, COLON, PERIOD, STRING, LESSTHAN, GREATERTHAN,
"SZH"   STAR, DQUOTE, OTHER);
    MARGINS = - 100 .. BUFFERSIZE;
    SYMBOLSET = SET OF SYMBOLS;
    OPTIONSIZE = - 99 .. 99;
    COMMENTTEXT = ARRAY [1 .. BUFFSZDIV10] OF ALFA;
    SYMBOLSTRING = ARRAY [WIDTH] OF CHAR;

 VAR
    READINGFORMATOPTIONS, PROGISPASCAL2: BOOLEAN;
    PROG: TEXT;
    I: INTEGER;
 (*USED AS FOR LOOP INDEX*)
    CHARACTER: CHAR;
    READCOLUMN, READCOL2: 0 .. 1000;
    OUTPUTCOL, WRITECOLUMN, LEFTMARGIN, ACTUALLEFTMARGIN:
       MARGINS;
    READCOL1, WRITECOL1, WRITECOL2: MARGINS;
    DISPLAYISON, PROCEDNAMESWANTED, ENDCOMMENTSWANTED,
       PACKERISOFF, SAVEDCOMPRESS, COMPRESSWANTED, NOFORMATTING,
       DISPLAYWANTED, CROSSREFWANTED: BOOLEAN;
    LINENUMBER, INCREMENT: - 999 .. 999;
    INDENTINDEX, LONGLINEINDENT, SYMBOLGAP, DECLARALIGNMENT,
       STATMTSEPARATION, PROCEDSEPARATION: OPTIONSIZE;
    LASTSYMBOL, SYMBOLNAME: SYMBOLS;
    ALPHASYMBOLS, ENDLABEL, ENDCONST, ENDTYPE, ENDVAR, ENDVALUE:
       SYMBOLSET;
    SYMBOL: SYMBOLSTRING;
    LENGTH: WIDTH;
"SZH"  FIRSTDQUOTE,
    SYMBOLISNUMBER, LASTPROGPARTWASBODY: BOOLEAN;
    DIGITS: SET OF CHSET;
    OLDEST: WIDTH;
    CHARCOUNT: INTEGER (* USED AS A TOTAL CHARACTERS COUNT*);
    MAIN: COMMENTTEXT;
    MAINNMLENGTH: WIDTH;
    BLANKS, ZEROES: ALFA;
    UNWRITTEN: ARRAY [WIDTH] OF RECORD
                                   CH: CHAR;
                                   CHISENDLINE: BOOLEAN;
                                   INDENTAFTEREOL: MARGINS;
                                END;
    TYPEOF: ARRAY [CHAR] OF CHARTYPES;
    PASCALSYMBOL: ARRAY [1 .. LASTPSYMBOLNAME] OF ALFA;
    PSYMBOLNAME: ARRAY [1 .. LASTPSYMBOLNAME] OF SYMBOLS;
    NAMEOF: ARRAY [CHAR] OF SYMBOLS;
    STATEMENTTYPEOF: ARRAY [SYMBOLS] OF STATMNTTYPES;
"szh"  CHMAP   : ARRAY [CHAR] OF 0..63  ;  (*CHAR CODE CONVERSION           *)
"szh"  PQ      : INTEGER;


"szh"procedure init_chmap;
"szh"
"szh"   (* this routine initializes the ebcdic to 'CCDC' display code MAP *)
"szh"   (* chmap : array[char] of 0..63                                   *)
"szh"
"szh"   const ordmaxch= 255;
"szh"
"szh"   var ch: char;
"szh"
"szh"   BEGIN
"szh"
"szh"    FOR ch := chr(0) TO chr(ORDMAXCH) DO CHMAP[ch] := 0;
"szh"    FOR CH := 'A' TO 'I' DO  CHMAP[CH] := ORD(CH)-192 ;
"szh"    FOR CH := 'J' TO 'R' DO  CHMAP[CH] := ORD(CH)-199 ;
"szh"    FOR CH := 'S' TO 'Z' DO  CHMAP[CH] := ORD(CH)-207 ;
"szh"    FOR CH := 'a' TO 'i' DO  CHMAP[CH] := ORD(CH)-128 ;
"szh"    FOR CH := 'j' TO 'r' DO  CHMAP[CH] := ORD(CH)-135 ;
"szh"    FOR CH := 's' TO 'z' DO  CHMAP[CH] := ORD(CH)-143 ;
"szh"    FOR CH := '0' TO '9' DO  CHMAP[CH] := ORD(CH)-213 ;
"szh"    CHMAP['+']  :=  37 ;
"szh"    CHMAP['-']  :=  38 ;
"szh"    CHMAP['*']  :=  39 ;
"szh"    CHMAP['/']  :=  40 ;
"szh"    CHMAP['(']  :=  41 ;
"szh"    CHMAP[')']  :=  42 ;
"szh"    CHMAP['$']  :=  43 ;
"szh"    CHMAP['=']  :=  44 ;
"szh"    CHMAP[' ']  :=  45 ;
"szh"    CHMAP[',']  :=  46 ;
"szh"    CHMAP['.']  :=  47 ;
"szh"    CHMAP['#']  :=  48 ;
"szh"    CHMAP['[']  :=  49 ;
"szh"    CHMAP[']']  :=  50 ;
"szh"    CHMAP['%']  :=  51 ;
"szh"    CHMAP['"']  :=  52 ;
"szh"    CHMAP['_']  :=  53 ;
"szh"    CHMAP['�']  :=  54 ;      (* SHOULD BE '!' *)
"szh"    CHMAP['&']  :=  55 ;
"szh"    CHMAP[''''] :=  56 ;
"szh"    CHMAP['�']  :=  57 ;
"szh"    CHMAP['<']  :=  58 ;
"szh"    CHMAP['>']  :=  59 ;
"szh"    CHMAP['@']  :=  60 ;
"szh"    CHMAP[':']  :=  61 ;       (* SHOULD BE '0', 61 IS BACK SLASH *)
"szh"    CHMAP['�']  :=  62 ;
"szh"    CHMAP[';']  :=  63 ;
"szh"
"szh"   END (*init_chmap*);
"szh"
 PROCEDURE CONSTANTSINITIALIZATION;

    BEGIN
"SZH"  FIRSTDQUOTE := FALSE;
       MAIN[1] := 'MAIN      ';   MAINNMLENGTH := 4;
       BLANKS := '          ';   ZEROES := '0000000000';
       FOR I := 0 TO BUFFERSIZE DO
          WITH UNWRITTEN[I] DO
             BEGIN
                CH := 'A';   CHISENDLINE := FALSE;
                INDENTAFTEREOL := 0;
             END;
       FOR CHARACTER := CHR(0) TO CHR(255) DO
          TYPEOF[CHARACTER] := OTHER;
       TYPEOF[CHR(0)] := BLANK;
       FOR CHARACTER := 'A' TO 'I' DO
          TYPEOF[CHARACTER] := ALPHANUMERIC;
       FOR CHARACTER := 'J' TO 'R' DO
          TYPEOF[CHARACTER] := ALPHANUMERIC;
       FOR CHARACTER := 'S' TO 'Z' DO
          TYPEOF[CHARACTER] := ALPHANUMERIC;
       FOR CHARACTER := '0' TO '9' DO
          TYPEOF[CHARACTER] := ALPHANUMERIC;
       TYPEOF['*'] := STAR;         TYPEOF['/'] := SLASH;
       TYPEOF['('] := LFTPAREN;     TYPEOF[' '] := BLANK;
       TYPEOF['.'] := PERIOD;       TYPEOF[''''] := STRING;
       TYPEOF[':'] := COLON;
"SZH"  TYPEOF['�'] := RIGHTARROW;   TYPEOF['"'] := DQUOTE;
"SZH"  TYPEOF['$'] := ALPHANUMERIC; TYPEOF['_'] := ALPHANUMERIC;
"SZH"  TYPEOF['#'] := BLANK;
       TYPEOF['<'] := LESSTHAN;     TYPEOF['>'] := GREATERTHAN;
"SZH"  FOR CHARACTER := CHR(0) TO CHR(255) DO
          NAMEOF[CHARACTER] := OTHERSYMBOL;
       NAMEOF['('] := LEFTPAREN;
       NAMEOF[')'] := RIGHTPARENTH;
       NAMEOF['='] := EQUALSYMBOL;   NAMEOF[','] := COMMASYMBOL;
       NAMEOF['.'] := PERIODSYMBOL;
       NAMEOF['['] := LEFTBRACKET;
       NAMEOF[']'] := RIGHTBRACKET;
       NAMEOF[':'] := COLONSYMBOL;   NAMEOF['@'] := UPARROW;
"SZH"  NAMEOF['�'] := COMMENT;   NAMEOF['<'] := EQUALSYMBOL;
"SZH"  NAMEOF['"'] := COMMENT;
       NAMEOF['>'] := EQUALSYMBOL;   NAMEOF[';'] := SEMICOLON;
       PASCALSYMBOL[1] := 'PROGRAM   ';
       PASCALSYMBOL[21] := 'FUNCTION  ';
       PASCALSYMBOL[2] := 'BEGIN     ';
       PASCALSYMBOL[22] := 'LABEL     ';
       PASCALSYMBOL[3] := 'END       ';
       PASCALSYMBOL[23] := 'IN        ';
       PASCALSYMBOL[4] := 'CONST     ';
       PASCALSYMBOL[24] := 'MOD       ';
       PASCALSYMBOL[5] := 'TYPE      ';
       PASCALSYMBOL[25] := 'DIV       ';
       PASCALSYMBOL[6] := 'VAR       ';
       PASCALSYMBOL[26] := 'AND       ';
       PASCALSYMBOL[7] := 'VALUE     ';
       PASCALSYMBOL[27] := 'OR        ';
       PASCALSYMBOL[8] := 'RECORD    ';
       PASCALSYMBOL[28] := 'NOT       ';
       PASCALSYMBOL[9] := 'CASE      ';
       PASCALSYMBOL[29] := 'LT        ';
       PASCALSYMBOL[10] := 'IF        ';
       PASCALSYMBOL[30] := 'LE        ';
       PASCALSYMBOL[11] := 'THEN      ';
       PASCALSYMBOL[31] := 'IS        ';
       PASCALSYMBOL[12] := 'ELSE      ';
       PASCALSYMBOL[32] := 'ISNT      ';
       PASCALSYMBOL[13] := 'DO        ';
       PASCALSYMBOL[33] := 'EQ        ';
       PASCALSYMBOL[14] := 'OF        ';
       PASCALSYMBOL[34] := 'NE        ';
       PASCALSYMBOL[15] := 'FOR       ';
       PASCALSYMBOL[35] := 'GE        ';
       PASCALSYMBOL[16] := 'WHILE     ';
       PASCALSYMBOL[36] := 'GT        ';
       PASCALSYMBOL[17] := 'WITH      ';
       PASCALSYMBOL[37] := 'JOIN      ';
       PASCALSYMBOL[18] := 'REPEAT    ';
       PASCALSYMBOL[38] := 'MEET      ';
       PASCALSYMBOL[19] := 'UNTIL     ';
       PASCALSYMBOL[39] := 'ARRAY     ';
       PASCALSYMBOL[20] := 'PROCEDURE ';
       PASCALSYMBOL[40] := 'NOSYMBOL  ';
       PSYMBOLNAME[1] := PROGRAMSYMBOL;
       PSYMBOLNAME[21] := FUNCTIONSYMBOL;
       PSYMBOLNAME[2] := BEGINSYMBOL;
       PSYMBOLNAME[22] := LABELSYMBOL;
       PSYMBOLNAME[3] := ENDSYMBOL;
       PSYMBOLNAME[4] := CONSTSYMBOL;
       PSYMBOLNAME[5] := TYPESYMBOL;
       PSYMBOLNAME[6] := VARSYMBOL;
       PSYMBOLNAME[7] := VALUESYMBOL;
       PSYMBOLNAME[8] := RECORDSYMBOL;
       PSYMBOLNAME[9] := CASESYMBOL;
       PSYMBOLNAME[10] := IFSYMBOL;
       PSYMBOLNAME[11] := THENSYMBOL;
       PSYMBOLNAME[12] := ELSESYMBOL;
       PSYMBOLNAME[13] := DOSYMBOL;   PSYMBOLNAME[14] := OFSYMBOL;
       PSYMBOLNAME[15] := FORSYMBOL;
       PSYMBOLNAME[16] := WHILESYMBOL;
       PSYMBOLNAME[17] := WITHSYMBOL;
       PSYMBOLNAME[18] := REPEATSYMBOL;
       PSYMBOLNAME[19] := UNTILSYMBOL;
       PSYMBOLNAME[20] := PROCEDSYMBOL;
       PSYMBOLNAME[40] := IDENTIFIER;
       FOR I := 23 TO 39 DO PSYMBOLNAME[I] := ALPHAOPERATOR;
       FOR SYMBOLNAME := PROGRAMSYMBOL TO ALPHAOPERATOR DO
          STATEMENTTYPEOF[SYMBOLNAME] := OTHERSTATEMENT;
       STATEMENTTYPEOF[BEGINSYMBOL] := COMPOUNDSTATEMENT;
       STATEMENTTYPEOF[CASESYMBOL] := CASESTATEMENT;
       STATEMENTTYPEOF[IFSYMBOL] := IFSTATEMENT;
       STATEMENTTYPEOF[FORSYMBOL] := FORWITHWHILESTATEMENT;
       STATEMENTTYPEOF[WHILESYMBOL] := FORWITHWHILESTATEMENT;
       STATEMENTTYPEOF[WITHSYMBOL] := FORWITHWHILESTATEMENT;
       STATEMENTTYPEOF[REPEATSYMBOL] := REPEATSTATEMENT;
    END (*CONSTANTSINITIALIZATION*);

 PROCEDURE WRITEA(CHARACTER: CHAR);

    VAR
       I: WIDTH;
       TESTNO: INTEGER;

    BEGIN
       CHARCOUNT := CHARCOUNT + 1;
       OLDEST := CHARCOUNT MOD BUFFERSIZE;
       WITH UNWRITTEN[OLDEST] DO
          BEGIN
             IF CHARCOUNT > BUFFERSZP1
             THEN
                BEGIN
                   IF CHISENDLINE
                   THEN
                      BEGIN
                         IF INDENTAFTEREOL < 0
                         THEN
                            BEGIN
                               WRITE(OUTPUT, BLANKS: - INDENTAFTEREOL);
                               OUTPUTCOL := OUTPUTCOL -
                                  INDENTAFTEREOL;
                            END
                         ELSE
                            BEGIN
                               IF INCREMENT < 0
                               THEN
                                  BEGIN
                                     I := WRITECOL2 - OUTPUTCOL
                                        + 1;
                                     IF I > 0
                                     THEN WRITE(OUTPUT, BLANKS: I);
                                     TESTNO := LINENUMBER;
                                     I := 0;
                                     REPEAT
                                        TESTNO := TESTNO DIV 10;
                                        I := I + 1;
                                     UNTIL TESTNO = 0;
                                     WRITE(OUTPUT, ZEROES: (6 - I),
                                        LINENUMBER: I);
                                     LINENUMBER := LINENUMBER -
                                        INCREMENT;
                                     IF LINENUMBER > 999999
                                     THEN
                                        LINENUMBER := LINENUMBER
                                           - 1000000;
                                     WRITELN(OUTPUT);  WRITE(OUTPUT, ' ');
                                  END
                               ELSE
                                  BEGIN
                                     WRITELN(OUTPUT, ' '); WRITE(OUTPUT, ' ');
                                     IF INCREMENT > 0 THEN
                                        BEGIN
                                           WRITE(OUTPUT, LINENUMBER: (
                                              WRITECOL1 - 2));
                                           INDENTAFTEREOL :=
                                              INDENTAFTEREOL -
                                              WRITECOL1 + 2;
                                           LINENUMBER :=
                                              LINENUMBER +
                                              INCREMENT;
                                        END
                                  END;
                               IF INDENTAFTEREOL > 0 THEN
                                  WRITE(OUTPUT, BLANKS: INDENTAFTEREOL);
                               OUTPUTCOL := INDENTAFTEREOL + 1;
                            END;
                         CHISENDLINE := FALSE;
                      END (*IF CHISENDLINE*)
                   ELSE
                      BEGIN
                         WRITE(OUTPUT, CH);
                         OUTPUTCOL := OUTPUTCOL + 1;
                      END (*ELSE*);
                END (*IF CHARCOUNT > *);
             CH := CHARACTER;   WRITECOLUMN := WRITECOLUMN + 1;
          END (*WITH*);
    END (*WRITEA*);

 PROCEDURE WRITEANEOLWITHNOINDENT;

    BEGIN
       WRITEA(' ');
       WITH UNWRITTEN[OLDEST] DO
          BEGIN
             CHISENDLINE := TRUE;
             INDENTAFTEREOL := WRITECOL1 - 1;
          END;
       WRITECOLUMN := WRITECOL1;
    END (*WRITEANEOLWITHNOINDENT*);

 PROCEDURE STARTNEWLINEANDINDENT;

    LABEL
       1, 2;

    VAR
       I: OPTIONSIZE;

    BEGIN
       IF PACKERISOFF AND DISPLAYISON
       THEN
          BEGIN
          2: WRITEA(' ');   1: LASTSYMBOL := PERIODSYMBOL;
             WITH UNWRITTEN[OLDEST] DO
                BEGIN
                   CHISENDLINE := TRUE;
                   INDENTAFTEREOL := WRITECOL1 + LEFTMARGIN - 1;
                END;
             WRITECOLUMN := WRITECOL1 + LEFTMARGIN;
          END (*IF PACKERISOFF*);
    END (*STARTNEWLINE*);

 PROCEDURE READACHARACTER;

    BEGIN
       IF READCOLUMN > READCOL2
       THEN
          BEGIN
             GET(PROG);
             IF READCOL2 < 999
             THEN BEGIN WHILE NOT EOLN(PROG) DO GET(PROG); END
             ELSE READCOLUMN := 2;
          END
       ELSE
          IF READCOLUMN <= 1
          THEN
             BEGIN
                IF READCOLUMN = 1   THEN GET(PROG)
                ELSE READCOLUMN := 1;
                WHILE READCOLUMN < READCOL1 DO
                   BEGIN
                      IF EOLN(PROG)   THEN READCOLUMN := 1
                      ELSE READCOLUMN := READCOLUMN + 1;
                      GET(PROG);
                   END;
             END
          ELSE GET(PROG);
       IF EOLN(PROG)
       THEN
          BEGIN
             CHARACTER := ' ';   READCOLUMN := 1;
             IF NOFORMATTING   THEN WRITEANEOLWITHNOINDENT;
          END
       ELSE
          BEGIN
             CHARACTER := PROG@;   READCOLUMN := READCOLUMN + 1;
             IF NOFORMATTING   THEN WRITEA(CHARACTER);
          END;

"SZH" IF CHARACTER = '"' THEN
"SZH"    IF FIRSTDQUOTE THEN
"SZH"       BEGIN  CHARACTER := '�';
"SZH"       FIRSTDQUOTE := FALSE;
"SZH"       END ;

    END (*READACHARACTER*);

 PROCEDURE WRITESYMBOL;

    VAR
       I: WIDTH;
       NUMBERBLANKSTOWRITE: OPTIONSIZE;
       WRITEWIDTH, TOTALINDENT: WIDTH;

    BEGIN
       IF DISPLAYISON
       THEN
          BEGIN
             NUMBERBLANKSTOWRITE := SYMBOLGAP;
             IF (LASTSYMBOL IN [LEFTPAREN, LEFTBRACKET,
                PERIODSYMBOL]) OR (SYMBOLNAME IN [SEMICOLON,
                RIGHTPARENTH, RIGHTBRACKET, COMMASYMBOL,
                PERIODSYMBOL, COLONSYMBOL]) OR (SYMBOLNAME IN [
                LEFTBRACKET, LEFTPAREN, UPARROW]) AND (
                LASTSYMBOL = IDENTIFIER)
             THEN NUMBERBLANKSTOWRITE := 0
             ELSE
                IF (SYMBOLNAME IN ALPHASYMBOLS) AND (LASTSYMBOL
                   IN ALPHASYMBOLS)
                THEN
                   IF WRITECOLUMN <= WRITECOL2 THEN
                      BEGIN
                         WRITEA(' ');
                         IF SYMBOLGAP > 0 THEN
                            NUMBERBLANKSTOWRITE := SYMBOLGAP - 1
                         ;
                      END;
             IF WRITECOLUMN + LENGTH + NUMBERBLANKSTOWRITE - 1 >
                WRITECOL2
             THEN
                BEGIN
                   WRITEA(' ');
                   WITH UNWRITTEN[OLDEST] DO
                      BEGIN
                         CHISENDLINE := TRUE;
                         IF PACKERISOFF
                         THEN
                            BEGIN
                               WRITEWIDTH := WRITECOL2 -
                                  WRITECOL1 + 1;
                               TOTALINDENT := LEFTMARGIN +
                                  LONGLINEINDENT;
                               IF LENGTH <= WRITEWIDTH -
                                  TOTALINDENT
                               THEN
                                  INDENTAFTEREOL := WRITECOL1 -
                                     1 + TOTALINDENT
                               ELSE
                                  IF LENGTH <= WRITEWIDTH
                                  THEN
                                     INDENTAFTEREOL := WRITECOL2
                                        - LENGTH
                                  ELSE
                                     BEGIN
                                        LENGTH := WRITEWIDTH;
                                        INDENTAFTEREOL :=
                                           WRITECOL1 - 1;
                                        IF SYMBOL[1] = ''''
                                        THEN
                                           SYMBOL[LENGTH] :=
                                              ''''
                                     END;
                               WRITECOLUMN := INDENTAFTEREOL + 1
                               ;
                            END
                         ELSE
                            BEGIN
                               IF LENGTH > WRITECOL2 - WRITECOL1
                                  + 1
                               THEN
                                  LENGTH := WRITECOL2 -
                                     WRITECOL1 + 1;
                               INDENTAFTEREOL := WRITECOL1 - 1;
                               WRITECOLUMN := WRITECOL1;
                            END;
                      END (*WITH*);
                END
             ELSE
                FOR I := 1 TO NUMBERBLANKSTOWRITE DO
                   WRITEA(' ');
             FOR I := 1 TO LENGTH DO WRITEA(SYMBOL[I]);
          END (*IF DISPLAYISON*);
       LASTSYMBOL := SYMBOLNAME;
    END (*WRITESYMBOL*);

 PROCEDURE READSYMBOL;

    CONST
       READNEXTCH = TRUE;
       DONTREADNEXTCH = FALSE;

    VAR
       TESTSYMBOL: ALFA;
       CHARNUMBER: WIDTH;
       I: WIDTH;

    PROCEDURE DOCOMMENT(TERMINATOR: CHAR);

       VAR
          I: OPTIONSIZE;
          SECONDTERMINATOR: CHAR;
          SAVEDSYMBGP: OPTIONSIZE;

       PROCEDURE COPYACHARACTER;

          LABEL
             10;

          BEGIN
             IF READINGFORMATOPTIONS
             THEN BEGIN CHARACTER := INPUT@;   GET(INPUT); END
             ELSE
                BEGIN
                   IF DISPLAYISON
                   THEN
                      BEGIN
                         IF WRITECOLUMN > WRITECOL2
                         THEN
                            BEGIN
                               IF (CHARACTER = ' ') AND
                                  NOT EOLN(PROG)
                               THEN
                                  BEGIN
                                     REPEAT READACHARACTER;
                                     UNTIL (CHARACTER <> ' ') OR
                                        EOLN(PROG);
                                     IF CHARACTER = TERMINATOR
                                     THEN GOTO 10;
                                  END;
                               IF NOT EOLN(PROG)
                               THEN
                                  BEGIN
                                     WRITEA(' ');
                                     WITH UNWRITTEN[OLDEST] DO
                                        BEGIN
                                           CHISENDLINE := TRUE;
                                           INDENTAFTEREOL :=
                                              WRITECOL1 +
                                              LEFTMARGIN - 1;
                                        END;
                                     WRITECOLUMN := WRITECOL1 +
                                        LEFTMARGIN;
                                  END;
                            END;
                         IF EOLN(PROG)
                         THEN WRITEANEOLWITHNOINDENT
                         ELSE WRITEA(CHARACTER);
                      END;
                   READACHARACTER;
                END;
         10:
          END (*COPYACHARACTER*);

       PROCEDURE COMPILERDIRECTIVES;

          BEGIN
             REPEAT
                REPEAT COPYACHARACTER;
"SZH"          UNTIL CHMAP[CHARACTER] IN
"SZH"                [CHMAP['E'], CHMAP['U'], CHMAP[' '], CHMAP['['],
"SZH"                 CHMAP[TERMINATOR] ];
             UNTIL NOT ((CHARACTER = 'E') AND PROGISPASCAL2);
             IF (CHARACTER = 'E') OR (CHARACTER = 'U')
             THEN
                BEGIN
                   COPYACHARACTER;
                   IF (CHARACTER = '+') OR (CHARACTER = '-') THEN
                      BEGIN
                         IF CHARACTER = '+'
                         THEN READCOL2 := 72
                         ELSE READCOL2 := 999;
                         IF (WRITECOL2 > 72)
                         THEN CHARACTER := '-';
                      END;
                END;
"SZH"       WHILE NOT (CHMAP[CHARACTER] IN [ CHMAP['['], CHMAP[TERMINATOR]]) DO
                COPYACHARACTER;
          END (*COMPILERDIRECTIVES*);

       PROCEDURE FORMATTERDIRECTIVES;

          CONST
             INVALID = - 1;

          TYPE
             PARAMCOUNT = 1 .. 2;
             PARAMS = ARRAY [PARAMCOUNT] OF MARGINS;

          VAR
             SPECIFICATION: PARAMS;
             FORMATOPTION: CHAR;
             PREVDISPLAY: BOOLEAN;
             ENDDIRECTV: SET OF CHSET;

          PROCEDURE READIN(N: PARAMCOUNT; VAR SPECIFICATION:
             PARAMS);

             VAR
                I: PARAMCOUNT;

             BEGIN
                FOR I := 1 TO N DO
                   BEGIN
"SZH"                 WHILE NOT (CHMAP[CHARACTER] IN (DIGITS +
                         ENDDIRECTV)) DO
                         COPYACHARACTER;
                      SPECIFICATION[I] := 0;
"SZH"                 IF NOT (CHMAP[CHARACTER] IN ENDDIRECTV)
                      THEN
                         REPEAT
                            SPECIFICATION[I] := 10 *
                               SPECIFICATION[I] + ORD(CHARACTER)
                               - ORD('0');
                            COPYACHARACTER;
"SZH"                    UNTIL NOT (CHMAP[CHARACTER] IN DIGITS)
                      ELSE SPECIFICATION[I] := INVALID;
                   END (*FOR*);
             END (*READIN*);

          BEGIN (*FORMATTERDIRECTIVES*)
"SZH"        ENDDIRECTV := [CHMAP[TERMINATOR], CHMAP[']']];
             REPEAT
"SZH"          IF ( CHMAP[CHARACTER] IN
"SZH"             [ CHMAP['A'], CHMAP['B'], CHMAP['C'], CHMAP['D'],
"SZH"               CHMAP['E'], CHMAP['G'], CHMAP['I'], CHMAP['L'],
"SZH"               CHMAP['N'], CHMAP['P'], CHMAP['R'], CHMAP['S'],
"SZH"               CHMAP['W'], CHMAP['F'], CHMAP['X'] ])
                THEN
                   BEGIN
                      FORMATOPTION := CHARACTER;
                      CASE FORMATOPTION OF
                         'A', 'E', 'I', 'G', 'P', 'L', 'S':
                            BEGIN
                               READIN(1, SPECIFICATION);
                               IF (SPECIFICATION[1] < WRITECOL2
                                  - WRITECOL1 - 9) OR (
                                  FORMATOPTION = 'P')
                               THEN
                                  CASE FORMATOPTION OF
                                     'A':
                                        DECLARALIGNMENT :=
                                           SPECIFICATION[1];
                                     'E':
                                        IF SPECIFICATION[1] < 4
                                        THEN
                                           BEGIN
                                              PROCEDNAMESWANTED
                                                 :=
                                                 SPECIFICATION[1
                                                 ] > 1;
                                              ENDCOMMENTSWANTED
                                                 := ODD(
                                                 SPECIFICATION[1]
                                                 );
                                           END;
                                     'G':
                                        SYMBOLGAP :=
                                           SPECIFICATION[1];
                                     'I':
                                        INDENTINDEX :=
                                           SPECIFICATION[1];
                                     'L':
                                        LONGLINEINDENT :=
                                           SPECIFICATION[1];
                                     'P':
                                        PROCEDSEPARATION :=
                                           SPECIFICATION[1];
                                     'S':
                                        STATMTSEPARATION :=
                                           SPECIFICATION[1];
                                  END (*CASE*);
                            END (*SINGLE PARAMETERS*);
                         'W', 'R', 'N':
                            BEGIN
                               READIN(2, SPECIFICATION);
                               IF SPECIFICATION[2] <> INVALID
                               THEN
                                  CASE FORMATOPTION OF
                                     'W':
                                        IF (SPECIFICATION[1] > 0
                                           ) AND (SPECIFICATION[
                                           2] < BUFFERSIZE - 2)
                                           AND (SPECIFICATION[2]
                                           - SPECIFICATION[1] >
                                           8)
                                        THEN
                                           BEGIN
                                              WRITECOL1 :=
                                                 SPECIFICATION[1
                                                 ];
                                              WRITECOL2 :=
                                                 SPECIFICATION[2
                                                 ];
                                           END;
                                     'R':
                                        IF (SPECIFICATION[1] > 0
                                           ) AND (SPECIFICATION[
                                           2] - SPECIFICATION[1]
                                           > 8)
                                        THEN
                                           BEGIN
                                              READCOL1 :=
                                                 SPECIFICATION[1
                                                 ];
                                              READCOL2 :=
                                                 SPECIFICATION[2
                                                 ];
                                           END;
                                     'N':
                                        BEGIN
                                           LINENUMBER :=
                                              SPECIFICATION[1];
                                           INCREMENT :=
                                              SPECIFICATION[2];
"SZH"                                     WHILE NOT ( CHMAP[CHARACTER]
"SZH"                                           IN ([CHMAP['+'], CHMAP['-'] ]+
                                              ENDDIRECTV)) DO
                                              COPYACHARACTER;
                                           IF CHARACTER = '-'
                                           THEN
                                              INCREMENT := -
                                                 INCREMENT
                                           ELSE
                                              IF WRITECOL1 < 3
                                              THEN
                                                 WRITECOL1 := 3;
                                        END;
                                  END (*CASE*);
                            END (*DOUBLE PARAMETERS*);
                         'B', 'C', 'D', 'F', 'X':
                            BEGIN
                               REPEAT COPYACHARACTER;
"SZH"                         UNTIL CHMAP[CHARACTER] IN
"SZH"                               ([ CHMAP['+'], CHMAP['-'] ] +
                                     ENDDIRECTV);
                               IF (CHARACTER = '+') OR (CHARACTER = '-')
                               THEN
                                  CASE FORMATOPTION OF
                                     'B':
                                        PACKERISOFF := CHARACTER
                                           = '-';
                                     'C':
                                        IF DISPLAYISON THEN
                                           COMPRESSWANTED :=
                                              CHARACTER = '+';
                                     'D':
                                        BEGIN
                                           PREVDISPLAY :=
                                              DISPLAYWANTED;
                                           DISPLAYWANTED :=
                                              CHARACTER = '+';
                                           IF PREVDISPLAY AND
                                              NOT DISPLAYWANTED
                                           THEN
                                              BEGIN
                                                 WRITEA(
                                                    TERMINATOR);
                                                 IF TERMINATOR =
                                                    '*'
                                                 THEN
                                                   IF
                                                   PROGISPASCAL2
                                                   THEN
                                                   WRITEA('(')
                                                   ELSE
                                                   WRITEA('/');
                                                 SAVEDCOMPRESS
                                                    :=
                                                  COMPRESSWANTED
                                                 ;
                                                 COMPRESSWANTED
                                                    := FALSE;
                                              END
                                           ELSE
                                              IF NOT PREVDISPLAY
                                                 AND
                                                 DISPLAYWANTED
                                              THEN
                                                 BEGIN

                                           STARTNEWLINEANDINDENT
                                                    ;
                                                   IF TERMINATOR
                                                      = '*'
                                                   THEN
                                                   BEGIN
                                                   IF PROGISPASCAL2
                                                   THEN
                                                   WRITEA('(')
                                                   ELSE
                                                   WRITEA('/');
                                                   WRITEA('*');
                                                   END
                                                   ELSE
                                                   WRITEA('_');

                                                  COMPRESSWANTED
                                                      :=
                                                   SAVEDCOMPRESS
                                                    ;
                                                 END (*IF NOT PR
                                                 EV*);
                                        END (* 'D': *);
                                     'F':
                                        BEGIN
                                           PREVDISPLAY :=
                                              NOFORMATTING;
                                           NOFORMATTING :=
                                              CHARACTER = '-';
                                           IF PREVDISPLAY AND
                                              NOT NOFORMATTING
                                           THEN READACHARACTER;
                                           IF NOT PREVDISPLAY
                                              AND NOFORMATTING
                                           THEN WRITEA('-');
                                        END;
                                     'X':
                                        CROSSREFWANTED :=
                                           CHARACTER = '+';
                                  END (*CASE*);
                               DISPLAYISON := DISPLAYWANTED AND
                                  NOT NOFORMATTING;
                            END (*BOOLEAN PARAMETERS*);
                      END (*CASE*)
                   END (*THEN*)
                ELSE
"SZH"              IF NOT (CHMAP[CHARACTER] IN ENDDIRECTV)
                   THEN COPYACHARACTER;
"SZH"        UNTIL CHMAP[CHARACTER] IN ENDDIRECTV;
             IF CHARACTER = ']'   THEN COPYACHARACTER;
          END (*FORMATTERDIRECTIVES*);

       BEGIN (*DOCOMMENT*)
          IF READINGFORMATOPTIONS   THEN FORMATTERDIRECTIVES
          ELSE
             BEGIN
                IF PROGISPASCAL2   THEN SECONDTERMINATOR := ')'
                ELSE SECONDTERMINATOR := '/';
                IF LASTSYMBOL IN [COMMENT, SEMICOLON] THEN
                   BEGIN
                      LEFTMARGIN := 0;   STARTNEWLINEANDINDENT;
                      LEFTMARGIN := ACTUALLEFTMARGIN;
                   END;
                WRITESYMBOL;
                IF CHARACTER = '$'   THEN COMPILERDIRECTIVES;
                IF CHARACTER = '['   THEN FORMATTERDIRECTIVES;
                SAVEDSYMBGP := SYMBOLGAP;   SYMBOLGAP := 0;
                REPEAT

                   WHILE CHARACTER <> TERMINATOR DO
                      BEGIN  COPYACHARACTER;
                      END (*WHILE*);

                   READSYMBOL;   WRITESYMBOL;
                UNTIL SYMBOLNAME = COMMENT;
                SYMBOLGAP := SAVEDSYMBGP;   READSYMBOL;
             END;
       END (*DOCOMMENT*);

    PROCEDURE CHECKFOR(SECONDCHAR: CHAR; TWOCHARSYMBOL: SYMBOLS;
       READALLOWED: BOOLEAN);

       BEGIN
          IF READALLOWED THEN
             BEGIN
                LENGTH := 1;   SYMBOL[1] := CHARACTER;
                SYMBOLNAME := NAMEOF[CHARACTER];
                READACHARACTER;
             END;
          IF CHARACTER = SECONDCHAR THEN
             BEGIN
                SYMBOL[2] := CHARACTER;   LENGTH := 2;
                SYMBOLNAME := TWOCHARSYMBOL;   READACHARACTER;
             END;
       END (*CHECKFOR*);

    BEGIN (*READSYMBOL*)
"SZH"
"SZH"  IF CHARACTER = '#' THEN CHARACTER := ' '
"SZH"  ELSE
"SZH"     IF CHARACTER = '"' THEN
"SZH"         IF FIRSTDQUOTE THEN
"SZH"            BEGIN CHARACTER := '�';  FIRSTDQUOTE := FALSE END
"SZH"         ELSE  BEGIN  CHARACTER := '�';  FIRSTDQUOTE := TRUE  END;
"SZH"
       IF READINGFORMATOPTIONS   THEN DOCOMMENT('*')
       ELSE
          CASE TYPEOF[CHARACTER] OF
             STAR:
                IF PROGISPASCAL2
                THEN CHECKFOR(')', COMMENT, READNEXTCH)
                ELSE CHECKFOR('/', COMMENT, READNEXTCH);
             RIGHTARROW:
                BEGIN
                   SYMBOLNAME := COMMENT;   SYMBOL[1] := '�';
                   LENGTH := 1;   READACHARACTER;
                   DOCOMMENT('�');
                END;
"SZH"    (*  DQUOTE:
"SZH"           BEGIN
"SZH"              SYMBOLNAME := COMMENT;   SYMBOL[1] := '"';
"SZH"              LENGTH := 1;   READACHARACTER;
"SZH"              DOCOMMENT('"');
"SZH"           END;   *)
             SLASH:
                BEGIN
                   CHECKFOR('*', COMMENT, READNEXTCH);
                   IF (SYMBOLNAME = COMMENT) THEN
                      BEGIN
                         PROGISPASCAL2 := FALSE;
                         DOCOMMENT('*')
                      END;
                END;
             LFTPAREN:
                BEGIN
                   CHECKFOR('*', COMMENT, READNEXTCH);
                   IF (SYMBOLNAME = COMMENT) THEN
                      BEGIN
                         PROGISPASCAL2 := TRUE;
                         DOCOMMENT('*')
                      END;
                END;
             ALPHANUMERIC:
                BEGIN
"SZH"              SYMBOLISNUMBER := CHMAP[CHARACTER] IN DIGITS;
                   CHARNUMBER := 1;
                   REPEAT
                      SYMBOL[CHARNUMBER] := CHARACTER;
                      READACHARACTER;
                      CHARNUMBER := CHARNUMBER + 1
                   UNTIL NOT (TYPEOF[CHARACTER] = ALPHANUMERIC);
                   IF SYMBOLISNUMBER AND (SYMBOL[CHARNUMBER - 1]
                      = 'E')
                   THEN
                      REPEAT
                         SYMBOL[CHARNUMBER] := CHARACTER;
                         READACHARACTER;
                         CHARNUMBER := CHARNUMBER + 1;
"SZH"                 UNTIL NOT (CHMAP[CHARACTER] IN DIGITS);
                   LENGTH := CHARNUMBER - 1;
                   IF SYMBOLISNUMBR
                   THEN SYMBOLNAME := IDENTIFIER
                   ELSE
                      BEGIN
                         FOR CHARNUMBER := CHARNUMBER TO 10 DO
                            SYMBOL[CHARNUMBER] := ' ';
"""                      PACK(SYMBOL, 1, TESTSYMBOL);   """  I := 1;
"SZH"                    FOR PQ := 1 TO ALFLEN DO TESTSYMBOL[PQ] := SYMBOL[PQ];
                         PASCALSYMBOL[LASTPSYMBOLNAME] :=
                            TESTSYMBOL;
                         WHILE TESTSYMBOL <> PASCALSYMBOL[I] DO
                            I := I + 1;
                         SYMBOLNAME := PSYMBOLNAME[I];
                      END (*ELSE*);
                END (*ALPHANUMERIC*);
             BLANK, ENDOFLINE:
                BEGIN
                   REPEAT READACHARACTER
                   UNTIL NOT (TYPEOF[CHARACTER] IN [BLANK,
                      ENDOFLINE]);
                   READSYMBOL
                END;
             GREATERTHAN, COLON:
                CHECKFOR('=', OTHERSYMBOL, READNEXTCH);
             LESSTHAN:
                BEGIN
                   CHECKFOR('=', OTHERSYMBOL, READNEXTCH);
                   IF SYMBOLNAME <> OTHERSYMBOL THEN
                      CHECKFOR('>', OTHERSYMBOL, DONTREADNEXTCH)
                   ;
                END;
             PERIOD:
                IF LASTSYMBOL <> ENDSYMBOL
                THEN CHECKFOR('.', RANGE, READNEXTCH)
                ELSE SYMBOLNAME := PERIODSYMBOL;
             STRING:
                BEGIN
                   CHARNUMBER := 1;
                   REPEAT
                      REPEAT
                         SYMBOL[CHARNUMBER] := CHARACTER;
                         CHARNUMBER := CHARNUMBER + 1;
                         READACHARACTER;
                      UNTIL CHARACTER = '''';
                      SYMBOL[CHARNUMBER] := CHARACTER;
                      CHARNUMBER := CHARNUMBER + 1;
                      READACHARACTER;
                   UNTIL CHARACTER <> '''';
                   LENGTH := CHARNUMBER - 1;
                   SYMBOLNAME := OTHERSYMBOL;
                END (*STRING*);
             OTHER:
                BEGIN
                   SYMBOL[1] := CHARACTER;
                   SYMBOLNAME := NAMEOF[CHARACTER];
                   LENGTH := 1;   READACHARACTER;
                END;
          END (*CASE*);
    END (*READSYMBOL*);

 PROCEDURE CHANGEMARGINTO(NEWLEFTMARGIN: MARGINS);

    BEGIN
       ACTUALLEFTMARGIN := NEWLEFTMARGIN;
       LEFTMARGIN := NEWLEFTMARGIN;
       IF LEFTMARGIN < 0   THEN LEFTMARGIN := 0
       ELSE
          IF LEFTMARGIN > WRITECOL2 - WRITECOL1 - 9 -
             LONGLINEINDENT
          THEN
             LEFTMARGIN := WRITECOL2 - WRITECOL1 - 9 -
                LONGLINEINDENT;
    END (*CHANGEMARGINTO*);

 PROCEDURE DODECLARATIONUNTIL(ENDDECLARATION: SYMBOLSET);

    PROCEDURE DOPARENTHESES;

       VAR
          SAVEDLGLNID: OPTIONSIZE;

       BEGIN
          SAVEDLGLNID := LONGLINEINDENT;
          IF DECLARALIGNMENT > 0
          THEN
             BEGIN
                LONGLINEINDENT := WRITECOLUMN + SYMBOLGAP + 1 -
                   LEFTMARGIN - WRITECOL1;
                REPEAT WRITESYMBOL;   READSYMBOL;
                UNTIL SYMBOLNAME = RIGHTPARENTH;
                WRITESYMBOL;   READSYMBOL;
             END
          ELSE
             BEGIN
                LONGLINEINDENT := 1;
                CHANGEMARGINTO(ACTUALLEFTMARGIN + INDENTINDEX);
                STARTNEWLINEANDINDENT;
                REPEAT WRITESYMBOL;   READSYMBOL
                UNTIL SYMBOLNAME = RIGHTPARENTH;
                WRITESYMBOL;   READSYMBOL;
                CHANGEMARGINTO(ACTUALLEFTMARGIN - INDENTINDEX);
             END (*ELSE*);
          LONGLINEINDENT := SAVEDLGLNID;
       END (*DOPARENTHESES*);

    PROCEDURE DOFIELDLISTUNTIL(ENDFIELDLIST: SYMBOLSET);

       VAR
          LASTEOL: MARGINS;
          ALIGNCOLUMN: WIDTH;

       PROCEDURE DORECORD;

          VAR
             SAVEDLEFTMARGIN: WIDTH;

          BEGIN
             SAVEDLEFTMARGIN := ACTUALLEFTMARGIN;   WRITESYMBOL;
             READSYMBOL;
             CHANGEMARGINTO(WRITECOLUMN - 6 + INDENTINDEX -
                WRITECOL1);
             STARTNEWLINEANDINDENT;
             DOFIELDLISTUNTIL([ENDSYMBOL]);
             CHANGEMARGINTO(ACTUALLEFTMARGIN - INDENTINDEX);
             STARTNEWLINEANDINDENT;   WRITESYMBOL;   READSYMBOL;
             CHANGEMARGINTO(SAVEDLEFTMARGIN);
          END (*DORECORD*);

       PROCEDURE DOVARIANTRECORDPART;

          VAR
             SAVEDLEFTMARGIN, OTHERSAVEDMARGIN: MARGINS;

          BEGIN
             OTHERSAVEDMARGIN := ACTUALLEFTMARGIN;
             IF DECLARALIGNMENT > 0
             THEN
                BEGIN
                   REPEAT WRITESYMBOL;   READSYMBOL;
                   UNTIL SYMBOLNAME = COLONSYMBOL;
                   WRITESYMBOL;   READSYMBOL;
                   WITH UNWRITTEN[LASTEOL] DO
                      BEGIN
                         INDENTAFTEREOL := INDENTAFTEREOL +
                            ALIGNCOLUMN - WRITECOLUMN;
                         IF INDENTAFTEREOL < 0
                         THEN INDENTAFTEREOL := 0;
                      END;
                   WRITECOLUMN := ALIGNCOLUMN;
                   CHANGEMARGINTO(ACTUALLEFTMARGIN + ALIGNCOLUMN -
                      WRITECOLUMN);
                END;
             REPEAT WRITESYMBOL;   READSYMBOL;
             UNTIL SYMBOLNAME = OFSYMBOL;
             CHANGEMARGINTO(ACTUALLEFTMARGIN + INDENTINDEX);
             REPEAT
                WRITESYMBOL;   READSYMBOL;
                IF SYMBOLNAME <> ENDSYMBOL
                THEN
                   BEGIN
                      STARTNEWLINEANDINDENT;
                      REPEAT WRITESYMBOL;   READSYMBOL;
                      UNTIL SYMBOLNAME IN [LEFTPAREN, SEMICOLON
                         , ENDSYMBOL];
                      IF SYMBOLNAME = LEFTPAREN
                      THEN
                         BEGIN
                            WRITESYMBOL;   READSYMBOL;
                            SAVEDLEFTMARGIN := ACTUALLEFTMARGIN;
                            CHANGEMARGINTO(WRITECOLUMN -
                               WRITECOL1);
                            DOFIELDLISTUNTIL([RIGHTPARENTH]);
                            WRITESYMBOL;   READSYMBOL;
                            CHANGEMARGINTO(SAVEDLEFTMARGIN);
                         END;
                   END;
             UNTIL SYMBOLNAME <> SEMICOLON;
             CHANGEMARGINTO(OTHERSAVEDMARGIN);
          END (*DOVARIANTRECORDPART*);

       BEGIN (*DOFIELDLISTUNTIL*)
          LASTEOL := OLDEST;
          IF LASTSYMBOL = LEFTPAREN THEN
             FOR I := 1 TO DECLARALIGNMENT - LENGTH DO WRITEA(' ');
          ALIGNCOLUMN := LEFTMARGIN + WRITECOL1 + DECLARALIGNMENT +
             1;
          WHILE NOT (SYMBOLNAME IN ENDFIELDLIST) DO
             BEGIN
                IF LASTSYMBOL IN [SEMICOLON, COMMENT] THEN
                   IF SYMBOLNAME <> SEMICOLON THEN
                      BEGIN
                         STARTNEWLINEANDINDENT;
                         LASTEOL := OLDEST
                      END;
                IF SYMBOLNAME IN [RECORDSYMBOL, CASESYMBOL,
                   LEFTPAREN, COMMASYMBOL, COLONSYMBOL,
                   EQUALSYMBOL]
                THEN
                   CASE SYMBOLNAME OF
                      RECORDSYMBOL: DORECORD;
                      CASESYMBOL: DOVARIANTRECORDPART;
                      LEFTPAREN: DOPARENTHESES;
                      COMMASYMBOL, COLONSYMBOL, EQUALSYMBOL:
                         BEGIN
                            WRITESYMBOL;
                            IF DECLARALIGNMENT > 0
                            THEN
                               IF ENDFIELDLIST <> ENDLABEL
                               THEN
                                  BEGIN
                                     WITH UNWRITTEN[LASTEOL] DO
                                        BEGIN
                                           INDENTAFTEREOL :=
                                              INDENTAFTEREOL +
                                              ALIGNCOLUMN -
                                              WRITECOLUMN;
                                           IF INDENTAFTEREOL < 0
                                           THEN
                                              INDENTAFTEREOL :=
                                                 0;
                                           WRITECOLUMN :=
                                              ALIGNCOLUMN;
                                        END;
                                     IF SYMBOLNAME = COMMASYMBOL
                                     THEN
                                        BEGIN
                                           STARTNEWLINEANDINDENT
                                           ;
                                           LASTEOL := OLDEST;
                                        END;
                                  END (*IF DECLARALIGN*);
                            READSYMBOL;
                         END (*  ,   :   = *)
                   END (*CASE*)
                ELSE BEGIN WRITESYMBOL;   READSYMBOL END;
             END (*WHILE*);
       END (*DOFIELDLISTUNTIL*);

    BEGIN (*DODECLARATIONUNTIL*)
       STARTNEWLINEANDINDENT;   WRITESYMBOL;
       CHANGEMARGINTO(ACTUALLEFTMARGIN + INDENTINDEX);
       STARTNEWLINEANDINDENT;   READSYMBOL;
       DOFIELDLISTUNTIL(ENDDECLARATION);
       STARTNEWLINEANDINDENT;
       CHANGEMARGINTO(ACTUALLEFTMARGIN - INDENTINDEX);
    END (*DOTYPES*);

 PROCEDURE BLOCK(BLOCKNAME: COMMENTTEXT; BLOCKNMLENGTH: WIDTH);

    VAR
       I: WIDTH;
       IFTHENCOMPRESSNEEDED: BOOLEAN;
       ATPROCEDBEGINNING: BOOLEAN;

    PROCEDURE PROCEDURES;

       VAR
          I: 0 .. 20;
          PROCEDNAME: COMMENTTEXT;
          PROCEDNMLENGTH: WIDTH;

       BEGIN
          FOR I := 2 TO PROCEDSEPARATION DO
             STARTNEWLINEANDINDENT;
          STARTNEWLINEANDINDENT;   WRITESYMBOL;   READSYMBOL;
          FOR I := 0 TO(LENGTH - 1) DIV 10 DO
"""          PACK(SYMBOL, I * 10 + 1, PROCEDNAME[I + 1]); """
"SZH"        FOR PQ := 1 TO ALFLEN DO PROCEDNAME[I+1,PQ] := SYMBOL[I*10+PQ];
          PROCEDNMLENGTH := LENGTH;   WRITESYMBOL;   READSYMBOL;
          IF SYMBOLNAME = LEFTPAREN THEN
             BEGIN
                WRITESYMBOL;
                REPEAT READSYMBOL;   WRITESYMBOL
                UNTIL SYMBOLNAME = RIGHTPARENTH;
                READSYMBOL;
             END;
          IF SYMBOLNAME = COLONSYMBOL THEN
             REPEAT WRITESYMBOL;   READSYMBOL;
             UNTIL SYMBOLNAME = SEMICOLON;
          WRITESYMBOL;   READSYMBOL;
          CHANGEMARGINTO(ACTUALLEFTMARGIN + INDENTINDEX);
          STARTNEWLINEANDINDENT;   LASTPROGPARTWASBODY := FALSE;
          BLOCK(PROCEDNAME, PROCEDNMLENGTH);
          LASTPROGPARTWASBODY := TRUE;
          CHANGEMARGINTO(ACTUALLEFTMARGIN - INDENTINDEX);
          WRITESYMBOL;   READSYMBOL;
 (* WRITE ";"*)
          STARTNEWLINEANDINDENT;
       END (*PROCEDURES*);

    PROCEDURE DOSTATEMENT(VAR ADDEDBLANKS: WIDTH; STATMTSYMBOL:
       COMMENTTEXT; STMTSYMLENGTH: WIDTH);

       VAR
          I: WIDTH;
          STATMTBEGINNING: INTEGER;
          STATMTPART: ARRAY [1 .. 4] OF INTEGER;
          BLKSONCURRNTLINE, BLKSADDEDBYTHISSTMT: INTEGER;
          SUCCESSFUL: BOOLEAN;

       PROCEDURE COMPRESS(BEGINNING, BREAKPT, ENDING: INTEGER;
          STATMTSEPARATION: OPTIONSIZE);

          BEGIN
             IF COMPRESSWANTED OR IFTHENCOMPRESSNEEDED
             THEN
                BEGIN
                   IF STATMTSEPARATION < 1
                   THEN STATMTSEPARATION := 1;
                   BLKSONCURRNTLINE := BLKSONCURRNTLINE +
                      STATMTSEPARATION - 1;
                   SUCCESSFUL := ((ENDING - BEGINNING +
                      BLKSONCURRNTLINE + UNWRITTEN[BEGINNING MOD
                      BUFFERSIZE].INDENTAFTEREOL) < WRITECOL2)
                      AND (CHARCOUNT - BEGINNING < BUFFERSIZE);
                   IF SUCCESSFUL THEN
                      BEGIN
                         BLKSADDEDBYTHISST := BLKSADDEDBYTHISST +
                            STATMTSEPARATION - 1;
                         UNWRITTEN[BREAKPT MOD BUFFERSIZE].
                            INDENTAFTEREOL := - STATMTSEPARATION
                         ;
                      END;
                END;
          END (*COMPRESS*);

       PROCEDURE WRITECOMMENT;

          VAR
             I: 0 .. BUFFSZDIV10;
             SAVEDLENGTH: WIDTH;
             SAVEDSYMBOLNAME: SYMBOLS;
             SAVEDCHARS: SYMBOLSTRING;

          BEGIN
             SAVEDSYMBOLNAME := SYMBOLNAME;
             FOR I := 1 TO LENGTH DO SAVEDCHARS[I] := SYMBOL[I];
             SAVEDLENGTH := LENGTH;   SYMBOLNAME := OTHERSYMBOL;
             IF PROGISPASCAL2   THEN SYMBOL[1] := '('
             ELSE SYMBOL[1] := '/';
             SYMBOL[2] := '*';   LENGTH := 2;   WRITESYMBOL;
             FOR I := 0 TO(STMTSYMLENGTH - 1) DIV 10 DO
            """ UNPACK(STATMTSYMBOL[I + 1], SYMBOL, (I * 10 + 1)
                   );"""
                FOR PQ := 1 TO ALFLEN DO  SYMBOL[I*10+PQ]:=STATMTSYMBOL[I+1,PQ];
             LENGTH := STMTSYMLENGTH;
             SYMBOLNAME := PERIODSYMBOL;
             LASTSYMBOL := PERIODSYMBOL;   WRITESYMBOL;
             SYMBOL[1] := '*';
             IF PROGISPASCAL2   THEN SYMBOL[2] := ')'
             ELSE SYMBOL[2] := '/';
             LENGTH := 2;   WRITESYMBOL;
             SYMBOLNAME := SAVEDSYMBOLNAME;
             LENGTH := SAVEDLENGTH;
             FOR I := 1 TO LENGTH DO SYMBOL[I] := SAVEDCHARS[I];
          END (*WRITECOMMENT*);

       PROCEDURE DOSTATMTLIST(ENDLIST: SYMBOLS);

          VAR
             BLKSAFTERPRT2: WIDTH;
             ATPROCEDEND: BOOLEAN;

          BEGIN
             ATPROCEDEND := ATPROCEDBEGINNING;   WRITESYMBOL;
             READSYMBOL;   STATMTPART[1] := CHARCOUNT + 1;
             STATMTPART[2] := STATMTPART[1];
             IF SYMBOLNAME <> ENDLIST
             THEN
                BEGIN
                   IF PROCEDNAMESWANTED THEN
                      IF ATPROCEDBEGINNING THEN
                         IF LASTPROGPARTWASBODY THEN
                            IF LASTSYMBOL = BEGINSYMBOL
                            THEN WRITECOMMENT;
                   ATPROCEDBEGINNING := FALSE;
                   DOSTATEMENT(ADDEDBLANKS, STATMTSYMBOL,
                      STMTSYMLENGTH);
                   BLKSAFTERPRT2 := ADDEDBLANKS;
                   BLKSADDEDBYTHISSTMT := BLKSADDEDBYTHISSTMT +
                      ADDEDBLANKS;
                   WHILE SYMBOLNAME <> ENDLIST DO
                      BEGIN
                         WRITESYMBOL;   READSYMBOL;
                         IF SYMBOLNAME <> ENDLIST
                         THEN
                            BEGIN
                               STATMTPART[3] := CHARCOUNT + 1;
                               DOSTATEMENT(ADDEDBLANKS,
                                  STATMTSYMBOL, STMTSYMLENGTH);
                               BLKSONCURRNTLINE := ADDEDBLANKS +
                                  BLKSAFTERPRT2;
                               BLKSADDEDBYTHSTMT :=
                                  BLKSADDEDBYTHSTMT +
                                  ADDEDBLANKS;
                               COMPRESS(STATMTPART[2],
                                  STATMTPART[3], CHARCOUNT,
                                  STATMTSEPARATION);
                               IF NOT SUCCESSFUL
                               THEN
                                  BEGIN
                                     BLKSAFTERPRT2 :=
                                        ADDEDBLANKS;
                                     STATMTPART[2] := STATMTPART
                                        [3];
                                  END
                               ELSE
                                  BLKSAFTERPRT2 :=
                                     BLKSONCURRNTLINE;
                            END;
                      END (*WHILE SYMBOLNAME <> ENDLIST*);
                END (*IF SYMBOLNAME <> ENDLIST*);
             BLKSONCURRNTLINE := BLKSADDEDBYTHISSTMT;
             COMPRESS(STATMTBEGINNING, STATMTPART[1], CHARCOUNT,
                SYMBOLGAP);
             STARTNEWLINEANDINDENT;
             STATMTPART[1] := CHARCOUNT;
             REPEAT WRITESYMBOL;   READSYMBOL;
             UNTIL SYMBOLNAME IN [SEMICOLON, UNTILSYMBOL,
                ENDSYMBOL, ELSESYMBOL, PERIODSYMBOL];
             IF SUCCESSFUL
             THEN
                BEGIN
                   IF ENDLIST = UNTILSYMBOL
                   THEN STATMTPART[4] := STATMTSEPARATION
                   ELSE STATMTPART[4] := SYMBOLGAP;
                   COMPRESS(STATMTBEGINNING, STATMTPART[1],
                      CHARCOUNT, STATMTPART[4]);
                END (*IF SUCCESSFUL*);
             IF NOT (SUCCESSFUL AND COMPRESSWANTED)
             THEN
                IF ENDLIST = ENDSYMBOL THEN
                   IF LASTSYMBOL = ENDSYMBOL THEN
                      IF ATPROCEDEND AND PROCEDNAMESWANTED
                      THEN WRITECOMMENT
                      ELSE
                         IF ENDCOMMENTSWANTED
                         THEN WRITECOMMENT;
          END (*DOSTATMTLIST*);

       BEGIN (*DOSTATEMENT*)
          BLKSONCURRNTLINE := 0;
          BLKSADDEDBYTHISST := 0;
          CHANGEMARGINTO(ACTUALLEFTMARGIN + INDENTINDEX);
          STARTNEWLINEANDINDENT;   STATMTBEGINNING := CHARCOUNT;
          IF SYMBOLISNUMBER
          THEN
             BEGIN
                WITH UNWRITTEN[OLDEST] DO
                   BEGIN
                      INDENTAFTEREOL := INDENTAFTEREOL - 1 -
                         LENGTH - SYMBOLGAP;
                      IF INDENTAFTEREOL < 0
                      THEN INDENTAFTEREOL := 0;
                   END;
                WRITESYMBOL;   READSYMBOL (*WRITE LABEL*);
                WRITESYMBOL;   READSYMBOL (*WRITE COLON*);
             END;
          CASE STATEMENTTYPEOF[SYMBOLNAME] OF
             FORWITHWHILESTATEMENT:
                BEGIN
"""                PACK(SYMBOL, 1, STATMTSYMBOL[1]);    """
"SZH"              FOR PQ := 1 TO ALFLEN DO STATMTSYMBOL[1,PQ] := SYMBOL[PQ];
                   STMTSYMLENGTH := LENGTH;
                   REPEAT WRITESYMBOL;   READSYMBOL
                   UNTIL SYMBOLNAME = DOSYMBOL;
                   WRITESYMBOL;   READSYMBOL;
                   STATMTPART[1] := CHARCOUNT + 1;
                   DOSTATEMENT(ADDEDBLANKS, STATMTSYMBOL,
                      STMTSYMLENGTH);
                   BLKSONCURRNTLINE := BLKSONCURRNTLINE +
                      ADDEDBLANKS;
                   BLKSADDEDBYTHISSTMT := BLKSADDEDBYTHISSTMT +
                      ADDEDBLANKS;
                   COMPRESS(STATMTBEGINNING, STATMTPART[1],
                      CHARCOUNT, SYMBOLGAP);
                END;
             REPEATSTATEMENT: DOSTATMTLIST(UNTILSYMBOL);
             IFSTATEMENT:
                BEGIN
"""                PACK(SYMBOL, 1, STATMTSYMBOL[1]);   """
"SZH"              FOR PQ := 1 TO ALFLEN DO STATMTSYMBOL[1,PQ] := SYMBOL[PQ];
                   STMTSYMLENGTH := LENGTH;
                   REPEAT WRITESYMBOL;   READSYMBOL
                   UNTIL SYMBOLNAME = THENSYMBOL;
                   STARTNEWLINEANDINDENT;
                   STATMTPART[1] := CHARCOUNT;   WRITESYMBOL;
                   READSYMBOL;   STATMTPART[2] := CHARCOUNT + 1;
                   DOSTATEMENT(ADDEDBLANKS, STATMTSYMBOL,
                      STMTSYMLENGTH);
                   BLKSONCURRNTLINE := ADDEDBLANKS;
                   BLKSADDEDBYTHISSTMT := ADDEDBLANKS;
                   COMPRESS(STATMTPART[1], STATMTPART[2],
                      CHARCOUNT, SYMBOLGAP);
                   IF SUCCESSFUL
                   THEN
                      COMPRESS(STATMTBEGINNING, STATMTPART[1],
                         CHARCOUNT, STATMTSEPARATION)
                   ELSE IFTHENCOMPRESSNEEDED := TRUE;
                   IF SYMBOLNAME = ELSESYMBOL
                   THEN
                      BEGIN
"""                      PACK(SYMBOL, 1, STATMTSYMBOL[1]);          """
"SZH"                    FOR PQ := 1 TO ALFLEN DO
"SZH"                        STATMTSYMBOL[1,PQ] := SYMBOL[PQ];
                         STMTSYMLENGTH := LENGTH;
                         IFTHENCOMPRESSNEEDED := FALSE;
                         STARTNEWLINEANDINDENT;
                         STATMTPART[3] := CHARCOUNT;
                         WRITESYMBOL;   READSYMBOL;
                         STATMTPART[4] := CHARCOUNT + 1;
                         DOSTATEMENT(ADDEDBLANKS, STATMTSYMBOL,
                            STMTSYMLENGTH);
                         BLKSONCURRNTLINE := ADDEDBLANKS;
                         BLKSADDEDBYTHISSTMT :=
                            BLKSADDEDBYTHISSTMT + ADDEDBLANKS;
                         COMPRESS(STATMTPART[3], STATMTPART[4],
                            CHARCOUNT, SYMBOLGAP);
                         BLKSONCURRNTLINE := BLKSADDEDBYTHISSTMT
                         ;
                         IF SUCCESSFUL THEN
                            COMPRESS(STATMTBEGINNING, STATMTPART[3],
                               CHARCOUNT, STATMTSEPARATION);
                      END
                   ELSE
                      IF (CHARCOUNT - STATMTBEGINNING) <
                         BUFFERSIZE
                      THEN
                         BEGIN
                            COMPRESSWANTED := NOT COMPRESSWANTED
                            ;
                            BLKSONCURRNTLINE := 0;
                            COMPRESS(STATMTBEGINNING, STATMTPART
                               [1], STATMTPART[2], SYMBOLGAP);
                            COMPRESSWANTED := NOT COMPRESSWANTED
                            ;
                         END;
                   IFTHENCOMPRESSNEEDED := FALSE;
                END (*IFSTATEMENT*);
             CASESTATEMENT:
                BEGIN
                   REPEAT WRITESYMBOL;   READSYMBOL
                   UNTIL SYMBOLNAME = OFSYMBOL;
                   WRITESYMBOL;   READSYMBOL;
                   CHANGEMARGINTO(ACTUALLEFTMARGIN + INDENTINDEX
                      );
                   WHILE SYMBOLNAME <> ENDSYMBOL DO
                      BEGIN
                         STARTNEWLINEANDINDENT;
                         STATMTPART[1] := CHARCOUNT;
                         FOR I := 0 TO(LENGTH - 1) DIV 10 DO
"""                         PACK(SYMBOL, (I * 10 + 1), STATMTSYMBOL[I + 1]);"""
"SZH"                       FOR PQ := 1 TO ALFLEN DO
                              STATMTSYMBOL[I+1, PQ] := SYMBOL[I*10 +PQ];
                         STMTSYMLENGTH := LENGTH;
                         REPEAT WRITESYMBOL;   READSYMBOL
                         UNTIL SYMBOLNAME = COLONSYMBOL;
                         WRITESYMBOL;   READSYMBOL;
 (*WRITE COLON*)
                         IF NOT (SYMBOLNAME IN [SEMICOLON,
                            ENDSYMBOL])
                         THEN
                            BEGIN
                               STATMTPART[2] := CHARCOUNT + 1;
                               DOSTATEMENT(ADDEDBLANKS,
                                  STATMTSYMBOL, STMTSYMLENGTH);
                               BLKSONCURRNTLINE := ADDEDBLANKS;
                               BLKSADDEDBYTHISSTMT :=
                                  BLKSADDEDBYTHISSTMT +
                                  ADDEDBLANKS;
                               COMPRESS(STATMTPART[1],
                                  STATMTPART[2], CHARCOUNT,
                                  SYMBOLGAP);
                            END (*IF NOT(SYMBOLNAME...)*);
                         IF SYMBOLNAME = SEMICOLON THEN
                            BEGIN WRITESYMBOL;   READSYMBOL;
                            END;
                      END;
                   CHANGEMARGINTO(ACTUALLEFTMARGIN - INDENTINDEX
                      );
                   STARTNEWLINEANDINDENT;   WRITESYMBOL;
                   READSYMBOL;
                   IF ENDCOMMENTSWANTED AND (LASTSYMBOL =
                      ENDSYMBOL)
                   THEN
                      BEGIN
                         STATMTSYMBOL[1] := 'CASE      ';
                         STMTSYMLENGTH := 4;   WRITECOMMENT;
                      END;
                END (*CASESTATEMENT*);
             OTHERSTATEMENT:
                BEGIN
                   WHILE NOT (SYMBOLNAME IN [SEMICOLON,
                      UNTILSYMBOL, ENDSYMBOL, ELSESYMBOL]) DO
                      BEGIN WRITESYMBOL;   READSYMBOL END;
                END (*OTHER*);
             COMPOUNDSTATEMENT: DOSTATMTLIST(ENDSYMBOL);
          END (*CASE*);
          ADDEDBLANKS := BLKSADDEDBYTHISSTMT;
          CHANGEMARGINTO(ACTUALLEFTMARGIN - INDENTINDEX);
       END (*DOSTATEMENT*);

    BEGIN (*BLOCK*)
       LASTPROGPARTWASBODY := LASTPROGPARTWASBODY AND (
          SYMBOLNAME = BEGINSYMBOL);
       IF SYMBOLNAME = LABELSYMBOL
       THEN DODECLARATIONUNTIL(ENDLABEL);
       IF SYMBOLNAME = CONSTSYMBOL
       THEN DODECLARATIONUNTIL(ENDCONST);
       IF SYMBOLNAME = TYPESYMBOL
       THEN DODECLARATIONUNTIL(ENDTYPE);
       IF SYMBOLNAME = VARSYMBOL
       THEN DODECLARATIONUNTIL(ENDVAR);
       IF SYMBOLNAME = VALUESYMBOL
       THEN DODECLARATIONUNTIL(ENDVALUE);
       WHILE SYMBOLNAME IN [FUNCTIONSYMBOL, PROCEDSYMBOL] DO
          PROCEDURES;
       IF SYMBOLNAME = BEGINSYMBOL
       THEN
          BEGIN
             IF LASTPROGPARTWASBODY THEN
                FOR I := 2 TO PROCEDSEPARATION DO
                   STARTNEWLINEANDINDENT;
             IFTHENCOMPRESSNEEDED := FALSE;
             ATPROCEDBEGINNING := TRUE;
             CHANGEMARGINTO(ACTUALLEFTMARGIN - INDENTINDEX);
             DOSTATEMENT(I, BLOCKNAME, BLOCKNMLENGTH);
 (*I = DUMMY PARAMETER*)
             LASTPROGPARTWASBODY := TRUE;
             CHANGEMARGINTO(ACTUALLEFTMARGIN + INDENTINDEX);
          END
       ELSE
          BEGIN WRITESYMBOL;   READSYMBOL;
 (*WRITE "FORWARD"*)
          END;
    END (*BLOCK*);

 PROCEDURE INITIALIZE;

    VAR
       I: WIDTH;

    BEGIN
"SZH" DIGITS := [ CHMAP['0'], CHMAP['1'], CHMAP['2'], CHMAP['3'],
"SZH"             CHMAP['4'], CHMAP['5'], CHMAP['6'], CHMAP['7'],
"SZH"             CHMAP['8'], CHMAP['9'] ];
       ALPHASYMBOLS := [PROGRAMSYMBOL, BEGINSYMBOL, ENDSYMBOL,
          CONSTSYMBOL, TYPESYMBOL, RECORDSYMBOL, CASESYMBOL,
          IFSYMBOL, THENSYMBOL, ELSESYMBOL, DOSYMBOL, OFSYMBOL,
          FORSYMBOL, WITHSYMBOL, WHILESYMBOL, REPEATSYMBOL,
          UNTILSYMBOL, IDENTIFIER, VARSYMBOL, VALUESYMBOL,
          PROCEDSYMBOL, FUNCTIONSYMBOL, LABELSYMBOL,
          ALPHAOPERATOR];
       ENDLABEL := [CONSTSYMBOL, TYPESYMBOL, VARSYMBOL,
          VALUESYMBOL, PROCEDSYMBOL, FUNCTIONSYMBOL, BEGINSYMBOL
          ];
       ENDCONST := ENDLABEL - [CONSTSYMBOL];
       ENDTYPE := ENDCONST - [TYPESYMBOL];
       ENDVAR := ENDTYPE - [VARSYMBOL];
       ENDVALUE := ENDVAR - [VALUESYMBOL];   WRITECOLUMN := 1;
       LEFTMARGIN := 0;   ACTUALLEFTMARGIN := 0;
       OUTPUTCOL := 1;   READCOL1 := 1;   READCOL2 := 999;
       WRITECOL1 := 1;   WRITECOL2 := 72;   OLDEST := 1;
       CHARCOUNT := 1;   LINENUMBER := 0;   INCREMENT := 0;
       PACKERISOFF := TRUE;   COMPRESSWANTED := FALSE;
       DISPLAYISON := TRUE;   DISPLAYWANTED := TRUE;
       NOFORMATTING := FALSE;   CROSSREFWANTED := FALSE;
       PROCEDNAMESWNTD := TRUE;   ENDCOMMENTSWNTD := FALSE;
       INDENTINDEX := 3;   LONGLINEINDENT := 3;
       PROCEDSEPARATION := 3;   SYMBOLGAP := 1;
       STATMTSEPARATION := 3;   DECLARALIGNMENT := 0;
       READCOLUMN := 0;   LASTSYMBOL := PERIODSYMBOL;
       LASTPROGPARTWASBODY := FALSE;
       READINGFORMATOPTIONS := FALSE;   PROGISPASCAL2 := FALSE;
       RESET(PROG);
    END (*INITIALIZE*);

 PROCEDURE READFORMATOPTIONS;

    BEGIN
       IF NOT EOF (*EOS*) (INPUT) THEN
          BEGIN
             READINGFORMATOPTIONS := TRUE;   READSYMBOL;
             READINGFORMATOPTIONS := FALSE;
          END;
    END (*READFORMATOPTIONS*);

 BEGIN (*MAINPROGRAM*)
"szh" init_chmap;
    (*MESSAGE*)
    WRITELN(OUTPUT, ' >>>> PASCAL PROGRAM FORMATTER VERS. 1/5/76 <<<<');
    (*MESSAGE*)
    WRITELN(OUTPUT, ' -----------------------------------------------');
    WRITELN(OUTPUT);

""" LINELIMIT(OUTPUT, - 1);    """
 (* UNLIMITED OUTPUT IS ALLOWED*)
    CONSTANTSINITIALIZATION;   INITIALIZE;   READFORMATOPTIONS;
    READACHARACTER;   WRITEA(' ');   READSYMBOL;
    PROGISPASCAL2 := SYMBOLNAME = PROGRAMSYMBOL;
    IF PROGISPASCAL2
    THEN
       BEGIN
          STARTNEWLINEANDINDENT;   WRITESYMBOL;   READSYMBOL;
          FOR I := 0 TO(LENGTH - 1) DIV 10 DO
"""          PACK(SYMBOL, (I * 10 + 1), MAIN[I + 1]);      """
"SZH"        FOR PQ := 1 TO ALFLEN DO MAIN[I+1,PQ] := SYMBOL[I*10+PQ];
          MAINNMLENGTH := LENGTH;
          REPEAT WRITESYMBOL;   READSYMBOL;
          UNTIL SYMBOLNAME = SEMICOLON;
          WRITESYMBOL;   READSYMBOL;   STARTNEWLINEANDINDENT;
          PSYMBOLNAME[7] := IDENTIFIER;
          PSYMBOLNAME[31] := IDENTIFIER;
          PSYMBOLNAME[32] := IDENTIFIER;
       END (*IF PROGISPASCAL2*);
    BLOCK(MAIN, MAINNMLENGTH);   WRITEA('.');   WRITEA(' ');
    WITH UNWRITTEN[OLDEST] DO
       BEGIN CHISENDLINE := TRUE;   INDENTAFTEREOL := 0; END;
    WRITECOLUMN := 0;   FOR I := 0 TO 159 DO WRITEA(' ');
    WRITELN(OUTPUT);

    IF SYMBOLNAME <> PERIODSYMBOL
    THEN (*MESSAGE*)  WRITELN(OUTPUT, ' >>>> ERROR(S) IN FORMATTING.');
    (*MESSAGE*)  WRITELN(OUTPUT, ' >>>> END FORMATTING');
 END (*MAINPROGRAM*).
PROGRAM PASCREF(INPUT,OUTPUT);      (*$D- N.WIRTH  2.7.75*)
(*CROSS REFERENCE GENERATOR FOR PASCAL PROGRAMS*)
(*QUADRATIC QUOTIENT HASH METHOD*)
(*MODIFIED SLIGHTLY BY A. MICKEL 75/12/08 AND D. LALIBERTE
  78/03/15 TO PRODUCE PROCEDURE LIST AND SKIP COMPILER TITLE*)
(*$T-,P-,R-,B4 TESTS OFF, PMD OFF, DYNAMIC STORAGE, BIG BUFFERS.*)
LABEL 99;
CONST P = 1499;     (*SIZE OF HASH TABLE*)
  NK = 33;          (*NO. OF KEYWORDS*)
  KLN = 10;         (*KEYLENGTH*)
  LPPG = 55;        (*NO. OF LINES PER PAGE*)
  LLMAX = 132;      (*LINE LENGTH DEFAULT MAX*)
  LLMIN = 72;       (*LINE LENGTH MINIMUM*)
  MAXN = 10000;     (*MAX NO. OF LINES*)
  DGPN =  6;        (*NO. OF DIGITS PER NUMBER*)
  LITL = 3;         (*NUMBER OF LINES IN COMPILER TITLE*)
  ADDRWIDTH = 6;    (*NUMBER OF DIGITS IN CODE ADDRESS*)
  EMPTY = '          ';
  STARS = ' *****';
TYPE INDEX = 0..P;
  ALFA = PACKED ARRAY [1..KLN] OF CHAR;
  REF = @ITEM;
  WORD = RECORD KEY: ALFA;
           FIRST: REF;
         END ;
  ITEM = PACKED RECORD
           LNO: 0..MAXN;
           NEXT: REF
         END ;
  PROCREF = @PROC;  (*PROCEDURE OR FUNCTION REFERENCE*)
  PROC = PACKED RECORD
           NAME: ALFA;
           LNO: 0..MAXN;
           NEXT: PROCREF
         END ;
VAR I: INDEX;
  K: INTEGER;
  M: INTEGER;       (*NO. OF LINES ON PAGE*)
  N: INTEGER;       (*NO. OF LINES INPUT*)
  LN: INTEGER;      (*CURRENT LINE NUMBER*)
  LLNGOUT: INTEGER; (*LINE LENGTH FOR OUTPUT*)
  LLNGIN: INTEGER;  (*LINE LENGTH FOR INPUT*)
  CCOUNT: INTEGER;  (*CHARACTER COUNT IN LINE*)
  NOPL: INTEGER;    (*NO. OF LINE-NUMBERS PER LINE*)
  ID: RECORD CASE BOOLEAN OF
             FALSE: (A: ALFA);
             TRUE:  (ORD: INTEGER)
      END ;
  T: ARRAY [INDEX] OF WORD;    (*HASH TABLE*)
  KEY: ARRAY [1..NK] OF ALFA;
  PROCORFUNC,
  COMPILERLISTING,
  LINENUMBERS: BOOLEAN;
  FIRSTPROC,
  PROCPTR: PROCREF; (*POINTERS TO CHAIN OF PROCEDURES*)


   FUNCTION LETTER(C: CHAR): BOOLEAN;

     BEGIN
     LETTER := (('A' <= C) AND (C <= 'Z')) OR
               (('a' <= C) AND (C <= 'i')) OR
               (('j' <= C) AND (C <= 'r')) OR
               (('s' <= C) AND (C <= 'z')) ;
     END ;

   FUNCTION DIGIT(C: CHAR): BOOLEAN ;

     BEGIN
     DIGIT := ('0' <= C) AND (C <= '9') ;
     END ;

   FUNCTION SPECIAL(C: CHAR): BOOLEAN;
     BEGIN  SPECIAL := (C = '$') OR (C = '_')  END ;

FUNCTION NOKEY: BOOLEAN;
   VAR I,J,K: INTEGER;
BEGIN I := 1; J := NK;   (*BINARY SEARCH*)
  REPEAT K := (I+J) DIV 2;
    IF KEY[K] <= ID.A THEN I := K+1 ELSE J := K-1
  UNTIL I > J;
  IF J = 0 THEN NOKEY := TRUE ELSE
    NOKEY := KEY[J] <> ID.A
END (*NOKEY*) ;

PROCEDURE COUNTLINE;
BEGIN
  IF M = LPPG THEN
    BEGIN PAGE(OUTPUT); WRITELN(OUTPUT);    WRITELN(OUTPUT);
      M := 0
    END;
  M := M + 1
END (*COUNTLINE*) ;

PROCEDURE ADVANCE;
BEGIN
  WRITE(OUTPUT,INPUT@); GET(INPUT);
  CCOUNT := CCOUNT + 1;
  IF CCOUNT = LLNGIN THEN
    WHILE NOT EOLN(INPUT) DO
      BEGIN WRITE(OUTPUT,INPUT@); GET(INPUT);
      END
END (*ADVANCE*);

PROCEDURE SPACE(J: INTEGER);
BEGIN
  REPEAT J := J-1; WRITELN(OUTPUT); COUNTLINE
  UNTIL J = 0
END (*SPACE*) ;

PROCEDURE NEWLINE;
BEGIN CCOUNT := 0;
  IF N < MAXN THEN
  BEGIN COUNTLINE;  N := N + 1;
    IF COMPILERLISTING THEN
      BEGIN IF NOT EOLN THEN
        BEGIN ADVANCE;
        IF NOT (INPUT@ IN ['0'..'9']) THEN  (* ERRORS *)
        IF NOT DIGIT(INPUT@) THEN  (* ERRORS *)
          WHILE NOT EOLN DO
            ADVANCE
        ELSE BEGIN
          FOR I := 1 TO ADDRWIDTH + 1  DO
            ADVANCE;
          WHILE (INPUT@ = ' ') AND NOT EOLN DO
            ADVANCE
          END
        END
      END
    ELSE WRITE(OUTPUT,' ');
    IF LINENUMBERS THEN
      BEGIN LN := 0;
      WHILE DIGIT(INPUT@) DO
      WHILE INPUT@ IN ['0'..'9'] DO
        BEGIN LN := 10*LN + ORD(INPUT@) - ORD('0');
          ADVANCE
        END
      END
    ELSE BEGIN
      LN := N;  WRITE(OUTPUT,LN:6, ' ')
      END
    END
  ELSE BEGIN
    WRITELN(STARS, ' TEXT TOO LONG', STARS);
     GOTO 99;  EXIT(99);
    END
END (*NEWLINE*) ;

PROCEDURE SEARCH;   (*MODULO P HASH SEARCH*)
  VAR H,D: INDEX;
      X: REF; F: BOOLEAN;
      K: INTEGER;
BEGIN  I := ABS(ID.ORD);  H := I MOD P;
  F := FALSE; D := 1;
  NEW(X); X@.LNO := LN;
  REPEAT
    IF T[H].KEY = ID.A THEN
    BEGIN (*FOUND*) F := TRUE;
      X@.NEXT := T[H].FIRST; T[H].FIRST := X;
    END ELSE
    IF T[H].KEY = EMPTY THEN
    BEGIN (*NEW ENTRY*) F := TRUE;
      T[H].KEY := ID.A;
      T[H].FIRST := X; X@.NEXT := NIL
    END ELSE
    BEGIN (*COLLISION*) H := H+D; D := D+2;
      IF H >= P THEN H := H-P;
      IF D = P THEN
        BEGIN WRITELN(OUTPUT); WRITELN(STARS,' TABLE FULL',STARS);    GOTO 99
        EXIT(99);
        END
    END
  UNTIL F
END (*SEARCH*) ;

PROCEDURE SORT(MIN, MAX: INTEGER);

(* QUICKSORT WITH BOUNDED RECURSION DEPTH *)
(* REQUIRES MIN <= MAX *)

   VAR
         LOW,
        HIGH: INDEX;
      MIDKEY: ALFA;
        TEMP: WORD;

   BEGIN
      REPEAT (*PICK SPLIT POINT*)
         MIDKEY := T[(MIN + MAX) DIV 2].KEY;
         LOW := MIN;
         HIGH := MAX;
         REPEAT (*PARTITION*)
            WHILE T[LOW].KEY < MIDKEY DO
               LOW := LOW + 1;
            WHILE T[HIGH].KEY > MIDKEY DO
               HIGH := HIGH - 1;
            IF LOW <= HIGH THEN
               BEGIN
                  TEMP := T[LOW];
                  T[LOW] := T[HIGH];
                  T[HIGH] := TEMP;
                  LOW := LOW + 1;
                  HIGH := HIGH - 1
               END;
         UNTIL LOW > HIGH;

         (*RECURSIVELY SORT SHORTER SUB-SEGMENT*)    (*A NOTE *)
         IF HIGH - MIN < MAX - LOW
         THEN    � ANOTHER NOTE�  � A FORTH ONE �
            BEGIN
               IF MIN < HIGH THEN
                  SORT(MIN, HIGH);
  �THIS ONE �  MIN := LOW
            END
" "      ELSE
            BEGIN
               IF LOW < MAX THEN
                  SORT(LOW, MAX);
               MAX := HIGH
               END
        UNTIL MAX <= MIN
   END " SORT" """(*SORT*)";


PROCEDURE " HERE " NOTEPROC;   (*NOTE INSTANCE OF PROCEDURE OR FUNCTION*)
  VAR P: PROCREF;
BEGIN PROCORFUNC := FALSE;
  NEW(P); PROCPTR@.NEXT := P;
  P@.NAME := ID.A; P@.LNO := LN; P@.NEXT := NIL;
  PROCPTR := P
END (*NOTEPROC*) ;

PROCEDURE PRINTWORD(W: WORD);
  VAR L: INTEGER; X,Y,Z: REF;
BEGIN COUNTLINE; WRITE(OUTPUT,' ', W.KEY);
  X := W.FIRST; Y := X@.NEXT; X@.NEXT := NIL;
  WHILE Y <> NIL DO
    BEGIN Z := Y@.NEXT; Y@.NEXT := X; X := Y; Y := Z
    END ;
  L := 0;
  REPEAT
    IF L = NOPL THEN
      BEGIN L := 0; WRITELN(OUTPUT); COUNTLINE; WRITE(OUTPUT,' ', EMPTY)
      END;
    L := L+1; WRITE(OUTPUT,X@.LNO: DGPN); X := X@.NEXT
  UNTIL X = NIL;
  WRITELN(OUTPUT);
END (*PRINTWORD*) ;

PROCEDURE PRINTTABLE;
  VAR I,M: INDEX;
BEGIN M := 0;    (*COMPRESS TABLE*)
  FOR I := 0 TO P-1 DO
    IF T[I].KEY <> EMPTY THEN
      BEGIN T[M] := T[I]; M := M+1
      END ;
  IF M > 0 THEN SORT(0,M-1);
  NOPL := (LLNGOUT-KLN-1) DIV DGPN;
  SPACE(2); WRITELN(' CROSS REFERENCE OF IDENTIFIERS,',
            ' LABEL DECLARATIONS AND GOTO STATEMENTS:');
  COUNTLINE; SPACE(1);
  FOR I := 0 TO M-1 DO PRINTWORD(T[I])
END (*PRINTTABLE*) ;

PROCEDURE PRINTPROCS;
BEGIN SPACE(1); COUNTLINE;
  WRITELN(' LIST OF PROCEDURES AND FUNCTIONS:');
  COUNTLINE; SPACE(1);
  PROCPTR := FIRSTPROC@.NEXT;
  WHILE PROCPTR <> NIL DO
    BEGIN WITH PROCPTR@ DO WRITELN(NAME:24,LNO:10);
      COUNTLINE; PROCPTR := PROCPTR@.NEXT
    END
END (*PRINTPROCS*) ;

PROCEDURE INITIALIZE;
  TYPE SETTING = PACKED RECORD
                   CASE SWITCH: BOOLEAN OF
                     TRUE: (ONOFF: CHAR);
                     FALSE: (SIZE: 0..999999)
                   END;
  VAR S: SETTING;
  FUNCTION OPTION(NAME: CHAR; VAR S: SETTING): BOOLEAN;
    EXTERNAL;
BEGIN N := 0; M := 0;
  LLNGIN := LLMAX; LLNGOUT := LLMAX;
   IF OPTION('U',S) THEN
    IF S.SWITCH AND (S.ONOFF = '+')
      THEN LLNGIN := LLMIN;
  IF OPTION('W',S) THEN
    IF S.SWITCH AND (S.ONOFF = '+')
      THEN LLNGOUT := LLMIN;
  FOR I := 0 TO P-1 DO T[I].KEY := EMPTY;
  NEW(PROCPTR); FIRSTPROC := PROCPTR; PROCPTR@.NEXT := NIL;
  PROCORFUNC := TRUE;   (*TO GET P R O G R A M NAME IN PROCEDURE INDEX*)
  KEY[ 1] := 'AND       '; KEY[ 2] := 'ARRAY     ';
  KEY[ 3] := 'BEGIN     '; KEY[ 4] := 'CASE      ';
  KEY[ 5] := 'CONST     '; KEY[ 6] := 'DIV       ';
  KEY[ 7] := 'DOWNTO    '; KEY[ 8] := 'DO        ';
  KEY[ 9] := 'ELSE      '; KEY[10] := 'END       ';
  KEY[11] := 'FILE      '; KEY[12] := 'FOR       ';
  KEY[13] := 'FUNCTION  '; KEY[14] := 'IF        ';
  KEY[15] := 'IN        '; KEY[16] := 'MOD       ';
  KEY[17] := 'NIL       '; KEY[18] := 'NOT       ';
  KEY[19] := 'OF        '; KEY[20] := 'OR        ';
  KEY[21] := 'PACKED    '; KEY[22] := 'PROCEDURE ';
  KEY[23] := 'PROGRAM   '; KEY[24] := 'RECORD    ';
  KEY[25] := 'REPEAT    '; KEY[26] := 'SET       ';
  KEY[27] := 'THEN      '; KEY[28] := 'TO        ';
  KEY[29] := 'TYPE      '; KEY[30] := 'UNTIL     ';
  KEY[31] := 'VAR       '; KEY[32] := 'WHILE     ';
  KEY[33] := 'WITH      '
END (*INITIALIZE*) ;

PROCEDURE SCANANDLISTINPUT;
BEGIN
  WHILE NOT EOF(INPUT) DO
  BEGIN NEWLINE;
    WHILE NOT EOLN(INPUT) DO
    CASE INPUT@ OF
     'a','b','c','d','e','f','g','h','i','j','k','l','m',
     'n','o','p','q','r','s','t','u','v','w','x','y','z',
     'A','B','C','D','E','F','G','H','I','J','K','L','M',
     'N','O','P','Q','R','S','T','U','V','W','X','Y','Z':
      BEGIN K := 0; ID.A := EMPTY;
        REPEAT
          IF K < KLN THEN
            BEGIN K := K+1; ID.A[K] := INPUT@
            END;
          ADVANCE
        UNTIL NOT(INPUT@ IN ['A'..'Z', '0'..'9']);
        UNTIL NOT(LETTER(INPUT@) OR DIGIT(INPUT@) OR SPECIAL(INPUT@));
        IF NOKEY THEN
        BEGIN SEARCH;
          IF PROCORFUNC THEN NOTEPROC
        END ELSE
        IF (ID.A = 'PROCEDURE ') OR (ID.A = 'FUNCTION  ') THEN
          PROCORFUNC := TRUE
      END;
     '0','1','2','3','4','5','6','7','8','9':
        REPEAT ADVANCE;
        UNTIL NOT (INPUT@ IN ['B','E','0'..'9']);
        UNTIL NOT DIGIT(INPUT@) ;
     '''':
      BEGIN (*STRING*)
        REPEAT ADVANCE;
        UNTIL (INPUT@ = '''') OR EOLN(INPUT);
        IF NOT EOLN(INPUT) THEN
          ADVANCE
      END;
#    '"':
      BEGIN (*COMMENT*)
        REPEAT ADVANCE;
          WHILE EOLN(INPUT) DO
            BEGIN WRITELN(OUTPUT); GET(INPUT); NEWLINE
            END
        UNTIL INPUT@ = '"';
        ADVANCE
      END;
     '(':
      BEGIN ADVANCE;
        IF INPUT@ = '*' THEN
        BEGIN (*COMMENT*) ADVANCE;
          REPEAT
            WHILE INPUT@ <> '*' DO
            BEGIN
              IF EOLN(INPUT) THEN
                BEGIN GET(INPUT); WRITELN(OUTPUT); NEWLINE
                END ELSE
                ADVANCE
            END ;
            ADVANCE
          UNTIL INPUT@ = ')';
          ADVANCE
        END
      END;
     '+','-','*','/',')','$','=',' ',',','.','[',']',
     ':','!','�','&','@','?','<','>','�','\','^',';','#','_','%','�':
      ADVANCE
    END (*CASE*) ;
    WRITELN(OUTPUT); GET(INPUT)
  END ;
END (*SCANANDLISTINPUT*) ;

PROCEDURE SKIPCOMPILERTITLE;
  VAR I: INTEGER;
BEGIN
  COMPILERLISTING := INPUT@ = '1';
  IF COMPILERLISTING THEN
  BEGIN I := 0; GET(INPUT);
    WHILE I < LITL DO
      BEGIN I := I + 1;
        WHILE NOT EOLN(INPUT) DO
          ADVANCE;
        READLN; WRITELN(OUTPUT);
      END;
    COUNTLINE;
    LINENUMBERS := TRUE
  END ELSE
  BEGIN WRITELN(OUTPUT); WRITELN(OUTPUT);
    LINENUMBERS :=    INPUT@ IN ['0'..'9']    DIGIT(INPUT@)
  END
END (*SKIPCOMPILERTITLE*) ;

BEGIN (*CROSSREF*)
   LINELIMIT(OUTPUT, MAXN);     PAGE(OUTPUT); INITIALIZE;
  IF NOT EOF(INPUT) THEN
  BEGIN SKIPCOMPILERTITLE;
    SCANANDLISTINPUT;    LINELIMIT(OUTPUT, MAXN);
    PRINTTABLE; PRINTPROCS
  END ELSE WRITELN(STARS,' NO PROGRAM FOUND TO CROSS REFERENCE',STARS);
99:END .
