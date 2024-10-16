   (*$D-                     *)

(*   COMPARE - COMPARE TWO TEXT FILES AND REPORT THEIR DIFFERENCES
     COPYRIGHT (C) 1977,1978
     JAMES F. MINER
     SOCIAL SCIENCE RESEARCH FACILITIES CENTER
     UNIVERSITY OF MINNESOTA

     GENERAL PERMISSION TO USE IN A NON PROFIT MANNER.

     COMPARE IS USED TO DISPLAY ON OUTPUT THE DIFFERENCES BETWEEN
     TWO SIMILAR TEXTS (FILEA AND FILEB). NOTABLE CHARACTERISTICS ARE

     - COMPARE IS LINE ORIENTED. THE SMALLEST UNIT OF COMPARISON
       IS THE TEXT LINE (IGNORING TRAILING BLANKS). THE PRESENT
       IMPLEMENTATION HAS A FIXED MAXIMUM LINE LENGTH.


     - BY MANIPULATING A PROGRAM PARAMETER, THE USER CAN AFFECT
       COMPARES SENSITIVITY TO THE "LOCALITY" OF DIFFERENCES.
       MORE SPECIFICALLY THIS PARAMETER MINLINESFORMATCH, SPECIFIES
       THE NUMBER OF CONSECUTIVE LINES ON EACH FILE WHICH MUST MATCH
       IN ORDER THAT THEY BE CONSIDERED AS TERMINATING THE CURRENT
       MISMATCH. A LARGE VALUE FOR MINLINESFORMATCH TENDS TO PRODUCE
       FEWER BUT LARGER MISMATCHES THAN DOES A SMALLER VALUE. THE VALUE
       SIX APPEARS TO GIVE GOOD RESULTS ON PASCAL SOURCE FILES BUT MAY
       BE INAPPROPIATE FOR OTHER APPLICATIONS.

       IF COMPARE IS TO BE USED AS A GENERAL UTILITY PROGRAM, THEN
       MINLINESFORMATCH SHOULD BE A PARAMETER. IT IS DECLARED AS
       A CONSTANT FOR THE SAKE OF PORTABILITY.

     - COMPARE EMPLOYS A SIMPLE BACKTRACKING SEARCH ALGORITHM TO
       ISOLATE MISMATCHES FROM THEIR SURROUNDING MATCHES. THIS
       REQUIRES (HEAP) STORAGE ROUGHLY PROPORTIONAL TO THE SIZE
       OF THE LARGEST MISMATCH, AND TIME ROUGHLY PROPORTIONAL
       TO THE SQUARE OF THE SIZE OF EACH MISMATCH FOR EACH MISMATCH.
       FOR THIS REASON IT MAY NOT BE FEASIBLE TO USE COMPARE ON FILES
       WITH VERY LONG MISMATCHES.

     - TO THE BEST OF THE AUTHORS KNOWLEDGE, COMPARE UTILIZES ONLY
       STANDARD COMPARE.

     - COMPARE WAS IMPLEMENTED AT UNL IN AUGUST 1978 BY CURT HILL.
       MODIFICATIONS REQUIRED TO FORCE COMPATIBILITY WITH STANFORD
       PASCAL WERE:
        ADDITION OF EMPTY PARENS FOR PARAMATERLESS WRITELN CALLS I.E.
        FROM < WRITELN; > TO < WRITELN(); >
        DELETION OF PACK CALLS (ALL CHARS ARE PACKED)
        ADDITION OF A PAGE PROCEDURE AND TEXT DECLARATION WHICH
        ARE ABSENT FROM THIS COMPILER.
        IMPLEMENTATION OF SQUARE BRACKETS AS (/ /)
        LIMITING MAXIMUM LINELENGTH TO 80
        DELETION OF NONSTANDARD FILES FROM PROGRAM HEAD.
        ADDITION OF FILLER TO ENSURE BOUNDARY ALLIGNMENT OF STREAM
         SUPPRESSION OF THE PRINTING OF THE SENTINEL

                                                             *)


PROGRAM COMPARE(OUTPUT);

  CONST
    VERSION = '1.2P (78/03/01)';
    LINELENGTH =134;  (* MAXIMUM SIGNIFICANT INPUT LINE LENGTH *)
    MINLINESFORMATCH = 6; (* LINES TO END MIS-MATCH *)

  TYPE
    LINEPOINTER = @LINE;
    LINE =                 (* SINGLE LINE BUFFER *)
       RECORD
         NEXTLINE : LINEPOINTER;
         LENGTH : 0..LINELENGTH;
         IMAGE : ARRAY (/0..LINELENGTH/) OF CHAR;
       END;
    STREAM =               (* BOOKKEEPING FOR EACH INPUT FILE *)
       RECORD
         CURSOR, HEAD, TAIL : LINEPOINTER;
         CURSORLINENO, HEADLINENO, TAILLINENO : INTEGER;
         ENDFILE : BOOLEAN;
         FILLER : ARRAY (/1..3/) OF CHAR; (* FORCES BOUNDARY ALIGNMENT*)
       END;
    TEXT = FILE OF CHAR;

  VAR
    FILEA, FILEB : TEXT;
    A, B : STREAM;
    MATCH :BOOLEAN;
    ENDFILE : BOOLEAN;    (* SET IF END OF STREAM FOR A OR B *)
    TEMPLINE :            (* USED BY READLINE *)
      RECORD
        LENGTH : INTEGER;
        IMAGE : ARRAY (/0..LINELENGTH/) OF CHAR;
        END;

    FREELINES : LINEPOINTER;   (* FREE LIST OF LINE BUFFERS *)

    SAME : BOOLEAN;            (* FALSE IF NO MIS-MATCHES OCCUR *)


  PROCEDURE PAGE (VAR FILEX : TEXT);
    BEGIN
    WRITELN(FILEX);
    WRITELN(FILEX,'1  ');
    END;


  PROCEDURE COMPAREFILES;

    FUNCTION ENDSTREAM(VAR X : STREAM) : BOOLEAN;
      BEGIN  (* ENDSTREAM *)
      ENDSTREAM := ((X.CURSOR = NIL)  AND X.ENDFILE)
      END; (* ENDSTREAM *)

    PROCEDURE MARK(VAR X : STREAM);
      (* CAUSES BEGINNING OF STREAM TO BE POSITIONED BEFORE
         CURRENT STREAM CURSOR. BUFFERS GET RECLAIMED, LINE
         COUNTERS RESET, ETC.                               *)
      VAR
        P : LINEPOINTER;
    BEGIN (* MARK *)
      WITH X DO
        IF HEAD <> NIL
           THEN BEGIN
                WHILE HEAD <> CURSOR DO (* RECLAIM BUFFERS *)
                  BEGIN
                    WITH HEAD@ DO
                      BEGIN
                      P := NEXTLINE;
                      NEXTLINE := FREELINES;
                      FREELINES := HEAD
                      END;
                   HEAD := P
                   END;
              HEADLINENO := CURSORLINENO;
              IF CURSOR = NIL
                 THEN BEGIN
                      TAIL := NIL;
                      TAILLINENO := CURSORLINENO
                      END
                 ELSE;
              END
           END; (* MARK *)


    PROCEDURE MOVECURSOR(VAR X : STREAM; VAR FILEX : TEXT);

      (* FILEX IS THE INPUT FILE ASSOCIATED WITH STREAM X. THE CURSOR
         FOR X IS MOVED FORWARD ONE LINE, READING FROM X IF NECESARY,
         AND INCREMENTING THE LINE COUNT. ENDFILE IS SET IF EOF IS
         ENCOUNTERED ON EITHER STREAM.                     *)


      PROCEDURE READLINE;
        VAR
          NEWLINE : LINEPOINTER;
          C, C2 : 0..LINELENGTH;

        BEGIN (* READLINE *)
          IF NOT X.ENDFILE
             THEN BEGIN
            �"+++ C := 0;
                  WHILE NOT EOLN(FILEX) AND (C < LINELENGTH) DO
                    BEGIN    (* READ A LINE INTO TEMPLINE.IMAGE *)
                    C := C + 1;
                    TEMPLINE.IMAGE(/C/) := FILEX@;
                    GET(FILEX)
                    END;
                  READLN(FILEX);
                  WHILE (TEMPLINE.IMAGE(/C/) = ' ') AND (C < LINELENGTH) DO
                     C := C + 1;
                  IF C < TEMPLINE.LENGTH
                     THEN
                       FOR C2 := C+1 TO TEMPLINE.LENGTH DO
                         TEMPLINE.IMAGE (/C2/) := ' '
                     ELSE;  +++"�
#
#                 readln(filex, templine.image);
#                 c := linelength;
#                   repeat
#                   c := c-1
#                   until (templine.image[c] <> ' ') or (c = 1);
#
                  TEMPLINE.LENGTH := C;
                  NEWLINE := FREELINES;
                  IF NEWLINE = NIL
                     THEN NEW (NEWLINE)
                     ELSE FREELINES := FREELINES@.NEXTLINE;
                  NEWLINE@.IMAGE := TEMPLINE.IMAGE; (* THIS STMT IS
                                             IN LIEU OF A PACK*)
                  NEWLINE@.LENGTH := C;
                  NEWLINE@.NEXTLINE := NIL;
                  IF X.TAIL = NIL
                     THEN BEGIN
                          X.HEAD := NEWLINE;
                          X.TAILLINENO := 1;
                          X.HEADLINENO := 1
                          END
                     ELSE BEGIN
                          X.TAIL@.NEXTLINE := NEWLINE;
                          X.TAILLINENO := X.TAILLINENO + 1
                          END;
                  X.TAIL := NEWLINE;
                  X.ENDFILE := EOF(FILEX)
                  END
              END; (* READLINE *)


      BEGIN    (* MOVECURSOR *)
        IF X.CURSOR <> NIL
           THEN BEGIN
                IF X.CURSOR = X.TAIL
                   THEN READLINE
                   ELSE;
                X.CURSOR := X.CURSOR@.NEXTLINE;
                IF X.CURSOR = NIL
                   THEN ENDFILE := TRUE
                   ELSE;
                X.CURSORLINENO := X.CURSORLINENO + 1;
                END
           ELSE IF NOT X.ENDFILE
                   THEN BEGIN  (* BEGINNING OF STREAM *)
                        READLINE;
                        X.CURSOR := X.HEAD;
                        X.CURSORLINENO := X.HEADLINENO
                        END
                   ELSE ENDFILE := TRUE;  (* END OF STREAM *)
        END;  (* MOVECURSOR *)



   PROCEDURE BACKTRACK(VAR X :STREAM; VAR XLINES : INTEGER);
     (* CAUSES THE CURRENT POSITION OF STREAM X TO BECOME THAT OF THE
        LAST MARK OPERATION. I.E. THE CURRENT LINE WHEN THE STREAM
        WAS MARKED LAST BECOMES THE NEW CURSOR. XLINES IS SET TO THE
        NUMBER OF LINES FROM THE NEW CURSOR TO THE OLD CURSOR INCLUSIVE
                                         *)

     BEGIN   (* BACKTRACK *)
       XLINES := X.CURSORLINENO + 1 - X.HEADLINENO;
       X.CURSOR := X.HEAD;
       X.CURSORLINENO := X.HEADLINENO;
       ENDFILE := ENDSTREAM (A) OR ENDSTREAM (B);
       END;   (* BACKTRACK *)


    PROCEDURE COMPARELINES (VAR MATCH : BOOLEAN);

      (* COMPARE THE CURRENT LINES OF STREAMS A AND B, RETURNING MATCH
         TO SIGNAL THEIR (NON-) EQUIVALENCE. EOF ON BOTH STREAMS IS
         CONSIDERED A MATCH, BUT EOF ON ONLY ONE STREAM IS A MISMATCH *)



      BEGIN       (* COMPARELINES *)
        IF (A.CURSOR = NIL) OR (B.CURSOR = NIL)
           THEN MATCH := ENDSTREAM (A) AND ENDSTREAM (B)
           ELSE BEGIN
                MATCH := (A.CURSOR@.LENGTH = B.CURSOR@.LENGTH);
                IF MATCH
                   THEN MATCH := (A.CURSOR@.IMAGE = B.CURSOR@.IMAGE)
                   ELSE
                END
      END;        (* COMPARELINES *)




    PROCEDURE FINDMISMATCH;
      BEGIN (* FINDMISMATCH *)
        (* NOT  ENDFILE AND MATCH *)
        REPEAT   (* COMPARENEXTLINES *)
          MOVECURSOR(A, FILEA);
          MOVECURSOR(B, FILEB);
          MARK (A);
          MARK (B);
          COMPARELINES(MATCH);
        UNTIL ENDFILE OR NOT MATCH;
      END;           (* FINDMISMATCH *)




    PROCEDURE FINDMATCH;
      VAR
        ADVANCEB : BOOLEAN; (* TOGGLE ONE-LINE
                              LOOKAHEAD BETWEEN STREAMS *)
      PROCEDURE SEARCH(VAR X : STREAM;  (* STREAM TO SEARCH *)
                       VAR FILEX : TEXT;
                       VAR Y : STREAM;  (* STREAM TO LOOKAHEAD *)
                       VAR FILEY : TEXT);

       (* LOOK AHEAD ONE LINE ON STREAM Y, AND SEARCH FOR THAT LINE
          BACKTRACKING ON STREAM X. *)

         VAR
           COUNT : INTEGER; (* NUMBER OF LINES BACKTRACKED ON X *)

         PROCEDURE CHECKFULLMATCH;
         (* FROM  THE CURRENT POSITIONS OF X AND Y, WHICH MATCH
            MAKE SURE THAT THE NEXT MINLINESFORMATCH - 1 LINES ALSO
            MATCH, OR SET MATCH := FALSE   *)

         VAR
           N : INTEGER;
           SAVEXCUR ,SAVEYCUR : LINEPOINTER;
           SAVEXLINE, SAVEYLINE : INTEGER;

         BEGIN        (* CHECKFULLMATCH *)
           SAVEXCUR := X.CURSOR;
           SAVEYCUR := Y.CURSOR;
           SAVEXLINE := X.CURSORLINENO;
           SAVEYLINE := Y.CURSORLINENO;
           COMPARELINES(MATCH);
           N := MINLINESFORMATCH - 1;
           WHILE MATCH AND (N <> 0) DO
              BEGIN
              MOVECURSOR(X, FILEX);
              MOVECURSOR(Y, FILEY);
              COMPARELINES(MATCH);
              N := N - 1
           END;
           X.CURSOR := SAVEXCUR;
           X.CURSORLINENO := SAVEXLINE;
           Y.CURSOR := SAVEYCUR;
           Y.CURSORLINENO := SAVEYLINE;
           END;    (* CHECKFULLMATCH *)



         BEGIN         (* SEARCH *)
           MOVECURSOR(Y, FILEY);
           BACKTRACK(X, COUNT);
           CHECKFULLMATCH;
           COUNT := COUNT - 1;
           WHILE (COUNT <> 0) AND NOT MATCH DO
              BEGIN
              MOVECURSOR(X, FILEX);
              COUNT := COUNT - 1;
              CHECKFULLMATCH
              END
           END;          (* SEARCH *)



    PROCEDURE PRINTMISMATCH;

      VAR
        EMPTYA, EMPTYB : BOOLEAN;

      PROCEDURE WRITETEXT(P, Q :LINEPOINTER);
        VAR I : INTEGER;
        BEGIN       (* WRITETEXT *)
          WRITELN();
          WHILE (P <> NIL) AND (P <> Q) DO
            BEGIN
              WRITE(' > ');
              IF P@.LENGTH = 0
                 THEN
                  ELSE FOR I := 1 TO P@.LENGTH DO
                           WRITE (OUTPUT,P@.IMAGE(/I/));
               WRITELN();
              P := P@.NEXTLINE
              END;
            IF P = NIL
               THEN WRITELN(' ==== EOF ====')
               "ELSE" ;
            WRITELN();
            END;    (* WRITETEXT *)



       PROCEDURE WRITELINENO(VAR X : STREAM);
         VAR
           F, L : INTEGER;

         BEGIN     (* WRITELINENO *)
           F := X.HEADLINENO;
           L := X.CURSORLINENO - 1;
           WRITE('LINE');
           IF F = L
              THEN WRITE(' ',F:1)
              ELSE WRITE('S ',F:1,' TO ',L:1);
           IF X.CURSOR = NIL
              THEN WRITE(' (BEFORE EOF)')
              ELSE;
           END;          (* WRITELINENO *)



       PROCEDURE PRINTEXTRATEXT (VAR X : STREAM; XNAME : CHAR
                                 VAR Y : STREAM; YNAME : CHAR);

         BEGIN     (* PRINTEXTRATEXT *)
           WRITE (' EXTRA TEXT ON FILE', XNAME,' --> ');
           WRITELINENO(X);        WRITELN();
           IF Y.HEAD = NIL
              THEN WRITELN(' BEFORE EOF ON FILE', YNAME)
              ELSE WRITELN(' BETWEEN LINES ', Y.HEADLINENO-1:1,' AND ',
                         Y.HEADLINENO:1,' OF FILE',YNAME);
           WRITETEXT(X.HEAD, X.CURSOR)
           END;         (* PRINTEXTRATEXT *)



    BEGIN            (* PRINTMISMATCH *)
      WRITELN (' =========================================');
      EMPTYA := (A.HEAD = A.CURSOR);
      EMPTYB := (B.HEAD = B.CURSOR);
      IF EMPTYA OR EMPTYB
         THEN IF EMPTYA
                 THEN PRINTEXTRATEXT(B, 'B', A, 'A')
                 ELSE PRINTEXTRATEXT(A, 'A', B, 'B')
         ELSE BEGIN
              WRITELN (' MISMATCH:');  WRITELN();
              WRITE(' FILEA --> ');   WRITELINENO(A);
              WRITELN(':');
              WRITETEXT(A.HEAD, A.CURSOR);
              WRITE(' FILEB --> '); WRITELINENO(B); WRITELN (':');
              WRITETEXT(B.HEAD, B.CURSOR);
              END
         END;       (* PRINTMISMATCH *)



    BEGIN    (* FINDMATCH *)
        (* NOT MATCH *)
      ADVANCEB := TRUE;
      REPEAT
        IF NOT ENDFILE
           THEN ADVANCEB := NOT ADVANCEB
           ELSE ADVANCEB := ENDSTREAM (A);
        IF ADVANCEB
           THEN SEARCH (A, FILEA, B, FILEB)
           ELSE SEARCH (B, FILEB, A, FILEA)
      UNTIL MATCH;
      PRINTMISMATCH;
    END;      (* FINDMATCH *)



    BEGIN        (* COMPAREFILES *)
      MATCH := TRUE;   (* I.E. BEGINNINGS OF FILES MATCH *)
      REPEAT
        IF MATCH
           THEN FINDMISMATCH
           ELSE BEGIN
                SAME := FALSE;
                FINDMATCH
                END
      UNTIL ENDFILE AND MATCH;
      (* MARK (A); MARK (B); MARK END OF FILES, THEREBY DISPOSING OF
                              BUFFERS     *)
     END;      (* COMPAREFILES *)




    PROCEDURE INITIALIZE;

       PROCEDURE INITSTREAM(VAR X : STREAM; VAR FILEX : TEXT);
         BEGIN    (* INITSTREAM *)
           WITH X DO
              BEGIN
              CURSOR := NIL;
              HEAD := NIL;
              TAIL := NIL;
              CURSORLINENO := 0;
              HEADLINENO := 0;
              TAILLINENO := 0
              END;
          RESET (FILEX);
          X.ENDFILE := EOF(FILEX);
          END;  (* INITSTREAM *)


      BEGIN   (* INITIALIZE*)

        INITSTREAM (A, FILEA);    INITSTREAM (B, FILEB);
        ENDFILE := A.ENDFILE OR B.ENDFILE;
        FREELINES := NIL;
        TEMPLINE.LENGTH := LINELENGTH;
        TEMPLINE.IMAGE(/0/) := 'X';   (* SENTINEL *)
        END;       (* INITIALIZE *)





    BEGIN       (* COMPARE *)
      INITIALIZE;
      PAGE(OUTPUT);
      WRITELN ('     COMPARE.  VERSION ', VERSION);
      WRITELN();
      WRITELN (' MATCH CRITERION = ',MINLINESFORMATCH:1, ' LINES.');
      WRITELN();
      IF A.ENDFILE
         THEN WRITELN (' FILEA IS EMPTY.')
         ELSE;
      IF B.ENDFILE
         THEN WRITELN(' FILEB IS EMPTY.');
      IF  NOT ENDFILE
          THEN BEGIN
               SAME := TRUE;
               COMPAREFILES;
               IF SAME
                  THEN WRITELN(' NO DIFFERENCES.')
                  ELSE;
               END
          ELSE;
  END.      (* COMPARE *)
