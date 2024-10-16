PROGRAM QUICKSORT( OUTPUT ) ;  (*$L+,D- *)

(* PARTITION-XCHANGE SORT, AFTER J. SEDGEWICK,  S. HAZEGHI.

   M ::= SIZE OF THE PARTITIONS TO BE BUBBLE-SORTED
   N ::= NUMBER OF ELEMENTS TO BE SORTED ;
   N1 ::= N+1 ;
   STACK_SIZE ::= MAX # OF UNSORTED PARTITIONS ( >= 2*(LOG(N)-3), LOG IS BASE 2)
*)

LABEL 101,111 ;

CONST   M = 9 ;  N = 40;  N1 = 41 ;STACK_SIZE = 25;

VAR L,R,P,I,J,V,T : INTEGER ;
    STACK : ARRAY [1..STACK_SIZE] OF INTEGER ;
    (* STACK_SIZE  2*(LG(N)-3) *)
    A : ARRAY [1..N1] OF INTEGER ;

PROCEDURE PRINTDATA ;

    (* TO PRINT THE RAW AND SORTED DATA, 10 NUMBERS PER LINE *)

    BEGIN
    FOR I := 1 TO N1 DO
        BEGIN
        IF (I MOD 10) = 1 THEN  WRITELN ;
        WRITE('  ',A[I]:11 ) ;
        END;
    END ;

    BEGIN  (* QUIKSORT *)

    (* I- GENERATE RANDOM DATA FOR SORTING *)

    A[1] := 0; L := 2; R := N1; P := 0;
    FOR I := 1 TO N DO
        BEGIN  A[I+1] := A[I]*314159269+453806245;
        IF A[I+1] < 0 THEN
            BEGIN  A[I+1] := A[I+1]+2147483647 ;
            A[I+1] := A[I+1]+1
            END
        END ;

    PRINTDATA ;

    (* II- PARTITION THE SORT DATA *)

    REPEAT  I := L-1; J := R; V := A[R];
        REPEAT
            REPEAT I := I+1 UNTIL (A[I] >= V) ;
            A[J] := A[I];
            REPEAT J := J-1 UNTIL (A[J] <= V) ;
            IF I >= J THEN GOTO 101 ;
            A[I] := A[J]
        UNTIL  FALSE ;
    101:IF I <> J THEN  J := J+1;
        A[J] := V;
        IF J-L > R-J THEN
            BEGIN
            IF M >= J-L THEN
                BEGIN  IF P = 0 THEN GOTO 111 ;
                R := STACK[P+1];  L := STACK[P];  P := P-2;
                END
            ELSE IF R-J > M THEN
                    BEGIN  P := P+2 ;  STACK[P] := L ;
                    STACK[P+1] := J-1 ;  L := J+1
                    END
                ELSE  R := J-1
            END
        ELSE IF M >= R-J THEN
                BEGIN  IF P = 0 THEN GOTO 111 ;
                R := STACK[P+1];  L := STACK[P];  P := P-2;
                END
            ELSE IF J-L > M  THEN
                    BEGIN  P := P+2 ;  STACK[P] := J+1 ;
                    STACK[P+1] := R ;  R := J-1
                    END
                ELSE  L := J+1
    UNTIL  FALSE ;

    (* III- EXCHANGE SORT EACH PARTITION *)

111:FOR I := 2 TO N1 DO
        IF A[I] < A[I-1] THEN
            BEGIN
            V := A[I] ;  J := I-1 ;
            REPEAT  A[J+1] := A[J];  J := J-1  UNTIL (A[J] <= V) ;
            A[J+1] := V
            END ;

    PRINTDATA ;

    END (* QUIKSORT *).
