PROGRAM  INVERT(OUTPUT);  (*$M-*)

  CONST  ARRAYSIZE =  9;
         NUMBEROFINVERSIONS =  50;

  VAR  A:  ARRAY[1..ARRAYSIZE, 1..ARRAYSIZE] OF REAL;
       TEMP, SUM:   REAL;
       I, J, K, L:  INTEGER;

(* THIS PROGRAM DOES REPEATED INVERSIONS OF A HILBERT MATRIX *)

BEGIN  (* INVERT *)
  FOR I := 1 TO ARRAYSIZE  DO
      FOR J := 1 TO ARRAYSIZE  DO
          A[I,J] := 1.0 / (I + J - 1.0);         (* SET UP THE HILBERT MATRIX *)

  FOR L := 1 TO NUMBEROFINVERSIONS  DO
      BEGIN FOR I := 1 TO ARRAYSIZE  DO
                BEGIN TEMP := A[I,I];
                      A[I,I] := 1.0;
                      FOR J := 1 TO ARRAYSIZE  DO
                          A[I,J] := A[I,J] / TEMP;
                      FOR K := 1 TO ARRAYSIZE  DO
                          BEGIN IF K <> I  THEN
                                   BEGIN TEMP := A[K,I];
                                         A[K,I] := 0.0;
                                         FOR J := 1 TO ARRAYSIZE  DO
                                             A[K,J] := A[K,J] -
                                                       (TEMP * A[I,J])
                                   END
                          END
                END;
            IF ODD(L)  THEN
               BEGIN SUM := 0.0;
                     FOR I := 1 TO ARRAYSIZE  DO
                         FOR J := 1 TO ARRAYSIZE  DO
                             SUM := SUM + A[I,J];
                     WRITELN(' SUM = ', SUM:17)
               END
      END
END  (* INVERT *)  .
