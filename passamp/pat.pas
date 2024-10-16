(*$M-*)
program pat;



    �***********************************************�
    �*       PAL     (pat,lalr)                    *�
    �*                                             *�
    �*   PATTERN  ACTION  LANGUAGE                 *�
    �*                                             *�
    �*    AUTHOR  :   G. POONEN                    *�
    �*                                             *�
    �*    modification :  J. Bloch, H. Eskin       *�
    �*    modification :  G. Minshall              *�
    �*                                             *�
    �*     VERSION :                               *�
    �*                                             *�
    �*      DATE    :        December, 1983        *�
    �*                                             *�
    �***********************************************�

� Dependancies - The algorithm for making a character
        upper case DEPENDS on (ord(LowerCaseLetter)-ord(UpperCaseLetter))
        being constant as 'Letter' varies from 'a' to 'z'
        (Both ASCII and EBCDIC have this property).
�






















    �  GLOBAL DECLARATIONS  �
    �***********************�



    � GLOBAL constANTS�
  const
    XCONTINUE = 9996 ;     � CODE FOR CONTINUE IN TABLE �
    ELSECODE = 9997  ;
    NOSCANCODE   = 9998 ; � CODE FOR SCAN�
    ERRORCODE = 9999 ;    � CODE FOR ERROR IN TABLE �
    UNKNOWN=-9999 ;       � UNKNOWN QTY �
    MAXSEGMENT = 200 ;
    MAXTABSIZE =3000;
    MAXADDR    = 4*65536-1;
    MAXNAME    = 15   ;   � MAX LENGTH OF NAMES  (was 15) �
    MAXERR     =  20  ;   � MAX ERRORS PER LINE�
    MAXERRNmr  =  200 ;   � MAX ERROR NUMBER�
    MAXLINECNT =   132;   �  LINE COUNT�
    GTMAXERR   =  199 ;   � EXCEEDED MAX ERRORS PER LINE�
    MAXSYM     =    6 ;   � SIZE OF SPECIAL SYMBOLS�
    STATETABSIZE= 2000 ;  � was 1000 �
    MAXSTATE = 2000 ;     � was 1000 �
    MAXRHS = 32 ;
    MAXVISIT = 3200  ;    � MAX NO OF STATES VISITED FOR �
                          � RESOLVING LA STATES � � was 1600�

    MAXVOCSIZE = 2048 ;   � MAX VOCABULARY CODE (was 512) �
    MAXTERMINALS = 1023 ; � was 255 �
    NAMETABSIZE = 4001 ;  � KEEP IT PRIME  (was 1023) �
    MAXRULENO    = 4001 ; � KEEP IT PRIME  (was 1023) �
    DIGMAX    =    5 ;    � MAXIMUM NUMBER of DIGITS FOR INTEGER�
    THRESHOLD =3 ;        � USED FOR SORTING AND MATCHING LISTS�
    FILENAMELEN = 133 ;   � MAX charACTERS IN FILESPEC�
    B14 = 12;             � was 14B in DEC version �
    WHITESPACE = [ ' ' ]; � What INsymbol skips. �


    � GLOBAL typeS �
type
    word  =  packed array [1 .. MAXNAME] of char ;
    symbol =  (Ident ,Res, Intconst,  SemiSy, SlashSy, OrSy, EqSy, ColSy
    ,PeriodSy    ) ;
    setofsymbol = set of symbol ;
    namety = (Term,NonTerm,
              Lab
              ) ;
    codety = -1 .. MAXTERMINALS ;
    listptrty = @ list ;
    list   =  packed  record
                             CDR     : listptrty;
                             CAR     : 1 .. '5FFF'X
                      end ;
    setofnamety = set of namety ;


    �  NAME TABLE ENTRIES   �
    �***********************�

    identptrty = @ identry ;
    prodptrty  = @ prod ;
    identry   =   record
                         Nic     : identptrty;
                         FirstAlt: prodptrty;

                         Defin   : listptrty;
                         UsedIn  : listptrty;
                         InternalName,
                         ExternalName    : word   ;
                         Kind    : namety ;
                         SpId    : symbol ;
                         Code    : codety
                  end ;
    prod = record
                  Next   : prodptrty;
                  XDef   : identptrty;
                  Action : identptrty;
                  NxtAlt : prodptrty;
                  Production : 1 .. MAXRULENO
           end ;


    �  STATE ENTRIES �
    �****************�
    statekind     = (Reads,Inadequate,Lookahead,Reduction,Subgoal,LAString) ;
    stateptrty    = @ statety ;
    tranlistptr   = @ tranlist ;
    statelistptr  = @ statelist ;
    tranptrty     = @ tranty ;
    statety = record
                 FirstTran : tranptrty  ;
                 Back      : tranptrty ;
                 Nic       : stateptrty ;
                 State     : 0 .. MAXSTATE ;
                 Kind      : statekind
              end ;

    statelist = record
                  CAR : stateptrty ;
                  CDR : statelistptr
                end ;
    tranlist = record
                  CAR : tranptrty ;
                  CDR : tranlistptr
               end ;
    tranty =  packed record
                        Next      : tranptrty ;
                        NextState : stateptrty ;
                        Rule      : prodptrty ;
                        Pos       : 0 .. MAXRHS ;
                        MarkE     : BOOLEAN
                     end ;

    � type specifiers for OPEN procedure �
    openOptions = ( OpenInput, OpenOutput, OpenUppercase );
    openOption  = set of openOptions;
    filenameType = packed array [1 .. FILENAMELEN] of char  ;
    promptType = packed array[1..121] of char;




    � GLOBAL varIABLES �
var
    Heap,HeapBot,HeapTop      : @integer ; � MARKS Heap �
    Visit   : array [ 1.. MAXVISIT ] of tranptrty ;
    CurrentVisit : integer ;

    � Table ENTRIES �


    Table : array [0 .. MAXTABSIZE] of packed record
                                              Tran : integer ;
                                              Action : integer
                                       end ;
    Index : -1 .. MAXTABSIZE ;
    Segment : array [0 .. MAXSEGMENT] of packed record
                                                Tran : integer ;
                                                Action : integer
                                         end ;
    SegIndex : -1 .. MAXSEGMENT ;
    NewState : array [0 .. MAXSTATE] of integer ;


    SymbolName : packed array [0 .. MAXVOCSIZE] of identptrty;
    NameTab  :  array [0 .. NAMETABSIZE ] of identptrty ;
    RuleTab   :  array [1 .. MAXRULENO ] of   prodptrty;
                 �  Contains no. of elements on rhs e.g  �
                 �  X =  A   B   C    will be 3          �
    PopTab  : array [ 1 .. MAXRULENO ] of integer ;

    NState : array [0 .. MAXSTATE ] of stateptrty ;
    StateTab  : array [0 .. STATETABSIZE ] of stateptrty ;
    Inadeq,Conflict : statelistptr ; � list of Inadequate StateS �
    CurrentState : 0 .. MAXSTATE ;
    Sy     : symbol  ; � LAST symbol�
    SyLeng : integer ; � LENGTH of symbol�
    SyKey  : integer ; � symbol KEY      �

    SyVal : integer ;
    MaxK  : integer ; � MAX VAL of Lookahead �
    InternalId,               � symbol InternalName     �
    ExternalId    :  word   ; � symbol ExternalName     �
    Ch    :  char   ; � LAST charACTER  �
    InSymbolState : ( InSymbolNormalState, InSymbolStringState );
                    � that is: returning a string, or just normal
                        processing �

    ChCnt : integer ;
    Letters
    ,Digits  :  set of char   ;

   MaxPop     : integer ;
   MaxCode    : integer ;
   MaxProdNo  : 0 .. MAXRULENO ;
      ProdNo  : 0 .. MAXRULENO ;

    L      : integer ; � CONTROL varIABLE �

                       �  OPTION FLAGS  �
    ListFl                  � L �
    , Look                  � A �
    , Grammar               � G �
    , Debug                 � D �
    , Optimize              � O �
    , Trace                 � R - Print trace info during run �
    , Tables                � T �
    , PN                    � P �
    , Terminals             � S �
    , Frequency             � F �
    , Lr0                   � C �
    , IFSM                  � I �
    , Research              � Z �
    , Symbolic              � Y - The state tables should be symbolic �
    , NonTerminals          � N - Put non terminals in .TER �
    , FSM      :   boolean; � M �

    �  ERROR  list  �

    FatalError,
    ErrorFlag  : boolean ;
    ErrInx   :   integer   ;   � NO of ERRORS IN THIS LINE�
    ErrList  :  array  [1 .. MAXERR]  of
                    record
                      Pos :  0 .. MAXLINECNT ;
                      Nmr :  0 .. MAXERRNmr
                    end ;
    �  FILES  �
    Filename  :  filenameType;
      TTYOut                                 �  Console output �
    , CrefPas                                �  Cref       �
    , TerPas                                 �  Terminals  �
    , NewPrgPas                              �  Formatted Grammar �
    , TabPas                                 �  Tables �
    , PNPas    :  text;                      �  Production num �

    �  LALR  �
    LrK
    :  boolean    ;

    �  SPECIAL symbolS  �
    SpList   :  array [1 .. MAXSYM]  of char  ;
    SpSy    :  array [1 .. MAXSYM]  of symbol  ;

LowerCaseLetters : set of char;


� The following are for the Arguments part of the implementation
        specific part of the Pascal/VS version (IBM)
�
    OurArgs : packed array[1..255] of char;
    OurArgsIndex,
    OurArgsLength : integer;
    OurArgsUpperCase : boolean;









� The following proceduRes are, hopefully,
    the only PASCAL/VS specific proceduRes.  Thus, to move to a new
    environment, one should only need to change the way they deal
    with your new I/O system.  NOTE that 'filename' is something
    one might have to reformat.
        (greg minshall - 12/10/83)
�

� OPEN - this procedure opens a file, doing ResET or REWRITE as
    necessary
�

procedure open( var f          : text;
                const filename : filenametype;
                const options  : openOption );
%include CMS
var
    opts : string(50);
    fn   : string(40);
    rc   : integer;
begin
    assert (( OpenInput in options ) or ( OpenOutput in options )) and
           not (( OpenInput in options ) and ( OpenOutput in options ));

    if OpenUppercase in options then
        opts := 'UCASE '    � translate input to UPPER case �
    else
        opts := '';

    fn := trim(ltrim(compRess(str(filename))));

    if fn = 'TTY:' then begin
        opts := opts �� 'DDNAME=STDIN';
        cms( 'FILEDEF STDIN CLEAR', rc );
        cms( 'FILEDEF STDIN TermINAL ( LRECL 255 BLOCK 255', rc );
        if OpenInput in options then
            Reset( f, opts )
        else
            rewrite( f, opts );
    end else begin
        opts := opts �� 'NAME=' �� fn;
        if OpenInput in options then
            Reset( f, opts )
        else
            rewrite( f, opts );
    end;
end;

� CanOpen - this boolean function returns TRUE if the filename
          LookS ok, FALSE otherwise
�
function CanOpen( const filename : filenametype ) : boolean;
%include CMS
var
    myfile : filenametype;
    index,
    rc     : integer;
begin
    for index := 1 to filenamelen do
        if filename[index] = '.' then
            myfile[index] := ' '
        else
            myfile[index] := filename[index];

    cms( 'SET CMStype HT', rc );

    cms( 'State ' �� str(myfile), rc );
    CanOpen := rc = 0;

    cms( 'SET CMStype RT', rc );
end;

PROCEDURE BREAK;
BEGIN
    � DEC has a procedure called BREAK, which causes the output
        buffer to be flushed.  Here is my version - it is
        good for nothing!
    �
end;


� The following proceduRes are designed to allow PAT to get arguments
    from the users.  In the Pascal/VS version, we get the options from
    the command line.  In other versions, one might (as in the DEC
    version this started from) write a prompt, and the read from the
    terminal.
�

procedure InitArgs;
var
    TempArgs : string(255);
    Index : integer;
begin
    TempArgs := parms;          � parms is a function, returns parms
                                  from the command line �
    OurArgs := TempArgs;        � Pascal/VS converts �
    OurArgsIndex := 1;
    OurArgsLength := length(TempArgs);
    while (OurArgs[OurArgsIndex] = ' ') and
          (OurArgsIndex <= OurArgsLength) do
            OurArgsIndex := OurArgsIndex+1;
    OurArgsUpperCase := FALSE;
end;


procedure PromptArgs( Prompt : promptType );
begin
    � We don't use this at all �
end;


procedure OpenArgs( Options : openOption );
begin
    if OpenUppercase in Options then
        OurArgsUpperCase := TRUE
    else
        OurArgsUpperCase := FALSE;
end;


Procedure GetArgCh( var ch : char; var EndLine, EndArgs : boolean );
begin
    if OurArgsIndex > OurArgsLength then begin
        EndArgs := TRUE;
        Ch := ' ';
    end else begin
        EndArgs := FALSE;
        Ch := OurArgs[OurArgsIndex];
        if (Ch in LowerCaseLetters) and OurArgsUpperCase then
            Ch := chr( ord(Ch) + ord('A')-ord('a') );
        OurArgsIndex := OurArgsIndex+1;
        if  Ch = ' ' then begin
            EndLine := TRUE;
            while (OurArgs[OurArgsIndex] = ' ') and
                  (OurArgsIndex <= OurArgsLength) do
                        OurArgsIndex := OurArgsIndex+1;
        end else
            EndLine := FALSE;
    end;
end;


procedure SetReturnCode( I : integer );
� In CMS, error code is propagatable back to caller, so let
    them know...
�
begin
    retcode(i);     � CMS Pascal/VS primitive �
end;




� Thus ends, hopefully, the implementation dependent part
    of PAT.
        greg minshall, 12/83
�




procedure InitSp;
begin
        SpList  [1] :=  '!'    ;  SpSy [1] := OrSy ;
        SpList  [2] :=  '/'    ;  SpSy [2] := SlashSy ;
        SpList  [3] :=  ';'    ;  SpSy [3] := SemiSy ;
        SpList  [4] :=  '='    ;  SpSy [4] := EqSy ;
        SpList  [5] :=  ':'    ;  SpSy [5] := ColSy ;
        SpList  [6]  := '.' ;    SpSy [6] := PeriodSy
end;


    �used to be an initprocedure�
    procedure  InitOptions ;  �  DEFAULT OPTIONS�
      begin
        Optimize   :=   FALSE   ;
        Tables     :=   TRUE   ;
        PN         :=   FALSE   ;
        Terminals  :=   TRUE   ;
        Frequency  :=   FALSE   ;
        FSM        :=   FALSE   ;
        Grammar    :=   TRUE   ;
        Look       :=  TRUE   ;
        ListFl     :=  TRUE   ;
        Lr0        :=  TRUE   ;
        IFSM       :=  TRUE   ;
        Research   :=   FALSE ;
        Debug      :=   FALSE ;
        Trace      :=   FALSE ;
        Symbolic   :=   FALSE ;
        NonTerminals := FALSE ;

        MaxK  :=  1     ; �  DEFAULT Lookahead�
      end  ;




    procedure InitGlob ;    � GLOBAL varIABLES  �
    �used to be an initprocedure�
      begin
        FatalError := FALSE;
        ErrorFlag := FALSE ;
        Conflict := NIL ;
        Inadeq := NIL   ;
        ErrInx := 0     ;
        InSymbolState := InSymbolNormalState;
        LowerCaseLetters := [ 'a', 'b', 'c', 'd', 'e', 'f',
            'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p',
            'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z' ];
      end ;



    procedure   EndOfLine  ;
        �*******************************************************�
        �*                                                     *�
        �*   end of LINE                                       *�
        �*                                                     *�
        �*       THIS procedure WRITES OUT THE                 *�
        �*     CURRENT LINE AND FOLLOWS IT WITH                *�
        �*     ANY ERROR MESSAGES AND POINTERS.                *�
        �*                                                     *�
        �*                                                     *�
        �*      GLOBAL IN: ErrInx  NO. of ERRORS IN THIS LINE  *�
        �*                                                     *�
        �*                 ErrList array CONTAINING            *�
        �*                         ATTRIBUTES of ERROR         *�
        �*                                                     *�
        �*                 list    listING REQUIRED IF         *�
        �*                         TRUE.                       *�
        �*                                                     *�
        �*         OUT :  ErrInx   set TO 0                    *�
        �*                                                     *�
        �*                                                     *�
        �*******************************************************�
    var
        LastPos
        , FreePos
        , CurrPos
        , CurrNmr
        ,  F                �  FORMAT  �
        ,  K                �  LOOP CONTROL  �
        :  integer  ;

      begin
        readln; writeln;
        if ErrInx > 0 then  �OUTPUT ERROR MESSAGES�
          begin
            write('****');
            LastPos := 0; FreePos := 1;
            for K := 1 to ErrInx do
              begin
                with ErrList[K] do
                  begin
                    CurrPos := Pos; CurrNmr := Nmr
                  end;
                if CurrPos = LastPos then write(',')
                else
                  begin
                    while FreePos < CurrPos do
                      begin
                        write(' '); FreePos := FreePos + 1
                      end;
                    write('@');
                    LastPos := CurrPos
                  end;
                if CurrNmr= 0 then F:= 0
                else if CurrNmr < 10 then F :=1
                else if CurrNmr < 100 then F := 2
                else F := 3;
                if F <> 0 then write(CurrNmr:F);
                FreePos:=FreePos+F+1
              end;
            writeln; ErrInx :=0
          end;
        ChCnt := 0
      end  �EndOfLine� ;



    procedure   Error  (FErrNr : integer) ;
        �******************************************************�
        �*                                                    *�
        �*        Error                                       *�
        �*                                                    *�
        �*     THIS procedure recordS THE Error               *�
        �*  AND PosITION of OCCURRENCE                        *�
        �*                                                    *�
        �*    GLOBAL  IN :  ChCnt   INDICATE PosITION         *�
        �*                          ON LINE                   *�
        �*                                                    *�
        �*                                                    *�
        �*                                                    *�
        �*         OUT :  ErrorFlag                           *�
        �*                ErrList     array of ErrorS         *�
        �*                ErrInx                              *�
        �*                                                    *�
        �*                            I                       *�
        �*                                                    *�
        �******************************************************�
      begin
        �  Error �
        ErrorFlag :=  TRUE  ;
        if ErrInx >= MAXERR  then
          begin
            ErrList [ MAXERR ].Nmr :=  GTMAXERR  ;
          end
        else
          begin
            ErrInx  :=  ErrInx +1  ;
            ErrList [ErrInx] . Nmr := FErrNr  ;
            ErrList [ErrInx].Pos   :=  ChCnt   ;
          end  ;
      end  ;
    �  Error �




    procedure  NextCh   ;
        �*******************************************************�
        �*                                                     *�
        �*  NextCh.                                            *�
        �*                                                     *�
        �*     THIS procedure RETURNS THE Next charACTER       *�
        �* IN Ch                                               *�
        �*                                                     *�
        �*                                                     *�
        �*                                                     *�
        �*   GLOBAL      IN:   ChCnt                           *�
        �*                     ListFl        boolean OPTION    *�
        �*                                                     *�
        �*              OUT:   ChCnt                           *�
        �*                                                     *�
        �*                      Ch         Next charACTER      *�
        �*                                                     *�
        �*******************************************************�
      begin
        � NextCh �
        if not ( eoln(Input) or eof(Input) )  then
          begin
            read (Ch)  ;
            if ListFl then write (Ch)  ;
            ChCnt :=  ChCnt +1  ;
          end
        else   Ch := ' '  ;
      end   ;



    procedure  InSymbol  ;
        �*******************************************************�
        �*                                                     *�
        �*   InSymbol                                          *�
        �*                                                     *�
        �*       THIS procedure RETURNS THE Next               *�
        �*  symbol FROM THE SOURCE Input                       *�
        �*                                                     *�
        �*  GLOBAL  IN:  Ch     Next charACTER                 *�
        �*              SP list                                *�
        �*              SP Sy                                  *�
        �*                                                     *�
        �*         OUT :  SyLeng    symbol LENGTH              *�
        �*                Sy        symbol type                *�
        �*                InternalId[I]     symbol charACTERS  *�
        �*                SyKey     KEY for IdentifIERS        *�
        �*                                                     *�
        �*                                                     *�
        �*                                                     *�
        �*******************************************************�

    label
        3;
    var
        I,K : integer ;
        Noteoln,
        Noteof : boolean;
begin � InSymbol �
    if InSymbolState = InSymbolStringState then begin
        ExternalId[1] := '''';
        ExternalId[2] := Ch;
        SyKey := ord(Ch);
        ExternalId[3] := '''';
        if Ch = '''' then
            ExternalId[4] := ''''
        else
            ExternalId[4] := ' ';
        for i := 5 to MAXNAME do
            ExternalId[i] := ' ';
        Sy := Res;
        InternalId := ExternalId;
                � End of lines and files terminate strings �
        if eoln(Input) or eof(Input) then
            InSymbolState := InSymbolNormalState;
        Nextch;
        if Ch = '''' then begin
            Nextch;
            if Ch <> '''' then
                InSymbolState := InSymbolNormalState;
        end;
    end else begin
        repeat begin
            repeat begin
                if  eoln(Input)  then EndOfLine ;
                if  not ( Ch in WHITESPACE ) then
                    leave;
                NextCh   ;          � skip whitespace �
            end until eof(Input);
            if (Ch <> '�') AND (Ch <> '%') then
                leave;
            if  Ch = '%' then
            begin
                NextCh   ;
                repeat begin
                    if  eoln(Input)  then EndOfLine ;
                    if  Ch = '�' then
                        leave;
                    NextCh
                end until eof(Input);
            end;
            if  Ch = '�' then
            begin
                NextCh   ;
                repeat begin
                    if  eoln(Input)  then EndOfLine ;
                    if  Ch = '�' then
                        leave;
                    NextCh
                end until eof(Input)
            end;
            NextCh
        end until eof(Input);
        SyLeng  := 0 ;
        SyVal :=0 ;
        SyKey   := 0 ;
        if  Ch  in  Letters  then begin
            K  := 0   ;
            repeat
                if  K  <MAXNAME   then begin
                    K := K+1 ;
                    ExternalId[K] := Ch   ;
                    if Ch in LowerCaseLetters then
                        InternalId[K] := chr( ord(ch) + ord('A')-ord('a') )
                    else
                        InternalId[K] := Ch;
                    SyKey  := SyKey + ord(InternalId[K]);
                  end   ;

                NextCh
            until  not  (Ch in Letters + Digits) ;
            if K < MAXNAME  then
            for I := K+1 to MAXNAME do begin
                ExternalId[I] := ' ' ;
                InternalId[I] := ' ' ;
            end;
            Sy  := Ident   ;
          end
        else  if  Ch  in  Digits  then begin
            Sy := Intconst ;
            I  :=  0     ;   SyVal :=0;
            repeat
                I := I+1 ;
                if   I < DIGMAX then
                SyVal := SyVal*10  + ord (Ch) - ord ('0') ;
                ExternalId[I] := Ch      ;
                NextCh
            until  not  (Ch in Digits)   ;
            if I < MAXNAME then
            for I:= I+1 to MAXNAME  do  ExternalId[I] := ' ';
            InternalId := ExternalID;
        end
        else if Ch = '''' then begin
            InSymbolState := InSymbolStringState;
            Nextch;
            if Ch = '''' then begin
                Nextch;
                if Ch <> '''' then    � If TRUE, null symbol - ignore �
                    InSymbolState := InSymbolNormalState;
            end;
            InSymbol;           � recurse to start reading string �
        end else begin
            for I := 1 to MAXSYM do begin
                if SpList[I]= Ch  then begin
                    Sy := SpSy [I] ;
                    GOto 3 ;
                end ;
            end ;
            3 : ExternalId[1] := Ch;
            NextCh ;
            for I:= 2 to MAXNAME  do ExternalId[I]:= ' ';
            InternalID := ExternalID;
        end ;
        if Trace then begin
            writeln(TTYOut,InternalId,' ',SyVal:3);BREAK
        end;
    end;
end  ;
        �  InSymbol  �



    procedure   InitP ;
        �***********************************************************�
        �*                                                         *�
        �*    InitP                                                *�
        �*                                                         *�
        �*        THIS procedure Reads in THE OPTIONS              *�
        �*   AND ALSO inITIALIZES ANY Tables AS                    *�
        �*   REQUIRED                                              *�
        �*                                                         *�
        �***********************************************************�
    var

        ChNext : char;
        EndLine,
        EndArgs : boolean;
        I : integer ;
        Distance,Dot,J, RBrak,Colon
                : 0..FILENAMELEN;       �PLACE MarkERS in FILESPEC�
        procedure     InitSets   ;
        var
            L : integer ;
          begin
            Digits := ['0','1','2','3','4','5','6','7','8','9'] ;
            Letters:= ['A','B','C','D','E','F','G','H','I','J','K',
            'L','M','N','O','P','Q','R','S','T','U','V',
            'W','X','Y','Z','_','a','b','c','d','e','f','g','h','i','j','k',
            'l','m','n','o','p','q','r','s','t','u','v',
            'w','x','y','z' ] ;

            for L := 0 to NAMETABSIZE do
            NameTab[L] :=  NIL ;
            for L:= 1 to MAXRULENO do
              begin
                RuleTab[L] :=  NIL;
                PopTab [L] := 0
              end   ;
            MaxProdNo := 0;
            ProdNo := 0 ;
            MaxPop := 0 ;
            for L := 0 to MAXSTATE do
              begin
                NewState [L] := 0 ;
                NState [ L ] := NIL ;
              end ;
            for L := 0 to MAXTABSIZE do
              begin
                Table [L].Tran := UNKNOWN ;
                Table [L].Action := UNKNOWN;
              end ;
            for L := 0 to STATETABSIZE do
            StateTab [L] := NIL ;
            CurrentVisit := 0 ;
            for L := 1 to MAXVISIT do
            Visit [L] := NIL ;
            MaxCode := 257 ;    � leave room for ord constants �
            CurrentState := 0
          end  ;
        � InitSets �



        procedure    Options   ;
            �************************************************************�
            �*                                                          *�
            �*  Options                                                 *�
            �*                                                          *�
            �*  THIS procedure Reads in THE Options                     *�
            �*   AND SETS THE OPTION FLAGS APPROPRIATELY                *�
            �*                                                          *�
            �*      GLOBAL in:  Ch        LAST charACTER                *�
            �*                                                          *�
            �*                                                          *�
            �************************************************************�
var
    ChNext : char;
    EndLine,
    EndArgs : boolean;

          begin
            � Options �
              PromptArgs('Options: ');
              OpenArgs([ OpenInput, OpenUppercase ]);
        /*    Reset(TTY,'TTY:','/I/U'); � convert tty input to upper case � */
              GetArgCh( ChNext, EndLine, EndArgs );
              while not EndArgs do begin
                Ch := ChNext;       � get Identifier �
                GetArgCh( ChNext, EndLine, EndArgs ); � get +/- �
                if Ch = 'L' then
                  begin
                    ListFl  :=  ChNext <> '-';
                    writeln(TTYOut,'Listfile: ',listfl)
                  end
                else if Ch = 'O' then
                  begin
                    Optimize  :=  ChNext <> '-';
                    writeln(TTYOut,'Optimize: ',optimize)
                  end
                else if Ch = 'T'  then
                  begin
                    Tables := ChNext<>'-';
                    writeln(TTYOut,'Tables: ',tables)
                  end
                else if Ch ='S'  then
                  begin
                    Terminals := ChNext<>'-';
                    writeln(TTYOut,'Terminals: ',terminals)
                  end
                else if Ch = 'F'   then
                  begin
                    Frequency  :=  ChNext <>'-';
                    writeln(TTYOut,'Frequency: ',frequency)
                  end
                else if  Ch = 'M'   then
                  begin
                    FSM  :=  ChNext<>'-';
                    writeln(TTYOut,'FSM: ',fsm)
                  end
                else  if  Ch  = 'D'   then
                  begin
                    Debug  := ChNext <> '-';
                    writeln(TTYOut,'Debug: ',debug)
                  end
                else  if  Ch  = 'R'   then
                  begin
                    Trace  :=  ChNext <> '-';
                    writeln(TTYOut,'Trace: ',trace)
                  end
                else  if  Ch in Digits  then
                begin
                    MaxK:=  ord(Ch) - ord('0');
                    writeln(TTYOut,'Maximum lookahead: ',maxk)
                end
                else  if  Ch = 'A'   then
                  begin
                    Look := ChNext<>'-';
                    writeln(TTYOut,'Look: ',look)
                  end
                else if Ch='G'  then
                  begin
                    Grammar:=  ChNext <> '-';
                    writeln(TTYOut,'Grammar: ',grammar)
                  end
                else if Ch ='Z' then
                  begin
                    Research:= ChNext<>'-';
                    writeln(TTYOut,'Research: ',Research)
                  end
                else if Ch ='N' then
                  begin
                    NonTerminals:= ChNext<>'-';
                    writeln(TTYOut,'Non-terminals: ',nonterminals)
                  end
                else if Ch ='Y' then
                  begin
                    Symbolic:= ChNext<>'-';
                    writeln(TTYOut,'Symbolic: ',symbolic)
                  end
                else if Ch = 'I' then
                  begin
                    IFSM := ChNext<>'-' ;
                    writeln(TTYOut,'IFSM: ',ifsm)
                  end
                else if Ch = 'C' then
                  begin
                    Lr0 := ChNext <> '-' ;
                    writeln(TTYOut,'Lr0: ',lr0)
                  end
                else  if  Ch = 'P'   then
                  begin
                    PN :=  ChNext <> '-';
                    writeln(TTYOut,'PN: ',pn)
                  end;
              end;
              writeln(TTYOut)
          end ;
        �  Options  �



      begin
        �  InitP    �
        �  GET Filename �

        InitSets;
        writeln(TTYOut, 'PAT/lalr -- December, 1983');
        writeln(TTYOut);
        InitArgs;


        PromptArgs('File: ');
        for I:= 1 to FILENAMELEN do
            Filename[I] := ' ';
        I:= 0;
        Dot:=0;
        RBrak:=0;
        Colon:=0;
        GetArgCh( ch, EndLine, EndArgs );
        while not EndLine and not EndArgs and
              not FatalError and (I < FILENAMELEN) do begin
            if (Ch in (Letters + Digits)) or (Ch = '<') or
                (Ch = '-') or (Ch = '.') or (Ch = '>') or (Ch = ':')
              then
                begin
                I := I + 1;
                Filename [I] := Ch;
                if ( Ch = '.' ) AND ( Dot = 0 ) then
                    Dot := I
                else if Ch = ':' then Colon := I
                else if Ch = '>' then RBrak := I;
                end
            else begin
                writeln (TTYOut,'?Error in filespecs');
                FatalError := TRUE;
            end;
            GetArgCh( ch, EndLine, EndArgs );
        end;

    if not FatalError then begin
        if dot=0 then �set default extension to PAT� begin
            dot := i+1;
            filename[i+1] := '.';
            filename[i+2] := 'P';
            filename[i+3] := 'A';
            filename[i+4] := 'T';
            i := i+4
        end;

        if CanOpen(filename) then
            open( input, filename, [ OpenInput, OpenUppercase ] )
     /*Reset(input,filename,'/U/O'); �trap errors & xlate input to UC�*/
        else begin
            writeln(TTYOut,'?Cannot open input file');
            FatalError := TRUE
        end;
    end;

    if not FatalError then begin
        Ch := ' ';
        if RBrak > Colon        � Get rid of device and directory names �
        then Distance := RBrak
        else Distance := Colon;
        if Distance > 0
        then
            for J:=1 to I+Distance do
                Filename[J] := Filename[J+Distance];
        Dot := Dot-Distance;            � Move down the dot �
        Options;
        Filename [Dot+1]  :=  'L'   ;
        Filename [Dot+2]  :=  'S'   ;
        Filename [Dot+3]  :=  'T'   ;
        open(OUTPUT,Filename,[ OpenOutput ]);
        if Frequency or FSM or Look or tables
        then
          begin
            Filename [Dot+1]  :=  'C'   ;
            Filename [Dot+2]  :=  'R'   ;
            Filename [Dot+3]  :=  'L'   ;
            open (CrefPas, Filename, [ OpenOutput ])
          end   ;
        if Terminals then
          begin
            Filename [Dot+1] := 'T';
            Filename [Dot+2] := 'E' ;
            Filename [Dot+3] := 'R' ;
            open (TerPas,Filename,[OpenOutput])
          end ;
        if Tables then
          begin
            Filename[Dot+1] := 'T';
            Filename[Dot+2] := 'A' ;
            Filename[Dot+3] := 'B';
            open (TabPas,Filename,[OpenOutput])
          end ;
        if PN then
          begin
            Filename [Dot+1] := 'P' ;
            Filename [Dot+2] := 'N' ;
            Filename [Dot+3] := ' ' ;
            open (PNPas,Filename,[OpenOutput])
          end ;
        NextCh;
    end;
end; � InitP �



    procedure  Skip (SyS : setofsymbol)  ;

        �********************************************�
        �*                                          *�
        �*  Skip                                    *�
        �*                                          *�
        �* Skip Input String until RELEVANT symbol  *�
        �*  IS FOUND                                *�
        �********************************************�
      begin
        while not(Sy in SyS) and not eof(Input) do InSymbol  ;
        Error (0)
      end  ;



    function FindId (IdName :word; Key : integer ; IdKind
        : setofnamety ) : identptrty ;
        �*********************************************�
        �*                                           *�
        �*  FindId                                   *�
        �*                                           *�
        �*     THIS procedure RETURNS A              *�
        �*   A POinTER to THE GIVEN NAME AND type    *�
        �*  in then symbol Table                     *�
        �*                                           *�
        �*********************************************�

    var
        I : integer ;
        Found : boolean  ;
        P,Q  : identptrty;
      begin
        I := Key mod NAMETABSIZE ;
        Q := NameTab[I] ;
        Found := FALSE;
        P := NIL ;
        while (Q <> NIL) AND (not Found ) do
          begin
            if Q@.InternalName = IdName
            then
              begin
                Found := Q@.Kind in IdKind ;
                P := Q;
              end ;
            Q := Q@.Nic
          end ;
        FindId := P
      end ;




    function EnterId (ExtIdName, IntIdName: word; Key: integer;
        IdKind: namety; IdCode: codety; IdSy : symbol)  :  identptrty  ;
        �***************************************************�
        �*                                                 *�
        �*  EnterId                                        *�
        �*                                                 *�
        �*THIS procedure ENTERS THE GIVEN NAME             *�
        �*  into THE symbol Table.  if THE NAME IS A       *�
        �*  TerminAL THE TerminAL Code IS ENTERED AS       *�
        �*  AN ATTRIBUTE of THE NEWLY ENTERED symbol.      *�
        �*                                                 *�
        �***************************************************�
    label
        2;
    var
        I : integer ;
        P,Q : identptrty;
      begin
        � EnterId �
        I := Key mod NAMETABSIZE ;
        Q := NameTab [I]  ;

        if Trace then
          begin
            writeln(TTYOut,'I= ',I,'Key= ',Key);
            BREAK
          end ;
        while Q <> NIL do
          begin
            if Q@.InternalName = IntIdName then
              begin
                Error (8) ;
                EnterId := Q;
                GOto 2
              end ;
            Q := Q@.Nic
          end ;
        new (P) ;
        with P@ do
          begin
            Nic := NIL  ;
            FirstAlt := NIL  ;
            ExternalName   := ExtIdName  ;
            InternalName   := IntIdName  ;
            SpId := IdSy ;
            Kind  := IdKind  ;
            if IdCode= -1 then IdCode :=MaxCode+1;
            if IdCode > MAXVOCSIZE then
                writeln(TTYOut,'Code is too big for cross indexing')
            else SymbolName[IdCode] := P ;
            Code  := IdCode  ;
            if IdCode > MaxCode then MaxCode := IdCode ;
            UsedIn  := NIL  ;
            Defin  := NIL
          end  ;
        Q := NameTab [I]  ;
        NameTab [I] := P  ;
        P@. Nic  := Q     ;
        EnterId  := P  ;
        2 :
      end  ;
    � EnterId �


    procedure Terminal  ;
        �************************************************************�
        �*                                                          *�
        �*   Terminal                                               *�
        �*                                                          *�
        �* THIS procedure Reads THE Terminal                        *�
        �* DefinITIONS AND ENTERS THEM in THE                       *�
        �* symbol Table                                             *�
        �************************************************************�
    var

        TP   : identptrty ;
        LKey : integer ;
        InternalName,
        ExternalName : word;
        NotEof : boolean;
begin
        � Terminal �
        InSymbol ;
    if InternalId <> 'BEGIN' then begin � Terminal part need not be there �
        if InternalId <> 'TERMINAL' then
          begin
            Error (5) ;
            Skip ([SemiSy])
          end ;
        NotEof := not eof(Input);
        InSymbol  ;
        while (InternalId <> 'END') and NotEof do
          begin
            if (Sy <> Ident) AND (Sy <> Res) then
              begin
                Error (6);
                Skip ([Ident,Res])
              end;
            InternalName := InternalId  ;
            ExternalName := ExternalId  ;
            LKey := SyKey ;
            InSymbol;
            if Sy <> EqSy  then
              begin
                Error (7)  ;
                Skip ([Intconst])
              end
            else InSymbol  ;
            TP :=EnterId(ExternalName, InternalName, LKey, Term, SyVal, Sy);
            NotEof := not eof(Input);
            InSymbol  ;
            if Sy = SemiSy  then begin
                NotEof := not eof(Input);
                InSymbol;
            end;
          end  ;
        InSymbol ;
        if Sy= SemiSy then
            InSymbol;
    end; � Allow Terminal part to be absent... �
end ;
    � Terminal �



    procedure ReadG   ;
        �********************************************�
        �*                                          *�
        �*  ReadG                                   *�
        �*                                          *�
        �* THIS procedure Reads in THE Grammar      *�
        �* AND SETS UP THE symbol Table             *�
        �*                                          *�
        �********************************************�
    label
        1,2;
    var
        T,U� CURRENT RHS TranSITION �
        ,Q:   prodptrty ;� LHS Production �
        V,P:   identptrty ;� LHS symbol �
        R,S:   listptrty ;� of Production NO �
        PopNo:   integer ;
        NotEof : boolean;
      begin
        � ReadG �
        if Trace then
          begin
            writeln(TTYOut,'ReadG') ;
            Break
          end ;
        if InternalId <> 'BEGIN' then
          begin
            Error(14) ;
            Skip([Ident,Res])
          end
        else InSymbol ;
        NotEof := TRUE;
        while (InternalId <> 'END') and NotEof do
          begin
            if (Sy <> Ident) AND (Sy <> Res) then
              begin
                  repeat
                    Error (9)  ;
                    Skip ([SemiSy])  ;
                    NotEof := not eof(Input);
                    InSymbol
                  until (Sy=Ident) or (Sy=Res) or not NotEof;
                goto 1
              end ;
            P := FindId (InternalId, SyKey, [NonTerm,Term])  ;
            if P <> NIL then
            if P@.Kind=Term then P@.Kind := NonTerm ;
            if P = NIL then
                P := EnterId ( ExternalId, InternalId, SyKey,
                               NonTerm, MaxCode+1, Sy );
            � ENTER RHS �
            InSymbol   ;
            if Sy <> EqSy then
              begin
                Error (10)  ;
                Skip ([SemiSy])  ;
                InSymbol  ;
                goto 1
              end  ;
            Q := P@.FirstAlt ;
            if Q<> NIL then while Q@.NxtAlt<> NIL do
            Q := Q@.NxtAlt ;
              repeat
                PopNo := 0  ;
                new (U)  ;
                if Q = NIL then P@.FirstAlt := U
                else Q@.NxtAlt   := U  ;
                Q := U  ;
                with Q@ do
                  begin
                    Next := NIL  ;
                    XDef :=  P   ;
                    Action := NIL  ;
                    NxtAlt := NIL
                  end ;
                if  ProdNo < MAXRULENO then ProdNo := ProdNo +1
                else   Error (12)  ;
                new (R)  ;
                S := P@.Defin;
                if S <> NIL  then
                  begin
                    while S@.CDR <> NIL  do  S:= S@.CDR  ;
                    S@.CDR  := R ;
                  end
                else  P@.Defin  := R  ;

                R@.CDR := NIL ;
                R@.CAR  := ProdNo     ;
                RuleTab [ProdNo]  := Q;
                Q@.Production := ProdNo ;
                InSymbol;
                if (Sy = SemiSy) or (Sy = OrSy ) then
                  begin
                    PopTab [ProdNo] := PopNo ;
                    if PopNo > MaxPop then MaxPop := PopNo;
                    if Sy= SemiSy
                    then
                      begin
                        InSymbol  ;
                        goto 1
                      end
                    else
                    goto 2
                  end
                else while (Sy=Ident) or (Sy=Res) do
                  begin
                    PopNo := PopNo+1 ;
                    V :=  FindId (InternalId,SyKey, [NonTerm , Term]) ;
                    if V = NIL then
                        V := EnterId (ExternalId, InternalId, SyKey,
                                      Term, MaxCode+1, Sy) ;
                    new (T)  ;
                    U@.Next  :=T ;
                    U   :=T ;
                    T@.Action  := V  ;
                    T@.XDef     := P  ;
                    T@.NxtAlt := NIL  ;
                    T@.Next    := NIL  ;
                    S := V@.UsedIn  ;
                    new (R)  ;  R@.CDR  := NIL ;
                    if  S <> NIL then
                      begin
                        while  S@.CDR <> NIL do  S := S@.CDR  ;
                        S@.CDR  := R  ;
                      end
                    else    V@.UsedIn  :=  R ;
                    R@.CAR := ProdNo ;
                    InSymbol
                  end  ;
                PopTab[ProdNo] := PopNo ;
                if PopNo > MaxPop then MaxPop := PopNo;
                if Sy = SlashSy  then
                  begin
                    InSymbol ;
                    if Sy <> Ident
                    then
                      begin
                        Error (9)  ;
                        Skip ([SemiSy]);
                        goto 1
                      end  ;
                    V := FindId (InternalId, SyKey, [Lab]) ;
                    if V=NIL then
                        V:= EnterId (ExternalId, InternalId, SyKey, Lab,0, Sy) ;
                    S := V@.UsedIn ;
                    new(R) ; R@.CDR := NIL;
                    if S <> NIL then
                      begin
                        while S@.CDR <> NIL do S :=S@.CDR;
                        S@.CDR := R
                      end
                    else V@.UsedIn := R;
                    R@.CAR := ProdNo ;
                    Q@.Action := V  ;
                    InSymbol
                  end ;

                2:
              until Sy <> OrSy ;
            if Sy <> SemiSy then
              begin
                Error (13)  ;
                Skip ([SemiSy])  ;
              end ;
            NotEof := not eof(Input);
            InSymbol ;
            1:
          end ;
        MaxProdNo := ProdNo ;
        if (PopTab [1] <> 2) or
            (RuleTab [1]@.Next@.Next@.Action@.InternalName <> 'GOALSY')
         or (RuleTab[1]@.NxtAlt <> NIL)
        then writeln(TTYOut,'First production must have GOAL symbol.');
      end ;
    � ReadG  �



    procedure  PrintG ;
        �*****************************************************�
        �*                                                   *�
        �*  PrintG                                           *�
        �*                                                   *�
        �*THIS procedure PRinTS OUT A forMATTED              *�
        �*  Grammar ONto FILENAM. new                        *�
        �*                                                   *�
        �*                                                   *�
        �*                                                   *�
        �*****************************************************�
    var
        I,Count  : integer ;



        procedure PrintProd ( LHS : prodptrty
            ;ProdNo  :  integer  ) ;
            �***********************************************�
            �*                                             *�
            �* Print Prod                                  *�
            �*                                             *�
            �*   THIS procedure PRINTS OUT A Production    *�
            �*                                             *�
            �*                                             *�
            �***********************************************�
        var
            RHS    : prodptrty ;



            procedure    PrintRHS  ( LinK  :  prodptrty )  ;
                �***********************************************�
                �*                                             *�
                �* PrintRHS                                    *�
                �*                                             *�
                �*    THIS procedure PRINTS OUT THE RIGHT      *�
                �*    HAND SIDE of A Production                *�
                �*                                             *�
                �*                                             *�
                �***********************************************�

              begin
                � PrintRHS�
                Count  := 0  ;
                if LinK= NIL
                then write(CrefPas,'                   ')
                else
                  repeat
                    if Count = 3  then
                      begin
                        writeln ( CrefPas)  ;
                        Count := 0  ;
                        write ( CrefPas,'                         ');
                      end   ;
                    Count := Count + 1   ;
                    write ( CrefPas, LinK@.Action@.ExternalName,'   ');
                    LinK :=  LinK@.Next
                  until  LinK = NIL  ;
                if LHS@.Action <> NIL
                then
                  begin
                    while Count <2 do
                      begin
                        write(CrefPas,'               ');
                        Count := Count+1 ;
                      end;
                    write ( CrefPas, '/  ', LHS@.Action@.ExternalName);
                  end ;
                if LHS@.NxtAlt = NIL
                then write ( CrefPas, '  ;  ');
              end ;
            �  PrintRHS �



          begin
            � PRINT prod �
            if ProdNo <  10
            then write (CrefPas, '  ', ProdNo:1)
            else if ProdNo < 100
            then write (CrefPas, ' ', ProdNo : 2)
            else if ProdNo < 1000
            then write (CrefPas, ProdNo : 3)  ;
            if LHS@.XDef@.FirstAlt = LHS then
            write ( CrefPas,'   ', LHS@.XDef@.ExternalName, '=   ')
            else write ( CrefPas, '               ', '   !   ' );
            RHS :=  LHS@.Next ;
            PrintRHS (RHS)  ;
            writeln(CrefPas,' # ',LHS@.Production) ;
          end ;



      begin
        � PRINT G�
        I := B14 ;
        Ch := ChR (I) ;
        writeln (CrefPas, Ch)  ;
        writeln (CrefPas, '                      Grammar ');
        writeln (CrefPas, '                      ======= ');
        writeln (CrefPas)  ;
        for I := 1 to MAXRULENO do
        if RuleTab [I] <> NIL
        then PrintProd (RuleTab [I], I) ;
        writeln (CrefPas, Ch)
      end  ;
    � PRINT G�








    procedure  PrintCref ;
        �******************************************************�
        �*                                                    *�
        �*PrintCref                                           *�
        �*                                                    *�
        �*   THIS procedure CONTROLS THE PRINTinG of THE      *�
        �*    CROSS REFERENCE listinG                         *�
        �*                                                    *�
        �*                                                    *�
        �******************************************************�
    type
        symlistptrty = @ symlist ;
        symlist =  record
                          Sym : identptrty;
                          CDR : symlistptrty
                   end ;
    var
        Head  :  symlist  ;
        I  :  integer  ;
        Ch  :  char     ;

procedure PrintTerm (IdKind : setofnamety) ;

        �*****************************************�
        �*                                       *�
        �* PrintTerm                             *�
        �*                                       *�
        �*  THIS procedure PRINTS OUT THE        *�
        �* Terminal symbol DefinITIONS           *�
        �*                                       *�
        �*****************************************�

var

        P : symlistptrty;



begin    � PrintTerm �


        P := Head.CDR ;

        while P <> NIL do begin
                if P@.Sym@.Kind in IdKind then begin
                    write(TerPas,P@.Sym@.ExternalName,' = ');
                    if P@.Sym@.SpId = Res then
                        writeln(TerPas, '-1 ;')
                    else
                        writeln(TerPas,P@.Sym@.Code,' ;');
                end;
                P := P@.CDR ;

        end ;

end  � PrintTerm � ;



        procedure   PrintVoc  (IdKind : setofnamety) ;
            �**********************************************************�
            �*                                                        *�
            �*    PrintVoc                                            *�
            �*                                                        *�
            �*        THIS procedure PRINTS OUT CROSS REFERENCE       *�
            �*   inforMATION for Terminals , NON-                     *�
            �*   Terminals AND Action labelS                          *�
            �*                                                        *�
            �**********************************************************�
        var
            P   :    symlistptrty ;
            Count :   integer    ;
            Q :    listptrty ;
          begin
            �  PRINT VOC  �
            if Trace then
              begin
                writeln(TTYOut,'VOC') ;
                Break
              end ;
            P :=   Head.CDR ;
            while  P <> NIL  do
              begin
                if       P@.Sym@.Kind
                in  IdKind
                then
                  begin
                    writeln (CrefPas)  ;
                    writeln(CrefPas);writeln(CrefPas);
                    write(CrefPas, P@.Sym@.ExternalName);
                    if P@.Sym@.SpId <> Res then
                        write(CrefPas, '  ',P@.Sym@.Code)
                    else
                        write(CrefPas,'     Reserved');
                    writeln(CrefPas) ;
                    write(CrefPas, ' Defined ') ;
                    Count := 0   ;
                    Q := P@.Sym@.Defin;
                    while  Q <> NIL  do
                      begin
                        if Count = 4 then
                          begin
                            writeln (CrefPas) ;
                            Count:=0          ;
                            write  (CrefPas,'         ')
                          end ;
                        Count := Count +1   ;
                        write (CrefPas, Q@.CAR, '   ') ;
                        Q := Q@.CDR
                      end     ;
                    writeln (CrefPas);
                    Count  :=0;
                    Q := P@.Sym@.UsedIn ;
                    write (CrefPas, ' Used    ') ;
                    while Q <> NIL  do
                      begin
                        if   Count =4   then
                          begin
                            writeln (CrefPas) ;
                            Count :=0  ;
                            write (CrefPas,'         ')
                          end   ;
                        Count := Count +1 ;
                        write (CrefPas , Q@.CAR, '   ') ;
                        Q := Q@.CDR
                      end   ;
                  end ;
                P:= P@.CDR
              end   ;
          end ;
        �  PRINT VOC  �



        procedure    Sort ;
            �************************************************�
            �*                                              *�
            �*    Sort                                      *�
            �*                                              *�
            �*       THIS procedure SortS THE symbol Table  *�
            �*                                              *�
            �************************************************�
        label
            1,2;
        var
            I,K :  integer      ;
            X   :   identptrty ;
            P,Q,R   :   symlistptrty ;
          begin
            �  Sort  �
            if Trace then
              begin
                writeln(TTYOut,'Sort');
                Break
              end ;
            Head.CDR := NIL ;
            for K :=  0 to  NAMETABSIZE         do
              begin
                X := NameTab [K] ;
                while X <> NIL  do
                  begin
                    new (R)   ;
                    R@.Sym  := X  ;
                    R@.CDR  := NIL;
                       �  Enter in chain  �
                    Q       := Head.CDR   ;
                    if Q=NIL then Head.CDR:= R
                    else
                      begin
                        if R@.Sym@.InternalName<Q@.Sym@.InternalName
                        then
                          begin
                            Head.CDR := R;
                            R@.CDR := Q
                          end
                        else
                          begin
                            P       :=  Q@.CDR    ;
                            while (P <> NIL)
                            do
                              begin
                                if R@.Sym@.InternalName < P@.Sym@.InternalName
                                    then goto 1;
                                Q :=P   ;
                                P := P@.CDR
                              end   ;
                            1:  Q@.CDR := R  ;
                            R@.CDR := P
                          end ;
                      end ;
                    X:= X@.Nic
                  end
              end   ;
            �  for  �
            2:
          end   ;
        �  Sort  �
      begin
        �  PRINT CREF  �
        mark(Heap);
        Sort ;
        if Grammar then begin
        I :=  B14   ;
        Ch :=  ChR(I)  ;
        writeln (CrefPas, Ch)   ;
        writeln (CrefPas, '                      Cross Reference ');
        writeln (CrefPas, '                      ================');
        writeln (CrefPas,'      Terminals') ;
        writeln (CrefPas);
        PrintVoc([Term])      ;
        writeln (CrefPas, Ch) ;
        writeln (CrefPas,'     Non Terminals')  ;
        PrintVoc([NonTerm])    ;
        writeln (CrefPas, Ch)  ;
        writeln (CrefPas, '     Label')  ;

        PrintVoc([Lab]) ;
        writeln (CrefPas, Ch)  ;
        end ;
        if Terminals and NonTerminals then
            PrintTerm([ Term, NonTerm ])
        else if Terminals then
            PrintTerm([Term])
        else if NonTerminals then
            PrintTerm([NonTerm]);
        release (Heap)
      end   ;
    �  PRINT CREF  �






    procedure PrintCon (T : tranptrty) ;
        �********************************************************�
        �*                                                      *�
        �*    PrintCon                                          *�
        �*                                                      *�
        �*   THIS procedure PRINT A CONFIGURATION in Symbolic   *�
        �* forM                                                 *�
        �*                                                      *�
        �*   in :   T  GIVEN CONFIGURATION                      *�
        �********************************************************�
    var
        X : integer ;
        J : integer ;
        Red,SameLine : boolean ;
        P : prodptrty ;



      begin
        � PRINT CON  �
        X := T@.Pos ;
        P := T@.Rule ;
        SameLine := TRUE ;
        Red := FALSE ;
        J := 0 ;
        write(CrefPas,'* ');
        write(CrefPas,P@.XDef@.ExternalName);
        write(CrefPas,' = ');
        while X <> 0 do
          begin
            P := P@.Next ;
            write (CrefPas,P@.Action@.ExternalName,' ') ;
            X := X-1 ;
            J := J+1;
            if J >= 3 then
              begin
                writeln(CrefPas,'   *');
                write(CrefPas,'*                   ');
                J := 0
              end ;
          end ;
        write(CrefPas,'# ');
        P := P@.Next ;
        if P= NIL then Red := TRUE else Red := FALSE ;
        while P <> NIL do
          begin
            write(CrefPas,P@.Action@.ExternalName,' ');
            J := J+1 ;
            if J >= 3 then
              begin
                writeln(CrefPas,' *');
                write(CrefPas,'*                     ');
                SameLine := FALSE ;
                J := 0 ;
              end ;
            P := P@.Next
          end ;
        if Red then
          begin
            while J < 2 do
              begin
                write(CrefPas,'                ');
                J := J+1 ;
              end ;
            P := T@.Rule ;
            if P@.Action <> NIL then
              begin
                write(CrefPas,'/ ');
                write(CrefPas,P@.Action@.ExternalName);
                writeln(CrefPas,'*');
                if T@.NextState <> NIL then
                writeln(CrefPas,'*                                  ',
                '                   ',T@.NextState@.State:10,'       *');
              end
            else if T@.NextState <> NIL
            then writeln(CrefPas,T@.NextState@.State:10,'       *')
            else writeln(CrefPas,'                 *');
          end
        else
          begin
            while J <2 do
              begin
                write(CrefPas,'                ');
                J := J+1
              end ;
            if T@.NextState <> NIL then
              begin
                write(CrefPas,T@.NextState@.State: 10);
                if SameLine then writeln(CrefPas,'       *')
                else writeln(CrefPas,'       *');
              end
            else if SameLine then writeln(CrefPas,'                 *')
            else writeln(CrefPas,'                 *');
          end
      end   �   PRINT CON �   ;



    procedure PrintState(S :stateptrty)  ;
        �**************************************************�
        �*                                                *�
        �*  PrintState                                    *�
        �*                                                *�
        �*  THIS PROC PRINTS A State                      *�
        �*   in   :   S State                             *�
        �**************************************************�
    var
        T : tranptrty ;
        ST : stateptrty ;


      begin
        � PrintState �

        writeln(CrefPas) ;
        writeln(CrefPas) ;
        writeln(CrefPas,'*******************************************',
                        '*****************************');
        write(CrefPas,'*     State ');
        write(CrefPas,S@.State) ;
          case S@.Kind of
            Reads : write(CrefPas,' read State ');
            Inadequate:write (CrefPas,' Inadequate ');
            Lookahead :write (CrefPas, ' Lookahead  ') ;
            Reduction  : write (CrefPas, ' Reduction  ') ;
            LAString : write (CrefPas,' LAString   ') ;
            Subgoal : write (CrefPas, ' Subgoal    ')
          end ;
        writeln( CrefPas, '                                   *');
        writeln(CrefPas,'*******************************************',
                        '*****************************');
        writeln(CrefPas,'*                                          ',
                        '                            *');
        T := S@.FirstTran ;
        while T <> NIL do
          begin
            PrintCon(T) ;
            T := T@.Next
          end ;
        writeln(CrefPas,'*******************************************',
                        '*****************************');
        writeln(CrefPas) ;
        writeln(CrefPas)
      end  � PrintState �   ;



    procedure OutFSM ;
        �*********************************************�
        �*                                           *�
        �*         OutFSM                            *�
        �*    THIS PROC OUTPUTS FSM Tables           *�
        �*                                           *�
        �*********************************************�

    label
        1;
    var
        I : integer ;


      begin
        � OutFSM �
        I := B14 ;
        Ch := ChR(I) ;
        writeln(CrefPas,Ch) ;
        writeln(CrefPas,'                      Characteristic States');
        writeln(CrefPas,'                      =====================');
        writeln(CrefPas);writeln(CrefPas);writeln(CrefPas);
        for I := 0 to MAXSTATE do
          begin
            if NState [ I ] = NIL then goto 1 ;
            PrintState (NState [I])
          end ;
        1:
      end     � OutFSM �  ;







    procedure PrintTab ;
        �************************************************�
        �*                                              *�
        �* PrintTab                                     *�
        �*                                              *�
        �*    THIS procedure PRINTS THE symbol Action   *�
        �*  Tables .                                    *�
        �*                                              *�
        �************************************************�

    var
        I : integer ;
        T,A : integer ;



      begin
        � PRINT TAB�
        I := B14 ;
        Ch := ChR(I);
        writeln(CrefPas,Ch);
      I := 3*Index +3 ; � NUMBER of ENTRIES�
        writeln(CrefPas);
        writeln(CrefPas,'                Parsing Tables');
        writeln(CrefPas,'                ==============');
        writeln(CrefPas);writeln(CrefPas);writeln(CrefPas);
        writeln(CrefPas,'.__________________________________________',
                        '___________________________.');
        writeln(CrefPas,'! Index  Symbol Name                 Action',
                        '  Pop    Semantics         !');
        writeln(CrefPas,'.__________________________________________',
'___________________________.');
        for I := 0 to Index do
          begin
            write(CrefPas,'!',I:5);
            T := Table[I].Tran ;
            A := Table[I].Action ;
            if T=elseCode then write(CrefPas,'    ELSE')
            else if T=XCONTinUE then  write(CrefPas,'    CONT')
            else write (CrefPas,'   ',T:5);
            if (T >=0 ) AND (T <= MAXVOCSIZE)
            then write(CrefPas,'  ',SymbolName[T]@.ExternalName)
            else write(CrefPas,'                 ');
            if A<= 0 then
              begin
                if A <= -NOSCANCODE then write(CrefPas,' ',-A-NOSCANCODE:4,'#')
                else
                write(CrefPas,' ',-A:4,'#');
                if A <= -NOSCANCODE then write(CrefPas,'NO SCAN')
                else write (CrefPas,'       ');
              end
            else if A= ERRORCODE  then write(CrefPas,'   Error            ')
            else write(CrefPas,'   ',A:5,'            ');


            if A < 0 then
              begin
                if A <= - NOSCANCODE then A := A+NOSCANCODE;
                write(CrefPas,PopTab[-A]:4,'     ');
                if RuleTab[-A]@.Action <> NIL then
                    write(CrefPas,RuleTab[-A]@.Action@.ExternalName)
                else write(CrefPas,'               ');
              end
            else write(CrefPas,'                 ');
            writeln(CrefPas,'  !');
            writeln(CrefPas,'.______________________________________',
                            '_______________________________.');
          end � for� ;
      end � PRINT TAB� ;



    function NextTran (S :stateptrty; T : tranptrty ):tranptrty ;

        �******************************************************�
        �*                                                    *�
        �* NextTran                                           *�
        �*                                                    *�
        �*    THIS function GETS THE Next UNMarkED            *�
        �* TranSITION FROM State S. THE CURRENT TranSITION IS *�
        �* T. IT RETURNS NIL if NO Next TranSITION EXISTS     *�
        �*                                                    *�
        �*  in    : S    CURRENT State                        *�
        �*          T    CURRENT TranSITION                   *�
        �*                                                    *�
        �*  OUT   :                                           *�
        �*                                                    *�
        �*  GLOBAL                                            *�
        �*                                                    *�
        �* ResULT    PTR to Next TranSITION                   *�
        �******************************************************�


    label
        1;
    var
        CurTran   : tranptrty   ;

      begin
        � NextTran �
        if Trace then
          begin
            writeln(TTYOut,'NextTran');Break
          end;
        CurTran := T ;
        if CurTran = NIL then
          begin
            CurTran := S@.FirstTran ;
            if CurTran = NIL then
                writeln(TTYOut,'**System error** in NextTran');
            CurTran@.MarkE := TRUE
          end
        else
          begin
            while  CurTran@.MarkE do
              begin
                CurTran := CurTran@.Next ;
                if CurTran = NIL then goto 1
              end ;
            1:    if CurTran = NIL then
              begin
                CurTran := S@.FirstTran ;
                  repeat
                    CurTran@.MarkE := FALSE ;
                    CurTran       := CurTran@.Next
                  until CurTran = NIL
              end
            else CurTran@.MarkE := TRUE
          end ;
        NextTran := CurTran
      end     � NextTran �   ;









    function ConMatch(C1,C2 : tranptrty) : boolean ;
        �***************************************************�
        �*                                                 *�
        �*   ConMatch                                      *�
        �*                                                 *�
        �*     THIS function COMPARes TWO CONFIGURATIONS   *�
        �*  AND RETURNS THE ResULT                         *�
        �*      TWO CONFIGURATIONS MATCh if EVERYTHinG     *�
        �* BEYOND THE Dot MATChES inCL THE Action          *�
        �*   for THE Research VERSION . OTHERWISE USUAL DRF *�
        �*                                                 *�
        �*    in      : C1,C2 CONFIGURATIONS               *�
        �*                                                 *�
        �*                                                 *�
        �*   GLOBAL          in :   RuleTab                *�
        �*                                                 *�
        �*  ResULT     TRUE if MATChES                     *�
        �***************************************************�


    label
        1;
    var
        I1, I2,I   : integer  ;
        X1,X2  : prodptrty ;



      begin
        � ConMatch    �
        I1 :=C1@.Pos ;
        I2 := C2@.Pos ;
        X1 := C1@.Rule ;
        X2 := C2@.Rule ;
        if not Research then begin
            if (I1=I2) and (X1=X2) then
                ConMatch := TRUE
            else
                ConMatch := FALSE;
        end else begin
            if (X1@.Action <> X2@.Action) or (I1 <> I2) then
                ConMatch := FALSE
            else begin
                for I := 0 to I1 do
                X1:= X1@.Next ;
                for I := 0 to I2 do
                X2 := X2@.Next ;
                while (X1<>NIL) and (X2 <> NIL) do
                  begin
                    if X1@.Action <> X2@.Action
                    then
                      begin
                        ConMatch := FALSE;
                        goto 1
                      end ;
                    X1:= X1@.Next;
                    X2 := X2@.Next

                  end ;
                if (X1=NIL) and (X2 = NIL)
                then ConMatch := TRUE
                else ConMatch  := FALSE ;
              end ;
          end ;
        1:

      end  � ConMatch  �   ;



    procedure AddT( S: stateptrty ; T : tranptrty) ;
        �*****************************************************�
        �*                                                   *�
        �*  AddT                                             *�
        �*                                                   *�
        �*     THIS procedure ADDS A CONFIGURATION to THE    *�
        �* GIVEN State . IT ALSO MAKES SURE THE doES not EXIST*�
        �*  BEforE ENTERinG IT                               *�
        �*     in       :S   GIVEN State                     *�
        �*               T  GIVEN CONFIGURATION              *�
        �*****************************************************�


    label
        1;
    var
        Tx,Ty   : tranptrty ;



      begin
        �  AddT  �

        Tx  := S@.FirstTran  ;
        if Tx = NIL then S@.FirstTran := T
        else
          begin
            while Tx <> NIL do
              begin
                if ConMatch(Tx,T) then goto 1  ;
                Ty := Tx ;
                Tx := Tx@.Next
              end ;
            Ty@.Next   := T  ;
          end ;
        1:
      end   � AddT �   ;






    function Token(PR : prodptrty ;PS : integer) : identptrty ;
        �***************************************************�
        �*                                                 *�
        �*  Token                                          *�
        �*                                                 *�
        �* THIS function RETURNS A POinTER to THE NE T     *�
        �* Token GIVEN A Rule NO. and  A Pos NUM           *�
        �*                                                 *�
        �*   in         : PS   PosITION                    *�
        �*                 PR Production                   *�
        �*   OUT                                           *�
        �*                                                 *�
        �   ResULT         PTR to Next Token               *�
        �***************************************************�

    label
        1;
    var
        X : identptrty    ;
        I :  integer    ;
        Y  : prodptrty  ;



      begin
        � Token   �
        Token := NIL;
        Y := PR ;
        for I := 0 to PS
        do
          begin
            Y := Y@.Next ;
            if Y = NIL then
            goto 1
          end ;
        Token :=Y@.Action ;
        1:

      end   � Token � ;



    procedure AddL (S :stateptrty) ;
        �***************************************************�
        �*                                                 *�
        �*  AddL                                           *�
        �*                                                 *�
        �*    THIS procedure ADDS A State to THE list of   *�
        �*  of Inadequate StateS                           *�
        �*                                                 *�
        �*    in : S State                                 *�
        �*                                                 *�
        �***************************************************�

    var
        X,Y  : statelistptr ;



      begin
        � AddL �
        new(X)  ;
        X@.CAR := S  ;
        X@.CDR := NIL ;
        if Inadeq = NIL then Inadeq := X
        else
          begin
            Y := Inadeq ;
            while Y@.CDR <> NIL do Y := Y@.CDR ;
            Y@.CDR := X
          end
      end   � AddL �  ;




    procedure   CFSM (var  Lr0 : boolean)   ;
        �*********************************************************�
        �*                                                       *�
        �*     CFSM                                              *�
        �*       THIS procedure COMPUTES THE charACTERISTIC      *�
        �*  FinITE State MAChinE. Lr0 IS set to FALSE            *�
        �*  if ANY Inadequate State EXISTS. A list of Inadequate *�
        �*  StateS IS MAinTAinED BY GENS                         *�
        �*                                                       *�
        �*                                                       *�
        �*                                                       *�
        �*********************************************************�
    var
        S : stateptrty ;
        S1   : stateptrty ;
        Sl1 : statelistptr ;
        AllReduce : boolean ;



        procedure Closure (A :stateptrty;C:tranptrty;
            var Reduce : boolean) ;
            �**************************************************�
            �*                                                *�
            �*   Closure                                      *�
            �*                                                *�
            �*     THIS procedure PERforMS THE Closure        *�
            �* function ON THE GIVEN CONFIGURATIONS           *�
            �*                                                *�
            �*    in     : A  THE GIVEN State                 *�
            �*             C   CURRENT CONFIG                 *�
            �*    OUT    : Reduce  set to TRUE if Reduction   *�
            �*                         N State                *�
            �*                                                *�
            �*   GLOBAL                                       *�
            �*                                                *�
            �**************************************************�


        label
            1;
        var
            CPos : integer   ;
            Con,CRule : prodptrty ;
            X,Tran   : tranptrty  ;
            Nt   : identptrty   ;
            function Config(Pr : prodptrty ; N: integer) : tranptrty ;






                �*****************************************************�
                �*                                                   *�
                �*  Config                                           *�
                �*                                                   *�
                �*     THIS function CREAES A ConfigURATION          *�
                �*                                                   *�
                �*    in          :  Pr  Production                  *�
                �*                N    PosITION in prod              *�
                �*                                                   *�
                �*  ResULT         PTR to new Config                 *�
                �*****************************************************�


            var
                X   : tranptrty    ;

              begin
                �  Config   �
                new(X) ;
                with X@
                do
                  begin
                    Rule := Pr ;
                    Pos := N  ;
                    MarkE  := FALSE ;
                    Next := NIL ;
                    NextState := NIL
                  end ;
                Config  := X


              end � Config �   ;



          begin
            � Closure   �
            if Trace then
              begin
                writeln(TTYOut,'Closure');Break
              end ;
            CRule  := C@.Rule ;
            CPos   := C@.Pos;
            Nt    :=  Token(CRule,CPos) ;
            if Nt <> NIL
            then
              begin
                AllReduce := FALSE ;
                if Nt@.Kind = NonTerm
                then
                  begin
                    Tran := A@.FirstTran ;
                      repeat
                        if (Nt =Tran@.Rule@.XDef) and
                        (Tran@.Pos = 0) then goto 1;
                        Tran := Tran@.Next
                      until Tran = NIL ;
                    Con := Nt@.FirstAlt;
                    while Con <> NIL do
                      begin
                        X := Config(Con,0);
                        AddT(A,X) ;
                        Con := Con@.NxtAlt
                      end
                  end
              end
            else Reduce := TRUE ;
            1:
          end � Closure   �    ;


        function CIS : stateptrty ;
            �*****************************************************�
            �*                                                   *�
            �*  CIS                                              *�
            �*                                                   *�
            �* THIS procedure SETS UP THE inITIAL State and      *�
            �* ALSO ChECKS to SEE if THE FIRST Production        *�
            �* WAS APPrOPrIATELY SET. if not IT GIVES AN Error   *�
            �*   IT RETURNS A POinTER to THE FIRST State         *�
            �*                                                   *�
            �*    GLOBAL    in:   RuleTab                        *�
            �*                    PopTab                         *�
            �*                                                   *�
            �*            OUT :   StateTab                       *�
            �*                    NState,CurrentState            *�
            �*                                                   *�
            �*                                                   *�
            �*   ResULT    POinTER to FIRST State                *�
            �*                                                   *�
            �*****************************************************�
        var
            X  : stateptrty ;
            Y  : tranptrty ;
            I   : integer ;
            B   : boolean ;
          begin
            �  CIS �
            if Trace then
              begin
                writeln(TTYOut,'CIS');
                Break
              end  ;
            I := 0 ;
            new(X) ;
            new(Y) ;
            CurrentState := 0 ;
            StateTab [0] := X ;
            with X@ do
              begin
                FirstTran := Y ;
                Back      := NIL ;
                Nic       := NIL ;
                State     := 0 ;
                Kind       := Reads
              end ;
            NState [ 0] := X;
            with Y@ do
              begin
                Next := NIL ;
                NextState := NIL ;
                Rule      := RuleTab [1] ;
                Pos       := 0 ;
                MarkE      := FALSE
              end ;
            I  := 1 ;
            CIS := X ;
            B := FALSE ;
            AllReduce := TRUE ;
            Y := NIL ;
              repeat begin
                Y := NextTran(X,Y) ;
                if Y = NIL then
                    leave;
                Closure( X,Y,B) ;
                I := I+1 ;
              end until 1 <> 1;
            if B then if I= 1 then X@.Kind := Reduction
            else
              begin
                AddL(X) ;
                X@.Kind := Inadequate ;
              end ;
          end    � CIS �   ;




        procedure GenS (S : stateptrty ; var Lr0: boolean) ;

            �*****************************************************�
            �*                                                   *�
            �*  GenS                                             *�
            �*                                                   *�
            �*  THIS procedure GENERATES THE State MAChinE       *�
            �* RECURSIVELY. IT SETS Lr0 to FALSE if NECESSARY    *�
            �*                                                   *�
            �*   in    :  S  CURRENt State                       *�
            �*                                                   *�
            �*   OUT   :  Lr0  inDICATES if MAChinE IS Lr0       *�
            �*                                                   *�
            �*   GLOBAL                                          *�
            �*****************************************************�


        var
            Z : prodptrty   ;
            I : integer   ; � ConTROL  varIABLE�
            X : tranptrty    ;
            Y : stateptrty   ;
            B : boolean     ;




            function CHash( S: stateptrty) : integer ;
                �**************************************************�
                �*                                                *�
                �*  CHash                                         *�
                �*                                                *�
                �*     THIS procedure produces A Key.IT USES      *�
                �* THash to produce THE Key                       *�
                �*                                                *�
                �*  in   : S State                                *�
                �*                                                *�
                �**************************************************�
            var
                Key   : integer   ;
                T     : tranptrty ;



                function THash( X: tranptrty) : integer ;
                    �**************************************************�
                    �*                                                *�
                    �* THash                                          *�
                    �*                                                *�
                    �*  THIS function RETURNS AN integer REPresENtinG *�
                    �* A ConfigURATION                                *�
                    �*                                                *�
                    �*   in :  X TranSITION                           *�
                    �*                                                *�
                    �*                                                *�
                    �**************************************************�

                var
                    Y   : prodptrty ;
                    I    : integer ;
                    TKey : integer ;



                    function Number(S: identptrty): integer ;
                      �*****************************************************�
                      �*                                                   *�
                      �*  Number                                           *�
                      �*                                                   *�
                      �*  THIS function RETURNS AN integer BASED ON THE    *�
                      �* VALUE of THE IdentifIER                           *�
                      �*                                                   *�
                      �*   in : S IdentifIER                               *�
                      �*****************************************************�
                   var
                        Y : word ;
                        I    : integer ;
                        Tally : integer ;
                      begin
                        � Number �
                        Tally := 0 ;
                        if S <> NIL then
                          begin
                            Y := S@.ExternalName ;
                            for  I := 1 to MAXNAME do
                            Tally := Tally +ord(Y[I])
                          end  ;
                        Number := Tally

                      end    � Number   �   ;


                  begin
                    � THash �
                    Y := X@.Rule ;
                    TKey :=  Number(Y@.Action) ;
                    for I := 0 to X@.Pos do Y := Y@.Next ;
                    if Y <> NIL then
                    TKey := TKey+Number(Y@.Action) ;
                    THash := TKey
                  end      � THash �    ;





              begin
                � CHash �
                Key := 0 ;
                T := S@.FirstTran ;
                while T <> NIL do
                  begin
                    Key := Key +THash(T) ;
                    T := T@.Next
                  end ;
                CHash := Key
              end ;
            � CHash�






            function Complete ( S : stateptrty; T : tranptrty) :
                stateptrty ;

                �****************************************************�
                �*                                                  *�
                �* Complete                                         *�
                �*                                                  *�
                �* THIS function CREATES A new State and CompleteS IT *�
                �* IT RETURNS A POinTER to THE CompleteD State       *�
                �*                                                   *�
                �*   in    :  S   CURRENt State                      *�
                �*            T   TranSITION State                   *�
                �*   OUT   :                                         *�
                �*                                                   *�
                �*   GLOBAL                                          *�
                �*                                                   *�
                �*  ResULT   PTR to CompleteD State                  *�
                �*****************************************************�

            var
                A : stateptrty ;
                C : tranptrty ;
                Reduce : boolean  ;
                I  : integer    ;



                function Collect (S :stateptrty;T : tranptrty):stateptrty ;
                    �************************************************�
                    �*                                              *�
                    �*  Collect                                     *�
                    �*                                              *�
                    �*  THIS function CREATES A new State  for THE  *�
                    �* GIVEN TranSITION. IT ALSO CollectS ALL       *�
                    �* TranSITIONS WHICh HAVE THE SAME TranSITION   *�
                    �* Token and ADDS IT to THE State               *�
                    �*                                              *�
                    �*  in     : S CURRENt State                    *�
                    �*              T CURRENt TranSITION            *�
                    �*                                              *�
                    �*   OUT                                        *�
                    �*                                              *�
                    �*   GLOBAL                                     *�
                    �*                                              *�
                    �*  ResULT  PTR to new State                    *�
                    �************************************************�

                var
                    X,Z : tranptrty ;
                    Y : stateptrty ;



                    function TranMatch(T1,T2 : tranptrty) : boolean ;
                       �***********************************************�
                       �                                              *�
                       �*  TranMatch                                  *�
                       �*                                             *�
                       �*  THIS function ChECKS to SEE if THE TranSITION *�
                       �*  Token for THE GIVEN TWO ConfigURATIONS     *�
                       �*  IS THE SAME                                *�
                       �*                                             *�
                       �*   in       : T1,T2  ConfigURATIONS          *�
                       �*                                             *�
                       �*    OUT     :                                *�
                       �*                                             *�
                       �*   GLOBAL                                    *�
                       �*                                             *�
                       �*    ResULT IS TRUE if THEY MATCh             *�
                       �***********************************************�


                    var
                        R : prodptrty ;
                        I  : integer  ;
                        S1,S2 : identptrty ;



                      begin
                        � TranMatch  �
                        R   := T1@.Rule  ;
                        for I := 0 to T1@.Pos do
                            R := R@.Next  ;
                        if R = NIL then S1 := NIL
                            else S1 := R@.Action ;

                        R := T2@.Rule  ;
                        for I := 0 to T2@.Pos
                            do R := R@.Next ;
                        if R = NIL then  S2 := NIL
                            else  S2 := R@.Action  ;

                        TranMatch := S1=S2  ;

                      end       � TranMatch �     ;



                    function CreateS (S:stateptrty; T: tranptrty):stateptrty ;
                      �******************************************************�
                      �*                                                    *�
                      �*   CreateS                                          *�
                      �*                                                    *�
                      �*    THIS function CreateS A new State with A SinGLE *�
                      �*  ConfigURATION T . IT doES not ENtER THE State     *�
                      �* into  THE  State Table  SinCE THE State CompleteD  *�
                      � MAY EXIST ALreadY                                   *�
                      �*                                                    *�
                      �*    IT RETURNS A POinTER to THE newLY CREATED State *�
                      �*                                                    *�
                      �*   in        : S PARENt State                       *�
                      �*               T  CURRENt ConfigURATION             *�
                      �*                                                    *�
                      �*   OUT       :                                      *�
                      �*                                                    *�
                      �*   GLOBAL                                           *�
                      �*                                                    *�
                      �    ResULT    PTR to new State                       *�
                      �******************************************************�



                    var
                        X   : stateptrty ;



                      begin
                        � CreateS   �
                        new(X)    ;
                        with X@ do
                          begin
                            FirstTran := T;
                            Nic   := NIL ;
                            Back     := NIL ;
                            State := 0    ;
                            Kind      :=Reads
                          end ;


                        CreateS := X
                      end        � CreateS �   ;



                    function  MoveDot (T : tranptrty) : tranptrty ;
                        �***************************************************�
                        �*                                                 *�
                        �*   MoveDot                                       *�
                        �*                                                 *�
                        �*       THIS function CreateS A new  ConfigURATION*�
                        �* with THE PosITION of THE Dot MOVED OVER         *�
                        �*    IT RETURNS A POinTER to THIS TranSITION      *�
                        �* IT doES not WATChOUT for ReductionS. THIS  ID   *�
                        �* BY THE Closure function                         *�
                        �*                                                 *�
                        �*     in         : T CURRENt Config               *�
                        �*                                                 *�
                        �*     OUT         :                               *�
                        �*                                                 *�
                        �*      GLOBAL                                     *�
                        �*                                                 *�
                        �*    ResULT     PTR to new ConfigURATION          *�
                        �***************************************************�


                    var
                        X  : tranptrty ;

                      begin
                        � MoveDot �
                        new(X)   ;
                        with X@ do
                          begin
                            MarkE := FALSE ;
                            Next := NIL ;
                            NextState := NIL  ;
                            Rule := T@.Rule ;
                            Pos := T@.Pos +1
                          end ;


                        MoveDot  := X
                      end    � MOVE Dot  �    ;









                  begin
                    � Collect  �
                    X := MoveDot (T) ;
                    Y := CreateS (S,X)  ;
                    Z := T@.Next ;
                    while Z <> NIL do
                      begin
                        if TranMatch (Z,T)
                        then
                          begin
                            Z@.MarkE := TRUE;
                            X := MoveDot(Z) ;
                            AddT(Y,X)
                          end ;
                        Z := Z@.Next ;
                      end  ;
                    Collect := Y
                  end      � Collect  �    ;






              begin
                � Complete �
                if Trace then
                  begin
                    writeln(TTYOut,'Complete');
                    Break
                  end ;
                A := Collect(S,T) ;
                C := NextTran( A,NIL) ;
                AllReduce := TRUE ;
                Reduce := FALSE ;
                I := 0   ;
                while C <> NIL do
                  begin
                    Closure(A,C,Reduce) ;
                    I   := I+1 ;
                    C := NextTran(A,C)
                  end ;
                if Reduce then if I=1 then A@.Kind := Reduction
                else
                  begin
                    A@.Kind := Inadequate ;
                    Lr0  := FALSE
                  end ;
                Complete := A
              end   � Complete �  ;

            procedure FindS(var S: stateptrty;var Found:boolean) ;
                �****************************************************�
                �*                                                  *�
                �*  FindS                                           *�
                �*                                                  *�
                �*    THIS procedure ChECKS to SEE if THE GIVEN     *�
                �* State ALreadY EXISTS. if SO IT UPDATES S to      *�
                �* POinT to THE EXISTinG State. in THIS case Found  *�
                �* IS set to TRUE                                   *�
                �*                                                  *�
                �*     in:                                          *�
                �*                                                  *�
                �*   OUT : S   POinTS to Found State                *�
                �*        Found  TRUE if State IS Found             *�
                �*                                                  *�
                �****************************************************�

            var
                Key   : integer ;
                X     : stateptrty  ;



                function StateMatch(S1,S2:stateptrty) : boolean ;
                    �***************************************************�
                    �*                                                 *�
                    �*  StateMatch                                     *�
                    �*                                                 *�
                    �*   THIS procedure ChECKS to SEE if THE GIVEN     *�
                    �* StateS Match                                    *�
                    �*                                                 *�
                    �*    in:  S1,S2 THE StateS to BE MatchED          *�
                    �*                                                 *�
                    �***************************************************�

                var
                    X,X2    : tranptrty ;
                    A,B  : integer   ;
                    Match  : boolean  ;

                  begin
                    � StateMatch �
                    StateMatch := FALSE ;
                    A := 0  ;
                    X := S1@.FirstTran   ;
                    while X <> NIL do
                      begin
                        A := A+1 ;
                        X := X@.Next ;
                      end ;

                    B := 0 ;
                    X := S2@.FirstTran ;
                    while X <> NIL do
                      begin
                        B := B+1 ;
                        X := X@.Next ;
                      end ;
                    if A =B
                    then
                      begin
                        X := S1@.FirstTran ;
                        Match := TRUE ;
                        while (X <> NIL) and Match do
                          begin
                            Match := FALSE ;
                            X2 := S2@.FirstTran ;
                            while (X2 <> NIL) and not Match do
                              begin
                                if ConMatch(X,X2) then Match := TRUE ;
                                X2 := X2@.Next
                              end ;
                            X := X@.Next
                          end ;
                        StateMatch := Match
                      end
                  end   � StateMatch �   ;



              begin
                � FindS �
                if Trace
                then
                  begin
                    writeln(TTYOut,'FindS');Break
                  end;
                Found := FALSE ;
                Key   := CHash(S) ;
                Key := Key mod STATETABSIZE ;
                X     := StateTab [Key ] ;
                while (X <> NIL) and (not Found) do
                  begin
                    if StateMatch(X,S)
                    then
                      begin
                        Found := TRUE ;
                        S     := X
                      end ;
                    X := X@.Nic
                  end
              end    � FindS  � ;





            procedure Links(From : stateptrty ;
                On   : tranptrty;
                Dest   : stateptrty  ) ;
                �**********************************************************�
                �*                                                        *�
                �*  Links                                                 *�
                �*                                                        *�
                �*    THIS procedure Links THE From and to StateS         *�
                �* BOTHWAYS                                               *�
                �*                                                        *�
                �*    in :     From  State                                *�
                �*               Dest  State                              *�
                �*               On  TranSITIOn                           *�
                �*                                                        *�
                �**********************************************************�

            var
                X,Y,Z   : tranptrty ;



              begin
                � Links �
                On@.NextState := Dest ;
                new(X)    ;
                with X@ do
                  begin
                    Next := NIL ;
                    MarkE := FALSE;
                    Pos  := On@.Pos ;
                    NextState := From ;
                    Rule   := On@.Rule
                  end ;
                Y := Dest@.Back ;
                if Y= NIL then Dest@.Back := X
                else
                  begin
                    while Y <> NIL do
                      begin
                        Z := Y ;
                        Y := Y@.Next
                      end;
                    Z@.Next := X
                  end

              end   � Links �   ;


            procedure EnterS ( From:stateptrty; On:tranptrty;Dest:stateptrty);
                �***************************************************�
                �*                                                 *�
                �*  EnterS                                         *�
                �*                                                 *�
                �*   THIS procedure EnterS A new State and then    *�
                �* Links THE From and Dest StateS                  *�
                �*                                                 *�
                �*  I   : From      State                          *�
                �*          Dest      State                        *�
                �*                                                 *�
                �*          On      TranSITIOn                     *�
                �*                                                 *�
                �***************************************************�

            var
                Key  :  integer ;
                X,Z  : stateptrty  ;



              begin
                � EnterS �
                if Trace
                then
                  begin
                    writeln(TTYOut,'EnterS'); Break
                  end;
                Key := CHash(Dest)  ;
                Key := Key  mod STATETABSIZE ;
                if CurrentState >= MAXSTATE then
                  begin
                    writeln(TTYOut,'toO MANY StateS');
                    CurrentState := MAXSTATE ;
                  end
                else                 CurrentState := CurrentState+1 ;
                NState [ CurrentState ] := Dest ;
                Dest@.State := CurrentState ;
                if StateTab [Key] = NIL then StateTab[Key] := Dest
                else
                  begin
                    X := StateTab [Key ] ;
                    while X <> NIL do
                      begin
                        Z := X ;
                        X := X@.Nic
                      end ;
                    Z@.Nic := Dest
                  end ;
                Links( From,On,Dest)
              end     �EnterS �   ;


          begin
            � GenS  �
            if Trace then
              begin
                writeln(TTYOut,'GenS');
                Break
              end ;
            if Debug then if FSM then PrintState(S) ;
            X := NextTran (S,NIL) ;
            while X <> NIL do
              begin
                Z := X@.Rule ;
                for I := 0 to X@.Pos
                do  Z := Z@.Next ;
                if Z <> NIL then
                  begin
                    mark(Heap) ;
                    Y := Complete (S,X) ;
                    FindS(Y,B) ;
                    if B then
                      begin
                        release(Heap) ; � NO new State WAS NEEDED �
                        Links (S,X,Y)
                      end
                    else
                      begin
                        EnterS(S,X,Y) ;
                        if Y@.Kind =Inadequate then
                          begin
                            AddL(Y) ;
                            if AllReduce then Y@.Kind:= Reduction ;
                          end ;
                        if Y@.Kind <> Reduction then
                        GenS(Y,Lr0)
                      end ;
                  end ;
                X := NextTran(S,X)
              end
          end    � GenS   �   ;





      begin
        if Trace then
          begin
            writeln(TTYOut, 'CFSM') ;
            Break
          end ;
        Lr0 := TRUE ;
        S   := CIS ; � CREATE inITIAL State �
        GenS(S,Lr0) ;
        if Debug then if IFSM then
          begin
            Sl1 := Inadeq ;
            while Sl1 <> NIL do
              begin
                S1 := Sl1@.CAR ;
                PrintState(S1);
                Sl1 := Sl1@.CDR ;
              end ;
          end ;
      end   � CFSM � ;




    procedure LaLr ( K: integer ; LrK : boolean ) ;
        �************************************************�
        �*                                              *�
        �*  LaLr                                        *�
        �*                                              *�
        �*   THIS procedure ATTEMPTS to ResOLVE         *�
        �* ALL Inadequate StateS, USinG A               *�
        �* MAXIMUM of K Lookahead. IT SETS              *�
        �* LrK to FALSE if THIS IS not PosSIBLE         *�
        �* for THE GIVEN K                              *�
        �*                                              *�
        �*                                              *�
        �*    GLOBAL       in:                          *�
        �*                                              *�
        �*                                              *�
        �*                                              *�
        �*               OUT :                          *�
        �************************************************�
    var
        Y : stateptrty ;
        I : integer;
        X,Z : statelistptr ;




        function Disjoint( X,Y :stateptrty) : boolean ;
            �*******************************************************�
            �*                                                     *�
            �*  Disjoint                                           *�
            �*                                                     *�
            �*     THIS function ChECKS to SEE if THE Lookahead    *�
            �* String of Y IS SUFFICIENt to ResOLVE State X        *�
            �* if SO IT RETURNS TRUE                               *�
            �*                                                     *�
            �*     in     :   X     Inadequate State               *�
            �*                Y     Lookahead String State         *�
            �*                                                     *�
            �*     ResULT      TRUE if Disjoint else FALSE         *�
            �*******************************************************�

        var
            Check : boolean ;
            Id,Id2 : identptrty ;
            S  : stateptrty ;
            Q,R,T : tranptrty;



          begin
            � Disjoint �
            Disjoint := TRUE ;
            Check := TRUE ;
            T := Y@.FirstTran ;
            while ( T <> NIL) and Check do
              begin
                Id := Token(T@.Rule,T@.Pos);
                Q := X@.FirstTran ;
                while (Q <> NIL) and Check do
                  begin
                    Id2 := Token(Q@.Rule,Q@.Pos);
                    if Id2<> NIL then
                      begin
                        if Id2=Id then Check := FALSE;
                      end

                    else
                      begin
                        S := Q@.NextState ;
                        if (S<> NIL) and(S<>Y)
                        then
                          begin
                            R := S@.FirstTran ;
                            while (R<> NIL) and Check do
                              begin
                                if Token(R@.Rule,R@.Pos)=Id then
                                    Check := FALSE;
                                R := R@.Next ;
                              end ;
                          end ;
                      end ;
                    Q := Q@.Next ;
                  end ;
                T := T@.Next ;
              end ;
            Disjoint := Check ;
          end   � Disjoint  �  ;


        procedure Follow(A : stateptrty;B :tranptrty;C :stateptrty) ;forward;

        procedure  Resolve( Y : stateptrty ;  var B : boolean) ;

            �****************************************************�
            �*                                                   *�
            �*  Resolve                                          *�
            �*                                                   *�
            �*   THIS procedure ResolveS ANY InadeqUACIES        *�
            �* IT IS CALLED for EACh Inadequate State            *�
            �* if UNABLE to Resolve A State IT SETS B to FALSE   *�
            �*                                                   *�
            �*    in    Y   GIVEN State                          *�
            �*                                                   *�
            �*    OUT   B   set to FALSE if UNABLE to Resolve    *�
            �*                                                   *�
            �*****************************************************�

        var
            Id : identptrty ;
            X,Z : statelistptr ;
            I   : integer ;
            T,U : tranptrty  ;
            Q,S : stateptrty ;



          begin
            � Resolve �
            if Trace then
              begin
                writeln(TTYOut,'Resolve');Break ;
              end ;
            T := Y@.FirstTran ;
            while T <> NIL do
              begin
                Id := Token( T@.Rule,T@.Pos) ;
                if Id = NIL then
                  begin
                    new(S) ;
                    with S@ do
                      begin
                        FirstTran := NIL ;
                        Back := NIL ;
                        Nic  := NIL ;
                        CurrentState := CurrentState+1 ;
                        if CurrentState > MAXSTATE then
                          begin
                            writeln(TTYOut,' Too many states.') ;
                            CurrentState := MAXSTATE ;
                          end  ;
                        State := CurrentState ;
                        Kind := LAString ;
                      end ;
                    NState [ CurrentState ] := S ;
                    CurrentVisit := 0;
                    Follow (Y,T,S) ;
                    T@.NextState := S ;
                    if CurrentVisit >0 then
                    for I := 1 to CurrentVisit do
                      begin
                        Visit[I]@.MarkE := FALSE ;
                        Visit [I] := NIL ;
                      end ;
                    CurrentVisit := 0;
                    if Debug and IFSM then
                      begin
                        writeln(TTYOut); writeln(TTYOut);
                        writeln(TTYOut,'Inadequate State ') ;
                        writeln(TTYOut);
                        PrintState(Y) ;
                        writeln(TTYOut);
                        writeln(TTYOut,'Lookahead String') ;
                        PrintState(Y) ;
                      end ;
                    if Disjoint (Y,S) then Y@.Kind := Lookahead
                    else
                      begin
                        Y@.Kind := Inadequate ; � Just in case it was set �
                                                � otherwise previously    �
                        B := FALSE ;
                        Z := Conflict ;
                        new(X) ;
                        X@.CAR := S ;
                        X@.CDR := NIL ;
                        if Z=NIL then Conflict := X
                        else
                          begin
                            while Z@.CDR <> NIL do
                            Z := Z@.CDR ;
                            Z@.CDR := X ;
                          end ;
                      end ;
                  end ;
                T := T@.Next ;
              end ;
          end � Resolve   � ;




        procedure Reverse (A:stateptrty; B :tranptrty;N:integer ;
            S : stateptrty )   ;
            �************************************************�
            �*                                               *�
            �* Reverse                                       *�
            �*                                               *�
            �*    THIS procedure EXECUTES THE Reduction and  *�
            �* MAKES THE APPrOPrIATE TranSITIOn              *�
            �*                                               *�
            �*   in    A   BASE State                        *�
            �*         B   Reduction                         *�
            �*         N   Number of BackUPS to GO           *�
            �*         S   LookSHead String State            *�
            �*                                               *�
            �*************************************************�

        var
            Id : identptrty ;
            A1 : stateptrty ;
            T : tranptrty ;


            function MakeTran( A:stateptrty; B :tranptrty ) : stateptrty ;
                �******************************************************�
                �*                                                    *�
                �* MakeTran                                           *�
                �*                                                    *�
                �*    THIS function MAKES THE APPrOPrIATE TranSITIOn  *�
                �* for THE GIVEN Reduction B and RETURNS THE ResULTANt*�
                �* State                                              *�
                �*                                                    *�
                �*   in     A State                                   *�
                �*          B GIVEN Reduction                         *�
                �*                                                    *�
                �*  ResULT  IS THE DestinATIOn State                  *�
                �*                                                    *�
                �******************************************************�

            label
                1;
            var
                X : tranptrty ;
                new : stateptrty ;
                Id : identptrty ;



              begin
                � MakeTran �
                new := NIL ;
                Id := B@.Rule@.XDef ;
                X := A@.FirstTran ;
                while X <> NIL do
                  begin
                    if Token(X@.Rule,X@.Pos)= Id
                    then
                      begin
                        new := X@.NextState ;
                        if new <> NIL then goto 1 ;
                      end ;
                    X := X@.Next ;
                  end ;
                1:
                MakeTran := new ;
              end � MakeTran �   ;


          begin
            � Reverse  �
            if Trace then
              begin
                writeln(TTYOut,'Reverse');Break ;
              end ;
            if N=0 then
              begin
                A1 := MakeTran(A,B) ;
                if A1 = NIL then writeln(TTYOut,'**SySTEM Error** in Reverse')
                else
                  begin
                    T := A1@.FirstTran ;
                    while T <> NIL do
                      begin
�suspect code:           Id := Token(T@.Rule,T@.Pos);           �
�                        if Id@.Kind <> NonTerm then             �
                        Follow(A1,T,S) ;
                        T := T@.Next ;
                      end ;
                  end
              end
            else
              begin
                T := A@.Back ;
                while T <> NIL do
                  begin
                    A1 := T@.NextState ;
                    Reverse(A1,B,N-1,S) ;
                    T := T@.Next ;
                  end ;
              end ;
          end � Reverse   �  ;


        procedure Follow ;
            �***************************************************�
            �*                                                 *�
            �* Follow                                          *�
            �*                                                 *�
            �* THIS IS A forward procedure                     *�
            �*      THIS procedure inITIALIZES C with THE      *�
            �* Terminals THAT Follow                           *�
            �*                                                 *�
            �*   in   : A   BASE State                         *�
            �*          B    GIVEN TranSITIOn                  *�
            �*          C   Lookahead String State             *�
            �*                                                 *�
            �***************************************************�

        var
            Id  : identptrty ;
            X   : stateptrty ;
            P  : prodptrty ;
            Pop : integer   ;
            T   : tranptrty ;



            procedure AddLook ( S: stateptrty;Q : tranptrty);
                �*******************************************************�
                �*                                                     *�
                �*  AddLook                                            *�
                �*                                                     *�
                �*      THIS procedure ADDS THE GIVEN TranSITIOn       *�
                �* to THE EXISTinG Lookahead String AS REPresENtED BY  *�
                �*   S                                                 *�
                �*                                                     *�
                �*   in      S   Lookahead String State                *�
                �*           Q   new TranSITIOn                        *�
                �*                                                     *�
                �*******************************************************�

            var
                X : tranptrty ;

              begin
                � AddLook �
                new(X)   ;
                with X@ do
                  begin
                    Rule := Q@.Rule ;
                    Pos  := Q@.Pos  ;
                    MarkE := FALSE  ;
                    Next  := NIL  ;
                    NextState := NIL ;
                  end ;
                AddT(S,X)  ;
              end � AddLook   �  ;



          begin
            � Follow �
            if Trace then
              begin
                writeln(TTYOut,'Follow');
                Break ;
              end ;
            Id := Token(B@.Rule,B@.Pos) ;
            if not B@.MarkE then
              begin
                B@.MarkE := TRUE ;
                CurrentVisit := CurrentVisit+1 ;
                if CurrentVisit > MAXVISIT then
                writeln(TTYOut,'OVERFLOW of Visit StateS')
                else Visit[CurrentVisit] := B ;
                if Id = NIL then
                  begin
                    Pop := 0 ;
                    P := B@.Rule ;
                    P := P@.Next ;
                    while P <> NIL
                    do
                      begin
                        Pop := Pop +1 ;
                        P := P@.Next ;
                      end ;
                    if B@.NextState = NIL then Reverse(A,B,Pop,C)
                    else
                      begin
                        X := B@.NextState ;
                        T := X@.FirstTran ;
                        while T <> NIL do
                          begin
                            AddLook(C,T) ;
                            T := T@.Next ;
                          end ;
                      end ;
                  end
                else
                if Id@.Kind = Term then AddLook(C,B) ;
              end ;
          end � Follow �   ;


      begin
        �  LaLr  �
        if Trace then
          begin
            writeln(TTYOut, 'LaLr') ;
            Break ;
          end ;
        Z := Inadeq ;
        X := Inadeq ;
        LrK := TRUE ;
        while X <> NIL do
          begin
            Y := X@.CAR ;
            Resolve(Y,LrK) ;
            if Y@.Kind = Lookahead
            then if X= Inadeq then Inadeq := X@.CDR
            else Z@.CDR := X@.CDR
            else Z := X ;
            X := X@.CDR ;
          end ;
        if not LrK then
          begin
            writeln(TTYOut);
            writeln(TTYOut,'The grammar is not laLR(',K:1,')');
            if IFSM then
              begin
                I := B14 ;
                Ch := ChR(I) ;
                writeln(CrefPas,Ch) ;
                writeln(CrefPas,'                      Conflict States');
                writeln(CrefPas,'                      ===============');
                writeln(CrefPas);writeln(CrefPas);writeln(CrefPas);
                X := Inadeq ;
                while X <> NIL do
                  begin
                    PrintState(X@.CAR) ;
                    X := X@.CDR ;
                  end ;
                X := Conflict ;
                while X <> NIL do
                  begin
                    PrintState(X@.CAR) ;
                    X := X@.CDR ;
                  end ;
              end � IFSM< �  ;
          end � LrK�   ;

      end ;
    �  LaLr  �



    procedure   Optim  ;
        �************************************************�
        �*                                              *�
        �*        Optim              ;                  *�
        �*                                              *�
        �*                                              *�
        �*   THIS procedure OptimizeS THE State Tables  *�
        �*  GENERATED ALSO. THE OptimIZATIOn IS         *�
        �*  doNE USinG FinITE State TEChNIQUES in ordER *�
        �*  to Reduce THE Number of StateS.             *�
        �*    THE FollowinG OptimIZATIOnS ARE doNE      *�
        �*    A. REMOVAL of NOn Terminal TranSITIOnS    *�
        �*    B. REMOVAL of ChAin DEVIATIOnS            *�
        �*                                              *�
        �************************************************�
      begin
        �  Optim  �
        writeln(TTYOut, 'Optim')
      end   ;



    function FindRule(T: tranptrty) : integer ;

        �*****************************************************�
        �*                                                   *�
        �* FindRule                                          *�
        �*                                                   *�
        �*     THIS function RETURNS THE Rule Number of THE  *�
        �* GIVEN Production                                  *�
        �*                                                   *�
        �*     in : T  GIVEN TranSITIOn                      *�
        �*                                                   *�
        �*    OUT :                                          *�
        �*    ResULT   Rule NO. of Production                *�
        �*****************************************************�

    label
        1;
    var
        L :   listptrty ;

      begin
        � FindRule �
        FindRule := UNKNOWN ;
        L := T@.Rule@.XDef@.Defin ;
        if L= NIL  then writeln(TTYOut,'**SySTEM Error 2**') ;
        while L<> NIL do
          begin
            if RuleTab[L@.CAR]=T@.Rule then
              begin
                FindRule := L@.CAR ;
                goto 1
              end ;
            L := L@.CDR ;
          end ;
        1:
      end � FindRule  �  ;



    procedure   OutT     ;
        �************************************************�
        �*                                              *�
        �*     OutT                                     *�
        �*                                              *�
        �*   THIS procedure GENERATES THE PARSER Tables,*�
        �* Production Number Tables and THE SEMANtICS   *�
        �* Tables                                       *�
        �************************************************�



        procedure GenTab ;
            �***********************************************�
            �*                                             *�
            �* GenTab                                      *�
            �*                                             *�
            �*  THIS procedure GENERATES Tables in THE     *�
            �* REQUIRed forM VIZ MERGinG symbolS and THE   *�
            �* THE Action list WHEREVER PosSIBLE           *�
            �*                                             *�
            �***********************************************�
        label
            1;
        var
            I,J  : integer ;
            S : stateptrty ;
            Entry : integer ;
            Done : boolean ;




            procedure EnterSeg(T,A : integer) ;

                �************************************************�
                �*                                              *�
                �* EnterSeg                                     *�
                �*                                              *�
                �*   THIS procedure EnterS A symbol Action PAIR *�
                �* into THE Table                               *�
                �************************************************�



              begin
                � EnterSeg �
                if Index >= MAXTABSIZE then writeln(TTYOut,'Overflow of Table')
                else Index := Index+1 ;
                Table [Index].Tran := T;
                Table [Index].Action := A ;
              end � EnterSeg �   ;



            procedure MakeSeg (S :stateptrty) ;
                �*************************************************�
                �*                                               *�
                �* MakeSeg                                       *�
                �*                                               *�
                �*  THIS procedure COPIES A Segment I.E. A State *�
                �* and forMATS into A symbol Action PAIR         *�
                �*                                               *�
                �*************************************************�
            label
                1;
            var
                Act : integer ;
                Id : identptrty ;
                First : boolean ;
                K,LastCount,CurrCount : integer ;
                P : integer ;
                Last,T : tranptrty ;






                function Count (S :stateptrty) : integer ;

                    �*********************************************�
                    �*                                           *�
                    �* Count                                     *�
                    �*                                           *�
                    �*   THIS function RETURNS THE Number of     *�
                    �* UNIQUE TranSITIOnS of A GIVEN State       *�
                    �*********************************************�

                var
                    Temp : integer ;
                    Id : identptrty ;
                    Q,T : tranptrty ;
                  begin
                    � Count �
                    Temp := 0;
                    T := NextTran(S,NIL);
                    while T <> NIL do
                      begin
                        Id := Token(T@.Rule,T@.Pos);
                        Q := T@.Next ;
                        while Q <> NIL do
                          begin
                            if Token(Q@.Rule,Q@.Pos)=Id then Q@.MarkE := TRUE;
                            Q := Q@.Next ;
                          end ;
                        Temp := Temp +1 ;
                        T := NextTran(S,T) ;
                      end ;
                    Count := Temp ;
                  end � Count � ;



                procedure Expand ( P : tranptrty) ;
                    �************************************************�
                    �*                                              *�
                    �* Expand                                       *�
                    �*                                              *�
                    �*    THIS procedure EXOandS A Lookahead String *�
                    �* A symbol Action list                         *�
                    �************************************************�

                var
                    S : stateptrty ;
                    PRule : integer ;
                    T,Q : tranptrty ;
                    Id : identptrty ;
                  begin
                    S := P@.NextState ;
                    T := NextTran (S,NIL) ;
                    PRule := P@.Rule@.Production ;
                    while T <> NIL do
                      begin
                        Id := Token (T@.Rule ,T@.Pos) ;
                        Q := T@.Next ;
                        while Q <> NIL do
                          begin
                            if Token(Q@.Rule,Q@.Pos)=Id then Q@.MarkE := TRUE;
                            Q := Q@.Next ;
                          end ;
                        EnterSeg(Id@.Code,-PRule-NOSCANCODE) ;
                        T := NextTran(S,T) ;
                      end ;
                  end � Expand �  ;



              begin
                � MakeSeg �
                First := TRUE ;
                Segment[0].Tran := UNKNOWN ;
                Segment[0].Action := UNKNOWN ;
                P := UNKNOWN ;

                SegIndex := -1 ;
                T :=S@.FirstTran ;
                while T <> NIL do
                  begin
                    �while�
                    if T@.NextState <> NIL then
                      begin
                        �COPY�
                        Id := Token(T@.Rule,T@.Pos) ;
                        if Id <> NIL then K := Id@.Code else K:=UNKNOWN;
                        if T@.NextState@.Kind=LAString then
                          begin
                            �LAString�
                            if First then
                              begin
                                �First�
                                First := FALSE;
                                Last := T ;
                                LastCount := Count(Last@.NextState) ;
                                goto 1 ;
                              end �First�
                            else
                              begin
                                �not First�
                                CurrCount := Count(T@.NextState);
                                if CurrCount > LastCount+THRESHOLD then
                                  begin
                                    �SWAP�
                                    Expand(Last) ;
                                    Last := T ;
                                    LastCount := CurrCount ;
                                  end �SWAP�
                                else Expand(T) ;
                                goto 1 ;
                              end �not First�
                          end � LAString�
                        else if T@.NextState@.Kind=Reduction then
                        P := -T@.Rule@.Production
                        else
                        P := T@.NextState@.State ;
                        if SegIndex >= MAXSEGMENT then
                            writeln(TTYOut,'Segment Overflow!')
                        else SegIndex := SegIndex+1;
                        Segment[SegIndex].Tran := K ;
                        Segment[SegIndex].Action := P ;
                      end �COPY � ;
                    1: T := T@.Next ;
                  end �while�  ;
                if S@.Kind = Lookahead then
                if First then writeln(TTYOut,'System error in MAKESEG2')
                         else Act := - Last@.Rule@.Production-NOSCANCODE ;

                if S@.Kind= Reads then Act := ERRORCODE;
                if (S@.Kind=Reads) or (S@.Kind=Lookahead)
                then
                  begin
                    if SegIndex>= MAXSEGMENT then
                        writeln(TTYOut,'*Segment Overflow!')
                    else SegIndex := SegIndex+1 ;
                    Segment[SegIndex].Tran := elseCode;
                    Segment[SegIndex].Action := Act ;
                  end ;
              end �MakeSeg�  ;






            function SegMatch(Ind:integer) : integer ;
                �*****************************************************�
                �*                                                   *�
                �*  SegMatch                                         *�
                �*                                                   *�
                �*     THIS function RETURNS TRUE if THE Segment     *�
                �* MatchES AN Entry in THE Table .if THERE IS NO     *�
                �* Match IT RETURNS UNKNOWN                          *�
                �*****************************************************�
            label
                1,2;
            var
                I,K : integer ;
                Mate : integer ;



              begin
                � SegMatch�
                SegMatch := UNKNOWN ;
                if Index > (SegIndex-Ind) then
                for Mate := 0 to Index-(SegIndex-Ind) do
                  begin
                    K := Mate ;
                    for I := Ind to SegIndex do
                      begin
                       if Table[K].Tran=XCONTINUE
                           then K := Table[K].Action;
                        if (Segment[I].Tran <> Table[K].Tran)
                        or (Segment[I].Action <> Table[K].Action) then goto 1;
                        K := K+1;
                      end ;
                    SegMatch := Mate ; goto 2;
                    1:
                  end � for � ;
                2:
              end  � SegMatch �  ;








          begin
            � GenTab �
            Index := -1 ;
            for I := 0 to CurrentState do
              begin
                S := NState [I ];
                if (S@.Kind <> Reduction) and (S@.Kind <> LAString) then
                  begin
                        NewState[I] := Index+1 ;
                    MakeSeg(S) ;
                    Entry := SegMatch(0);
                    if Entry <> UNKNOWN then NewState[I] := Entry
                    else
                      begin
                        EnterSeg(Segment[0].Tran,Segment[0].Action) ;
                        if SegIndex > 0 then

                        begin
                        for J := 1 to SegIndex-1 do
                          begin
                            Entry := SegMatch(J);
                            if Entry <> UNKNOWN then
                              begin
                                EnterSeg(XCONTINUE,Entry);
                                goto 1 ;
                              end
                            else EnterSeg(Segment[J].Tran,Segment[J].Action);
                          end ;
                        EnterSeg(Segment[SegIndex].Tran,
                                 Segment[SegIndex].Action);
                        end ;
                        1:
                      end ;
                  end ;
              end  � for � ;
            � RENumber new StateS �
            for I := 0 to CurrentState do
            if NState[I]@.Kind <> LAString then
            NState[I] @.State := NewState[I] ;
            for I := 0 to Index do
            if (Table[I].Action >=0) and (Table[I].Action <=CurrentState)
            and (Table[I].Tran <> XCONTINUE) then
            Table[I].Action := NewState[Table[I].Action ];
          end � GenTab �   ;



        procedure OutTab ;
        �***********************************************�
        �*                                             *�
        �* OutTab                                      *�
        �*                                             *�
        �*    THIS procedure GENERATES TabPas          *�
        �* THE Tables                                  *�
        �***********************************************�

var
    I,K,T,A : integer ;
    DoSymbolic : boolean;
    X : identptrty;
    Id : word;



 begin  � OutTab �
        I := B14;
        Ch := ChR(I);
        writeln(TabPas,Ch);
        writeln(TabPas,XCONTINUE,' ',elseCode,' ',ERRORCODE,' ',NOSCANCODE);
        writeln(TabPas);

        writeln(TabPas,ERRORCODE);
        writeln(TabPas,MaxCode);
        writeln(TabPas,MaxPop);
        writeln(TabPas,MaxProdNo);
        writeln(TabPas,Index);
        writeln(TabPas,elseCode);
        writeln(TabPas,-1); �PARTITION�
        writeln(TabPas);
        writeln(TabPas);

                � GENERATE labelS�

        for K := 0 to NAMETABSIZE do
        begin
                X := NameTab[K];
                while X<> NIL do
                begin
                   if X@.Kind in  [Lab]
                        then writeln(TabPas,X@.ExternalName);
                   X := X@.Nic;
                end ;
        end ;
        writeln(TabPas,-1) ; � PART�
        writeln(TabPas);writeln(TabPas);writeln(TabPas);


                � GENERATE Table arrayS�

        for I := 0 to Index do
        begin
            T := Table[I].Tran ;
            A := Table[I].Action;
            write(TabPas, I, ' ');
            if (T >= 0) and (T < MaxVocSize) then begin
                DoSymbolic := Symbolic and Terminals and
                                (SymbolName[T]@.Kind = Term);
                DoSymbolic := DoSymbolic or (
                                Symbolic and NonTerminals and
                                (SymbolName[T]@.Kind = NonTerm));
                DoSymbolic := DoSymbolic or (SymbolName[T]@.Spid = Res);
            end else
                DoSymbolic := FALSE;
            if DoSymbolic then
                write(TabPas, SymbolName[T]@.ExternalName)
            else
                write(TabPas, T);
            writeln(TabPas, ' ', A);
        end ;
        writeln(TabPas,-1);
        writeln(TabPas);

                � GENERATE Pop and SEMANtICS�

        DoSymbolic := Symbolic and NonTerminals;

        for I := 1 to ProdNo do
        begin

            T := PopTab[I] -1;
            A := RuleTab[I]@.XDef@.Code;
            X := RuleTab[I]@.Action;
            if X = NIL then Id := 'NULL           '
                else Id :=X@.ExternalName;
            write(TabPas,I,' ', T, ' ');
            if DoSymbolic then
                write(TabPas, RuleTab[i]@.XDef@.ExternalName)
            else
                write(TabPas, A);
            writeln(TabPas,' ',Id);
        end ;

        writeln(TabPas,-1) ; � PART�

        writeln(TabPas);
   end �OutTab�  ;







      begin
        �  OutT  �
        writeln(TTYOut,' Generating Tables...') ;
        GenTab ;
        if FSM or tables then    � always want tables �
        PrintTab ;   � even when you don't want the to see the FSM -hde '83�
        OutTab;
      end   ;



    procedure   OutFreq          ;
        �************************************************�
        �*                                              *�
        �*    OutFreq                                   *�
        �*                                              *�
        �*     THIS procedure OUTPUTS ALL STATISTICS    *�
        �*  ConCERNinG THE PrOGRAM, to FILNAM.CRL       *�
        �*     THE FollowinG WILL BE inCLUDED           *�
        �*   1. Frequency of Terminals, NOn Terminals.  *�
        �************************************************�
      begin
        �  OutFreq  �
        writeln(TTYOut,'OutFreq')
      end   ;



  begin
    �  main  �
    InitSp;           � initialize some data �
    InitOptions ;     �initialize options to defaults �
    InitGlob;         �initialize global variables �
    mark(HeapBot) ;
    open( TTYOut, 'TTY:', [ OpenOutput ] );
    InitP          ;  �  Initialize tables as read. �
    if not FatalError then
        Terminal       ;  �  Read terminals �
    if not FatalError then begin
        ReadG          ;  �  Read grammar �
        writeln(TTYOut,' There are ', maxprodno:2,' rules.');
    end;
    if Grammar or Terminals and not FatalError then
      begin
        if Grammar then  PrintG ;
        PrintCref ;
      end ;
    if ErrorFlag or FatalError then
        writeln(TTYOut,'Error in the productions - NO tables generated.')
    else
    if Lr0 then
      begin
        � Lr0 �
        CFSM(Lr0) ;
        if (not Lr0) and Look
        then
          begin
            LrK := TRUE ;
            LaLr(MaxK,LrK) ;
          end ;
        if LrK or Lr0 then
          begin
            if Optimize then Optim ;
            if Tables or PN then OutT ;
          end ;
        if FSM then OutFSM ;
      end   � Lr0 � ;
    if Frequency then  OutFreq ;
    mark(HeapTop) ;
�   writeln(TTYOut,'Storage used: ',HeapBot-HeapTop : 5) ;         �
� the above line is commented out because pointers aren't integers
    (in PASCALVS), and also, it is not TRUE since other marks and
    releases are done in the Rest of the code
�
    if FatalError then
        SetReturnCode(2)
    else if ErrorFlag then
        SetReturnCode(1);

  end.

