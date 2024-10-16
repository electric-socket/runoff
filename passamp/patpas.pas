(*$M-*)
program patpas;
�
    This program is a backend to PAT, and constructs the pascal file
    implementing the grammar specified to PAT.

    We read two files, FILE.tab and FILE.patpas.  The .tab file is one
    of the PAT output files.  The patpas file contains bits and pieces
    of what the user wants in the final pascal file, including the
    semantic actions.

    We also attempt to read FILE.ter, and create CONSTs to define the
    output of the INSYMBOL procedure symbolically.
�

const
    SYMBOLSIZE = 32;
    BUFFERSIZE = 200;
    DEBUGMAX = 4001;    � number of TERM/NONTERM symbols we can save �
    OURFLAG = '� This is a flag that this file was created by PATPAS �';
    MAXNUMBEROFSTATEMENTS = 100; � This is the number of statements
              we allow in InitProcedure's before seperating them up �

type
    symbol = packed array [1..SYMBOLSIZE] of char;

    TheirStates = ( Initializing,
                    GlobalConst,
                                 DefSymbolSize, DefSymbol,
                                 GlobalType, GlobalVar, GlobalInit,
                    InSymbol,
                    LocalConst, LocalType, LocalVar, LocalInit,
                    Action, Main,
                    Finished );

var
    DoDebug,                        � output Debugging code? �
    TerExists,                      � does foo.TER exist? �
    HadError    : boolean;

    TTYout,
    Us,
    Ter,
    Them    : text;

    TheirBuffer : packed array [1..BUFFERSIZE] of char;

    TheirState : TheirStates;

    FileName : packed array [1..16] of char;

    DebugTable : packed array [1..DEBUGMAX] of symbol; � symbolic names �
    DebugLowest,                � lowest entry in table used, or 0 �
    DebugHighest : integer;     � highest entry in table used, or 1 �

    LowerCaseLetters,
    WhiteSpace : set of char;


� The following are the procedures that are system specific.
    They will need to be changed to run on other systems.
 �

procedure InitProcedure;
var
    i,
    retcode : integer;
%include CMS
begin
    TheirState := Initializing;

    LowerCaseLetters := [ 'a', 'b', 'c', 'd', 'e', 'f',
                    'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p',
                    'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z' ];
    WhiteSpace := [ ' ', chr(12), chr(9) ];

    DoDebug := TRUE;

    for i := 1 to DEBUGMAX do
        DebugTable[i] := ' ';

    � open the terminal for error output �
    cms( 'FILEDEF STDERR TERMINAL', retcode );  � bind STDERR to tty �
    rewrite( TTYout, 'DDNAME=STDERR' );
end;


procedure GetFileName;
var
    i : integer;
begin
    i := 1;
    token( i, parms, filename );
end;


procedure OpenFiles;
var
    BaseName,
    TabName,
    STabName,
    PatpasName,
    SPatpasName,
    TerName,
    STerName,
    PascalName,
    SPascalName  :  string(30);

    Temporary : packed array [1..200] of char;

function DoesExist( const Name : string ) : boolean;

%include CMS

var
    retcode : integer;
begin
    cms( 'SET CMSTYPE HT', retcode );   � turn off error messages �

    cms( 'STATE ' �� Name, retcode );   � does file exist? �

    if retcode = 0 then
        DoesExist := TRUE
    else
        DoesExist := FALSE;

    cms( 'SET CMSTYPE RT', retcode );   � turn back on error messages �
end;


begin
    BaseName := trim(ltrim(compress(str(FileName))));
    TabName := BaseName �� '.TAB.*';
    STabName := BaseName �� ' TAB *';
    PatpasName := BaseName �� '.PATPAS.*';
    SPatpasName := BaseName �� ' PATPAS *';
    TerName := BaseName �� '.TER.*';
    STerName := BaseName �� ' TER *';
    PascalName := BaseName �� '.PASCAL.A';
    SPascalName := BaseName �� ' PASCAL A';

    if DoesExist(STabName) then begin

        reset( Us, 'NAME=' �� TabName );

        if DoesExist(SPatpasName) then
            reset( Them, 'NAME=' �� PatpasName )
        else
            TheirState := Finished;

        TerExists := DoesExist(STerName);
        if TerExists then
            reset( Ter, 'NAME=' �� TerName );

        if DoesExist(SPascalName) then begin
            reset( output, 'NAME=' �� PascalName );
            readln( output, temporary );
            if temporary <> OURFLAG then begin
                writeln( TTYout, SPascalName, ' already exists, and does' );
                writeln( TTYout, 'not appear to have been created by PATPAS.' );
                writeln( TTYout, 'No action taken.' );
                HadError := TRUE;
            end
        end;

        if not HadError then
            rewrite( output, 'NAME=' �� PascalName );

    end else begin
        writeln( TTYout, STabName, ' does not exist.  No action taken.' );
        HadError := TRUE;
    end;
end;


procedure SetReturnCode( I : integer );
� In CMS, error code is propagatable back to caller, so let
    them know...
�
begin
    retcode(i);     � CMS Pascal/VS primitive �
end;


� IsEqual - does a case independant compare of the
            two symbols
�
function IsEqual( Field1, Field2 : symbol ) : boolean;
var
    i : integer;

function UpperCase( Ch : char ) : char;
begin
    if ch in LowerCaseLetters then
        UpperCase := chr( ord(ch) + ord('A')-ord('a') )
    else
        UpperCase := Ch;
end;


begin
    IsEqual := TRUE;
    if Field1 <> Field2 then begin
        for i := 1 to SYMBOLSIZE do begin
            if Field1[i] <> Field2[i] then
                if UpperCase(Field1[i]) <> UpperCase(Field2[i]) then
                    IsEqual := FALSE;
        end;
    end;
end;

function GetOurNumber : integer;
var
    Found : boolean;
    Result : integer;
begin
    Found := FALSE;

    while not ( Found or eof(Us) ) do
        if eoln(Us) then
            readln(Us)
        else if Us@ in WhiteSpace then
            get(Us)
        else begin
            read( Us, Result );
            Found := True
        end;

    if found then
        GetOurNumber := Result
    else
        GetOurNumber := -1;
end;


function GetSymbol( var AFile : text ) : symbol;
var
    Quoting,                    � true if unbalanced single quote �
    Found : boolean;
    Temp : integer;
    Result : symbol;
begin
    Found := FALSE;

    while not ( Found or eof(AFile) ) do
        if eoln(AFile) then
            readln(AFile)
        else if AFile@ = ' ' then
            get(AFile)
        else begin
            Found := TRUE;
            Temp := 1;
            Quoting := FALSE;
            while ( Temp <= SYMBOLSIZE ) and
                  ((AFile@ <> ' ') or (Quoting)) and
                   not eof(AFile) and
                   not eoln(AFile) do begin
                Result[Temp] := AFile@;
                if AFile@ = '''' then
                    Quoting := not Quoting;
                get(AFile);
                Temp := Temp+1;
            end;
            for Temp := Temp to SYMBOLSIZE do
                Result[Temp] := ' ';
        end;

    if found then
        GetSymbol := Result
    else
        GetSymbol := '-1';
end;


procedure WriteSymbol( OurSymbol : symbol );
var
    k, j : integer;
begin
    k := 0;
    for j := SYMBOLSIZE downto 1 do
        if OurSymbol[j] <> ' ' then
            k := max( j, k );

    if (k <> 0) then
        write(OurSymbol:k);
end;


procedure TheirError;
begin
    writeln( TTYout, 'The following line was not understood:' );
    writeln( TTYout, TheirBuffer );

    HadError := TRUE;
end;



procedure GetTheirLine;
var
    CtlRecord : boolean;
    Index : integer;

procedure SetTheirState;
var
    Result1, Result2 : symbol;

function GetTheirSymbol : symbol;
var
    ResultIndex : integer;
    Result : symbol;
begin � GetTheirSymbol �

    Result := '';

    while ( Index <= BufferSize ) and ( TheirBuffer[Index] = ' ' ) do
        Index := Index + 1;

    ResultIndex := 1;

    while ( ResultIndex <= SymbolSize ) and
          ( TheirBuffer[Index] <> ' ' ) do begin
        Result[Resultindex] := TheirBuffer[Index];
        ResultIndex := ResultIndex+1;
        Index := Index+1;
    end;

    GetTheirSymbol := Result;
end; � GetTheirSymbol �



begin � SetTheirState �
    Index := 3;

    Result1 := GetTheirSymbol;

    if IsEqual( Result1,'GLOBAL' ) or IsEqual( Result1,'LOCAL' ) then
        Result2 := GetTheirSymbol;

    if IsEqual( Result1, 'GLOBAL') then begin
        if IsEqual( Result2, 'CONST') then TheirState := GlobalConst
        else if IsEqual( Result2, 'TYPE') then TheirState := GlobalType
        else if IsEqual( Result2, 'VAR') then TheirState := GlobalVar
        else if IsEqual( Result2, 'INIT') then TheirState := GlobalInit
        else TheirError
    end else if IsEqual( Result1, 'SYMBOLSIZE') then TheirState := DefSymbolSize
    else if IsEqual( Result1, 'SYMBOL') then TheirState := DefSymbol
    else if IsEqual( Result1, 'INSYMBOL') then TheirState := InSymbol
    else if IsEqual( Result1, 'LOCAL') then begin
        if IsEqual( Result2, 'CONST') then TheirState := LocalConst
        else if IsEqual( Result2, 'TYPE') then TheirState := LocalType
        else if IsEqual( Result2, 'VAR') then TheirState := LocalVar
        else if IsEqual( Result2, 'INIT') then TheirState := LocalInit
        else TheirError
    end else if IsEqual( Result1, 'ACTIONS') then TheirState := Action
    else if IsEqual( Result1, 'MAIN') then TheirState := Main
    else TheirError;
end; � SetTheirState �


begin � GetTheirLine �

    if eof( Them ) then
        TheirState := Finished
    else
        repeat begin
            readln( Them, TheirBuffer );
            CtlRecord := (TheirBuffer[1] = '%') and (TheirBuffer[2] = '%');
            if CtlRecord then
                SetTheirState;
        end until (not CtlRecord) or eof(Them);

    if CtlRecord then
        TheirState := Finished;
end; � GetTheirLine �


procedure InitTheirFIle;
begin
    while TheirState = Initializing do
        GetTheirLine
end;

procedure DoTheir( DesiredState : TheirStates );
begin
    while TheirState = DesiredState do begin
        writeln( TheirBuffer );
        GetTheirLine
    end;
end;


procedure DoOurFlag;
begin
    writeln( OURFLAG )      � Try to prevent overwriting a pascal file �
end;


procedure DoOurGlobalConsts;
var
    Equal,
    Name : symbol;
    NameValue,
    Temp : integer;
begin
    write( 'program ' );    � write program heading �

    for Temp := 1 to 16 do
        if Filename[Temp] <> ' ' then
            write( FileName[Temp] );

    writeln( ';' );
    writeln( '�     Global constants used by the parse  �');
    writeln;
    writeln( 'const' );
    writeln;
    writeln( '    ContCode = ', GetOurNumber,
             '; � Code for CONTINUE in SymList �' );
    writeln( '    ElseCode = ', GetOurNumber,
             '; � Code for ELSE in SymList �' );
    writeln( '    ErrorCode = ', GetOurNumber,
             '; � Code for ERROR in Action list �' );
    writeln( '    ScanCode = ', GetOurNumber,
             '; � Code for SCAN in SymList �' );
    writeln;
    writeln( '    MaxAction = ', GetOurNumber,
             '; � Max code used in Action �' );
    writeln( '    MaxNonTerm = ', GetOurNumber,
             '; � Number of symbols �' );
    writeln( '    MaxPop = ', GetOurNumber,
             '; � Maximum number of pops �' );
    writeln( '    MaxProd = ', GetOurNumber,
             '; � Number of productions �' );
    writeln( '    MaxStack = ', '100',
             '; � Depth of parse stack �' );
    writeln( '    MaxState = ', GetOurNumber,
             '; � Highest index of parser tables �' );
    writeln( '    MaxVoc = ', GetOurNumber,
             '; � Size of vocabulary �' );
    writeln;

    DebugHighest := 0;
    DebugLowest := DEBUGMAX+1;
    if TerExists then begin
        while not eof(Ter) do begin
            Name := GetSymbol(Ter);
            Equal := GetSymbol(Ter);
            readln( Ter, NameValue );
            if (NameValue <= DEBUGMAX) and (NameValue > 0) then begin
                DebugTable[NameValue] := Name;
                DebugHighest := max( NameValue, DebugHighest );
                DebugLowest := min( NameValue, DebugLowest );
            end;

            if NameValue <> -1 then begin
                write('    ');
                WriteSymbol(Name);
                writeln('  =  ', NameValue:1, ';');
            end;
        end;
    end;

    DebugLowest := min(DebugHighest, DebugLowest);
    if DoDebug then begin
        writeln;
        writeln;
        writeln('    DEBUGMAX = ', DebugHighest:1, ';');
        writeln('    DEBUGMIN = ', DebugLowest:1 , ';');
        writeln('    DEBUGSIZE = 32;' );
        writeln('    HIGHESTPRINTABLE = 256;' );
    end;


    if GetOurNumber <> -1 then begin
        writeln( TTYout, 'Badly formed .TAB file.' );
        HadError := TRUE;
    end;
end;



procedure DoOurSymbolSize;
begin
    writeln;
    writeln( '    SYMBOLSIZE = 32;           � size of User Symbol �' );
end;


procedure DoOurGlobalTypes;
var
    Token : Symbol;
begin
    writeln;
    writeln;
    writeln( 'type' );
    writeln;
    writeln( '� Labels representing semantic actions �' );
    writeln( '    SemSet = (' );
    writeln( '               ', 'NULL' );
    Token := GetSymbol(Us);
    while Token <> '-1' do begin
        writeln( '                ,  ', Token );
        Token := GetSymbol(Us);
    end;
    writeln( '             );' );

end;



procedure DoOurSymbol;
begin
    writeln;
    writeln( '    symbol = packed array[1..SYMBOLSIZE] of char;' );
end;


procedure DoOurGlobalVars;
begin
writeln;
writeln;
writeln( 'var' );
writeln;
writeln( '    IntValue,             � Value destined for IntStack �' );
writeln( '    Currentstate,         � The state of the Parse machine �' );
writeln( '    SP,                   � Stack Index �' );
writeln( '    Token : integer;      � Code to scanner from parser �'     );
� no IF around this variable - could be set by user program �
writeln( '    Debug,          � Can be set TRUE to do debugging �' );
writeln( '    Match,          � True if a matching transition found �' );
writeln( '    Parse,          � True if parse needs to be continued �' );
writeln( '    Scan : boolean; � True if next symbol needs be scanned �');
writeln( '    SymValue : symbol;    � Value destined for SymStack �' );
writeln;
writeln( '� Arrays used by the Parser �' );
writeln;
writeln( '    Action : array [0 .. MaxState] of integer;' );
writeln( '    SymList : array [0 .. MaxState] of integer;' );
writeln( '    LHS : array [0 .. MaxProd] of integer;' );
writeln( '    Pop : array [0 .. MaxProd] of integer;' );
writeln( '    Semantics : array [0 .. Maxprod] of SemSet;' );
writeln( '    PStack : array [0 .. MaxStack] of integer',
         ';  � Parse Stack �' );
if DoDebug then begin
writeln( '    PATBlanks : packed array [1..50] of char;' );
writeln( '    DebugTable : packed array [DEBUGMIN..DEBUGMAX]' );
writeln( '                 of packed array [1..DEBUGSIZE] of char;' );
writeln( '    Printables : set of char;' );
end; �DoDebug�
writeln( '    SymStack : array [0 .. MaxStack] of symbol',
         ';  � User symbol stack �' );
writeln( '    IntStack : array [0 .. MaxStack] of integer',
         ';  � User integer stack �' );
end;


procedure DoOurGlobalInits;
var
    NumberOfStatements,
    NumberOfInitProcedures,
    Index : integer;
    OurSymbol : symbol;

procedure Statement( var NumberOfStatements, NumberOfInitProcedures
                            : integer );
begin
    NumberOfStatements := NumberOfStatements+1;
    if NumberOfStatements > MAXNUMBEROFSTATEMENTS then begin
        NumberOfStatements := 0;
        if NumberOfInitProcedures <> 0 then
            writeln('end;');
        NumberOfInitProcedures := NumberOfInitProcedures+1;
        writeln;
        writeln;
        writeln;
        writeln('procedure InitProcedure', NumberOfInitProcedures:1, ';' );
        writeln('begin');
    end;
end;



begin
    NumberOfInitProcedures := 0;
    NumberOfStatements := MAXNUMBEROFSTATEMENTS+1;
    Statement(NumberOfStatements, NumberOfInitProcedures);
    Index := GetOurNumber;
    while Index <> -1 do begin
        write( '    SymList[', Index:3, '] := ' );
        OurSymbol := GetSymbol(Us);
        if OurSymbol[1] = '''' then begin
            write('ord(');
            WriteSymbol(OurSymbol);
            write(')');
        end else
            WriteSymbol(OurSymbol);
        writeln( ';    Action[', Index:3, '] := ', GetOurNumber:6, ';' );
        Statement(NumberOfStatements, NumberOfInitProcedures);
        Index := GetOurNumber;
    end;
    writeln;
    Statement(NumberOfStatements, NumberOfInitProcedures);

    Index := GetOurNumber;
    while Index <> -1 do begin

        write  ( '    Pop[', Index:3, '] := ', GetOurNumber:2,
                 ';   LHS[', Index:3, '] := ' );
        WriteSymbol(GetSymbol(Us));
        write  ( ';   Semantics[', Index:3, '] := ' );
        WriteSymbol(GetSymbol(Us));
        writeln( ';' );

        Statement(NumberOfStatements, NumberOfInitProcedures);
        Index := GetOurNumber;
    end;

    if DoDebug then begin
        � initialize the Debug Symbol table �
        writeln( '    PATBlanks := '' '';' );
        for Index := DebugLowest to DebugHighest do begin
            if DebugTable[Index] <> ' ' then begin
                write( '    DebugTable[', Index:1, '] :=  ''' );
                WriteSymbol(DebugTable[Index]);
                writeln( ''';' );
                Statement(NumberOfStatements, NumberOfInitProcedures);
            end;
        end;
        writeln(
'   Printables := [ '' '', ''!'', ''"'', ''#'', ''$'', ''%'', ''&'', '''''''','
);      writeln(
'                   ''('', '')'', ''*'', ''+'', '','', ''-'', ''.'', ''/'','
);      writeln(
'                   ''0'', ''1'', ''2'', ''3'', ''4'', ''5'', ''6'', ''7'','
);      writeln(
'                   ''8'', ''9'', '':'', '';'', ''<'', ''='', ''>'', ''?'','
);      writeln(
'                   ''@'', ''A'', ''B'', ''C'', ''D'', ''E'', ''F'', ''G'','
);      writeln(
'                   ''H'', ''I'', ''J'', ''K'', ''L'', ''M'', ''N'', ''O'','
);      writeln(
'                   ''P'', ''Q'', ''R'', ''S'', ''T'', ''U'', ''V'', ''W'','
);      writeln(
'                   ''X'', ''Y'', ''Z'', ''['', ''�'', '']'', ''^'', ''_'','
);      writeln(
'                   ''`'', ''a'', ''b'', ''c'', ''d'', ''e'', ''f'', ''g'','
);      writeln(
'                   ''h'', ''i'', ''j'', ''k'', ''l'', ''m'', ''n'', ''o'','
);      writeln(
'                   ''p'', ''q'', ''r'', ''s'', ''t'', ''u'', ''v'', ''w'','
);      writeln(
'                   ''x'', ''y'', ''z'', ''�'', ''�'', ''�'', ''~''     ];'
);

    end; �DoDebug�


    writeln( 'end;' );

    writeln;
    writeln;
    writeln;
    writeln('procedure InitProcedure;');
    writeln('begin');

    for Index := 1 to NumberOfInitProcedures do
        writeln('    InitProcedure', Index:1, ';' );

    writeln('    Debug := FALSE;' );
    writeln('end;');
end;



procedure DoOurRoutines;
begin
    writeln;
    writeln;
    writeln(   'function GetIndex( Index : integer ) : integer;' );
    writeln(   'begin' );
    writeln(   '    if Index+sp-1 < MAXSTACK then' );
    writeln(   '        GetIndex := Index+sp-1' );
    writeln(   '    else' );
    writeln(   '        GetIndex := MAXSTACK-1' );
    writeln(   'end;' );
    writeln;
    writeln;
    writeln(   'function GetInt( Index : integer ) : integer;' );
    writeln(   'begin' );
    writeln(   '    if Index+sp-1 < MAXSTACK then' );
    writeln(   '        GetInt := IntStack[Index+sp-1]' );
    writeln(   '    else' );
    writeln(   '        GetInt := IntStack[MAXSTACK-1]' );
    writeln(   'end;' );
    writeln;
    writeln;
    writeln(   'function GetSym( Index : integer ) : symbol;' );
    writeln(   'begin' );
    writeln(   '    if Index+sp-1 < MAXSTACK then' );
    writeln(   '        GetSym := SymStack[Index+sp-1]' );
    writeln(   '    else' );
    writeln(   '        GetSym := SymStack[MAXSTACK-1]' );
    writeln(   'end;' );
end;


procedure DoOurLocalConsts;
begin
writeln( 'procedure LRParse ;' );
writeln( '    �****************************************************�' );
writeln( '    �*                                                  *�' );
writeln( '    �*   laLR Parse                                     *�' );
writeln( '    �*                                                  *�' );
writeln( '    �*      This is the parser driver. It uses the      *�' );
writeln( '    �* the tables generated by PAT. The driver itself   *�' );
writeln( '    �* is language independent. The only portion        *�' );
writeln( '    �* that needs to be entered is the semantics as     *�' );
writeln( '    �* indicated.for more details on the parsing method *�' );
writeln( '    �* see the reference manual for PAT.                *�' );
writeln( '    �*                                                  *�' );
writeln( '    �*                                                  *�' );
writeln( '    �*   Global                                         *�' );
writeln( '    �*     in/out :  SP  stack pointer for parser       *�' );
writeln( '    �*               CurrentState                       *�' );
writeln( '    �*               Scan indicates if next symbol      *�' );
writeln( '    �*                    needs to be scanned           *�' );
writeln( '    �*               Parse  true while parsing          *�' );
writeln( '    �*               Token  for making transition       *�' );
writeln( '    �*               Action     parser table            *�' );
writeln( '    �*               SymList    parser table            *�' );
writeln( '    �*               Pop        parser table            *�' );
writeln( '    �*               LHS        parser table            *�' );
writeln( '    �*               Semantics  parser table            *�' );
writeln( '    �*                                                  *�' );
writeln( '    �*  Functions used                                  *�' );
writeln( '    �*                                                  *�' );
writeln( '    �*     InSymbol  scans the next symbol and must     *�' );
writeln( '    �*               token to the required code         *�' );
writeln( '    �*                                                  *�' );
writeln( '    �*     Error     This routine does error recovery   *�' );
writeln( '    �*         ##### user needs to write this as reqd   *�' );
writeln( '    �*                                                  *�' );
writeln( '    �*     #### other user defined semantic routines    *�' );
writeln( '    �****************************************************�' );
writeln;
writeln;
end;


procedure DoOurLocalTypes;
begin
end;


procedure DoOurLocalVars;
begin
writeln( 'var' );
writeln;
writeln( '    j : integer ;       � Local control VAR              �' );
writeln( '    Act : integer ;     � Action from Action array       �' );
writeln( '    Tran : integer ;    � Transition code from SymList �' );
writeln( '    Next : integer ;    � saves the Next token �' );
writeln;
writeln;

end;


procedure DoOurLocalInits;
begin
if DoDebug then begin
writeln(
'procedure TokenWrite( Token : integer );' );
writeln(
'� Write out the Token as nicely as possible... �' );
writeln(
'var' );
writeln(
'   i,' );
writeln(
'   j : integer; ' );
writeln(
'begin' );
writeln(
'   if (Token >= DEBUGMIN) and (Token <= DEBUGMAX) then begin' );
writeln(
'       if DebugTable[Token] = '' '' then begin' );
writeln(
'           if Token <= HighestPrintables then begin' );
writeln(
'               if chr(Token) in Printables then' );
writeln(
'                   write('''''''', chr(Token), '''''''')' );
writeln(
'               else' );
writeln(
'                   write(Token:1)' );
writeln(
'           end else' );
writeln(
'               write(Token:1)' );
writeln(
'       end else begin' );
writeln(
'           j := 0;' );
writeln(
'           for i := DEBUGSIZE downto 1 do' );
writeln(
'               if DebugTable[Token] <> '' '' then' );
writeln(
'                   j := max( i, j );' );
writeln(
'           write(DebugTable[Token]:j);' );
writeln(
'       end;' );
writeln(
'   end else begin' );
writeln(
'       if Token = ContCode then' );
writeln(
'           write(''ContCode'')' );
writeln(
'       else if Token = ElseCode then' );
writeln(
'           write(''ElseCode'')' );
writeln(
'       else if Token = ErrorCode then' );
writeln(
'           write(''ErrorCode'')' );
writeln(
'       else if Token = ScanCode then' );
writeln(
'           write(''ScanCode'')' );
writeln(
'       else begin' );
writeln(
'           if (Token <= HighestPrintables) and (Token >= 0) then begin' );
writeln(
'               if chr(Token) in Printables then begin' );
writeln(
'                   write('''''''', chr(Token), '''''''')' );
writeln(
'               end else' );
writeln(
'                   write(Token:1)' );
writeln(
'           end else' );
writeln(
'               write(Token:1);' );
writeln(
'       end;' );
writeln(
'   end;' );
writeln(
'end;' );
writeln;
writeln;
writeln;
writeln(
'� DebugOut - do what is needed for debug �' );
writeln(
'procedure DebugOut;' );
writeln(
'begin' );
writeln(
'   write  (PATBlanks:min(sp, 50));' );
writeln(
'   TokenWrite(Token);' );
writeln(
'   writeln;' );
writeln(
'end;' );
end; �DoDebug�
writeln( 'begin' );
writeln( '     � LR Parse        �' );
writeln;
writeln;
writeln( '    sp := 0;' );
writeln( '    InSymbol ;' );
if DoDebug then begin
writeln( '    if Debug then' );
writeln( '        DebugOut;' );
end; �DoDebug�
writeln( '    CurrentState := 0;' );
writeln( '    Scan := TRUE ;' );
writeln( '    Parse := TRUE ;' );
writeln( '    Match := FALSE ;' );
writeln;
writeln( '    �************************************************�' );
writeln( '    �*  User Defined Initialization for Semantics   *�' );
writeln( '    �************************************************�' );
writeln;
end;



procedure DoOurBeforeActions;
begin
writeln( '    repeat' );
writeln( '       � Parse loop�' );
writeln( '        j := CurrentState;' );
writeln( '        repeat' );
writeln( '                          � Search for Matching Token �' );
writeln( '            Match := FALSE;' );
writeln( '            Tran := symlist [j] ;' );
writeln( '            if (Tran=Token) OR (Tran=ElseCode) then begin' );
writeln( '                                            � Match �' );
writeln( '                Match := TRUE;' );
writeln( '                Act := Action[j] ;' );
writeln( '                if Act <> ErrorCode then begin' );
writeln( '                                            � Read or Reduce �' );
writeln( '  ');�              if Act > -ScanCode then begin' );     �
writeln( '                                            � Process �' );
writeln( '                    PStack[sp] := CurrentState;' );
writeln;
writeln( '                    IntStack[sp] := IntValue;' );
writeln( '                    SymStack[sp] := SymValue;' );
writeln;
writeln;
writeln( '  ' );         �    end� Process �;' );�
writeln( '                    if Act >=0 then begin' );
writeln( '                                            � Read �' );
writeln( '                        CurrentState :=Act ;' );
writeln;
writeln( '                        if sp >= MaxStack then begin' );
writeln( '                                           � Overflow �' );
writeln( '                            writeln(Output,',
         '''Parse Stack Overflow'');' );
writeln( '                            Parse := FALSE ;' );
writeln( '                        end � Overflow� else' );
writeln( '                            sp := sp+1;' );
writeln( '                        if Scan then begin' );
writeln( '                            InSymbol;' );
if DoDebug then begin
writeln( '                            if Debug then' );
writeln( '                                DebugOut; ' );
end; �DoDebug�
writeln( '                        end else begin' );
writeln( '                                   � NO Scan�' );
writeln( '                            Scan := TRUE;' );
writeln( '                            Token := Next ;' );
writeln( '                        end � NO Scan � ;' );
writeln( '                    end � READ � else begin' );
writeln( '                                   � Reduce �' );
writeln( '                        if Act <= -ScanCode then begin' );
writeln( '                                   � Set NO Scan �' );
writeln( '                            Act := Act + ScanCode ;' );
writeln( '                            Scan := FALSE ;' );
writeln( '                            sp := sp-1 ',
         '; �One extra Pop for' );
writeln( '                                       ',
         '  NO Scan Reductions�' );
writeln( '                            Next := Token ; � Save it �' );
writeln( '                        end ;' );
writeln( '                                          � Set NO Scan �' );
writeln( '                        Act := - Act ; � Make it positive�' );
writeln( '                        sp := sp - Pop[Act] ; � Pop stack�' );
writeln( '                        CurrentState := PStack[sp];' );
writeln( '                        Token := LHS[Act] ;' );
if DoDebug then begin
writeln( '                        if Debug then' );
writeln( '                            DebugOut;' );
end; �DoDebug�
writeln;
writeln( '       � Semantic Action for each reduction �' );
writeln( '       �------------------------------------�' );
writeln;
writeln;
writeln;
writeln( '                        case Semantics[Act] of' );
writeln;
end;




procedure DoOurAfterActions;
begin
writeln;
writeln( '                        end � case �;' );
writeln;
writeln( '                    end � Reduce � ;' );
writeln( '                end � Read or Reduce� else begin' );
writeln( '                                             � Error �' );
writeln( '                    writeln(Output,''Error'');' );
writeln( '                    Parse := FALSE;' );
writeln( '                end  � Error � ;' );
writeln( '            end � Match � else' );
writeln( '                if Tran = ContCode then' );
writeln( '                    j := Action[j]' );
writeln( '                else' );
writeln( '                    j := j+1;' );
writeln;
writeln( '        until   Match ;' );
writeln;
writeln( '    until not Parse ;' );
writeln;
writeln( 'end   � LR Parse � ;' );
writeln;
writeln;
writeln;
writeln( '   � M A I N �' );
end;


procedure DoSymbolSize;
begin
    if TheirState = DefSymbolSize then
        DoTheir(DefSymbolSize)
    else
        DoOurSymbolSize;
end;


procedure DoSymbol;
begin
    if TheirState = DefSymbol then
        DoTheir(DefSymbol)
    else
        DoOurSymbol;
end;



begin

    InitProcedure;

    GetFileName;
    OpenFiles;

    if not HadError then begin

        InitTheirFile;
        DoOurFlag;

        DoOurGlobalConsts;
        DoTheir(GlobalConst);

        DoSymbolSize;

        DoOurGlobalTypes;

        DoSymbol;

        DoTheir(GlobalType);
        DoOurGlobalVars;
        DoTheir(GlobalVar);
        DoOurGlobalInits;
        DoTheir(GlobalInit);

        DoOurRoutines;

        DoTheir(InSymbol);

        DoOurLocalConsts;
        DoTheir(LocalConst);
        DoOurLocalTypes;
        DoTheir(LocalType);
        DoOurLocalVars;
        DoTheir(LocalVar);
        DoOurLocalInits;
        DoTheir(LocalInit);

        DoOurBeforeActions;

        DoTheir(Action);

        DoOurAfterActions;

        DoTheir(Main);
    end;

    if HadError then
        SetReturnCode(1);

end.
