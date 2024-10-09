 program ed(output);
 (* software tools editor translated into PASCAL *)

(* Notes:                                                         *)
(* 1) Program now runs under ORVYL/370 on SLAC's 370/168          *)
(* 2) We did not change (much) more than was necessary to get it  *)
(*    running.                                                    *)
(* 3) Open and terminal I/O are done through external subroutines.*)
(* 4) The character set is EBCDIC, which may explain some things  *)
(*    in the program.                                             *)
(*             -- M Powell, SLAC Computation Research Group, 1979 *)

 const
   (* global flags *)
   (* constant definitons *)
   ARB = 100;
   MAXCARD = 163;
   MAXLINE = 165;
   MAXPACK = 165;
   MAXNAME = 30;
   MAXTOK = 200;
   MAXDEF = 200;
   MAXSYMBOL = 10;
   ASSGSTRSIZE = 163;

   LETTER = 1;
   DIGIT = 2;

   ESCAPE = '@';
   BANG = '!';
   CFLEX = '�';
   TILDE = '^';
   BOL = '%';
   ANY = '?';
   EOL = '$';
   CLOSURE = '*';
   AMPER = '&';
(* DITTO = chr(1); *)
   CCL = '[';
   CCLEND = ']';
   NCCL = 'n';
   CHARA = 'a';
   (* constants for pattern routines *)
   COUNT = 1;
   PREVCL = 2;
   START = 3;
   CLOSIZE = 4;

   MAXPAT = MAXLINE;  (* must be same due to parameter passing *)
   GLOBAL = 'g';
   PRINT = 'p';
   LIST = 'l';
   FORW = 0;
   BACK = -1;
   EXCLUDE = 'x';
   APPENDCOM = 'a';
   BLOCK = 'b';
   CHANGE = 'c';
   DELCOM = 'd';
   ENTER = 'e';
   PRINTFILE = 'f';
   READCOM = 'r';
   WRITECOM = 'w';
   INSERT = 'i';
   PRINTCUR = '=';
   MOVECOM = 'm';
   QUIT = 'q';
   SUBSTITUTE = 's';
   QUERY = '?';
   CURLINE = '.';
   LASTLINE = '$';
   SCAN = '/';
   BACKSCAN = '\';
   PERIOD = '.';
   NEWLINE = '
';
   PLUS = '+';
   MINUS = '-';
   DASH = '-';
   COMMA = ',';
   SEMICOL = ';';
   DIG0 = '0';
   DIG7 = '7';
   DIG9 = '9';
   BLANK = ' ';
   LETA = 'a';
   LETN = 'n';
   LETT = 't';
(* TAB = chr(9); *)
   DIRIN = 0;
   DIROUT = 1;

type
   (* global type declarations *)
   lineString = array [1..MAXLINE] of char;
   argString = lineString;
   lineLength = 0..MAXLINE;
   fileNameString = array [1..MAXNAME] of char;
   patternString = array [1..MAXPAT] of char;
   traceString = array [1..6] of char;
   assignString = array [1..ASSGSTRSIZE] of char;
   linePtr = @lineRec;
   lineRec = record
     next, prev : linePtr;
     mark : boolean;
     length : lineLength;
     line : lineString;
   end;
   statusRange = (NOSTATUS, OKSTATUS, ERRSTATUS, EOFSTATUS, INTSTATUS);

var
   (* constant variables *)
   EOS, TAB, DITTO : char;
   LINE0 : linePtr;
   ONE, ZERO : integer;
   (* global variable definitions *)
   terminopen : boolean;
   freeList : linePtr;
   n0 : integer;
   k0 : linePtr;
   line1, line2, nlines, curln, cursav, lastln : integer;
   increment : integer;
   pat : patternString;
   txt : lineString;
   pak : array [1..MAXPACK] of char;
   lpak : integer;
   savfil : array [1..MAXNAME] of char;
   status : statusRange;
   lin : lineString;
   i : integer;
   glob : boolean;
   termin, termout, filex : text;
(*******************************************************************)

(***************)
(* subroutines *)
(***************)

(* a debugging routine *)
   (* etrace - trace status of edit *)
   procedure etrace(traceName : traceString);
   begin
     (*writeln(debug,traceName,':   i=',i,',  s=',ord(status),
         ',  g=',glob);*)
   end;

(* miscellaneous i/o routines *)
   (* readline - reads a line *)
   procedure readline(var filex : text; var lin : lineString;
       var lineLen : integer);
   var
     i : integer;
   begin
     if not eof(filex) then begin
       i := 1;
       while not eoln(filex) and not eof(filex) do begin
         lin[i] := filex@;
         get(filex);
         i := i + 1;
       end;
       readln(filex);
       lineLen := i-1;
       lin[i] := EOS;
     end
     else begin
       lineLen := -1;
       lin[1] := EOS;
     end;
   end;

   (* intrpt - indicate a user attempt to interrupt *)
   function intrpt(var x : integer) : boolean; fortran;

   (* readcmd - read a line from the terminal *)
   function readcmd(var lin : lineString; promptCh : char) : boolean;
   const
     MAXPROMPT = 163;
   type
     promptString = array [1..MAXPROMPT] of char;
     promptRec = record
       len : integer;
       str : promptString;
     end;
   var
     i : integer;
     len, ret, readlen : integer;
     prompt : promptRec;
     function readterm(var lin : lineString; var len : integer;
         var prompt : promptRec; var readlen : integer) : integer; fortran;
   begin
     (* read from the terminal *)
     readcmd := TRUE;
     repeat
       prompt.len := 1;
       prompt.str[1] := promptCh;
       len := MAXLINE-2;
       ret := readterm(lin,len,prompt,readlen);
       if (ret = 4) and (readlen = 1) and (lin[1] = '@') then begin
         ret := 0;
         readcmd := FALSE;
       end;
     until ret = 0;
     lin[readlen+1] := NEWLINE;
     lin[readlen+2] := EOS;
   end;

   (* assign - external function to assign a file *)
   function assign(var str : assignString) : integer; fortran;

   (* open - opens a file using uram *)
   function open(fileName : fileNameString; dir : integer) : integer;
   var
   assgStr : assignString;
     tempStr6 : array [1..6] of char;
     tempStr9 : array [1..9] of char;
     tempStr21 : array [1..21] of char;
     i : integer;
   begin
   (* clean up file name *)
   i := 1;
   while fileName[i] <> EOS do begin
     if (fileName[i] >= 'a') and (fileName[i] <= 'z') then
       fileName[i] := chr(ord(fileName[i])+ord('A')-ord('a'));
     i := i + 1;
   end;
   for i := i to MAXNAME do fileName[i] := ' ';
     tempStr21 := 'ASSIGN FILEX TO FILE ';
     tempStr9 := ' RECFM=E ';
     if dir = DIRIN then tempStr6 := 'INPUT '
     else if dir = DIROUT then tempStr6 := 'OUTPUT'
     else tempStr6 := '??????';
     (* build up string *)
     for i := 1 to 21 do assgStr[i] := tempStr21[i];
     for i := 22 to 51 do assgStr[i] := fileName[i-21];
     for i := 52 to 60 do assgStr[i] := tempStr9[i-51];
     for i := 61 to 66 do assgStr[i] := tempStr6[i-60];
     assgStr[67] := ';';
     i := assign(assgStr);
     if i = 0 then begin
       if dir = DIRIN then reset(filex)
       else if dir = DIROUT then rewrite(filex);
     end
     else writeln(output,'assign error',i);
     open := i;
   end;

   (* ctoi - convert character to integer *)
   function ctoi(lin : lineString; var i : integer) : integer;
   var
     temp : integer;
   begin
     temp := 0;
     while (lin[i] >= DIG0) and (lin[i] <= DIG9) do begin
       temp := temp * 10 + ord(lin[i]) - ord(DIG0);
       i := i + 1;
     end;
     ctoi := temp;
   end;
(* string manipulation routines *)

   (* addset - put c in str[j] if it fits. increment j *)
   function addset(c : char; var str : lineString; var j : integer;
       maxsiz : integer) : boolean;
   begin
     if j > maxsiz then addset := FALSE
     else begin
       str[j] := c;
       j := j + 1;
       addset := TRUE;
     end;
   end;

   (* esc - map str[i] into escaped character if appropriate *)
   function esc(str : lineString; var i : integer) : char;
   label 1;
   var
     temp, j : integer;
   begin
     if str[i] <> ESCAPE then esc := str[i]
     else if str[i+1] = EOS then esc := ESCAPE (* not special at end *)
     else begin
       i := i + 1;
       if str[i] = LETN then esc := NEWLINE
       else if str[i] = LETT then esc := TAB
       else if (str[i] >= DIG0) and (str[i] <= DIG7) then begin
         temp := 0;
         for j := 1 to 3 do begin
           if (str[i] < DIG0) or (str[i] > DIG7) then goto 1;
           temp := temp * 8 + ord(str[i]) - ord(DIG0);
           i := i + 1;
         end;
       1:
         esc := chr(temp);
         i := i - 1;
       end
       else esc := str[i];
     end;
   end;

   (* filset - expand set at lin[i] into str[j] *)
   procedure filset(delim : char; lin : lineString; var i : integer;
       var str : lineString; var j : integer; maxstr : integer);
   var
     limit, k : char;
     junk : boolean;
   begin
     while (lin[i] <> delim) and (lin[i] <> EOS) do begin
       if lin[i] = ESCAPE then
         junk := addset(esc(lin,i),str,j,maxstr)
       else if lin[i] <> DASH then
         junk := addset(lin[i],str,j,maxstr)
       else if (j <= 1) or (lin[i] = EOS) then
         junk := addset(DASH,str,j,maxstr)  (* literal - *)
       else begin
         i := i + 1;
         j := j - 1;
         limit := esc(lin,i);
         for k := str[j] to limit do
           junk := addset(k,str,j,maxstr);
       end;
       i := i + 1;
     end;
   end;

(* pattern matching subroutines *)

   (* dumppat - print a pattern, starting at j *)
   procedure dumppat(pat : patternString; j : integer);
   var p, i : integer;
   begin
     p := j;
     write(output,'[');
     while pat[p] <> EOS do begin
       case pat[p] of
         BOL : write(output,'<bol>');
         EOL : write(output,'<eol>');
         ANY : write(output,'<any>');
         CHARA : begin write(output,'<char><',pat[p+1],'>'); p := p + 1; end;
         CLOSURE : begin
           write(output,'<clos><',ord(pat[p+1]):1,'><',ord(pat[p+2]):1,
             '><',ord(pat[p+3]):1,'>');
           p := p + 3;
         end;
         CCL, NCCL : begin
           if pat[p] = CCL then write(output,'<ccl>')
           else write(output,'<nccl>');
           write(output,'<',ord(pat[p+1]):1,'><');
           for i := 1 to ord(pat[p+1]) do write(output,pat[p+1+i]);
           write(output,'>');
           p := p + 1 + ord(pat[p+1]);
         end;
       end;
       p := p + 1;
     end;
     writeln(output,']');
   end;

   (* patsiz - returns size of the pattern entry at pat[n] *)
   function patsiz(pat : patternString; n : integer) : integer;
   begin
     if pat[n] = CHARA then patsiz := 2
     else if (pat[n] = BOL) or (pat[n] = EOL) or (pat[n] = ANY) then
       patsiz := 1
     else if (pat[n] = CCL) or (pat[n] = NCCL) then
       patsiz := ord(pat[n+COUNT]) + 2
     else if pat[n] = CLOSURE then patsiz := CLOSIZE
     else writeln(termout,'patsize: impossible');
   end;

   (* locate - look for c in char class at pat[offset] *)
   function locate(c : char; pat : patternString; offset : integer) : boolean;
   label 1;
   var
     i : integer;
   begin
     locate := FALSE;
     for i := offset + ord(pat[offset]) downto offset + 1 do begin
       if c = pat[i] then begin
         locate := TRUE;
         goto 1;
       end;
     end;
   1:
   end;

   (* omatch - try to match a single pattern at pat[j] *)
   function omatch(lin : lineString; var i : integer; pat : patternString;
       j : integer) : boolean;
   var
     bump : integer;
   begin
     omatch := FALSE;
     if lin[i] <> EOS then begin
       bump := -1;
       if pat[j] = CHARA then begin
         if lin[i] = pat[j+1] then bump := 1;
       end
       else if pat[j] = BOL then begin
         if i = 1 then bump := 0;
       end
       else if pat[j] = ANY then begin
         if lin[i] <> NEWLINE then bump := 1;
       end
       else if pat[j] = EOL then begin
         if lin[i] = NEWLINE then bump := 0;
       end
       else if pat[j] = CCL then begin
         if locate(lin[i],pat,j+1) then bump := 1;
       end
       else if pat[j] = NCCL then begin
         if lin[i] <> NEWLINE then
           if not locate(lin[i],pat,j+1) then bump := 1;
       end
       else begin
         writeln(termout,'omatch: impossible');
       end;
       if bump >= 0 then begin
         i := i + bump;
         omatch := TRUE;
       end;
     end;
   end;


   (* amatch (non-recursive) - look for match starting at lin[from] *)
   function amatch(lin : lineString; from : integer; var pat : patternString)
       : integer;
   label 1, 2, 3;
   var
     i, j, offset, stack : integer;
   begin
     stack := 0;
     offset := from;
     j := 1;
     while pat[j] <> EOS do begin
       if pat[j] = CLOSURE then begin
         stack := j;
         j := j + CLOSIZE;
         i := offset;
         while lin[i] <> EOS do
           if not omatch(lin,i,pat,j) then goto 1;
       1:
         pat[stack+COUNT] := chr(i-offset);
         pat[stack+START] := chr(offset);
         offset := i;
       end
       else if not omatch(lin,offset,pat,j) then begin (* non-closure *)
         while stack > 0 do begin
           if ord(pat[stack+COUNT]) > 0 then goto 2;
           stack := ord(pat[stack+PREVCL]);
         end;
       2:
         if stack <= 0 then begin
           offset := 0;
           goto 3;
         end;
         pat[stack+COUNT] := chr(ord(pat[stack+COUNT]) - 1);
         j := stack + CLOSIZE;
         offset := ord(pat[stack+START]) + ord(pat[stack+COUNT]);
       end;
       (* else omatch succeeded *)
       j := j + patsiz(pat,j);
     end;
   3:
     amatch := offset;
   end;

   (* match - find match anywhere on line *)
   function match(lin : lineString; var pat : patternString) : boolean;
   var
     i : integer;
     result : boolean;
   begin
     i := 1;
     result := FALSE;
     while (lin[i] <> EOS) and not result do begin
       result := amatch(lin,i,pat) > 0;
       i := i + 1;
     end;
     match := result;
   end;

   (* stclos - insert closure entry at pat[j] *)
   function stclos(var pat : patternString; var j, lastj, lastcl : integer)
       : integer;
   var
     jp, jt : integer;
     junk : boolean;
   begin
     for jp := j-1 downto lastj do begin
       jt := jp + CLOSIZE;
       junk := addset(pat[jp],pat,jt,MAXPAT);
     end;
     j := j + CLOSIZE;
     stclos := lastj;
     junk := addset(CLOSURE,pat,lastj,MAXPAT);  (* put closure in it *)
     junk := addset(chr(0),pat,lastj,MAXPAT); (* COUNT *)
     junk := addset(chr(lastcl),pat,lastj,MAXPAT); (* PREVCL *)
     junk := addset(chr(0),pat,lastj,MAXPAT); (* START *)
   end;

   (* getccl - expand character class at arg[i] into pat[j] *)
   function getccl(arg : argString; var i : integer; var pat : patternString;
       var j : integer) : boolean;
   var
     junk : boolean;
     jstart : integer;
   begin
     i := i + 1;
     if (arg[i] = BANG) or (arg[i] = CFLEX) or (arg[i] = TILDE) then begin
       junk := addset(NCCL,pat,j,MAXPAT);
       i := i + 1;
     end
     else junk := addset(CCL,pat,j,MAXPAT);
     jstart := j;
     junk := addset(chr(0),pat,j,MAXPAT); (* leave room for count *)
     filset(CCLEND,arg,i,pat,j,MAXPAT);
     pat[jstart] := chr(j-jstart-1);
     getccl := arg[i] = CCLEND;
   end;

   (* makpat - make pattern from arg[from], terminate at delim *)
   function makpat(arg : argString; from : integer; delim : char;
       var pat : patternString) : integer;
   label 1;
   var
     i, j, lastcl, lastj, lj : integer;
     junk : boolean;
   begin
     j := 1;  (* pat index *)
     lastj := 1;
     lastcl := 0;
     i := from;
     while (arg[i] <> delim) and (arg[i] <> EOS) do begin
       lj := j;
       if arg[i] = ANY then junk := addset(ANY,pat,j,MAXPAT)
       else if (arg[i] = BOL) and (i = from) then
         junk := addset(BOL,pat,j,MAXPAT)
       else if (arg[i] = EOL) and (arg[i+1] = delim) then
         junk := addset(EOL,pat,j,MAXPAT)
       else if arg[i] = CCL then begin
         if not getccl(arg,i,pat,j) then goto 1;
       end
       else if (arg[i] = CLOSURE) and (i > from) then begin
         lj := lastj;
         if (pat[lj] = BOL) or (pat[lj] = EOL) or (pat[lj] = CLOSURE) then
           goto 1;
         lastcl := stclos(pat,j,lastj,lastcl);
       end
       else begin
        junk := addset(CHARA,pat,j,MAXPAT);
        junk := addset(esc(arg,i),pat,j,MAXPAT);
       end;
       lastj := lj;
       i := i + 1;
     end;
   1:
     if arg[i] <> delim then makpat := -1 (* error making pattern *)
     else if not addset(EOS,pat,j,MAXPAT) then makpat := -1 (* no room *)
     else makpat := i;
   end;

(* string substitution routines *)

   (* catsub - add replacement text to end of str *)
   procedure catsub(lin : lineString; f, t : integer; sub : patternString;
       var str : lineString; var k : integer; maxnew : integer);
   var
     i, j : integer;
     junk : boolean;
   begin
     (*writeln(debug,'catsub',f,t);*)
     (*writeln(debug,lin:20);*)
     (*writeln(debug,sub:20);*)
     i := 1;
     while sub[i] <> EOS do begin
       if sub[i] = DITTO then begin
         for j := f to t-1 do junk := addset(lin[j],str,k,maxnew);
       end
       else junk := addset(sub[i],str,k,maxnew);
       i := i + 1;
     end;
   end;

   (* maksub - make substitution string in sub *)
   function maksub(arg : argString; f : integer; delim : char;
       var sub : patternString) : integer;
   var
     i, j : integer;
     junk : boolean;
   begin
     j := 1;
     i := f;
     while (arg[i] <> delim) and (arg[i] <> EOS) do begin
       if arg[i] = AMPER then junk := addset(DITTO,sub,j,MAXPAT)
       else junk := addset(esc(arg,i),sub,j,MAXPAT);
       i := i + 1;
     end;
     if arg[i] <> delim then
       maksub := -1  (* error - missing delimiter *)
     else if not addset(EOS,sub,j,MAXPAT) then
       maksub := -1  (* error - no room *)
     else maksub := i;
   end;

(* line manipulating routines *)


   (* nextln - get line after "line" *)
   function nextln(line : integer) : integer;
   var
     result : integer;
   begin
     result := line + 1;
     if result > lastln then result := 0;
     nextln := result;
   end;

   (* prevln - get line before "line" *)
   function prevln(line : integer) : integer;
   var
     result : integer;
   begin
     result := line - 1;
     if result < 0 then result := lastln;
     prevln := result;
   end;

   (* linkup - make line k2 follow line k1 *)
   procedure linkup(k1, k2 : linePtr);
   begin
     k1@.next := k2;
     k2@.prev := k1;
     (* reset line cache *)
     n0 := 0;
     k0 := LINE0;
   end;

   (* alloline - allocate a line record *)
   function alloline : linePtr;
   var
     result : linePtr;
   begin
     if freeList = NIL then new(result)
     else begin
       result := freeList;
       freeList := freeList@.next;
     end;
     result@.next := NIL;
     result@.prev := NIL;
     result@.mark := FALSE;
     alloline := result;
   end;

   (* freeline - release a line record *)
   procedure freeline(var linep : linePtr);
   begin
     linep@.next := freeList;
     freeList := linep;
   end;

   (* getind - locate line index in buffer *)
   function getind(line : integer) : linePtr;
   var
     d, d0, n1 : integer;
   begin
     (*writeln(debug,'getind',line);*)
     n1 := lastln + 1;
     d0 := line;
     (* d := min(d0,n1-d0,abs(d0-n0)); *)
     d := d0;
     if n1-d0 < d then d := n1-d0;
     if abs(d0-n0) < d then d := abs(d0-n0);
     (*writeln(debug,d0,n1-d0,abs(d0-n0));*)
     if d = d0 then begin (* go forward from LINE0 *)
       n0 := 0;
       k0 := LINE0;
     end
     else if d = n1-d0 then begin (* go backwards from LINE0 *)
       n0 := n1;
       k0 := LINE0;
     end;
     (*writeln(debug,'search',n0,' ',k0@.line:k0@.length);*)
     while n0 > d0 do begin  (* search backward *)
     (*writeln(debug,'back');*)
       k0 := k0@.prev;
       n0 := n0 - 1;
     (*writeln(debug,'search',n0,' ',k0@.line:k0@.length);*)
     end;
     while n0 < d0 do begin  (* search forward *)
     (*writeln(debug,'forw');*)
       k0 := k0@.next;
       n0 := n0 + 1;
     (*writeln(debug,'search',n0,' ',k0@.line:k0@.length);*)
     end;
     getind := k0;
     (*writeln(debug,'k0',ord(k0));*)
   end;

   (* getpak - get packed text for line *)
   function getpak(line : integer) : linePtr;
   var
     result : linePtr;
   begin
     (* this routine should do something more *)
     (*writeln(debug,'getpak',line);*)
     result := getind(line);
     (*writeln(debug,'result',ord(result));*)
     pak := result@.line;
     lpak := result@.length;
     (*writeln(debug,'pak=',pak:lpak,result@.line:result@.length);*)
     getpak := result;
   end;

   (* gettxt - locate text for a line *)
   function gettxt(line : integer) : linePtr;
   begin
     (* this routine should do something more *)
     (*writeln(debug,'gettxt');*)
     gettxt := getpak(line);
     txt := pak;
     (*writeln(debug,'txt=pak?');*)
     (*writeln(debug,pak:lpak);*)
     (*writeln(debug,txt:lpak);*)
     txt[lpak+1] := NEWLINE;
     txt[lpak+2] := EOS;
   end;

   (* injpak - insert packed line from pak after curln *)
   function injpak(var sts : statusRange) : statusRange;
   var
     k1, k2, k3 : linePtr;
   begin
     (*writeln(debug,'injpak',curln,lpak,pak:lpak);*)
     k1 := getind(curln);
     k2 := k1@.next;
     k3 := alloline;
     k3@.line := pak;
     k3@.length := lpak;
     linkup(k1,k3);
     linkup(k3,k2);
     k1 := k3;
     curln := curln + 1;
     lastln := lastln + 1;
     n0 := curln;
     k0 := k1;
     injpak := OKSTATUS;
     sts := OKSTATUS;
   end;

   (* inject - insert text from lin after curln *)
   function inject(lin : lineString) : statusRange;
   label 1, 2;
   var
     i, j : integer;
     result : statusRange;
   begin
     (*writeln(debug,'inject',lin:20);*)
     i := 1;
     while lin[i] <> EOS do begin
       j := i;
       while lin[i] <> EOS do begin
         i := i + 1;
         if lin[i-1] = NEWLINE then goto 1;
       end;
     1:
       if lin[i-1] = NEWLINE then begin
         lin[i-1] := EOS;
         (* move in line *)
         lpak := 0;
         repeat
           lpak := lpak + 1;
           pak[lpak] := lin[j+lpak-1];
         until pak[lpak] = EOS;
         lpak := lpak - 1;
         lin[i-1] := NEWLINE;
         if injpak(result) <> OKSTATUS then goto 2;
       end;
     end;
     result := OKSTATUS;
   2:
     inject := result;
   end;

   (* append lines after 'line' *)
   function append(line:integer) : statusRange;
   var
     result : statusRange;
     lin : lineString;
   begin
     etrace('append');
     if glob then begin
       result := ERRSTATUS;
     end
     else begin
       curln := line;
       result := NOSTATUS;
       while result = NOSTATUS do begin
         if not readcmd(lin,'*') then result := ERRSTATUS
         else if (lin[1] = PERIOD) and (lin[2] = NEWLINE) then
           result := OKSTATUS
         else if inject(lin) = ERRSTATUS then result := ERRSTATUS;
       end;
     end;
     append := result;
   end;

   (* delete - delete lines "from" through "to" *)
   function delete(fromLine, toLine : integer) : statusRange;
   label 1;
   var
     number : integer;
     k1, k2, k3 : linePtr;
   begin
     status := ERRSTATUS;
     if fromLine > 0 then begin
       status := INTSTATUS;
       curln := prevln(fromLine);
       k1 := getind(curln);
       k2 := k1@.next;
       for number := toLine-fromLine+1 downto 1 do begin
         if intrpt(ONE) then goto 1;
         k3 := k2@.next;
         linkup(k1,k3);
         freeline(k2);
         lastln := lastln - 1;
         k2 := k3;
       end;
       status := OKSTATUS;
     1:
       n0 := curln;
       k0 := k1;
     end;
     delete := status;
   end;

   (* clrbuf - initialize for new file *)
   procedure clrbuf;
   begin
     (* perhaps this routine should do something *)
   end;

   (* setbuf - initialize line buffer *)
   procedure setbuf(initial : boolean);
   var
     junk : statusRange;
   begin
     if initial then begin
       n0 := 0;
       k0 := alloline;
       k0@.next := k0;
       k0@.prev := k0;
       k0@.line[1] := 'L';
       k0@.line[2] := 'I';
       k0@.line[3] := 'N';
       k0@.line[4] := 'E';
       k0@.line[5] := '0';
       k0@.length := 5;
       lastln := 0;
       LINE0 := k0;
     end;
     junk := delete(1,lastln);
   end;

(* command interpretation routines *)


   (* ckp - check for 'p' after command *)
   function ckp(var pflag : char) : statusRange;
   begin
     etrace('   ckp');
     if lin[i] = PRINT then begin
       i := i + 1;
       pflag := PRINT;
     end
     else if lin[i] = LIST then begin
       i := i + 1;
       pflag := LIST;
     end
     else pflag := BLANK;
     if lin[i] = NEWLINE then status := OKSTATUS
     else status := ERRSTATUS;
     ckp := status;
   end;

   (* defalt - set defaulted line numbers *)
   function defalt(def1, def2 : integer) : statusRange;
   begin
     etrace('defalt');
     if nlines = 0 then begin;
       line1 := def1;
       line2 := def2;
     end;
     if (line1 > line2) or (line1 <= 0) then status := ERRSTATUS
     else status := OKSTATUS;
     defalt := status;
   end;

   (* skipbl - skip blanks and tabs at lin[i] *)
   procedure skipbl;
   begin
     while (lin[i] = BLANK) or (lin[i] = TAB) do i := i + 1;
   end;

   (* getfn - get file name from lin[i] *)
   function getfn(var fileName : fileNameString) : statusRange;
   var
     result : statusRange;
     k : integer;
   begin
     etrace(' getfn');
     result := ERRSTATUS;
     if lin[i] = BLANK then begin
       (* get new file name *)
       i := i + 1;
       skipbl;
       k := 1;
       while lin[i] <> NEWLINE do begin
         if k < MAXNAME then begin
           fileName[k] := lin[i];
           k := k + 1;
         end;
         i := i + 1;
       end;
       fileName[k] := EOS;
       if k > 1 then result := OKSTATUS;
     end
     else if (lin[i] = NEWLINE) and (savfil[1] <> EOS) then begin
       fileName := savfil;
       result := OKSTATUS;
     end;
     (* else error *)
     if (result = OKSTATUS) and (savfil[1] = EOS) then
       savfil := fileName; (* if no old name, save new one *)
     getfn := result;
   end;

   (* ptscan - scan for next occurrence of pattern *)
   function ptscan(way : integer; var num0 : integer) : statusRange;
   label 1;
   var
     num : integer;
     k : linePtr;
   begin
     etrace('ptscan');
     num := curln;
     ptscan := INTSTATUS;
     repeat
       if intrpt(ONE) then goto 1;
       if way = FORW then num := nextln(num)
       else num := prevln(num);
       k := gettxt(num);
       if match(txt,pat) then begin
         num0 := num;
         ptscan := OKSTATUS;
         goto 1;
       end;
     until num = curln;
     ptscan := ERRSTATUS;
   1:
   end;

   (* optpat - make pattern if specified at lin[i] *)
   procedure optpat;
   var
     j : integer;
     delim : char;
   begin
     etrace('optpat');
     status := ERRSTATUS;
     if lin[i] <> EOS then begin
       delim := lin[i];
       i := i + 1;
       if (lin[i] = delim) and (pat[1] <> EOS) then
         status := OKSTATUS   (* use old pattern *)
       else if lin[i] <> EOS then begin
         j := makpat(lin,i,delim,pat);
         if j > 0 then begin
           i := j;
           status := OKSTATUS;
         end
         else pat[1] := EOS;
       end;
     end;
   end;

   (* getnum - convert one term to line number *)
   function getnum(var pnum : integer) : statusRange;
   var
     result : statusRange;
   begin
     etrace('getnum');
     result := OKSTATUS;
     if (lin[i] >= DIG0) and (lin[i] <= DIG9) then
       pnum := ctoi(lin,i)
     else if lin[i] = CURLINE then begin
       pnum := curln;
       i := i + 1;
     end
     else if lin[i] = LASTLINE then begin
       pnum := lastln;
       i := i + 1;
     end
     else if (lin[i] = SCAN) or (lin[i] = BACKSCAN) then begin
       optpat;  (* build pattern *)
       if status = ERRSTATUS then result := ERRSTATUS
       else if lin[i] = SCAN then begin
         getnum := ptscan(FORW,pnum);
         i := i + 1;
       end
       else begin
         getnum := ptscan(BACK,pnum);
         i := i + 1;
       end;
     end
     else result := EOFSTATUS;
     status := result;
     getnum := result;
     etrace('gotnum');
   end;

   (* getone - evaluate one line number expression *)
   function getone(var num0 : integer) : statusRange;
   label 1;
   var
     istart, mul, num, pnum : integer;
     result : statusRange;
   begin
     etrace('getone');
     istart := i;
     num := 0;
     skipbl;
     if (lin[i] = PLUS) or (lin[i] = MINUS) then begin
       num := curln;
       status := OKSTATUS;
     end
     else status := getnum(num);
     while status = OKSTATUS do begin
       skipbl;
       if lin[i] = PLUS then mul := +1
       else if lin[i] = MINUS then mul := -1
       else begin
         status := EOFSTATUS;
         goto 1;
       end;
       i := i + 1;
       skipbl;
       if getnum(pnum) = OKSTATUS then num := num + mul * pnum
       else if status = EOFSTATUS then begin
         num := num + mul;
         status := OKSTATUS;
       end;
     end;
   1:
     if (status = ERRSTATUS) or (status = INTSTATUS) then result := status
     else if (num < 0) or (num > lastln) then result := ERRSTATUS
     else if i <= istart then result := EOFSTATUS
     else result := OKSTATUS;
     num0 := num;
     status := result;
     getone := result;
     etrace('gotone');
   end;

   (* getlst - collect line numbers (if any) at lin[i] *)
   procedure getlst;
   label 1;
   var
     num : integer;
   begin
     etrace('getlst');
     line2 := 0;
     nlines := 0;
     while getone(num) = OKSTATUS do begin
       line1 := line2;
       line2 := num;
       nlines := nlines + 1;
       if (lin[i] <> COMMA) and (lin[i] <> SEMICOL) then goto 1;
       if lin[i] = SEMICOL then curln := num;
       i := i + 1;
     end;
   1:
     if nlines > 2 then nlines := 2;
     if nlines = 0 then line2 := curln;
     if nlines <= 1 then line1 := line2;
     if status = EOFSTATUS then status := OKSTATUS;
     etrace('gotlst');
     (*writeln(debug,line1,line2,nlines);*)
   end;

   (* getrhs - get substitution string for "s" command *)
   function getrhs(var sub : patternString; var gflag : boolean) : statusRange;
   var
     delim : char;
     j : integer;
     result : statusRange;
   begin
     etrace('getrhs');
     result := ERRSTATUS;
     gflag := FALSE;
     if lin[i] <> EOS then begin
       delim := lin[i];
       i := i + 1;
       if lin[i] <> EOS then begin
         j := maksub(lin,i,delim,sub);
         if j > 0 then begin
           i := j + 1;
           if lin[i] = GLOBAL then begin
             i := i + 1;
             gflag := TRUE;
           end;
           result := OKSTATUS;
         end;
       end;
     end;
     status := result;
     getrhs := result;
   end;

(* command routines *)


   (* dolist - print lines fromLine through toLine with all chars visible *)
   function dolist(fromLine, toLine : integer) : statusRange;
   label 1;
   var
     line, k, m : integer;
     j : linePtr;
     ch : char;
     numb : array [1..2] of char;
     junk : integer;
   begin
     dolist := ERRSTATUS;
     if fromLine > 0 then begin
       dolist := INTSTATUS;
       for line := fromLine to toLine do begin
         if intrpt(ONE) then goto 1;
         j := gettxt(line);
         k := 1;
         while txt[k] <> NEWLINE do begin
           ch := txt[k];
           if (ch=' ')or((ch>='�')and(ch<='&'))or((ch>='!')and(ch<='/'))
             or((ch>=',')and(ch<='?'))or((ch>='`')and(ch<='#'))
             or((ch>='''')and(ch<='"'))or((ch>='a')and(ch<='i'))
             or((ch>='j')and(ch<='r'))or((ch>='s')and(ch<='z'))
             or(ch='�')or(ch='�')or(ch='[')or(ch=']')or(ch='\')
             or((ch>='A')and(ch<='I'))or((ch>='J')and(ch<='R'))
             or((ch>='S')and(ch<='Z'))or((ch>='0')and(ch<='9')) then
             write(termout,ch)
           else begin
             if ch = ESCAPE then write(termout,ESCAPE,ESCAPE)
             else if ch = NEWLINE then write(termout,ESCAPE,LETN)
             else if ch = TAB then write(termout,ESCAPE,LETT)
             else begin
               m := ord(ch) mod 16;
               if m > 9 then numb[2] := chr(m-10+ord(LETA))
               else numb[2] := chr(m+ord(DIG0));
               m := ord(ch) div 16;
               if m > 9 then numb[1] := chr(m-10+ord(LETA))
               else numb[1] := chr(m+ord(DIG0));
               write(termout,ESCAPE,numb);
             end;
           end;
           k := k + 1;
         end;
         writeln(termout);
         curln := line;
       end;
       dolist := OKSTATUS;
     end;
   1:
   end;

   (* doprnt - print lines fromLine through toLine *)
   function doprnt(fromLine, toLine : integer) : statusRange;
   label 1;
   var
     line : integer;
     j : linePtr;
   begin
     (*writeln(debug,'doprnt');*)
     doprnt := ERRSTATUS;
     if fromLine > 0 then begin
       doprnt := INTSTATUS;
       for line := fromLine to toLine do begin
         if intrpt(ONE) then goto 1;
         j := gettxt(line);
         writeln(termout,txt:lpak);
         (*writeln(debug,'txt=',txt:lpak);*)
         curln := line;
       end;
       doprnt := OKSTATUS;
     end;
   1:
   end;

   (* doread - read 'file' after 'line' *)
   function doread(line : integer; fileName : fileNameString) : statusRange;
   label 1;
   var
     errNum, number : integer;
     result : statusRange;
   begin
     result := ERRSTATUS;
     errNum := open(fileName,DIRIN);
     if errNum = 0 then begin
       result := OKSTATUS;
       curln := line;
       for number := 0 to MAXINT do begin
         if intrpt(ONE) then begin
           result := INTSTATUS;
           goto 1;
         end;
         readline(filex,pak,lpak);
         if lpak < 0 then goto 1;
         if injpak(result) <> OKSTATUS then goto 1;
       end;
     1:
       reset(filex);
       writeln(termout,number:1);
     end;
     doread := result;
   end;

   (* dowrit - write fromLine through toLine into file *)
   function dowrit(fromLine, toLine : integer; fileName : fileNameString)
           : statusRange;
   label 1;
   var
     errNum, number : integer;
     line : integer;
     k : linePtr;
   begin
     dowrit := ERRSTATUS;
     errNum := open(fileName,DIROUT);
     if errNum = 0 then begin
       dowrit := OKSTATUS;
       number := 0;
       for line := fromLine to toLine do begin
         if intrpt(ONE) then begin
           dowrit := INTSTATUS;
           goto 1;
         end;
         k := getpak(line);
         writeln(filex,pak:lpak);
         number := number + 1;
       end;
     1:
       reset(filex);
       writeln(termout,number:1);
     end;
   end;

   (* move - move "line1" through "line2" after "line3" *)
   function move(line3 : integer) : statusRange;
   var
     k0, k1, k2, k3, k4, k5 : linePtr;
     line4 : integer;
   begin
     line4 := line3;
     if (line1 <= 0) or ((line1 <= line4) and (line4 <= line2)) then
       move := ERRSTATUS
     else begin
       k0 := getind(prevln(line1));
       k1 := getind(line1);
       k2 := getind(line2);
       k3 := getind(nextln(line2));
       k4 := getind(line4);
       k5 := getind(nextln(line4));
       linkup(k0,k3);
       if line4 > line1 then begin
         curln := line4;
       end
       else curln := line4 + (line2 - line1 + 1);
       linkup(k4,k1);
       linkup(k2,k5);
       move := OKSTATUS;
     end;
   end;

   (* subst - substitute "sub" for occurrences of pattern *)
   function subst(sub : patternString; gflag : boolean) : statusRange;
   label 1, 2;
   var
     newString : lineString;
     j, k, lastm, line, m : integer;
     junk1 : linePtr;
     junk2 : boolean;
     gotcha : statusRange;
     subbed : boolean;
   begin
     etrace(' subst');
     subst := ERRSTATUS;
     if line1 > 0 then begin
       subst := INTSTATUS;
       gotcha := ERRSTATUS;
       for line := line1 to line2 do begin
         if intrpt(ONE) then goto 1;
         j := 1;
         subbed := FALSE;
         junk1 := gettxt(line);
         lastm := 0;
         k := 1;
         while txt[k] <> EOS do begin
           if gflag or not subbed then
             m := amatch(txt,k,pat)
           else m := 0;
           if (m > 0) and (lastm <> m) then begin (* replace matched text *)
             subbed := TRUE;
             catsub(txt,k,m,sub,newString,j,MAXLINE);
             lastm := m;
           end;
           if (m = 0) or (m = k) then begin (* no match or null match *)
             junk2 := addset(txt[k],newString,j,MAXLINE);
             k := k + 1;
           end
           else k := m; (* skip matched text *)
         end;
         if subbed then begin
           if not addset(EOS,newString,j,MAXLINE) then goto 2;
           gotcha := delete(line,line);   (* remembers dot *)
           if gotcha <> OKSTATUS then goto 2;
           if inject(newString) = ERRSTATUS then begin
             subst := ERRSTATUS;
             goto 1;
           end;
         end;
       end;
     2:
       subst := gotcha;
     end;
   1:
   end;

(* global command execution routines *)

   (* docmd - handle all commands except globals *)
   procedure docmd;
   var
     fileName : fileNameString;
     sub : patternString;
     ch : char;
     line3 : integer;
     gflag : boolean;
     pflag : char;
     ix : integer;
   begin
     etrace(' docmd');
     pflag := BLANK;   (* may be set by d, m, s *)
     status := ERRSTATUS;
     ch := lin[i];
     if lin[i] <> EOS then i := i + 1;
     case ch of
       APPENDCOM: begin
         if lin[i] = NEWLINE then status := append(line2);
       end;
       BLOCK: begin
         if nlines <> 1 then status := ERRSTATUS
         else begin
           increment := line2;
           status := OKSTATUS;
         end;
       end;
       CHANGE: begin
         if lin[i] = NEWLINE then
           if defalt(curln,curln) = OKSTATUS then
             if delete(line1,line2) = OKSTATUS then
               status := append(prevln(line1));
       end;
       DELCOM: begin
         if ckp(pflag) = OKSTATUS then
           if defalt(curln,curln) = OKSTATUS then
             if delete(line1,line2) = OKSTATUS then
               if nextln(curln) <> 0 then curln := nextln(curln);
       end;
       INSERT: begin
         if lin[i] = NEWLINE then status := append(prevln(line2));
       end;
       PRINTCUR: begin
         if ckp(pflag) = OKSTATUS then begin
           writeln(termout,line2:1);
         end;
       end;
       MOVECOM: begin
         if getone(line3) = EOFSTATUS then status := ERRSTATUS;
         if status = OKSTATUS then
           if ckp(pflag) = OKSTATUS then
             if defalt(curln,curln) = OKSTATUS then status := move(line3);
       end;
       SUBSTITUTE: begin
         optpat;
         if status = OKSTATUS then
           if getrhs(sub,gflag) = OKSTATUS then
             if ckp(pflag) = OKSTATUS then
               if defalt(curln,curln) = OKSTATUS then
                 status := subst(sub,gflag);
       end;
       ENTER: begin
         if nlines = 0 then
           if getfn(fileName) = OKSTATUS then begin
             savfil := fileName;
             clrbuf;
             setbuf(FALSE);
             status := doread(0,fileName);
           end;
       end;
       PRINTFILE: begin
         if nlines = 0 then
           if getfn(fileName) = OKSTATUS then begin
             savfil := fileName;
             ix := 1;
             while savfil[ix] <> EOS do ix := ix + 1;
             writeln(termout,savfil:ix-1);
             status := OKSTATUS;
           end;
       end;
       READCOM: begin
         if getfn(fileName) = OKSTATUS then status := doread(line2,fileName);
       end;
       WRITECOM: begin
         if getfn(fileName) = OKSTATUS then
           if defalt(1,lastln) = OKSTATUS then
             status := dowrit(line1,line2,fileName);
       end;
       PRINT: begin
         if lin[i] = NEWLINE then
           if defalt(curln,curln) = OKSTATUS then
             status := doprnt(line1,line2);
       end;
       LIST: begin
         if lin[i] = NEWLINE then
           if defalt(curln,curln) = OKSTATUS then
             status := dolist(line1,line2);
       end;
       QUERY: begin
         if lin[i] = NEWLINE then
           if nlines = 0 then begin
             etrace('     ?');
             (* qrymem; *) writeln(termout,'not yet implemented');
             status := OKSTATUS;
           end;
       end;
       NEWLINE: begin
         if nlines = 0 then begin
           line1 := nextln(curln);
           line2 := line1 + increment - 1;
           if line2 > lastln then line2 := lastln;
         end;
         status := doprnt(line1,line2);
       end;
       QUIT: begin;
         if (lin[i] = NEWLINE) and (nlines = 0) and not glob then
           status := EOFSTATUS;
       end;
     (* else status is ERRSTATUS *)
     end;
     if (status = OKSTATUS) and (pflag = PRINT) then
       status := doprnt(curln,curln)
     else if (status = OKSTATUS) and (pflag = LIST) then
       status := dolist(curln,curln);
   end;

   (* ckglob - if global prefix, mark lines to be affected *)
   procedure ckglob;
   label 1, 2;
   var
     line : integer;
     gflag : boolean;
     k : linePtr;
   begin
     etrace('ckglob');
     if (lin[i] <> GLOBAL) and (lin[i] <> EXCLUDE) then begin
       glob := FALSE;
       status := EOFSTATUS;
     end
     else begin
       glob := TRUE;
       gflag := lin[i] = GLOBAL;
       i := i + 1;
       optpat;
       if status = OKSTATUS then status := defalt(1,lastln);
       if status = OKSTATUS then begin
         i := i + 1;
         for line := line1 to line2 do begin
           if intrpt(ONE) then begin
             status := INTSTATUS;
             goto 1;
           end;
           k := gettxt(line);
           k@.mark := match(txt,pat) = gflag;
         end;
       1:
         if status = OKSTATUS then begin
           line := nextln(line2);
           while line <> line1 do begin
             if intrpt(ONE) then begin
               status := INTSTATUS;
               goto 2;
             end;
             k := getind(line);
             k@.mark := FALSE;
             line := nextln(line);
           end;
         2:
         end;
       end;
     end;
   end;

   (* doglob - do command at lin[i] on all marked lines *)
   procedure doglob(var cursav : integer);
   var
     number, istart, line : integer;
     k : linePtr;
   begin
     etrace('doglob');
     status := OKSTATUS;
     number := 0;
     line := line1;
     istart := i;
     repeat
       k := getind(line);
       if intrpt(ONE) then status := INTSTATUS
       else if k@.mark then begin
         k@.mark := FALSE;
         curln := line;
         cursav := curln;
         i := istart;
         getlst;
         if status = OKSTATUS then docmd;
         if status = OKSTATUS then number := 0;
       end
       else begin
         line := nextln(line);
         number := number + 1;
       end;
     until (number > lastln) or (status <> OKSTATUS);
   end;

(* global initialization *)

   (* initialize - perform initialization *)
   procedure initialize;
   var
     tempstr34 : array [1..34] of char;
     assgstr : assignString;
     i : integer;
     dummy : boolean;
   begin
     tempstr34 := 'ASSIGN TERMIN  TO TERMINAL INPUT ;';
     for i := 1 to 34 do assgstr[i] := tempstr34[i];
     i := assign(assgstr);
     if i <> 0 then writeln(output,'error in assign of termin',i);
     tempstr34 := 'ASSIGN TERMOUT TO TERMINAL OUTPUT;';
     for i := 1 to 34 do assgstr[i] := tempstr34[i];
     i := assign(assgstr);
     if i <> 0 then writeln(output,'error in assign of termout',i);
     EOS := chr(0);
     TAB := chr(9);
     ONE := 1;
     ZERO := 0;
     DITTO := chr(1);
     terminopen := FALSE;
     freeList := NIL;
     rewrite(termout);
     dummy := intrpt(ZERO);
     increment := 1;
   end;

(*******************************************************************)
begin

   (****************)
   (* main program *)
   (****************)

   (* initialize *)
   initialize;

   setbuf(TRUE);

   pat[1] := EOS;
   savfil[1] := EOS;
   (*??? look for argument, if possible *)
   status := OKSTATUS;
   while status <> EOFSTATUS do if readcmd(lin,'.') then begin
     i := 1;
     etrace('  edit');
     cursav := curln;
     getlst;
     if status = OKSTATUS then begin
       ckglob;
       if status = OKSTATUS then doglob(cursav)
       else if status = EOFSTATUS then docmd;
     end;
     etrace('  exit');
     if status = ERRSTATUS then begin
       writeln(termout,'?');
       curln := cursav;
     end
     else if status = INTSTATUS then writeln(termout,'!');
   end;
   clrbuf;
end.
