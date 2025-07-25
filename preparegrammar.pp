program Project1;

{$mode objfpc}
{$h+}

uses sysutils;

type
  Tstate = (comment, other,ident,quoted);

function QuoteIdents(var state: tstate; aLine : string) : string;

var
  I : integer;
  c : char;
  whitespaceonly : boolean;
  isWhitespace : boolean;

  function NextChar : Char;
  begin
    if I<Length(aLine) then
      Result:=aLine[i+1]
    else
      Result:=#0;
  end;
begin
  whitespaceonly:=true;
  result:='';
  I:=1;
  While I<=Length(aLine) do
    begin
    C:=aLine[i];
    isWhiteSpace:=false;
    case c of
      'a'..'z',
      '_',
      'A'..'Z' : 
         begin
         if state=other then
         begin
           result:=Result+'<';
           state:=ident;
         end;
         if c='_' then
           Result:=Result+'\';
         end;
      '"' :
         if state=quoted then
           state:=other
         else
           state:=quoted;
      '(' : if (state<>comment) and (nextchar='*') then
              begin
              state:=comment;
              inc(i)
              end;
      '*' : if (state=Comment) and (nextchar=')') then
              begin
              inc(i);
              c:=' ';
              state:=other;
              end;
      else
        if state=ident then
          begin
          Result:=Result+'>';
          state:=other;
          end;
        case C of
          ' ' : iswhitespace:=True;
          '|' : if whitespaceonly then
               begin
               result:=Result+'\al';
               c:='t';
               end;
          '&' : Result:=Result+'\';
        end;
      end;
    inc(i);
    if not (state=comment) then
      Result:=Result+c;
    if whitespaceonly and not isWhiteSpace then
      whitespaceonly := False;
    end;
  if state=ident then
    begin
    Result:=Result+'>';
    state:=other
    end;
end;


var
  prev_empty : boolean;
  state : tstate;
  Line, quotedLine: String;

begin
  prev_empty:=false;
  state:=other;
  While not EOF do
    begin
    Readln(Line);
    QuotedLine:=QuoteIdents(state,Line);
    if Trim(QuotedLine)<>'' then
      begin
      Writeln(QuotedLine);
      prev_empty:=false;
      end
    else
      begin
      if not prev_empty then
        Writeln('');
      prev_empty:=True;
      end;
    if not (state=comment) then
      state:=other;
    end;
end.

