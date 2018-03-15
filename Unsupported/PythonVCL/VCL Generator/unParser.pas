unit unParser;

(**************************************************************************)
(*                                                                        *)
(* VCL Generator                       Copyright (c) 1998                 *)
(*                                                                        *)
(*                                     Morgan Martinet                    *)
(*                                     23 rue du 14 juillet               *)
(*                                     94270 le Kremlin-Bicetre           *)
(*                                     Phone (Work): 01 47 25 70 77       *)
(*                                     e-mail: mmm@imaginet.fr            *)
(*                                                                        *)
(**************************************************************************)
(* This source code is distributed with no WARRANTY, for no reason or use.*)
(* Everyone is allowed to use and change this code free for his own tasks *)
(* and projects, as long as this header and its copyright text is intact. *)
(* For changed versions of this code, which are public distributed the    *)
(* following additional conditions have to be fullfilled:                 *)
(* 1) The header has to contain a comment on the change and the author of *)
(*    it.                                                                 *)
(* 2) A copy of the changed source has to be sent to the above E-Mail     *)
(*    address or my then valid address, if this is possible to the        *)
(*    author.                                                             *)
(* The second condition has the target to maintain an up to date central  *)
(* version of the component. If this condition is not acceptable for      *)
(* confidential or legal reasons, everyone is free to derive a component  *)
(* or to generate a diff file to my or other original sources.            *)
(**************************************************************************)


interface
uses Classes, SysUtils;

type
  CharsSet = set of Char;

  TMyParser = class
    protected
      FText : String;
      FCurrentChar : PChar;
      FCurrentPos : Integer;
      FLength : Integer;
      FLine : Integer;
      FOffset : Integer;
      FParenthesisDepth : Integer;
      FBracketDepth : Integer;
      FParsingComment : Boolean;

      procedure SetText( const val : String );

    public
      procedure RaiseErr( const s : String );
      procedure RaiseErrFmt( const s : String; args : array of const );
      function  ReadChar : Char;
      function  PeekChar : Char;
      function  PeekPeekChar : Char;
      function  IsSeparator( val : Char ) : Boolean;
      function  IsWhite( val : Char ) : Boolean;
      function  ReadSymbol : String;
      function  PeekSymbol : String;
      function  PeekCharAfterSymbol : String;
      function  ReadConstant : String;
      function  ReadType : String;
      function  ReadExpr : String;
      function  ReadUntil( chars : CharsSet ) : String;
      procedure SkipWhite;
      procedure SkipEndOfLine;
      procedure SkipSemiColon;

      property Text : String read FText write SetText;
      property Line : Integer read FLine;
      property Offset : Integer read FOffset;
      property ParenthesisDepth : Integer read FParenthesisDepth write FParenthesisDepth;
      property BracketDepth : Integer read FBracketDepth write FBracketDepth;
  end;

implementation

procedure TMyParser.SetText( const val : String );
begin
  FLength := length(val);
  FText := val;
  //FText := Copy(val,1,FLength);
  FCurrentPos := 0;
  FCurrentChar := PChar(FText);
  FLine := 1;
  FOffset := 1;
end;

procedure TMyParser.RaiseErr( const s : String );
var
  tmp : String;
begin
  tmp := Format('At line %d, offset %d: ', [Line, Offset]);
  raise Exception.Create( tmp + s );
end;

procedure TMyParser.RaiseErrFmt( const s : String; args : array of const );
var
  tmp : String;
begin
  tmp := Format('At line %d, offset %d: ', [Line, Offset]);
  raise Exception.CreateFmt( tmp + s, args );
end;

function TMyParser.ReadChar : Char;
begin
  if FCurrentPos = FLength then
    raise Exception.Create( 'End of buffer reached' );
  Result := FCurrentChar^;
  if not FParsingComment then
    begin
      if Result = '(' then
        Inc(FParenthesisDepth)
      else if (Result = ')') and (FParenthesisDepth > 0) then
        Dec(FParenthesisDepth)
      else if Result = '[' then
        Inc(FBracketDepth)
      else if (Result = ']') and (FBracketDepth > 0) then
        Dec(FBracketDepth);
    end;
  Inc( FCurrentChar );
  Inc( FCurrentPos );
  Inc( FOffset );
  if Result = #13 then
    begin
      Inc(FLine);
      FOffset := 1;
    end;
end;

function TMyParser.PeekChar : Char;
begin
  if FCurrentPos = FLength then
    raise Exception.Create( 'End of buffer reached' );
  Result := FCurrentChar^;
end;

function  TMyParser.PeekPeekChar : Char;
begin
  if FCurrentPos+1 >= FLength then
    raise Exception.Create( 'End of buffer reached' );
  Result := (FCurrentChar+1)^;
end;

function  TMyParser.IsSeparator( val : Char ) : Boolean;
begin
  Result := val in [' ', #9, #10, #13, ';', ',', '=', '(', ')'];
end;

function  TMyParser.IsWhite( val : Char ) : Boolean;
begin
  Result := val in [' ', #9, #13, #10];
end;

function  TMyParser.ReadSymbol : String;
begin
  Result := '';
  while PeekChar in ['a'..'z', 'A'..'Z', '0'..'9', '_'] do
    Insert( ReadChar, Result, Length(Result)+1 );
  if Result = '' then
    RaiseErr( 'Could not read a symbol' );
end;

function  TMyParser.PeekSymbol : String;
var
  old : PChar;
  old_i : Integer;
  old_ParDepth : Integer;
  old_BraDepth : Integer;
begin
  old := FCurrentChar;
  old_i := FCurrentPos;
  old_ParDepth := FParenthesisDepth;
  old_BraDepth := FBracketDepth;
  try
    Result:= ReadSymbol;
  except
    Result := '';
  end;
  FCurrentChar := old;
  FCurrentPos := old_i;
  FParenthesisDepth := old_ParDepth;
  FBracketDepth := old_BraDepth;
end;

function  TMyParser.PeekCharAfterSymbol : String;
var
  old : PChar;
  old_i : Integer;
  old_ParDepth : Integer;
  old_BraDepth : Integer;
begin
  old := FCurrentChar;
  old_i := FCurrentPos;
  old_ParDepth := FParenthesisDepth;
  old_BraDepth := FBracketDepth;
  try
    Result:= ReadSymbol;
    SkipWhite;
    Result:= PeekChar;
  except
    Result := '';
  end;
  FCurrentChar := old;
  FCurrentPos := old_i;
  FParenthesisDepth := old_ParDepth;
  FBracketDepth := old_BraDepth;
end;

function  TMyParser.ReadConstant : String;
begin
  if PeekChar = '''' then
    begin
      Result := ReadChar;
      while PeekChar <> '''' do
        Insert( ReadChar, Result, Length(Result) );
      Insert( ReadChar, Result, Length(Result) );
    end
  else if PeekChar = '-' then
    begin
      Result := ReadChar;
      Result := Result + ReadSymbol;
    end
  else
    Result := ReadSymbol;
  SkipWhite;
end;

function  TMyParser.ReadType : String;
begin
  Result := ReadConstant;
  if PeekChar = '[' then
    begin
      ReadChar; // pass the '['
      SkipWhite;
      Result := Result + '[' + ReadExpr + ']';
      if ReadChar <> ']' then
        RaiseErr('no "]" matching a "["');
      SkipWhite;
    end
  else if (PeekChar = '.') and (PeekPeekChar = '.' ) then
    begin
      ReadChar; // pass the '.'
      ReadChar; // pass the '.'
      SkipWhite;
      Result := Result + '..' + ReadExpr;
      SkipWhite;
    end;
end;

function  TMyParser.ReadExpr : String;
var
  BraDepth : Integer;

  function IsEndOfExpr : Boolean;
  var
    c : Char;
  begin
    c := PeekChar;
    if (c = '.') and (PeekPeekChar = '.') then
      Result := True
    else if c in [',', ';'] then
      Result := True
    else if (c = ']') and (FBracketDepth = BraDepth ) then
      Result := True
    else
      Result := False;
  end;
begin
  Result := '';
  BraDepth := FBracketDepth;
  while not IsEndOfExpr do
    begin
      // Check if it's a string
      if PeekChar = '''' then
        begin
          repeat
            Insert( ReadChar, Result, Length(Result)+1 );
          until PeekChar = '''';
          Insert( ReadChar, Result, Length(Result)+1 );
        end
      else if PeekChar = '"' then
        begin
          repeat
            Insert( ReadChar, Result, Length(Result)+1 );
          until PeekChar = '"';
          Insert( ReadChar, Result, Length(Result)+1 );
        end
      else
        Insert( ReadChar, Result, Length(Result)+1 );
    end;
  SkipWhite;
  if Result = '' then
    RaiseErr( 'Could not read an expr' );
end;

function  TMyParser.ReadUntil( chars : CharsSet ) : String;
begin
  Result := '';
  while not (PeekChar in chars) do
    begin
      // Check if it's a string
      if PeekChar = '''' then
        begin
          repeat
            Insert( ReadChar, Result, Length(Result)+1 );
          until PeekChar = '''';
          Insert( ReadChar, Result, Length(Result)+1 );
        end
      else if PeekChar = '"' then
        begin
          repeat
            Insert( ReadChar, Result, Length(Result)+1 );
          until PeekChar = '"';
          Insert( ReadChar, Result, Length(Result)+1 );
        end
      else if PeekChar = '{' then
        begin
          repeat
            ReadChar;
          until PeekChar = '}';
          ReadChar;
        end
      else
        Insert( ReadChar, Result, Length(Result)+1 );
    end;
  SkipWhite;
end;

procedure TMyParser.SkipWhite;

  function IsStartOfComment : Boolean;
  begin
    if ((PeekChar = '(') and (PeekPeekChar = '*')) or
       ((PeekChar = '/') and (PeekPeekChar = '/')) or
       (PeekChar = '{') then
      Result := True
    else
      Result := False;
  end;

  procedure SkipComments;
  var
    c : Char;
  begin
    FParsingComment := True;
    try
      if (PeekChar = '/') and (PeekPeekChar = '/' ) then
        SkipEndOfLine
      else if PeekChar = '{' then
        begin
          while ReadChar <> '}' do
            ;
        end
      else if (PeekChar = '(') and (PeekPeekChar = '*') then
        begin
          ReadChar;
          ReadChar;
          repeat
            c := ReadChar;
          until (c = '*') and (PeekChar = ')');
          ReadChar;
        end;
    finally
      FParsingComment := False;
    end;
  end;
begin
  repeat
    while IsWhite( PeekChar ) do
      ReadChar;
    repeat
      SkipComments;
    until not IsStartOfComment;
  until not IsWhite(PeekChar);
end;

procedure TMyParser.SkipEndOfLine;
begin
  while PeekChar <> #13 do
    ReadChar;
  if PeekChar = #10 then
    ReadChar;
end;

procedure TMyParser.SkipSemiColon;
begin
  SkipWhite;
  if PeekChar <> ';' then
    RaiseErr('Could not read a semi-colon' );
  ReadChar;
  SkipWhite;
end;


end.
