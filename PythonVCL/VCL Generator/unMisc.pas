unit unMisc;

interface
uses Classes, Grids;

  function  ReadInteger( S : TStream ) : Integer;
  procedure WriteInteger( S : TStream; val : Integer );
  function  ReadBoolean( S : TStream ) : Boolean;
  procedure WriteBoolean( S : TStream; val : Boolean );
  function  ReadString( S : TStream ) : String;
  procedure WriteString( S : TStream; val : String );

  procedure RemoveLine( sg : TStringGrid; idx : Integer );
  function  AppendSlash( const str : String ) : String;

implementation

/////////////////////////////////////////
// Misc functions
/////////////////////////////////////////

function ReadInteger( S : TStream ) : Integer;
begin
  S.Read( Result, Sizeof(Result) );
end;

procedure WriteInteger( S : TStream; val : Integer );
begin
  S.Write( val, Sizeof(val) );
end;

function ReadBoolean( S : TStream ) : Boolean;
begin
  S.Read( Result, Sizeof(Result) );
end;

procedure WriteBoolean( S : TStream; val : Boolean );
begin
  S.Write( val, Sizeof(val) );
end;


function ReadString( S : TStream ) : String;
var
  len : Integer;
begin
  len := ReadInteger(S);
  SetString(Result, PChar(nil), len);
  S.Read(Pointer(Result)^, len);
end;

procedure WriteString( S : TStream; val : String );
begin
  WriteInteger( S, Length(val) );
  S.Write(Pointer(val)^, Length(val));
end;

procedure RemoveLine( sg : TStringGrid; idx : Integer );
var
  i, j : Integer;
begin
  with sg do
    begin
      if idx >= RowCount - 1 then
        Exit;
      Objects[0,idx].Free;
      Objects[0,idx] := nil;
      for i := idx+1 to RowCount - 1 do
        for j := 0 to ColCount - 1 do
          begin
            Cells[j, i-1]   := Cells[j, i];
            Objects[j, i-1] := Objects[j, i];
          end;
      RowCount := RowCount - 1;
    end;
end;

function  AppendSlash( const str : String ) : String;
begin
  if (Length(str)>0) and (str[Length(str)]<>'\') then
    Result := str + '\'
  else
    Result := str;
end;


end.
