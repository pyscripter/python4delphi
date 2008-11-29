unit unHash;

interface

uses Classes, SysUtils;

const
    htDefaultHashTableSize = 512;
    htMaxArraySize = 32000;

type
    TDynamicArray = array[0..htMaxArraySize] of TStringList;
    THTArray = array [0 .. htDefaultHashTableSize] of TStringList;
    TProcForEach = procedure ( Container : TObject; key : String; value : TObject ) of object;

    THashTable = class( TObject )
    private
    protected
       FTable : ^THTArray;
       FCount : Integer;
       FOwnsObjects : Boolean;
       FTableSize : Integer;

       procedure AllocArray;
       procedure FreeArray;
       function  Hash( const key : String ) : Integer; virtual;
       function  GetIndex( const key : String ) : Integer;

    public
       constructor Create( IsOwner : Boolean );
       constructor CreateWith( aSize : Integer; IsOwner : Boolean );
       destructor  Destroy; override;

       procedure Add( const key : String; obj : TObject );
       procedure Remove( const key : String );
       function  Find( const key : String ) : TObject;
       procedure Clear;
       procedure Print;
       procedure ForEach( proc : TProcForEach );

       property Count : Integer read FCount;
       property OwnsObjects : Boolean read FOwnsObjects write FOwnsObjects default False;
    end;

implementation

uses Dialogs;

procedure THashTable.AllocArray;
begin
  FTable := AllocMem(FTableSize * SizeOf(Pointer));
end;

procedure THashTable.FreeArray;
begin
  FreeMem( FTable, FTableSize * SizeOf(Pointer));
end;

function THashTable.Hash( const key : String ) : Integer;
var
   i : Integer;
begin
     result := 0;
     for i := 1 to Length( key ) do
         result := ( 32 * result + Ord( key[i] ) ) mod htDefaultHashTableSize;
end;

function THashTable.GetIndex( const key : String ) : Integer;
begin
     result := Hash( key );
end;

constructor THashTable.Create( IsOwner : Boolean );
begin
     inherited Create;
     FOwnsObjects := IsOwner;
     FTableSize := htDefaultHashTableSize;
     AllocArray;
end;

constructor THashTable.CreateWith( aSize : Integer; IsOwner : Boolean );
begin
     inherited Create;
     FOwnsObjects := IsOwner;
     if aSize < htMaxArraySize then
       FTableSize := aSize
     else
       FTableSize := htMaxArraySize;
     AllocArray;
end;

destructor  THashTable.Destroy;
begin
     Clear;
     FreeArray;
     inherited Destroy;
end;

procedure THashTable.Add( const key : String; obj : TObject );
var
   idx : Integer;
begin
     idx := GetIndex( UpperCase(key) );
     if not Assigned( FTable[idx] ) then begin
        FTable^[idx] := TStringList.Create;
        FTable^[idx].AddObject( UpperCase(key), obj );
     end
     else begin
          if FTable^[idx].IndexOf( key ) = -1 then
             FTable^[idx].AddObject( UpperCase(key), obj );
     end;

end;

procedure THashTable.Remove( const key : String );
var
   idx : Integer;
   item : Integer;
begin
     idx := GetIndex( UpperCase(key) );
     if Assigned( FTable^[idx] ) then begin
        item := FTable^[idx].IndexOf( key );
        if item >= 0 then begin
           if OwnsObjects then
              FTable^[idx].Objects[item].Free;
           FTable^[idx].Delete( item );
        end;
     end
end;

function  THashTable.Find( const key : String ) : TObject;
var
   idx : Integer;
   item : Integer;
begin
     result := nil;
     idx := GetIndex( UpperCase(key) );
     if Assigned( FTable^[idx] ) then begin
        item := FTable^[idx].IndexOf( key );
        if item >= 0 then
           result := FTable^[idx].Objects[item];
     end;
end;

procedure THashTable.Clear;
var
   i : Integer;
   j : Integer;
begin
     for i := 0 to FTableSize - 1 do begin
         if Assigned( FTable^[i] ) then begin
            if OwnsObjects then begin
               for j := 0 to FTable^[i].Count - 1 do
                   FTable^[i].Objects[j].Free;
            end;
            FTable^[i].Free;
         end;
     end;
end;

procedure THashTable.Print;
var
  i, j : Integer;
begin
     for i := 0 to FTableSize - 1 do begin
         if Assigned( FTable^[i] ) then begin
           for j := 0 to FTable^[i].Count-1 do
             ShowMessage( 'Item: '+FTable^[i].Strings[j] );
         end;
     end;
end;

procedure THashTable.ForEach( proc : TProcForEach );
var
  i, j : Integer;
begin
     for i := 0 to FTableSize - 1 do begin
         if Assigned( FTable^[i] ) then begin
           for j := 0 to FTable^[i].Count-1 do
             proc( Self, FTable^[i].Strings[j], FTable^[i].Objects[j] );
         end;
     end;
end;

end.
