unit NewMemoryManager;

interface
var
  GetMemCount: Integer;
  FreeMemCount: Integer;
  ReallocMemCount: Integer;

implementation

var
  OldMemMgr: TMemoryManager;

function NewGetMem(Size: Integer): Pointer;
var
  ptr : PChar;
begin
  Inc(GetMemCount);
  ptr := OldMemMgr.GetMem(Size+4);
  //Inc(ptr, 4);
  Result := ptr;
end;

function NewFreeMem(P: Pointer): Integer;
var
  ptr : PChar;
begin
  Inc(FreeMemCount);
  ptr := P;
  //Dec(ptr, 4);
  Result := OldMemMgr.FreeMem(ptr);
end;

function NewReallocMem(P: Pointer; Size: Integer): Pointer;
var
  ptr : PChar;
begin
  Inc(ReallocMemCount);
  ptr := P;
  //Dec(ptr, 4);
  Result := OldMemMgr.ReallocMem(ptr, Size+4);
end;

const
  NewMemMgr: TMemoryManager = (
  GetMem: NewGetMem;
  FreeMem: NewFreeMem;
  ReallocMem: NewReallocMem);

procedure SetNewMemMgr;
begin
  GetMemoryManager(OldMemMgr);
  SetMemoryManager(NewMemMgr);
end;

procedure SetOldMemMgr;
begin
  SetMemoryManager(OldMemMgr);
end;

initialization
  SetNewMemMgr;
finalization
  SetOldMemMgr;
end.
 