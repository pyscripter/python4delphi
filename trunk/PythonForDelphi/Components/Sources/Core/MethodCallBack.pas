(**************************************************************************)
(*                                                                        *)
(* Module:  Unit 'MethodCallback'      Copyright (c) 1998                 *)
(*                                                                        *)
(* Version: 0.0                        Dr. Dietmar Budelsky               *)
(* Sub-Version: 0.3                    dbudelsky@web.de                   *)
(*                                     Germany                            *)
(*                                                                        *)
(**************************************************************************)
(*  Functionality: Generates synthetic callback functions which calls     *)
(*  DELPHI Class Methods. A callback mechanism (DDE, PYTHON, TCL) can now *)
(*  use DELPHI objects.                                                   *)
(*                                                                        *)
(**************************************************************************)
(*  Contributors:                                                         *)
(*      Grzegorz Makarewicz (mak@mikroplan.com.pl)                        *)
(*      Morgan Martinet     (p4d@mmm-experts.com)                         *)
(*      Samuel Iseli        (iseli@vertec.ch)                             *)
(**************************************************************************)
(* This source code is distributed with no WARRANTY, for no reason or use.*)
(* Everyone is allowed to use and change this code free, as long as this  *)
(* header and its copyright text is intact.                               *)
(* Dr. Dietmar Budelsky, 1998-01-07                                       *)
(**************************************************************************)

{$I Definition.Inc}

unit MethodCallBack;

interface
uses SysUtils;

type
  TCallType = (ctSTDCALL, ctCDECL);
  TCallBack = procedure of object;
  TDDEAPIfunc = function( CallType, Fmt: Integer; Conv: longint;
                          hsz1, hsz2: longint;
                          Data: longint; Data1, Data2: integer):
                          longint of object; stdcall;
// Method declaration for DDE(ML) interface. Callbackmethods for DDE(ML) have
// to be declared according to this.

function GetDDECallBack(method: TDDEAPIfunc): Pointer;
// Call for example with
// GetDDECallBack(DDECallBackMethod);

function  GetCallBack( self: TObject; method: Pointer;
                       argnum: Integer; calltype: tcalltype): Pointer;
// Call for example with
// CallBackProc := GetCallBack( self, @TSelfObject.Method, 2, ctSTDCALL);
//
// "self" is a valid TSelfObject,
// "Method" is a pointer to the class method, which should be triggered,
// when CallBackProc is called. It has to be declared according to the
// calltype!
// argnum is the number of callback parameters. There are the following
// exceptions: Double and Currency count for two. (sure)
//             Float counts for two               (not tested yet)
//             Extended counts for three          (not tested yet)
//             Records count for SizeOf(record)/4 rounded up.
// calltype is the calling convention of the callback function.

function  GetOfObjectCallBack( CallBack: TCallBack;
                               argnum: Integer; calltype: TCallType): Pointer;
// More sophisticated interface for standardized callback mechanisms.
// Usage for example:
// type
// TMyCallBack = function(x: Integer):Integer of object; cdecl;
// TMyClass = Class
//   CallBackProc: Pointer;
//   function y(x: Integer):Integer; cdecl;
//   procedure Init;
// end;
// ...
// function SetCallBack(f: TMyCallBack): Pointer;
// begin
//   result := GetOfObjectCallBack( TCallBack(f), 1, ctCDECL);
// end;
// procedure TMyClass.Init;
// begin
//   CallBackProc := SetCallBack(y);
// end;

procedure DeleteCallBack( Proc: Pointer );
// frees the memory used for Proc. Call with
// DeleteCallBack( CallBackProc);

function CodeMemPageCount: integer;
// returns the page count allocated for callbacks
// mainly for test purposes

procedure FreeCallBacks;
// frees all callbacks
// is called on finalize unit
// should only be called explicitely for testing

implementation

uses
  Windows, Classes;

type
  PByte = ^Byte;

  PCodeMemBlock = ^TCodeMemBlock;
  TCodeMemBlock = packed record
    Next: PCodeMemBlock;
    // code length is variable
    Code: array[0..1] of byte;
  end;

  PCodeMemPage = ^TCodeMemPage;
  TCodeMemPage = packed record
    Next: PCodeMemPage;
    CodeBlocks: PCodeMemBlock;
  end;

const
  PageSize = 4096;

var
  CodeMemPages: PCodeMemPage;

procedure GetCodeMem(var ptr: PByte; size: integer);
var
  page: PCodeMemPage;
  block: PCodeMemBlock;
begin
  //---allocates Block from executable memory
  // executable memory is requested in pages via VirtualAlloc
  // handed back in blocks of requested size

  // determine if there is already a page assigned and
  // that it has enough space requested block
  page:=CodeMemPages;
  if (page = nil) or (Longint(CodeMemPages^.CodeBlocks) - Longint(Pointer(CodeMemPages)) <= (size + 3*sizeof(PCodeMemBlock))) then
  begin
    // allocate new Page
    page:=VirtualAlloc(nil, PageSize, MEM_COMMIT, PAGE_EXECUTE_READWRITE);
    page^.next:=CodeMemPages;
    CodeMemPages:=page;
    // init pointer to end of page
    page^.CodeBlocks:=Pointer(Longint(page) + PageSize);
  end;

  //---blocks are assigned starting from the end of the page
  block:=Pointer(Longint(page^.codeBlocks) - (size + sizeof(PCodeMemBlock)));
  block^.Next:=page^.CodeBlocks;
  page^.CodeBlocks:=block;

  ptr:=@(block^.Code[0]);
end;

procedure FreeCodeMem(ptr: Pointer);
var
  page, lastpage: PCodeMemPage;
  block, lastblock: PCodeMemBlock;
begin
  //---freeing code mem is not very efficient
  // we need to search through all the assigned blocks
  // A page is only released when all blocks in it have been freed
  page:=CodeMemPages;
  lastblock:=nil;
  lastpage:=nil;
  if page <> nil then
    block:=page^.CodeBlocks
  else
    block:=niL;

  while page <> nil do
  begin
    while Longint(block) < (Longint(page) + pagesize) do
    begin
      if @(block^.Code[0]) = ptr then
      begin
        // we found our block
        // remove it
        if lastblock <> nil then
          lastblock^.Next:=block^.Next
        else
          page^.CodeBlocks:=block^.Next;

        // return the page if it is empty
        if Longint(page^.CodeBlocks) = Longint(page) + pagesize then
        begin
          if lastpage <> nil then
            lastpage^.Next:=page^.Next
          else
            CodeMemPages:=page^.Next;

          // free the memory
          VirtualFree(page, 0, MEM_RELEASE);
        end;

        exit;
      end;
      lastblock:=block;
      block:=block^.Next;
    end;
    lastpage:=page;
    page:=page^.Next;
    block:=page^.CodeBlocks;
    lastblock:=nil;
  end;

end;

function CodeMemPageCount: integer;
var
  page: PCodeMemPage;
begin
  //---counts the used codemem pages
  result:=0;
  page:=CodeMemPages;

  while page <> nil do
  begin
    inc(result);
    page:=page^.Next;
  end;
end;

function GetDDECallBack(method: TDDEAPIfunc): Pointer;
begin
  result := GetOfObjectCallBack(TCallBack(method),8,ctSTDCALL);
end;

function  GetOfObjectCallBack( CallBack: TCallBack;
                               argnum: Integer; calltype: TCallType): Pointer;
begin
  result := GetCallBack( TObject(TMethod(CallBack).Data),
                         TMethod(CallBack).Code,
                         argnum, calltype);
end;

function  GetCallBack( self: TObject; method: Pointer;
                       argnum: Integer; calltype: tcalltype): Pointer;
const
// Short handling of stdcalls:
S1: array [0..14] of byte = (
$5A,            //00  pop  edx  // pop return address
$B8,0,0,0,0,    //01  mov  eax, self
$50,            //06  push eax
$52,            //07  push edx // now push return address
// call the real callback
$B8,0,0,0,0,    //08  mov  eax, Method
$FF,$E0);       //13  jmp  eax

//Handling for ctCDECL:
C1: array [0..2] of byte = (
// begin of call
$55,            //00      push ebp
$8B,$EC);       //01      mov  ebp, esp

// push arguments
//  for i:= argnum-1 downto 0 do begin
C2: array [0..3] of byte = (
$8B,$45,0,      //03+4*s  mov eax,[ebp+8+4*i]
$50);           //06+4*s  push eax
//  end;

// self parameter
C3: array [0..17] of byte = (
$B8,0,0,0,0,    //03+4*s  mov eax, self
$50,            //08+4*s  push eax
// call the real callback
$B8,0,0,0,0,    //09+4*s  mov  eax,Method
$FF,$D0,        //14+4*s  call eax
// clear stack
$83,$C4,0,      //16+4*s  add esp, 4+bytes
$5D,            //19+4*s  pop  ebp
$C3);           //20+4*s  ret
var
  bytes: Word;
  i: Integer;
  P,Q: PByte;
begin
  if calltype = ctSTDCALL then begin
    GetCodeMem(Q,15);
    P := Q;
    move(S1,P^,SizeOf(S1));
    Inc(P,2);
    move(self,P^,SizeOf(self));
    Inc(P,7);
    move(method,P^,SizeOf(method));
    {Inc(P,6); End of proc}
  end else begin  {ctCDECL}
    bytes := argnum * 4;
    GetCodeMem(Q,21+4*argnum);
    P := Q;
    move(C1,P^,SizeOf(C1));
    Inc(P,SizeOf(C1));
    for i:=argnum-1 downto 0 do begin
      move(C2,P^,SizeOf(C2));
      Inc(P,2);
      P^:=8+4*i;
      Inc(P,2);
    end;
    move(C3,P^,SizeOf(C3));
    Inc(P,1);
    move(self,P^,SizeOf(self));
    Inc(P,6);
    move(method,P^,SizeOf(method));
    Inc(P,8);
    P^ := 4+bytes;
    {Inc(P,3); End of proc}
  end;
  result := Q;
end;

procedure DeleteCallBack( Proc: Pointer);
begin
  FreeCodeMem(Proc);
end;

procedure FreeCallBacks;
var
  page, nextpage: PCodeMemPage;
begin
  // free each allocated page
  page := CodeMemPages;
  while page <> nil do
  begin
    nextpage := page^.Next;

    // free the memory
    VirtualFree(page, 0, MEM_RELEASE);

    page := nextpage;
  end;
  CodeMemPages := nil;
end;

initialization
finalization
  FreeCallBacks;
end.
