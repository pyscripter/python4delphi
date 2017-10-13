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
(*      Andrey Gruzdev      (andrey.gruzdev@gmail.com)                    *)
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
  {$IFDEF MSWINDOWS}
  Windows,
  {$ELSE WINDOWS}
  {$IFDEF FPC}
  BaseUnix,
  {$ELSE}
  Posix.SysMMan,
  {$ENDIF}
  {$ENDIF WINDOWS}
  Classes;

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
  
type
{$IFDEF FPC}
  PtrCalcType = PtrUInt;
{$ELSE}
  {$IFDEF CPUX64}
  PtrCalcType = NativeInt;
  {$ELSE}
  PtrCalcType = Longint;
  {$ENDIF}
{$ENDIF}

{$IFNDEF MSWINDOWS}
{$IFDEF FPC}
function mprotect(Addr: Pointer; Len: Integer; Prot: Integer): Integer; cdecl;
  external 'c' name 'mprotect';
const
//  PROT_NONE   =0;
//  PROT_READ   =1;
//  PROT_WRITE  =2;
//  PROT_EXEC   =4;
  MAP_PRIVATE =2;
  MAP_ANON=$1000;  
{$ENDIF}
{$ENDIF}

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
  if (page = nil) or (PtrCalcType(CodeMemPages^.CodeBlocks) - PtrCalcType(Pointer(CodeMemPages)) <= (size + 3*sizeof(PCodeMemBlock))) then
  begin
    // allocate new Page
	{$IFDEF MSWINDOWS}	
    page:=VirtualAlloc(nil, PageSize, MEM_COMMIT, PAGE_EXECUTE_READWRITE);
	{$ELSE}
    page := GetMem(PageSize);
    //page := fpmmap(nil, PageSize, PROT_NONE, MAP_ANONYMOUS, -1, 0);
    mprotect(page, PageSize, PROT_READ or PROT_WRITE or PROT_EXEC);
	{$ENDIF}	
    page^.next:=CodeMemPages;
    CodeMemPages:=page;
    // init pointer to end of page
    page^.CodeBlocks:=Pointer(PtrCalcType(page) + PageSize);
  end;

  //---blocks are assigned starting from the end of the page
  block:=Pointer(PtrCalcType(page^.codeBlocks) - (size + sizeof(PCodeMemBlock)));
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
    while PtrCalcType(block) < (PtrCalcType(page) + pagesize) do
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
        if PtrCalcType(page^.CodeBlocks) = PtrCalcType(page) + pagesize then
        begin
          if lastpage <> nil then
            lastpage^.Next:=page^.Next
          else
            CodeMemPages:=page^.Next;

          // free the memory
	  	  {$IFDEF MSWINDOWS}
          VirtualFree(page, 0, MEM_RELEASE);
		  {$ELSE}
          FreeMem(page);
          //fpmunmap(page,PageSize);
		  {$ENDIF}		  
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

{$IFDEF CPUX64}
{$DEFINE 64_BIT_CALLBACK}
{$ELSE}
{$IFDEF MACOS}
{$DEFINE ALIGNED_32_BIT_CALLBACK}
{$ELSE}
{$DEFINE SIMPLE_32_BIT_CALLBACK}
{$ENDIF MACOS}
{$ENDIF CPUX64}

{$IFDEF SIMPLE_32_BIT_CALLBACK}
// win32 inplementation
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
{$ENDIF SIMPLE_32_BIT_CALLBACK}

{$IFDEF 64_BIT_CALLBACK}
function  GetCallBack( self: TObject; method: Pointer;
                       argnum: Integer; calltype: tcalltype): Pointer;
const
{$IFDEF MSWINDOWS}
   RegParamCount = 4;
   ShadowParamCount = 4;
{$ELSE}
   RegParamCount = 6;
   ShadowParamCount   = 0;
{$ENDIF}

Size32Bit = 4;
Size64Bit = 8;

ShadowStack   = ShadowParamCount * Size64Bit;
SkipParamCount = RegParamCount - ShadowParamCount;

StackSrsOffset = 3;
c64stack: array[0..14] of byte = (
$48, $81, $ec, 00, 00, 00, 00,//     sub rsp,$0
$4c, $89, $8c, $24, ShadowStack, 00, 00, 00//     mov [rsp+$20],r9
);

CopySrcOffset=4;
CopyDstOffset=4;
c64copy: array[0..15] of byte = (
$4c, $8b, $8c, $24,  00, 00, 00, 00,//     mov r9,[rsp+0]
$4c, $89, $8c, $24, 00, 00, 00, 00//     mov [rsp+0],r9
);

RegMethodOffset = 10;
{$IFDEF MSWINDOWS}
RegSelfOffset = 11;
c64regs: array[0..28] of byte = (
$4d, $89, $c1,      //   mov r9,r8
$49, $89, $d0,      //   mov r8,rdx
$48, $89, $ca,      //   mov rdx,rcx
$48, $b9, 00, 00, 00, 00, 00, 00, 00, 00, // mov rcx, self
$48, $b8, 00, 00, 00, 00, 00, 00, 00, 00 // mov rax, method
);
{$ELSE}
RegSelfOffset = 17;
c64regs: array[0..34] of byte = (
$4d, $89, $c1,      //   mov r9,r8
$49, $89, $c8,      //   mov r8,rcx
$48, $89, $d1,      //   mov rcx,rdx
$48, $89, $f2,      //   mov rdx,rsi
$48, $89, $fe,      //   mov rsi,rdi
$48, $bf, 00, 00, 00, 00, 00, 00, 00, 00, // mov rdi, self
$48, $b8, 00, 00, 00, 00, 00, 00, 00, 00 // mov rax, method
);
{$ENDIF}


c64jump: array[0..2] of byte = (
$48, $ff, $e0  // jump rax
);

CallOffset = 6;
c64call: array[0..10] of byte = (
$48, $ff, $d0,    //    call rax
$48, $81,$c4,  00, 00, 00, 00,   //     add rsp,$0
$c3// ret
);
var
  i: Integer;
  P,PP,Q: PByte;
  lCount : integer;
  lSize : integer;
  lOffset : integer;
begin
    lCount := SizeOf(c64regs);
    if argnum>=RegParamCount then
       Inc(lCount,sizeof(c64stack)+(argnum-RegParamCount)*sizeof(c64copy)+sizeof(c64call))
    else
       Inc(lCount,sizeof(c64jump));

    GetCodeMem(Q,lCount);
    P := Q;

    lSize := 0;
    if argnum>=RegParamCount then
    begin
        lSize := ( 1+ ((argnum + 1 - SkipParamCount) div 2) * 2 )* Size64Bit;   // 16 byte stack align

        pp := p;
        move(c64stack,P^,SizeOf(c64stack));
        Inc(P,StackSrsOffset);
        move(lSize,P^,Size32Bit);
        p := pp;
        Inc(P,SizeOf(c64stack));
        for I := 0 to argnum - RegParamCount -1 do
        begin
            pp := p;
            move(c64copy,P^,SizeOf(c64copy));
            Inc(P,CopySrcOffset);
            lOffset := lSize + (i+ShadowParamCount+1)*Size64Bit;
            move(lOffset,P^,Size32Bit);
            Inc(P,CopyDstOffset+Size32Bit);
            lOffset := (i+ShadowParamCount+1)*Size64Bit;
            move(lOffset,P^,Size32Bit);
            p := pp;
            Inc(P,SizeOf(c64copy));
        end;
    end;

    pp := p;
    move(c64regs,P^,SizeOf(c64regs));
    Inc(P,RegSelfOffset);
    move(self,P^,SizeOf(self));
    Inc(P,RegMethodOffset);
    move(method,P^,SizeOf(method));
    p := pp;
    Inc(P,SizeOf(c64regs));

    if argnum<RegParamCount then
      move(c64jump,P^,SizeOf(c64jump))
    else
    begin
      move(c64call,P^,SizeOf(c64call));
      Inc(P,CallOffset);
      move(lSize,P^,Size32Bit);
    end;
  result := Q;
end;
{$ENDIF 64_BIT_CALLBACK}

{$IFDEF ALIGNED_32_BIT_CALLBACK}
// 32 bit with stack align
function  GetCallBack( self: TObject; method: Pointer;
                       argnum: Integer; calltype: tcalltype): Pointer;
const

//Handling for ctCDECL:
C1: array [0..5] of byte = (
// begin of call
$55,            //00      push ebp
$8B,$EC,        //01      mov  ebp, esp
$83,$EC,$0);    //03      sub  esp, align

// push arguments
//  for i:= argnum-1 downto 0 do begin
C2: array [0..3] of byte = (
$8B,$45,0,      //06+4*s  mov eax,[ebp+8+4*i]
$50);           //09+4*s  push eax
//  end;

// self parameter
C3: array [0..19] of byte = (
$B8,0,0,0,0,    //06+4*s  mov eax, self
$50,            //11+4*s  push eax
// call the real callback
$B8,0,0,0,0,    //12+4*s  mov  eax,Method
$FF,$D0,        //17+4*s  call eax
// clear stack
$83,$C4,0,      //20+4*s  add esp, 4+bytes+align
$5D,            //23+4*s  pop  ebp
$C2,00,00);           //24+4*s  ret   [0]

var
  bytes: Word;
  i: Integer;
  P,Q: PByte;
  align : integer;
begin
// On mac FPC ctSTDCALL and ctCDECL are the same
    {$IFDEF FPC}
    {$IFDEF MACOS32}
    calltype := ctCDECL;
    {$ENDIF}
    {$ENDIF}

    bytes := argnum * 4;
	  align :=  ($10 - (bytes + 4{self} + 4{address} + 4{push bp}) and $f) and $f; // align to $10 for Mac compatibility

    GetCodeMem(Q,sizeof(c1)+sizeof(c3)+sizeof(c2)*argnum);
    P := Q;
    move(C1,P^,SizeOf(C1));
    Inc(P,SizeOf(C1)-1);
	  p^ := align;
    Inc(P);
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
    if calltype = ctCDECL then
    begin
       P^ := 4+bytes+align;
    end
    else
    begin
       P^ := {4+}align;
       Inc(P,3);
       P^ := bytes;
    end;


    result := Q;
end;
{$ENDIF}

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
  {$IFDEF MSWINDOWS}
    VirtualFree(page, 0, MEM_RELEASE);
  {$ELSE}
    FreeMem(page);
    //fpmunmap(page,PageSize);
  {$ENDIF}		  

    page := nextpage;
  end;
  CodeMemPages := nil;
end;

initialization
finalization
  FreeCallBacks;
end.
