(**************************************************************************)
(*                                                                        *)
(* Module:  Unit 'MethodCallbackTest'  Copyright (c) 2006                 *)
(*                                                                        *)
(* Version: 0.0                        Vertec AG, Samuel Iseli            *)
(*                                     samuel.iseli@vertec.ch             *)
(*                                     Zurich, Switzerland                *)
(*                                                                        *)
(**************************************************************************)
(* Test unit for MethodCallback                                           *)
(**************************************************************************)
(* This source code is distributed with no WARRANTY, for no reason or use.*)
(* Everyone is allowed to use and change this code free, as long as this  *)
(* header and its copyright text is intact.                               *)
(* Samuel Iseli 2006-02-27                                                *)
(**************************************************************************)

unit MethodCallBackTest;

interface

uses
  DUnitX.TestFramework,
  MethodCallback;

implementation


type
  TTwoArgStdFunction = function (arg1, arg2: string): integer; stdcall;
  TThreeArgCDeclProcedure = procedure (arg1, arg2, arg3: string); cdecl;

  TFourArgStdFunction = function(arg1, arg2, arg3, arg4: integer): integer; stdcall;
  TFiveArgCdeclFunction = function(arg1, arg2, arg3, arg4, arg5: integer): integer; cdecl;

  TMyFuncCallback = function(arg1, arg2: string): integer of object; stdcall;
  TMyProcCallback = procedure (arg1, arg2, arg3: string) of object; cdecl;


  TTestObj = class
  public
    Argument1: string;
    Argument2: string;
    Argument3: string;
    function TwoArgStdFunction(arg1, arg2: string): integer; stdcall;
    procedure ThreeArgCdeclProcedure(arg1, arg2, arg3: string); cdecl;
    function FourArgStdFunction(arg1, arg2, arg3, arg4: integer): integer; stdcall;
    function FiveArgCdeclFunction(arg1, arg2, arg3, arg4, arg5: integer): integer; cdecl;
  end;

  [TestFixture]
  TMethodCallbackTest = class
  private
    fTestObj: TTestObj;
  public
    [SetupFixture]
    procedure SetupFixture;
    [TearDownFixture]
    procedure Teardown;
    [Test]
    procedure TestDeleteOnEmptyAllocator;
    [Test]
    procedure TestCallBackStdCall;
    [Test]
    procedure TestCallBackCDecl;
    [Test]
    procedure TestOfObjectCallBackStdCall;
    [Test]
    procedure TestOfObjectCallBackCDecl;
    [Test]
    procedure TestDeleteCallBack;
    [Test]
    procedure TestFourArgStdFunction;
    [Test]
    procedure TestFiveArgCdeclFunction;
    [Test]
    procedure TestMemoryMgmt;
    [Test]
    procedure TestBug01;
  end;

{ TTestObj }

function TTestObj.FiveArgCdeclFunction(arg1, arg2, arg3, arg4,
  arg5: integer): integer;
begin
  Result := arg1 * arg4 + arg2 * arg5 + arg3;
end;

function TTestObj.FourArgStdFunction(arg1, arg2, arg3, arg4: integer): integer;
begin
  Result := arg1 * arg3 + arg2 * arg4;
end;

procedure TTestObj.ThreeArgCdeclProcedure(arg1, arg2, arg3: string);
begin
  Argument1:=arg1;
  Argument2:=arg2;
  Argument3:=arg3;
end;

function TTestObj.TwoArgStdFunction(arg1, arg2: string): integer;
begin
  Argument1:=arg1;
  Argument2:=arg2;
  result:=1;
end;

{ TMethodCallbackTest }

{ TMethodCallbackTest }

procedure TMethodCallbackTest.SetupFixture;
begin
  fTestObj:=TTestObj.Create;
end;

procedure TMethodCallbackTest.Teardown;
begin
  fTestObj.Free;
  FreeCallBacks;
end;

procedure TMethodCallbackTest.TestBug01;
const
   AllocCount = {$IFDEF CPUX64}{$IFDEF MSWINDOWS}51{$ELSE}88{$ENDIF}{$ELSE}90{$ENDIF};
var
  i: integer;
  ptr: Pointer;
begin
  {
I discovered a severe Bug in my memory manager code in MethodCallbacks.
The pointer arithmetic when checking free space in an allocated memory block is wrong.

The line that checks for free space in the current page is:

  if (page = nil) or (Longint(CodeMemPages^.CodeBlocks) -
Longint(Pointer(CodeMemPages)) <= (size + sizeof(PCodeMemBlock))) then

but it should be

  if (page = nil) or (Longint(CodeMemPages^.CodeBlocks) -
Longint(Pointer(CodeMemPages)) <= (size + 3*sizeof(PCodeMemBlock))) then

The old version didn't count the pointers of the TCodeMemPage structure (Next and Codeblocks). 
The error causes access violations on allocating the last block in a page when the requested size is smaller then the supposed (wrong) free space +8.

Therefore it doesn't occur very often and the tests didn't catch it.
I'm sorry about that. Hope it didn't cause too many problems up to now.
  }
  
  //---Test the code-memory manager

  FreeCallBacks;
  Assert.AreEqual(0, CodeMemPageCount);

  for i:=1 to AllocCount do
    ptr:=GetCallBack(fTestObj, @TTestObj.ThreeArgCdeclProcedure, 5, ctCdecl);

  // there should still be 1 page allocated
  Assert.AreEqual(1, CodeMemPageCount);

  // get one callback more and we should have 2 pages
  ptr:=GetCallBack(fTestObj, @TTestObj.ThreeArgCdeclProcedure, 5, ctCdecl);
  // getting CodeMemPageCount would crash as the next page pointer was overwritten by the
  // last allocation
  Assert.AreEqual(2, CodeMemPageCount);

end;

procedure TMethodCallbackTest.TestCallBackCDecl;
var
  ptr: pointer;
  proc: TThreeArgCdeclProcedure;
begin
  ptr:=GetCallBack(fTestObj, @TTestObj.ThreeArgCdeclProcedure, 3, ctCdecl);

  //---call method through pointer
  proc:=TThreeArgCdeclProcedure(ptr);

  proc('first arg', 'second arg', 'third arg');

  Assert.AreEqual('first arg', fTestObj.Argument1);
  Assert.AreEqual('second arg', fTestObj.Argument2);
  Assert.AreEqual('third arg', fTestObj.Argument3);

end;

procedure TMethodCallbackTest.TestCallBackStdCall;
var
  ptr: pointer;
  func: TTwoArgStdFunction;
begin
  ptr:=GetCallBack(fTestObj, @TTestObj.TwoArgStdFunction, 2, ctStdcall);

  //---call method through pointer
  func:=TTwoArgStdFunction(ptr);

  Assert.AreEqual(1, func('first arg', 'second arg'));
  Assert.AreEqual(string('first arg'), fTestObj.Argument1);
  Assert.AreEqual(string('second arg'), fTestObj.Argument2);
end;

procedure TMethodCallbackTest.TestDeleteCallBack;
var
  ptr1, ptr2, ptr3: Pointer;
  proc: TThreeArgCdeclProcedure;
  func: TTwoArgStdFunction;
begin
  //---we create 3 callbacks and delete them.
  // if there aren't any AV, we assume it works...
  ptr1:=GetCallBack(fTestObj, @TTestObj.ThreeArgCdeclProcedure, 3, ctCdecl);
  ptr2:=GetCallBack(fTestObj, @TTestObj.TwoArgStdFunction, 2, ctStdcall);
  DeleteCallBack(ptr1);
  ptr1:=GetCallBack(fTestObj, @TTestObj.TwoArgStdFunction, 2, ctStdcall);
  ptr3:=GetCallBack(fTestObj, @TTestObj.ThreeArgCdeclProcedure, 3, ctCdecl);

  func:=TTwoArgStdFunction(ptr1);
  func('arg1', 'arg2');
  func:=TTwoArgStdFunction(ptr2);
  func('arg1', 'arg2');
  proc:=TThreeArgCdeclProcedure(ptr3);
  proc('arg1', 'arg2', 'arg3');

  DeleteCallBack(ptr1);
  DeleteCallBack(ptr2);
  DeleteCallback(ptr3);
  Assert.Pass;
end;

procedure TMethodCallbackTest.TestDeleteOnEmptyAllocator;
var
  ptr1 : Pointer;
begin
  ptr1 := nil;
  DeleteCallBack(ptr1);
  Assert.Pass();
end;

procedure TMethodCallbackTest.TestFiveArgCdeclFunction;
Var
  CallBack : TFiveArgCdeclFunction;
begin
   CallBack := GetCallBack(fTestObj, @TTestObj.FiveArgCdeclFunction, 5, ctCDECL);
   Assert.AreEqual(CallBack(1,2,3,4,5), 1*4+2*5+3);
   DeleteCallBack(@CallBack);
end;

procedure TMethodCallbackTest.TestFourArgStdFunction;
Var
  CallBack : TFourArgStdFunction;
begin
   CallBack := GetCallBack(fTestObj, @TTestObj.FourArgStdFunction, 4, ctSTDCALL);
   Assert.AreEqual(CallBack(1,2,3,4), 1*3+2*4);
   DeleteCallBack(@CallBack);
end;

procedure TMethodCallbackTest.TestMemoryMgmt;
const
   AllocCount = {$IFDEF CPUX64}{$IFDEF MSWINDOWS}101{$ELSE}88{$ENDIF}{$ELSE}110{$ENDIF};
var
  i: integer;
  ptr, ptr1, ptr2: Pointer;
begin
  //---Test the code-memory manager

  // one ThreeArgDecl callback uses 33 bytes code + 4 bytes Next pointer = 37 bytes
  // we should be able to allocate 110 Callbacks per page

  FreeCallBacks;
  Assert.AreEqual(0, CodeMemPageCount);

  for i:=1 to AllocCount do
    ptr:=GetCallBack(fTestObj, @TTestObj.ThreeArgCdeclProcedure, 3, ctCdecl);

  // there should still be 1 page allocated
  Assert.AreEqual(1, CodeMemPageCount);

  // get one callback more and we should have 2 pages
  ptr:=GetCallBack(fTestObj, @TTestObj.ThreeArgCdeclProcedure, 3, ctCdecl);
  Assert.AreEqual(2, CodeMemPageCount);

  // get some more memory
  ptr1:=GetCallBack(fTestObj, @TTestObj.ThreeArgCdeclProcedure, 3, ctCdecl);
  ptr2:=GetCallBack(fTestObj, @TTestObj.ThreeArgCdeclProcedure, 3, ctCdecl);
  Assert.AreEqual(2, CodeMemPageCount);


  // now Free the callbacks on page 2
  DeleteCallBack(ptr1);
  Assert.AreEqual(2, CodeMemPageCount);
  DeleteCallBack(ptr);
  Assert.AreEqual(2, CodeMemPageCount);
  DeleteCallBack(ptr2);
  // page count should be back to 1
  Assert.AreEqual(1, CodeMemPageCount);

  // allocate one more and page count should go up to 2 again
  ptr:=GetCallBack(fTestObj, @TTestObj.ThreeArgCdeclProcedure, 3, ctCdecl);
  Assert.AreEqual(2, CodeMemPageCount);
end;

procedure TMethodCallbackTest.TestOfObjectCallBackCDecl;
var
  ptr: pointer;
  proc: TThreeArgCdeclProcedure;
  cb: TMyProcCallBack;
begin
  cb:= fTestObj.ThreeArgCdeclProcedure;
  ptr:=GetOfObjectCallBack(TCallBack(cb), 3, ctCdecl);

  //---call method through pointer
  proc:=TThreeArgCdeclProcedure(ptr);

  proc('first arg', 'second arg', 'third arg');

  Assert.AreEqual('first arg', fTestObj.Argument1);
  Assert.AreEqual('second arg', fTestObj.Argument2);
  Assert.AreEqual('third arg', fTestObj.Argument3);

end;

procedure TMethodCallbackTest.TestOfObjectCallBackStdCall;
var
  ptr: pointer;
  func: TTwoArgStdFunction;
  cb: TMyFuncCallBack;
begin
  cb:=fTestObj.TwoArgStdFunction;
  ptr:=GetOfObjectCallBack(TCallBack(cb), 2, ctStdcall);

  //---call method through pointer
  func:=TTwoArgStdFunction(ptr);

  Assert.AreEqual(1, func('first arg', 'second arg'));
  Assert.AreEqual('first arg', fTestObj.Argument1);
  Assert.AreEqual('second arg', fTestObj.Argument2);
end;


initialization
  TDUnitX.RegisterTestFixture(TMethodCallBackTest);

end.
