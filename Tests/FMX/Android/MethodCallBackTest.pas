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
  MethodCallback,
  PythonLoad;

implementation


type
  TTwoArgArmStdFunction = function (arg1, arg2: string): integer;
  TThreeArgArmStdProcedure = procedure (arg1, arg2, arg3: string);

  TFourArgArmStdFunction = function(arg1, arg2, arg3, arg4: integer): integer;
  TFiveArgArmStdFunction = function(arg1, arg2, arg3, arg4, arg5: integer): integer;

  TMyFuncCallback = function(arg1, arg2: string): integer of object;
  TMyProcCallback = procedure (arg1, arg2, arg3: string) of object;


  TTestObj = class
  public
    Argument1: string;
    Argument2: string;
    Argument3: string;
    function TwoArgArmStdFunction(arg1, arg2: string): integer;
    procedure ThreeArgArmStdProcedure(arg1, arg2, arg3: string);
    function FourArgArmStdFunction(arg1, arg2, arg3, arg4: integer): integer;
    function FiveArgArmStdFunction(arg1, arg2, arg3, arg4, arg5: integer): integer;
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
    procedure TestCallBackArmStd;
    [Test]
    procedure TestOfObjectCallBackArmStd;
    [Test]
    procedure TestDeleteCallBack;
    [Test]
    procedure TestFourArgArmStdFunction;
    [Test]
    procedure TestFiveArgArmStdFunction;
    [Test]
    procedure TestMemoryMgmt;
  end;

{ TTestObj }

function TTestObj.FiveArgArmStdFunction(arg1, arg2, arg3, arg4,
  arg5: integer): integer;
begin
  Result := arg1 * arg4 + arg2 * arg5 + arg3;
end;

function TTestObj.FourArgArmStdFunction(arg1, arg2, arg3, arg4: integer): integer;
begin
  Result := arg1 * arg3 + arg2 * arg4;
end;

procedure TTestObj.ThreeArgArmStdProcedure(arg1, arg2, arg3: string);
begin
  Argument1:=arg1;
  Argument2:=arg2;
  Argument3:=arg3;
end;

function TTestObj.TwoArgArmStdFunction(arg1, arg2: string): integer;
begin
  Argument1:=arg1;
  Argument2:=arg2;
  result:=1;
end;

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

procedure TMethodCallbackTest.TestMemoryMgmt;
const
   AllocCount = {$IFDEF CPUX64}
                  {$IFDEF MSWINDOWS}
                  51
                  {$ELSE}
                  88
                  {$ENDIF}
                {$ELSE}
                {$IFDEF ANDROID}
                    {$IFDEF CPUARM32}
                    31
                    {$ELSE}
                    90
                    {$ENDIF CPUARM32}
                  {$ELSE}
                  90
                  {$ENDIF}
                {$ENDIF};
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
    ptr:=GetCallBack(fTestObj, @TTestObj.ThreeArgArmStdProcedure, 5, ctArmStd);

  // there should still be 1 page allocated
  Assert.AreEqual(1, CodeMemPageCount);

  // get one callback more and we should have 2 pages
  ptr:=GetCallBack(fTestObj, @TTestObj.ThreeArgArmStdProcedure, 5, ctArmStd);
  // getting CodeMemPageCount would crash as the next page pointer was overwritten by the
  // last allocation
  Assert.AreEqual(2, CodeMemPageCount);
end;

procedure TMethodCallbackTest.TestCallBackArmStd;
var
  ptr: pointer;
  func: TTwoArgArmStdFunction;
begin
  ptr:=GetCallBack(fTestObj, @TTestObj.TwoArgArmStdFunction, 2, ctArmStd);

  //---call method through pointer
  func:=TTwoArgArmStdFunction(ptr);

  Assert.AreEqual(1, func('first arg', 'second arg'));
  Assert.AreEqual(string('first arg'), fTestObj.Argument1);
  Assert.AreEqual(string('second arg'), fTestObj.Argument2);
end;

procedure TMethodCallbackTest.TestDeleteCallBack;
var
  ptr1, ptr2, ptr3: Pointer;
  proc: TThreeArgArmStdProcedure;
  func: TTwoArgArmStdFunction;
begin
  //---we create 3 callbacks and delete them.
  // if there aren't any AV, we assume it works...
  ptr1:=GetCallBack(fTestObj, @TTestObj.ThreeArgArmStdProcedure, 3, ctArmStd);
  ptr2:=GetCallBack(fTestObj, @TTestObj.TwoArgArmStdFunction, 2, ctArmStd);
  DeleteCallBack(ptr1);
  ptr1:=GetCallBack(fTestObj, @TTestObj.TwoArgArmStdFunction, 2, ctArmStd);
  ptr3:=GetCallBack(fTestObj, @TTestObj.ThreeArgArmStdProcedure, 3, ctArmStd);

  func:=TTwoArgArmStdFunction(ptr1);
  func('arg1', 'arg2');
  func:=TTwoArgArmStdFunction(ptr2);
  func('arg1', 'arg2');
  proc:=TThreeArgArmStdProcedure(ptr3);
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

procedure TMethodCallbackTest.TestFiveArgArmStdFunction;
Var
  CallBack : TFiveArgArmStdFunction;
begin
   CallBack := GetCallBack(fTestObj, @TTestObj.FiveArgArmStdFunction, 5, ctArmStd);
   Assert.AreEqual(CallBack(1,2,3,4,5), 1*4+2*5+3);
   DeleteCallBack(@CallBack);
end;

procedure TMethodCallbackTest.TestFourArgArmStdFunction;
Var
  CallBack : TFourArgArmStdFunction;
begin
   CallBack := GetCallBack(fTestObj, @TTestObj.FourArgArmStdFunction, 4, ctArmStd);
   Assert.AreEqual(CallBack(1,2,3,4), 1*3+2*4);
   DeleteCallBack(@CallBack);
end;

procedure TMethodCallbackTest.TestOfObjectCallBackArmStd;
var
  ptr: pointer;
  func: TTwoArgArmStdFunction;
  cb: TMyFuncCallBack;
begin
  cb:=fTestObj.TwoArgArmStdFunction;
  ptr:=GetOfObjectCallBack(TCallBack(cb), 2, ctARMSTD);

  //---call method through pointer
  func:=TTwoArgArmStdFunction(ptr);

  Assert.AreEqual(1, func('first arg', 'second arg'));
  Assert.AreEqual('first arg', fTestObj.Argument1);
  Assert.AreEqual('second arg', fTestObj.Argument2);
end;


initialization
  TDUnitX.RegisterTestFixture(TMethodCallBackTest);

end.
