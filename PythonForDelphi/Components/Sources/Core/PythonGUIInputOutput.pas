{$I Definition.Inc}
unit PythonGUIInputOutput;

(**************************************************************************)
(*                                                                        *)
(* Module: Unit 'PythonGUIInputOutput' Copyright (c) 1997                 *)
(*                                                                        *)
(*                                     Dr. Dietmar Budelsky               *)
(*                                     dbudelsky@web.de                   *)
(*                                     Germany                            *)
(*                                                                        *)
(*                                     Morgan Martinet                    *)
(*                                     4723 rue Brebeuf                   *)
(*                                     H2J 3L2 MONTREAL (QC)              *)
(*                                     CANADA                             *)
(*                                     e-mail: p4d@mmm-experts.com        *)
(*                                                                        *)
(*  look our page at: http://mmm-experts.com/                             *)
(**************************************************************************)
(*  Functionality:  Delphi Components that provide an interface to the    *)
(*                  Python language (see python.txt for more infos on     *)
(*                  Python itself).                                       *)
(*                                                                        *)
(**************************************************************************)
(*  Contributors:                                                         *)
(*      Mark Watts(mark_watts@hotmail.com)                                *)
(*      Michiel du Toit (micdutoit@hsbfn.com)                             *)
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
(* Dr. Dietmar Budelsky, 1997-11-17                                       *)
(**************************************************************************)

interface

uses
{$IFDEF MSWINDOWS}
  Windows, Messages,
{$ENDIF}
  SysUtils, Classes;//, Graphics, Controls, Forms, Dialogs,
  StdCtrls, PythonEngine;

{$IFDEF MSWINDOWS}
const
  WM_WriteOutput = WM_USER + 1;
{$ENDIF}

type
  {$IF not Defined(FPC) and (CompilerVersion >= 23)}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$IFEND}
  TPythonGUIInputOutput = class(TPythonInputOutput)
  private
    { Private declarations }
    FCustomMemo : TCustomMemo;
{$IFDEF MSWINDOWS}
    FWinHandle : HWND;
{$ENDIF}
  protected
    { Protected declarations }
{$IFDEF MSWINDOWS}
    procedure pyGUIOutputWndProc (var Message: TMessage);
{$ENDIF}
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SendData( const Data : AnsiString ); override;
    function  ReceiveData : AnsiString; override;
    procedure SendUniData( const Data : UnicodeString ); override;
    function  ReceiveUniData : UnicodeString; override;
    procedure AddPendingWrite; override;
    procedure WriteOutput;
  public
    { Public declarations }
    constructor Create( AOwner : TComponent ); override;
    destructor  Destroy; override;

    procedure DisplayString( const str : string );

  published
    { Published declarations }
    property Output : TCustomMemo read FCustomMemo write FCustomMemo;
  end;

procedure Register;

implementation

{$IFDEF FPC}
{$IFDEF MSWINDOWS}
Uses
  InterfaceBase;
{$ENDIF}
{$ENDIF}

{PROTECTED METHODS}

{------------------------------------------------------------------------------}
procedure TPythonGUIInputOutput.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if Operation = opRemove then
    if aComponent = fCustomMemo then
      fCustomMemo := nil;
end;

{------------------------------------------------------------------------------}
{$IFDEF MSWINDOWS}
procedure TPythonGUIInputOutput.pyGUIOutputWndProc(var Message: TMessage);
begin
  case Message.Msg of
    WM_WriteOutput : WriteOutput;
  end;{case}
end;
{$ENDIF}
{------------------------------------------------------------------------------}
procedure TPythonGUIInputOutput.SendData( const Data : AnsiString );
begin
  if Assigned(FOnSendData) then
    inherited
  else
    DisplayString( string(Data) );
end;

procedure TPythonGUIInputOutput.SendUniData(const Data: UnicodeString);
begin
  if Assigned(FOnSendUniData) then
    inherited
  else
    DisplayString( Data );
end;

{------------------------------------------------------------------------------}
function  TPythonGUIInputOutput.ReceiveData : AnsiString;
Var
  S : string;
begin
  if Assigned( FOnReceiveData ) then
    Result := inherited ReceiveData
  else
  begin
    InputQuery( 'Query from Python', 'Enter text', S);
    Result := AnsiString(S);
  end;
end;

function TPythonGUIInputOutput.ReceiveUniData: UnicodeString;
Var
  S : string;
begin
  if Assigned( FOnReceiveUniData ) then
    Result := inherited ReceiveUniData
  else
  begin
    InputQuery( 'Query from Python', 'Enter text', S);
    Result := S;
  end;
end;

{------------------------------------------------------------------------------}
procedure TPythonGUIInputOutput.AddPendingWrite;
begin
{$IFDEF MSWINDOWS}
  PostMessage( fWinHandle, WM_WriteOutput, 0, 0 );
{$ENDIF}
end;

{------------------------------------------------------------------------------}
procedure TPythonGUIInputOutput.WriteOutput;
var
  S : IOString;
begin
  if FQueue.Count = 0 then
    Exit;
  S := FQueue.Strings[ 0 ];
  FQueue.Delete(0);
  DisplayString( S )
end;

{PUBLIC METHODS}

{------------------------------------------------------------------------------}
constructor TPythonGUIInputOutput.Create( AOwner : TComponent );
begin
  inherited Create(AOwner);
{$IFDEF MSWINDOWS}
  // Create an internal window for use in delayed writes
  // This will allow writes from multiple threads to be queue up and
  // then written out to the associated TCustomMemo by the main UI thread.
  {$IFDEF FPC}
    fWinHandle := WidgetSet.AllocateHWnd(pyGUIOutputWndProc);
  {$ELSE}
    fWinHandle := Classes.AllocateHWnd(pyGUIOutputWndProc);
  {$ENDIF}
{$ENDIF}
{$IFDEF UNICODE}
   UnicodeIO := True;
{$ENDIF}
end;

{------------------------------------------------------------------------------}
destructor TPythonGUIInputOutput.Destroy;
begin
{$IFDEF MSWINDOWS}
  // Destroy the internal window used for Delayed write operations
  {$IFDEF FPC}
    WidgetSet.DeallocateHWnd(fWinHandle);
  {$ELSE}
    Classes.DeallocateHWnd(fWinHandle);
  {$ENDIF}
{$ENDIF}
  inherited Destroy;
end;

{------------------------------------------------------------------------------}
type
  TMyCustomMemo = class(TCustomMemo);

procedure TPythonGUIInputOutput.DisplayString( const str : string );
begin
  if Assigned(Output) then
  begin
    if TMyCustomMemo(Output).Lines.Count >= MaxLines then
      TMyCustomMemo(Output).Lines.Delete(0);
    TMyCustomMemo(Output).Lines.Add(str);
{$IFDEF MSWINDOWS}
    SendMessage( Output.Handle, em_ScrollCaret, 0, 0);
    //SendMessage( Output.Handle, EM_LINESCROLL, 0, Output.Lines.Count);
{$ENDIF}
  end;{if}
end;

{------------------------------------------------------------------------------}
procedure Register;
begin
  RegisterComponents('Python', [TPythonGUIInputOutput]);
end;

end.
