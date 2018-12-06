(**************************************************************************)
(*                                                                        *)
(* Module:  Unit 'PythonEngine'     Copyright (c) 1997                    *)
(*                                                                        *)
(* Version: 3.0                     Dr. Dietmar Budelsky                  *)
(* Sub-Version: 0.33                dbudelsky@web.de                      *)
(*                                  Germany                               *)
(*                                                                        *)
(*                                  Morgan Martinet                       *)
(*                                  4723 rue Brebeuf                      *)
(*                                  H2J 3L2 MONTREAL (QC)                 *)
(*                                  CANADA                                *)
(*                                  e-mail: p4d@mmm-experts.com           *)
(*                                                                        *)
(*  look at the project page at: http://python4Delphi.googlecode.com/     *)
(**************************************************************************)
(*  Functionality:  Delphi Components that provide an interface to the    *)
(*                  Python language (see python.txt for more infos on     *)
(*                  Python itself).                                       *)
(*                                                                        *)
(**************************************************************************)
(*  Contributors:                                                         *)
(*      Grzegorz Makarewicz (mak@mikroplan.com.pl)                        *)
(*      Andrew Robinson (andy@hps1.demon.co.uk)                           *)
(*      Mark Watts(mark_watts@hotmail.com)                                *)
(*      Olivier Deckmyn (olivier.deckmyn@mail.dotcom.fr)                  *)
(*      Sigve Tjora (public@tjora.no)                                     *)
(*      Mark Derricutt (mark@talios.com)                                  *)
(*      Igor E. Poteryaev (jah@mail.ru)                                   *)
(*      Yuri Filimonov (fil65@mail.ru)                                    *)
(*      Stefan Hoffmeister (Stefan.Hoffmeister@Econos.de)                 *)
(*      Michiel du Toit (micdutoit@hsbfn.com) - Lazarus Port              *)
(*      Chris Nicolai (nicolaitanes@gmail.com)                            *)
(*      Kiriakos Vlahos (pyscripter@gmail.com)                            *)
(*      Andrey Gruzdev      (andrey.gruzdev@gmail.com)                    *)
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

{$I Definition.Inc}

unit DynamicDll;

{ TODO -oMMM : implement tp_as_buffer slot }
{ TODO -oMMM : implement Attribute descriptor and subclassing stuff }

{$IFNDEF FPC}
  {$IFNDEF DELPHI7_OR_HIGHER}
      Error! Delphi 7 or higher is required!
  {$ENDIF}
{$ENDIF}

interface

uses
  Types,
{$IFDEF MSWINDOWS}
  Windows,
{$ELSE}
{$IFDEF FPC}
  dynlibs,
{$ELSE}
{$IFDEF LINUX}
  Libc,
{$ENDIF}
{$ENDIF}
{$ENDIF}
  Classes,
  SysUtils,
  SyncObjs,
  Variants,
{$IFDEF DELPHI2005_OR_HIGHER}
{$IFNDEF UNICODE}
  WideStrings,
{$ENDIF}
{$ELSE}
  TinyWideStrings,
{$ENDIF}
  MethodCallBack;

type
  EDLLImportError = class(Exception)
    public
      WrongFunc : String;
      ErrorCode : Integer;
  end;

//-------------------------------------------------------
//--                                                   --
//--      Base class:  TDynamicDll                     --
//--                                                   --
//-------------------------------------------------------

type
  TDynamicDll = class(TComponent)
  private
    function IsAPIVersionStored: Boolean;
    function IsDllNameStored: Boolean;
    function IsRegVersionStored: Boolean;
    procedure SetDllName(const Value: String);
  protected
    FDllName            : String;
    FDllPath            : String;
    FAPIVersion         : Integer;
    FRegVersion         : String;
    FAutoLoad           : Boolean;
    FAutoUnload         : Boolean;
    FFatalMsgDlg        : Boolean;
    FFatalAbort         : Boolean;
    FDLLHandle          : THandle;
    FUseLastKnownVersion: Boolean;
    FOnBeforeLoad       : TNotifyEvent;
    FOnAfterLoad        : TNotifyEvent;
    FOnBeforeUnload     : TNotifyEvent;

    function  Import(const funcname: AnsiString; canFail : Boolean = True): Pointer;
    procedure Loaded; override;
    procedure BeforeLoad; virtual;
    procedure AfterLoad; virtual;
    procedure BeforeUnload; virtual;
    function  GetQuitMessage : String; virtual;
    procedure DoOpenDll(const aDllName : String); virtual;
    function  GetDllPath : String;

  public
    // Constructors & Destructors
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy;                    override;

    // Public methods
    procedure OpenDll(const aDllName : String);
    function  IsHandleValid : Boolean;
    procedure LoadDll;
    procedure UnloadDll;
    procedure Quit;

    // Public properties
  published
    property AutoLoad : Boolean read FAutoLoad write FAutoLoad default True;
    property AutoUnload : Boolean read FAutoUnload write FAutoUnload default True;
    property DllName : String read FDllName write SetDllName stored IsDllNameStored;
    property DllPath : String read FDllPath write FDllPath;
    property APIVersion : Integer read FAPIVersion write FAPIVersion stored IsAPIVersionStored;
    property RegVersion : String read FRegVersion write FRegVersion stored IsRegVersionStored;
    property FatalAbort :  Boolean read FFatalAbort write FFatalAbort default True;
    property FatalMsgDlg : Boolean read FFatalMsgDlg write FFatalMsgDlg default True;
    property UseLastKnownVersion: Boolean read FUseLastKnownVersion write FUseLastKnownVersion default True;
    property OnAfterLoad : TNotifyEvent read FOnAfterLoad write FOnAfterLoad;
    property OnBeforeLoad : TNotifyEvent read FOnBeforeLoad write FOnBeforeLoad;
    property OnBeforeUnload : TNotifyEvent read FOnBeforeUnload write FOnBeforeUnload;
  end;


implementation

uses PythonEngine;

(*******************************************************)
(**                                                   **)
(**            class TDynamicDll                      **)
(**                                                   **)
(*******************************************************)

procedure TDynamicDll.DoOpenDll(const aDllName : String);
begin
  if not IsHandleValid then
  begin
    FDllName := aDllName;
    FDLLHandle := SafeLoadLibrary(
      {$IFDEF FPC}
        PAnsiChar(AnsiString(GetDllPath+DllName))
      {$ELSE}
        GetDllPath+DllName
      {$ENDIF}
    );
  end;
end;

function  TDynamicDll.GetDllPath : String;
{$IFDEF MSWINDOWS}
var
  AllUserInstall: Boolean;
{$ENDIF}
begin
  Result := DllPath;

  {$IFDEF MSWINDOWS}
  if DLLPath = '' then begin
    IsPythonVersionRegistered(RegVersion, Result, AllUserInstall);
  end;
  {$ENDIF}

  if Result <> '' then
  begin
    Result := IncludeTrailingPathDelimiter(Result);
  end;
end;

procedure  TDynamicDll.OpenDll(const aDllName : String);
var
  s : String;
begin
  UnloadDll;

  BeforeLoad;

  FDLLHandle := 0;

  DoOpenDll(aDllName);

  if not IsHandleValid then begin
{$IFDEF MSWINDOWS}
    s := Format('Error %d: Could not open Dll "%s"',[GetLastError, DllName]);
{$ENDIF}
{$IFDEF LINUX}
    s := Format('Error: Could not open Dll "%s"',[DllName]);
{$ENDIF}
    if FatalMsgDlg then
{$IFDEF MSWINDOWS}
      MessageBox( GetActiveWindow, PChar(s), 'Error', MB_TASKMODAL or MB_ICONSTOP );
{$ENDIF}
{$IFDEF LINUX}
      WriteLn(ErrOutput, s);
{$ENDIF}

    if FatalAbort then
      Quit;
  end else
    AfterLoad;
end;

constructor TDynamicDll.Create(AOwner: TComponent);
begin
  inherited;
  FFatalMsgDlg          := True;
  FFatalAbort           := True;
  FAutoLoad             := True;
  FUseLastKnownVersion  := True;
end;

destructor TDynamicDll.Destroy;
begin
  if AutoUnload then
    UnloadDll;
  inherited;
end;

function TDynamicDll.Import(const funcname: AnsiString; canFail : Boolean = True): Pointer;
var
  E : EDllImportError;
begin
  Result := GetProcAddress( FDLLHandle, PAnsiChar(funcname) );
  if (Result = nil) and canFail then begin
    {$IFDEF MSWINDOWS}
    E := EDllImportError.CreateFmt('Error %d: could not map symbol "%s"', [GetLastError, funcname]);
    E.ErrorCode := GetLastError;
    {$ELSE}
    E := EDllImportError.CreateFmt('Error: could not map symbol "%s"', [funcname]);
    {$ENDIF}
    E.WrongFunc := funcname;
    raise E;
  end;
end;

procedure TDynamicDll.Loaded;
begin
  inherited;
  if AutoLoad and not (csDesigning in ComponentState) then
    LoadDll;
end;

function  TDynamicDll.IsHandleValid : Boolean;
begin
{$IFDEF MSWINDOWS}
  Result := (FDLLHandle >= 32);
{$ENDIF}
{$IFDEF LINUX}
  Result := FDLLHandle <> 0;
{$ENDIF}
end;

procedure TDynamicDll.LoadDll;
begin
  OpenDll( DllName );
end;

procedure TDynamicDll.UnloadDll;
begin
  if IsHandleValid then begin
    BeforeUnload;
    FreeLibrary(FDLLHandle);
    FDLLHandle := 0;
  end;
end;

procedure TDynamicDll.BeforeLoad;
begin
  if Assigned( FOnBeforeLoad ) then
    FOnBeforeLoad( Self );
end;

procedure TDynamicDll.AfterLoad;
begin
  if Assigned( FOnAfterLoad ) then
    FOnAfterLoad( Self );
end;

procedure TDynamicDll.BeforeUnload;
begin
  if Assigned( FOnBeforeUnload ) then
    FOnBeforeUnload( Self );
end;

function  TDynamicDll.GetQuitMessage : String;
begin
  Result := Format( 'Dll %s could not be loaded. We must quit.', [DllName]);
end;

procedure TDynamicDll.Quit;
begin
  if not( csDesigning in ComponentState ) then begin
{$IFDEF MSWINDOWS}
    MessageBox( GetActiveWindow, PChar(GetQuitMessage), 'Error', MB_TASKMODAL or MB_ICONSTOP );
    ExitProcess( 1 );
{$ELSE}
    WriteLn(ErrOutput, GetQuitMessage);
{$IFDEF FPC}
    Halt( 1 );
{$ELSE}
    __exit(1);
{$ENDIF}
{$ENDIF}
  end;
end;

function TDynamicDll.IsAPIVersionStored: Boolean;
begin
  Result := not UseLastKnownVersion;
end;

function TDynamicDll.IsDllNameStored: Boolean;
begin
  Result := not UseLastKnownVersion;
end;

function TDynamicDll.IsRegVersionStored: Boolean;
begin
  Result := not UseLastKnownVersion;
end;

procedure TDynamicDll.SetDllName(const Value: String);
begin
  FDllName := Value;
end;

end.

