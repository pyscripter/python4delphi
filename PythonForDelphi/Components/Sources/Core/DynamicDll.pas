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
  protected
    function IsAPIVersionStored: Boolean;
    function IsDllNameStored: Boolean;
    function IsRegVersionStored: Boolean;
    procedure SetDllName(const Value: String); virtual;
    procedure SetDllPath(const Value: String); virtual;
    function GetDllFullFileName: String;
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

    procedure CallMapDll; virtual;
    function  Import(const funcname: String; canFail: Boolean = True): Pointer;
    function  Import2(funcname: String; args: integer=-1; canFail: Boolean = True): Pointer;
    procedure Loaded; override;
    procedure BeforeLoad; virtual;
    procedure AfterLoad; virtual;
    procedure BeforeUnload; virtual;
    function  GetQuitMessage : String; virtual;
    procedure DoOpenDll(const aDllName : String); virtual;
    function  GetDllPath : String; virtual;
    procedure MapDll; virtual; abstract;

  public
    // Constructors & Destructors
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy;                    override;

    // Public methods
    procedure OpenDll(const aDllName : String);
    function  IsHandleValid : Boolean;
    function LoadDll: Boolean;
    procedure UnloadDll;
    procedure Quit;
    class function CreateInstance(DllPath: String = ''; DllName: String = ''): TDynamicDll;
    class function CreateInstanceAndLoad(DllPath: String = ''; DllName: String = ''): TDynamicDll;

    // Public properties
  published
    property AutoLoad : Boolean read FAutoLoad write FAutoLoad default True;
    property AutoUnload : Boolean read FAutoUnload write FAutoUnload default True;
    property DllName : String read FDllName write SetDllName stored IsDllNameStored;
    property DllPath : String read FDllPath write SetDllPath;
    property DllFullFileName : String read GetDllFullFileName;
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

(*******************************************************)
(**                                                   **)
(**            class TDynamicDll                      **)
(**                                                   **)
(*******************************************************)

procedure TDynamicDll.DoOpenDll(const aDllName : String);
begin
  if not IsHandleValid then
  begin
    if aDllName<>'' then
      FDllName := aDllName;
    SetDllDirectory(PChar(GetDllPath));
    FDLLHandle := SafeLoadLibrary(
      {$IFDEF FPC}
        PAnsiChar(AnsiString(DllName))
      {$ELSE}
        GetDllPath+DllName
      {$ENDIF}
    );
  end;
end;

function TDynamicDll.GetDllFullFileName: String;
begin
  Result := DllPath + DllName;
end;

function  TDynamicDll.GetDllPath : String;
begin
  Result := DllPath;
  if Result <> '' then
    Result := IncludeTrailingPathDelimiter(Result);
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
  FDLLHandle            := 0;
end;

destructor TDynamicDll.Destroy;
begin
  if AutoUnload then
    UnloadDll;
  inherited;
end;

function TDynamicDll.Import(const funcname: String; canFail: Boolean): Pointer;
var
  E : EDllImportError;
begin
  Result := GetProcAddress( FDLLHandle, PChar(funcname) );
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

function TDynamicDll.Import2(funcname: String; args: integer; canFail: Boolean): Pointer;
begin
  {$IFDEF WIN32}
  // using STDCall name decoration
  // copy paste the function names from dependency walker to notepad and search for the function name there.
  if args>=0 then
    funcname := '_'+funcname+'@'+IntToStr(args);
  {$ENDIF}
  Result := Import(funcname, canFail);
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

function TDynamicDll.LoadDll: Boolean;
begin
  OpenDll( DllName );
  Result := IsHandleValid;
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
  CallMapDll;
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

procedure TDynamicDll.SetDllPath(const Value: String);
begin
  FDllPath := Value;
end;

procedure TDynamicDll.CallMapDll;
begin
  try
    MapDll;
  except
    on E: Exception do begin
      if FatalMsgDlg then
{$IFDEF MSWINDOWS}
        MessageBox( GetActiveWindow, PChar(E.Message), 'Error', MB_TASKMODAL or MB_ICONSTOP );
{$ELSE}
        WriteLn( ErrOutput, E.Message );
{$ENDIF}
      if FatalAbort then Quit;
    end;
  end;
end;

class function TDynamicDll.CreateInstance(DllPath, DllName: String): TDynamicDll;
begin
  Result := Create(nil);
  if DllPath<>'' then
    Result.DllPath := DllPath;
  if DllName<>'' then
    Result.DllName := DllName;
end;

class function TDynamicDll.CreateInstanceAndLoad(DllPath, DllName: String): TDynamicDll;
begin
  Result := CreateInstance(DllPath, DllName);
  Result.LoadDll;
  if not Result.IsHandleValid then
    FreeAndNil(Result);
end;

end.

