(**************************************************************************)
(*                                                                        *)
(* Module:  Unit 'ExecCmdCommon'                                          *)
(*                                                                        *)
(*                                  Copyright (c) 2021                    *)
(*                                  Lucas Moura Belo - lmbelo             *)
(*                                  lucas.belo@live.com                   *)
(*                                  Brazil                                *)
(*                                                                        *)
(* Project page:             https://github.com/Embarcadero/python4delphi *)
(**************************************************************************)
(*  Functionality:  Supplier for Execute Commands                         *)
(*                                                                        *)
(*                                                                        *)
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
(**************************************************************************)
unit ExecCmdCommon;

interface

type
  TExecCmdCommon = class
  public
    class function BuildArgv(const AExecutable: string;
      const AInput: TArray<string>): TArray<string>;
    class function BuildEnvp(const AHome, AExecutable,
      ASharedLibrary: string): TArray<string>;
  end;

implementation

uses
  System.IOUtils, System.SysUtils;

{ TPathHandler }

class function TExecCmdCommon.BuildArgv(const AExecutable: string;
  const AInput: TArray<string>): TArray<string>;
begin
  {$IFDEF MSWINDOWS}
  Result := AInput;
  {$ELSE}
  Result := [AExecutable] + AInput;
  {$ENDIF MSWINDOWS}
end;

class function TExecCmdCommon.BuildEnvp(const AHome, AExecutable,
  ASharedLibrary: string): TArray<string>;
begin
  {$IFDEF MSWINDOWS}
  Result := [];
  {$ELSEIF DEFINED(OSX)}
  Result := ['DYLD_LIBRARY_PATH='
           + ExtractFileDir(ASharedLibrary)
           + ':'
           + ExtractFileDir(AExecutable),
             'LD_LIBRARY_PATH=' + ExtractFileDir(ASharedLibrary),
             'PATH=' + ExtractFileDir(AExecutable)];
  {$ELSEIF DEFINED(POSIX)}
  Result := ['LD_LIBRARY_PATH=' + ExtractFileDir(ASharedLibrary),
             'PYTHONHOME=' + AHome,
             'PATH=' + ExtractFileDir(AExecutable)];
  {$ENDIF MSWINDOWS}
end;

end.
