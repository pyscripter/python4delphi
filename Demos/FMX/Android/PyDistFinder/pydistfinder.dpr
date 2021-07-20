(**************************************************************************)
(*                                                                        *)
(* Module:  Unit 'pydistfinder'     Copyright (c) 2021                    *)
(*                                                                        *)
(*                                  Lucas Moura Belo - lmbelo             *)
(*                                  lucas.belo@live.com                   *)
(*                                  Brazil                                *)
(*                                                                        *)
(*                                  PyScripter                            *)
(*                                  e-mail: pyscripter@gmail.com          *)
(*                                                                        *)
(*  Project pages:      https://github.com/Embarcadero/python4delphi      *)
(*                      https://github.com/pyscripter/python4delphi       *)
(**************************************************************************)
(*  Functionality:  Python distribution compressor/uncompressor           *)
(*                  Making Python distribution available for Android      *)
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

program pydistfinder;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  System.IOUtils,
  System.Zip;

const
  ZIP_CMD = 'ZIP';
  UNZIP_CMD = 'UNZIP';
  PYTHON_DIST_FOLDER_NAME = 'dist';
  PYTHON_BUILD_FOLDER_NAME = 'build';
  PYTHON_BUILD_ZIP_FILE_NAME = 'build.zip';

function FindPythonDistFolder(AProjectPath: string): string;
begin
  //look for dist folder into project's dir
  var LTarget := TPath.Combine(AProjectPath, PYTHON_DIST_FOLDER_NAME);
  //go up until find dist folder or empty if not exists
  while (AProjectPath <> String.Empty) and not TDirectory.Exists(LTarget) do begin
    AProjectPath := TDirectory.GetParent(AProjectPath);
    LTarget := TPath.Combine(AProjectPath, PYTHON_DIST_FOLDER_NAME)
  end;

  if TDirectory.Exists(LTarget) then
    Result := LTarget
  else
    Result := String.Empty;
end;

function FindPythonTargetFolder(const ATargetPath, APlatform, AWithDebugSymbols: string): string;
begin
  Result := String.Empty;
  if (not ATargetPath.IsEmpty()) then
    WriteLn(Format('Python''s distribution folder found at: %s', [ATargetPath]))
  else begin
    WriteLn('Python''s distribution folder (dist) not found in project directory tree.');
    ExitCode := 3;
    Exit();
  end;

  Result := ATargetPath;

  if (APlatform = 'arm') then begin
    Result := TPath.Combine(Result, 'arm');
    if (AWithDebugSymbols = 'true') then begin
      Result := TPath.Combine(Result, 'Python3.9d')
    end else begin
      Result := TPath.Combine(Result, 'Python3.9');
    end;
  end else begin
    Result := TPath.Combine(Result, 'aarch64');
    if (AWithDebugSymbols = 'true') then begin
      Result := TPath.Combine(Result, 'Python3.9d')
    end else begin
      Result := TPath.Combine(Result, 'Python3.9');
    end;
  end;
end;

procedure ZipFolder(const APath, APlatform, AWithDebugSymbols, AClear: string);
begin
  WriteLn('');
  Writeln(Format('Compressing Python distribution', []));

  var LCompressor := procedure(const ABuildPath, ABuildZipFile: string)
  begin
    Writeln(Format('Build folder is: %s', [ABuildPath]));
    WriteLn('');
    WriteLn('Compressing...');
    WriteLn('');
    TZipFile.ZipDirectoryContents(ABuildZipFile, ABuildPath);
    WriteLn(Format('File created: %s', [ABuildZipFile]));
  end;

  var LBuildPath := TPath.Combine(APath, PYTHON_BUILD_FOLDER_NAME);
  //Check for folder structure
  if not TDirectory.Exists(LBuildPath) then begin
    WriteLn('Invalid directory structure. Expected directory not found: ' + LBuildPath);
    ExitCode := 2;
    Exit();
  end;

  var LBuildZipFile := TPath.Combine(APath, PYTHON_BUILD_ZIP_FILE_NAME);
  if TFile.Exists(LBuildZipFile) then begin
    if (AClear = 'true') then begin
      TFile.Delete(LBuildZipFile);
      LCompressor(LBuildPath, LBuildZipFile);
    end else begin
      WriteLn('File already exists: ' + LBuildZipFile);
    end;
  end else
    LCompressor(LBuildPath, LBuildZipFile);
end;

procedure UnzipFile(const APath, APlatform, AWithDebugSymbols, AClear: string);
begin
  WriteLn('');
  Writeln(Format('Decompressing Python distribution', []));
  WriteLn('');

  var LDecompressor := procedure(const ABuildZipFile, ABuildPath: string)
  begin
    Writeln(Format('Build zip file is: %s', [ABuildZipFile]));
    WriteLn('');
    WriteLn('Decompressing...');
    WriteLn('');
    TZipFile.ExtractZipFile(ABuildZipFile, ABuildPath);
    WriteLn(Format('Folder created: %s', [ABuildPath]));
  end;

  var LZipFile := TPath.Combine(APath, PYTHON_BUILD_ZIP_FILE_NAME);
  //Check for folder structure
  if not TFile.Exists(LZipFile) then begin
    WriteLn('Invalid directory structure. Expected file not found: ' + LZipFile);
    ExitCode := 2;
    Exit();
  end;

  var LBuildPath := TPath.Combine(APath, PYTHON_BUILD_FOLDER_NAME);
  if TDirectory.Exists(LBuildPath) then begin
    if (AClear = 'true') then begin
      TDirectory.Delete(LBuildPath, true);
      LDecompressor(LZipFile, LBuildPath);
    end else begin
      WriteLn('Folder already exists: ' + LBuildPath);
    end;
  end else
    LDecompressor(LZipFile, LBuildPath);
end;

procedure MakeDist(const ACmd, AProjectPath, APlatform, AWithDebugSymbols, AClear: string);
begin
  WriteLn('');
  Writeln(Format('Command set to: %s', [ACmd]));
  Writeln(Format('Platform set to: %s', [APlatform]));
  Writeln(Format('With debug symbols set to: %s', [AWithDebugSymbols]));
  Writeln(Format('Clear if exists set to: %s', [AClear]));
  WriteLn('');

  var LPath := FindPythonTargetFolder(FindPythonDistFolder(AProjectPath),
    APlatform, AWithDebugSymbols);

  if LPath.IsEmpty then
    Exit();

  if (ACmd = ZIP_CMD) then
    ZipFolder(LPath, APlatform, AWithDebugSymbols, AClear)
  else if (ACmd = UNZIP_CMD) then
    UnzipFile(LPath, APlatform, AWithDebugSymbols, AClear);
end;

begin
  try
    WriteLn('');
    WriteLn('');

    WriteLn('#######################################################');
    WriteLn('#                                                     #');
    WriteLn('#               Python4Delphi - Android               #');
    WriteLn('#                                                     #');
    WriteLn('#######################################################');
    WriteLn('#                                                     #');
    WriteLn('#                                                     #');
    WriteLn('#                 Python distribution                 #');
    WriteLn('#                       finder                        #');
    WriteLn('#                                                     #');
    WriteLn('#                                                     #');
    WriteLn('#######################################################');

    WriteLn('');
    WriteLn('');

    if (ParamCount < 4) or (ParamCount > 5) then begin
      WriteLn('Invalid parameters');
      ExitCode := 1;
    end else if ParamCount = 4 then
      MakeDist(ParamStr(1), ParamStr(2), ParamStr(3), ParamStr(4), ParamStr(5))
    else
      MakeDist(ParamStr(1), ParamStr(2), ParamStr(3), ParamStr(4), 'false');
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
