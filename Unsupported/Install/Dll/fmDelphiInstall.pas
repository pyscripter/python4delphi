unit fmDelphiInstall;

{-------------------------------------------------------------------------------
  $Header: /P4D/Install/Dll/fmDelphiInstall.pas 1     03-04-09 19:04 Morgan $
  Copyright © MMM Inc. 2003 - All Rights Reserved.
  ------------------------------------------------------------------------------
  Author: Morgan Martinet

  Description:

  ------------------------------------------------------------------------------
  $Log: /P4D/Install/Dll/fmDelphiInstall.pas $
 * 
 * 1     03-04-09 19:04 Morgan
 * initial release

-------------------------------------------------------------------------------}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ExtCtrls;

const
  kPageCount = 9;
type
  TCompiler = (coDelphi, coBuilder, coBDS);

  TDelphiInstall = class(TForm)
    Notebook1: TNotebook;
    GroupBox1: TGroupBox;
    cbD3: TCheckBox;
    cbD4: TCheckBox;
    cbD5: TCheckBox;
    btnCompile: TBitBtn;
    btnIgnore: TBitBtn;
    Label1: TLabel;
    BitBtn3: TBitBtn;
    BitBtn4: TBitBtn;
    LogNext: TBitBtn;
    BitBtn6: TBitBtn;
    BitBtn7: TBitBtn;
    BitBtn8: TBitBtn;
    BitBtn9: TBitBtn;
    BitBtn10: TBitBtn;
    BitBtn11: TBitBtn;
    BitBtn12: TBitBtn;
    BitBtn13: TBitBtn;
    BitBtn14: TBitBtn;
    BitBtn15: TBitBtn;
    BitBtn16: TBitBtn;
    BitBtn17: TBitBtn;
    BitBtn18: TBitBtn;
    BitBtn19: TBitBtn;
    BitBtn20: TBitBtn;
    BitBtn21: TBitBtn;
    Label2: TLabel;
    memLog: TMemo;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    BitBtn22: TBitBtn;
    BitBtn24: TBitBtn;
    memChanges: TMemo;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    BitBtn25: TBitBtn;
    Label11: TLabel;
    lCompVer: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    BitBtn1: TBitBtn;
    BitBtn5: TBitBtn;
    BitBtn26: TBitBtn;
    BitBtn27: TBitBtn;
    Label20: TLabel;
    BitBtn28: TBitBtn;
    Label21: TLabel;
    Label22: TLabel;
    Label23: TLabel;
    Label24: TLabel;
    cbCB3: TCheckBox;
    cbCB4: TCheckBox;
    cbCB5: TCheckBox;
    Label25: TLabel;
    memCBNotes: TMemo;
    BitBtn29: TBitBtn;
    BitBtn30: TBitBtn;
    BitBtn31: TBitBtn;
    cbD6: TCheckBox;
    GroupBox2: TGroupBox;
    Label26: TLabel;
    rbPython15: TRadioButton;
    rbPython20: TRadioButton;
    rbPython21: TRadioButton;
    rbPython22: TRadioButton;
    BitBtn2: TBitBtn;
    btnEditDefinition: TBitBtn;
    cbD7: TCheckBox;
    rbPython23: TRadioButton;
    rbPython24: TRadioButton;
    cbD2005: TCheckBox;
    BitBtn32: TBitBtn;
    cbD2006: TCheckBox;
    rbPython25: TRadioButton;
    btnWiki: TBitBtn;
    procedure FormCreate(Sender: TObject);
    procedure BitBtn6Click(Sender: TObject);
    procedure btnCompileClick(Sender: TObject);
    procedure btnIgnoreClick(Sender: TObject);
    procedure BitBtn9Click(Sender: TObject);
    procedure BitBtn8Click(Sender: TObject);
    procedure BitBtn12Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure BitBtn22Click(Sender: TObject);
    procedure btnWikiClick(Sender: TObject);
    procedure BitBtn25Click(Sender: TObject);
    procedure BitBtn24Click(Sender: TObject);
    procedure LogNextClick(Sender: TObject);
    procedure BitBtn15Click(Sender: TObject);
    procedure BitBtn11Click(Sender: TObject);
    procedure BitBtn14Click(Sender: TObject);
    procedure BitBtn18Click(Sender: TObject);
    procedure BitBtn17Click(Sender: TObject);
    procedure BitBtn21Click(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure BitBtn5Click(Sender: TObject);
    procedure BitBtn27Click(Sender: TObject);
    procedure BitBtn28Click(Sender: TObject);
    procedure BitBtn29Click(Sender: TObject);
    procedure BitBtn30Click(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure btnEditDefinitionClick(Sender: TObject);
    procedure BitBtn32Click(Sender: TObject);
  private
    { Déclarations privées }
    FPageVisible : array [0..kPageCount-1] of Boolean;

    function  IsDelphiVerXInstalled( Version : Integer; Compiler : TCompiler ) : Boolean;
    function  IsPythonVerXInstalled( const Version : String ) : Boolean;
    function  IsDefaultPythonVersionSelected : Boolean;
    function  GetRootDir( Version : Integer; Compiler : TCompiler ) : String;
    function  GetSearchPath( Version : Integer; Compiler : TCompiler ) : String;
    procedure SetSearchPath( Version : Integer; const path : String; Compiler : TCompiler );
    procedure InstallDelphiVer( Version : Integer; Compiler : TCompiler );
    function  GetPytKey( const Key : String) : String;
    procedure AddToPath( const path : String; Version : Integer; Compiler : TCompiler );
    procedure AddPackage(const FileName, Description: String; Version: Integer; Compiler : TCompiler);
    procedure RemovePackage(const PackageName : String; Version : Integer; Compiler : TCompiler );
    procedure AddToLog( const msg : String );
    procedure WebExec( const url : String );
    function  CheckRegLocalKey( const key : String ) : Boolean;
    function  CheckRegUserKey( const key : String ) : Boolean;
    procedure NextPage;
    procedure PreviousPage;
    procedure SplitPath( const Path : String; Dest : TStrings );
    function  StringsToPath( Strings : TStrings ) : String;
    procedure CheckPath( Version : Integer; Compiler : TCompiler );
    function  GetCompiler( Compiler : TCompiler ) : String;
    procedure RemoveDisabledPackage(const PackageName: String;
      Version: Integer; Compiler: TCompiler);
    function  GetDefaultPythonVersion : String;
    function  GetDefaultPythonVersionNumber : String;
    procedure ModifyDefaultPythonVersion;
    function  GetDefinitionFileName : String;
    procedure DefineVisiblePages;
  public
    { Déclarations publiques }
    function GetMainPath : String;
  end;

  function fileExec(const aCmdLine: String; aHide, aWait: Boolean): Boolean;

  function OpenDialog : Integer; stdcall;

var
  DelphiInstall: TDelphiInstall;

implementation
uses Registry, ShellAPI, FileCtrl;
{$R *.DFM}

function OpenDialog : Integer;
begin
  Result := 0;
  try
    with TDelphiInstall.Create(nil) do
    try
      ShowModal;
    finally
      Free;
    end;
  except
  end;
end;

function CanWriteToRegistry : Boolean;
const
  KEY = 'Software\Open Source\Python for Delphi\3.x';
begin
  try
    with TRegistry.Create do
      try
        Access := KEY_WRITE;
        RootKey := HKEY_LOCAL_MACHINE;
        OpenKey(KEY, True);
        WriteString('TEST', 'TEST');
        DeleteValue('TEST');
      finally
        Free;
      end;
    Result := True;
  except
    Result := False;
  end;
end;

procedure TDelphiInstall.FormCreate(Sender: TObject);
var
  i : Integer;
begin
  for i := Low(FPageVisible) to High(FPageVisible) do
    FPageVisible[i] := True;
  Notebook1.PageIndex := 0;
  cbD3.Enabled := IsDelphiVerXInstalled(3, coDelphi);
  cbD4.Enabled := IsDelphiVerXInstalled(4, coDelphi);
  cbD5.Enabled := IsDelphiVerXInstalled(5, coDelphi);
  cbD6.Enabled := IsDelphiVerXInstalled(6, coDelphi);
  cbD7.Enabled := IsDelphiVerXInstalled(7, coDelphi);
  cbD2005.Enabled := IsDelphiVerXInstalled(3, coBDS);
  cbD2006.Enabled := IsDelphiVerXInstalled(4, coBDS);
  cbCB3.Enabled := IsDelphiVerXInstalled(3, coBuilder);
  cbCB4.Enabled := IsDelphiVerXInstalled(4, coBuilder);
  cbCB5.Enabled := IsDelphiVerXInstalled(5, coBuilder);
  if IsPythonVerXInstalled('25') then
    rbPython25.Checked := True
  else if IsPythonVerXInstalled('24') then
    rbPython24.Checked := True
  else if IsPythonVerXInstalled('23') then
    rbPython23.Checked := True
  else if IsPythonVerXInstalled('22') then
    rbPython22.Checked := True
  else if IsPythonVerXInstalled('21') then
    rbPython21.Checked := True
  else if IsPythonVerXInstalled('20') then
    rbPython20.Checked := True
  else if IsPythonVerXInstalled('15') then
    rbPython15.Checked := True;
end;

procedure TDelphiInstall.BitBtn6Click(Sender: TObject);
begin
  PreviousPage;
end;

function fileExec(const aCmdLine: String; aHide, aWait: Boolean): Boolean;
var
  StartupInfo : TStartupInfo;
  ProcessInfo : TProcessInformation;
begin
  {setup the startup information for the application }
  FillChar(StartupInfo, SizeOf(TStartupInfo), 0);
  with StartupInfo do
  begin
    cb:= SizeOf(TStartupInfo);
    dwFlags:= STARTF_USESHOWWINDOW or STARTF_FORCEONFEEDBACK;
    if aHide then wShowWindow:= SW_HIDE
             else wShowWindow:= SW_SHOWNORMAL;
  end;

  Result := CreateProcess(nil,PChar(aCmdLine), nil, nil, False,
               NORMAL_PRIORITY_CLASS, nil, nil, StartupInfo, ProcessInfo);
  if aWait then
     if Result then
     begin
       WaitForInputIdle(ProcessInfo.hProcess, INFINITE);
       WaitForSingleObject(ProcessInfo.hProcess, INFINITE);
     end;
end;

procedure TDelphiInstall.AddToPath(const path: String; Version: Integer; Compiler : TCompiler);
var
  sp : String;
  L : TStringList;
  i : Integer;
begin
  sp := GetSearchPath( Version, Compiler );
  L := TStringList.Create;
  try
    SplitPath( sp, L );
    for i := 0 to L.Count-1 do
      if CompareText( L.Strings[i], path ) = 0 then
        Exit;
    L.Add( path );
    SetSearchPath( Version, StringsToPath(L), Compiler );
    AddToLog( Format('Updated %s%d path with: %s', [GetCompiler(Compiler), Version, path]) );
  finally
    L.Free;
  end;
end;

function TDelphiInstall.GetPytKey(const Key : String): String;
begin
  with TRegistry.Create do
    try
      Access := KEY_READ;
      RootKey := HKEY_LOCAL_MACHINE;
      if KeyExists( 'Software\Open Source\Python for Delphi\3.x' ) then
        begin
          OpenKey( 'Software\Open Source\Python for Delphi\3.x', False );
          Result := ReadString(Key);
          if (Length(Result)>0) and (Result[Length(Result)] <> '\') then
            Result := Result + '\';
        end
      else
        raise Exception.CreateFmt( 'Could not get key %d of Python for Delphi', [Key] );
    finally
      Free;
    end;
end;

function TDelphiInstall.GetRootDir(Version: Integer; Compiler : TCompiler): String;
begin
  with TRegistry.Create do
    try
      Access := KEY_READ;
      RootKey := HKEY_LOCAL_MACHINE;
      if KeyExists( Format( 'Software\Borland\%s\%d.0', [GetCompiler(Compiler), Version] ) ) then
        begin
          OpenKey( Format( 'Software\Borland\%s\%d.0', [GetCompiler(Compiler), Version] ), False );
          Result := ReadString('RootDir');
          if (Length(Result)>0) and (Result[Length(Result)] <> '\') then
            Result := Result + '\';
        end
      else
        raise Exception.CreateFmt( 'Could not get root dir of %s version %d', [GetCompiler(Compiler), Version] );
    finally
      Free;
    end;
end;

function TDelphiInstall.GetSearchPath(Version: Integer; Compiler : TCompiler): String;
var
  search : String;
begin
  if (Version < 4) and (Compiler = coDelphi) then
    search := 'SearchPath'
  else
    search := 'Search Path';
  with TRegistry.Create do
    try
      Access := KEY_READ;
      RootKey := HKEY_CURRENT_USER;
      if KeyExists( Format( 'Software\Borland\%s\%d.0\Library', [GetCompiler(Compiler), Version] ) ) then
        begin
          OpenKey( Format( 'Software\Borland\%s\%d.0\Library', [GetCompiler(Compiler), Version] ), False );
          Result := ReadString(search);
        end
      else
        raise Exception.CreateFmt( 'Could not get Search path of %s version %d', [GetCompiler(Compiler), Version] );
    finally
      Free;
    end;
end;

procedure TDelphiInstall.InstallDelphiVer(Version: Integer; Compiler : TCompiler);

var
  s_ver, s_compiler, comp, s_core, s_vcl : String;

  function GetPackageExt( Version : Integer; Compiler : TCompiler ) : String;
  begin
    if (Version < 4) and (Compiler = coDelphi) then
      Result := 'dpl'
    else
      Result := 'bpl';
  end;

  procedure WriteCmd( const dcc, args, path, name : String);
  begin
    with TStringList.Create do
      try
        if Compiler in [coDelphi, coBDS] then
          Add( dcc + ' ' + args )
        else
          Add( Format('%smake" -f %s.bpk', [ExtractFilePath(dcc), name]) );
        {if Compiler = coBuilder then
          begin
            Add( dcc + ' -jphn ' + args );
            Add( Format('%smake" -f %s.bpk', [ExtractFilePath(dcc), name]) );
          end;}
        SaveToFile( Format('%sCompile_%s.bat', [path, name]) );
      finally
        Free;
      end;
  end;

  procedure DeleteFiles( const path : String );
  var
    sr: TSearchRec;
    stop : Integer;
  begin
    stop := FindFirst(path, faAnyFile, sr);
    while stop = 0 do
      begin
        DeleteFile( ExtractFilePath(path)+SR.Name );
        stop := FindNext(sr);
      end;
    FindClose(sr);
  end;

  function CompilePackage( const packName, packLabel : String ) : Boolean;

    function GetSysDir : String;
    begin
      SetLength(Result, 512);
      SetLength(Result, GetSystemDirectory(PChar(Result), Length(Result)));
    end;

  var
    fname, dcc, args, cmd : String;
  begin
    Result := True;
    fname := Format('%s%s_%s.%s', [comp, packName, s_ver, GetPackageExt(Version, Compiler)]);
    DeleteFile(fname);
    dcc := Format( '"%s"', [s_compiler]);
    args := Format( '%s_%s.dpk /USources\Core;Sources\Vcl /RSources\Core;Sources\Vcl /ISources\Core /LE. /LN.', [packName, s_ver]);
    if Compiler in [coDelphi, coBDS] then
      begin
        cmd := dcc + ' ' + args;
        WriteCmd(dcc, args, comp, Format('%s_%s', [packName, s_ver]));
        if FileExec( cmd, False, True ) then
          AddToLog( Format('Compiled package %s%s_%s.dpk', [comp, packName, s_ver]) )
        else
          begin
            Result := False;
            AddToLog( Format( '[ERROR] Could not compile package %s_%s.dpk. Please do it yourself.', [packName, s_ver]) );
            AddToLog( Format( '[HINT] Look at the batch file %sCompile_%s_%s.bat', [comp, packName, s_ver]) );
            Exit;
          end;
      end
    else // if C++ Builder
      begin
        WriteCmd(dcc, args, comp, Format('%s_%s', [packName, s_ver]));
        cmd := Format('%smake" -f %s.bpk', [ExtractFilePath(dcc), Format('%s_%s', [packName, s_ver])]);
        if FileExec( cmd, False, True ) then
          AddToLog( Format('Compiled package %s%s_%s.bpk', [comp, packName, s_ver]) )
        else
          begin
            Result := False;
            AddToLog( Format( '[ERROR] Could not compile package %s_%s.bpk. Please do it yourself.', [packName, s_ver]) );
            AddToLog( Format( '[HINT] Look at the batch file %sCompile_%s_%s.bat', [comp, packName, s_ver]) );
            Exit;
          end;
      end;
{
    if Compiler = coBuilder then
      begin
        CopyFile( PChar(fname), PChar(GetSysDir+'\'+ExtractFileName(fname)), False);
        cmd := dcc + ' -jphn ' + args;
        if FileExec( cmd, False, True ) then
          AddToLog( Format('Compiled package %s%s_%s.bpi', [comp, packName, s_ver]) )
        else
          begin
            Result := False;
            AddToLog( Format( '[ERROR] Could not compile package %s_%s.bpi. Please do it yourself.', [packName, s_ver]) );
            AddToLog( Format( '[HINT] Look at the batch file %sCompile_%s_%s.bat', [comp, packName, s_ver]) );
            Exit;
          end;
        cmd := Format('%smake" -f %s.bpk', [ExtractFilePath(dcc), Format('%s_%s', [packName, s_ver])]);
        if FileExec( cmd, False, True ) then
          begin
            // if we didn't get the lib file, try again !
            if not FileExists(Format('%s%s.lib', [comp, Format('%s_%s', [packName, s_ver])])) then
              begin
                // First delete all OBJ, DCU, HPP files to force the compilation
                if packName = 'Python' then
                  begin
                    DeleteFiles( comp+'Sources\Core\*.dcu' );
                    DeleteFiles( comp+'Sources\Core\*.hpp' );
                    DeleteFiles( comp+'Sources\Core\*.obj' );
                  end
                else if packName = 'PythonVCL' then
                  begin
                    DeleteFiles( comp+'Sources\Vcl\*.dcu' );
                    DeleteFiles( comp+'Sources\Vcl\*.hpp' );
                    DeleteFiles( comp+'Sources\Vcl\*.obj' );
                  end;
                // Compile directly with -jphn
                cmd := dcc + ' -jphn ' + args;
                if FileExec( cmd, False, True ) then
                  begin
                    cmd := Format('%smake" -f %s.bpk', [ExtractFilePath(dcc), Format('%s_%s', [packName, s_ver])]);
                    FileExec( cmd, False, True );
                    if FileExists(fname) then
                      CopyFile( PChar(fname), PChar(GetSysDir+'\'+ExtractFileName(fname)), False);
                  end;
              end;
            if FileExists(Format('%s%s.lib', [comp, Format('%s_%s', [packName, s_ver])])) then
              AddToLog( Format('Compiled package %s%s_%s.bpk', [comp, packName, s_ver]) )
            else
              begin
                AddToLog( Format( '[WARNING] Could not compile package %s_%s.bpk. Please do it yourself.', [packName, s_ver]) );
                AddToLog( Format( '[HINT] Look at the batch file %sCompile_%s_%s.bat', [comp, packName, s_ver]) );
              end;
          end
        else
          begin
            AddToLog( Format( '[WARNING] Could not compile package %s_%s.bpk. Please do it yourself.', [packName, s_ver]) );
            AddToLog( Format( '[HINT] Look at the batch file %sCompile_%s_%s.bat', [comp, packName, s_ver]) );
          end;
      end;
}
    Application.ProcessMessages;
    if FileExists( fname ) then
      AddPackage( fname, packLabel, Version, Compiler )
    else
      begin
        Result := False;
        if Compiler in [coDelphi, coBDS] then
          AddToLog( Format('[ERROR] Compilation of package %s_%s.dpk failed ! Please, do it yourself.', [packName, s_ver]))
        else
          AddToLog( Format('[ERROR] Compilation of package %s_%s.bpk failed ! Please, do it yourself.', [packName, s_ver]));
        AddToLog( Format( '[HINT] Look at the batch file %sCompile_%s_%s.bat', [comp, packName, s_ver]) );
        Exit;
      end;
  end;

begin
  try
    if Compiler = coDelphi then
      s_ver := Format('d%d', [Version])
    else if Compiler = coBDS then
      s_ver := Format('bds%d', [Version])
    else
      s_ver := Format('cb%d', [Version]);
    // Remove packages
    RemovePackage(Format('Python.%s',[GetPackageExt(Version, Compiler)]), Version, Compiler);
    RemovePackage( Format('Python_%s.%s',[s_ver, GetPackageExt(Version, Compiler)]), Version, Compiler );
    RemovePackage( Format('PythonVCL_%s.%s',[s_ver, GetPackageExt(Version, Compiler)]), Version, Compiler );
    CheckPath(Version, Compiler);
    comp := GetPytKey('ComponentsPath');
    chdir( comp );
    s_compiler := Format( '%sbin\dcc32.exe', [GetRootDir( Version, Compiler )]);
    s_core := Format('%sSources\Core', [comp]);
    s_vcl := Format('%sSources\VCL', [comp]);
    if Compiler = coBuilder then
      AddToPath( comp, Version, Compiler );
    AddToPath( s_core, Version, Compiler );
    AddToPath( s_vcl, Version, Compiler );

    if not CompilePackage( 'Python', 'Components for Python' ) then
      Exit;
    if Compiler = coBuilder then
      if not FileExec( Format('"%s" -jphn Sources\Core\PythonAtom.pas', [s_compiler]), False, True ) then
        AddToLog( 'Could not compile unit Sources\Core\PythonAtom.pas' );
    if not CompilePackage( 'PythonVCL', 'Components for Python (VCL)' ) then
      Exit;

    AddToLog( Format( '[SUCCESS] *** Installation for %s %d successful.', [GetCompiler(Compiler), Version] ) );
  except
    on E : Exception do
      AddToLog( '[ERROR] '+E.Message );
  end;
  AddToLog( '' );
end;

function TDelphiInstall.IsDelphiVerXInstalled(Version: Integer; Compiler : TCompiler): Boolean;
begin
  with TRegistry.Create do
    try
      Access := KEY_READ;
      RootKey := HKEY_LOCAL_MACHINE;
      Result := KeyExists( Format( 'Software\Borland\%s\%d.0', [GetCompiler(Compiler), Version] ) );
    finally
      Free;
    end;
end;

procedure TDelphiInstall.SetSearchPath(Version: Integer; const path: String; Compiler : TCompiler);
var
  search : String;
begin
  if (Version < 4) and (Compiler = coDelphi) then
    search := 'SearchPath'
  else
    search := 'Search Path';
  with TRegistry.Create do
    try
      Access := KEY_WRITE;
      RootKey := HKEY_CURRENT_USER;
      if KeyExists( Format( 'Software\Borland\%s\%d.0\Library', [GetCompiler(Compiler), Version] ) ) then
        begin
          OpenKey( Format( 'Software\Borland\%s\%d.0\Library', [GetCompiler(Compiler), Version] ), False );
          WriteString(search, path);
        end
      else
        raise Exception.CreateFmt( 'Could not get Search path of %s version %d', [GetCompiler(Compiler), Version] );
    finally
      Free;
    end;
end;

procedure TDelphiInstall.AddPackage(const FileName, Description: String; Version: Integer; Compiler : TCompiler);
begin
  with TRegistry.Create do
    try
      Access := KEY_WRITE;
      RootKey := HKEY_CURRENT_USER;
      if KeyExists( Format( 'Software\Borland\%s\%d.0\Known Packages', [GetCompiler(Compiler), Version] ) ) then
        begin
          OpenKey( Format( 'Software\Borland\%s\%d.0\Known Packages', [GetCompiler(Compiler), Version] ), False );
          WriteString(FileName, Description);
          AddToLog( Format('Added package %s to %s%d', [FileName, GetCompiler(Compiler), Version]) );
        end
      else
        raise Exception.CreateFmt( 'Could not get Known packages of %s version %d', [GetCompiler(Compiler), Version] );
    finally
      Free;
    end;
end;

procedure TDelphiInstall.btnCompileClick(Sender: TObject);
begin
  if not IsDefaultPythonVersionSelected then
  begin
    MessageDlg('Sorry, you must specify your default Python version before compiling the packages.', mtWarning, [mbOk], 0);
    Exit;
  end;
  ModifyDefaultPythonVersion;
  if CanWriteToRegistry then
  begin
    FPageVisible[1] := True;
    DefineVisiblePages;
    NextPage;
    Application.ProcessMessages;
    if cbD3.Enabled and cbD3.Checked then
      InstallDelphiVer(3, coDelphi);
    if cbD4.Enabled and cbD4.Checked then
      InstallDelphiVer(4, coDelphi);
    if cbD5.Enabled and cbD5.Checked then
      InstallDelphiVer(5, coDelphi);
    if cbD6.Enabled and cbD6.Checked then
      InstallDelphiVer(6, coDelphi);
    if cbD7.Enabled and cbD7.Checked then
      InstallDelphiVer(7, coDelphi);
    if cbD2005.Enabled and cbD2005.Checked then
      InstallDelphiVer(3, coBDS);
    if cbD2006.Enabled and cbD2006.Checked then
      InstallDelphiVer(4, coBDS);
    if cbCB3.Enabled and cbCB3.Checked then
      InstallDelphiVer(3, coBuilder);
    if cbCB4.Enabled and cbCB4.Checked then
      InstallDelphiVer(4, coBuilder);
    if cbCB5.Enabled and cbCB5.Checked then
      InstallDelphiVer(5, coBuilder);
  end
  else
  begin
    MessageDlg('Sorry, you must have Administrator priviledges to compile and install PythonForDelphi properly.', mtWarning, [mbOk], 0);
    btnIgnoreClick(btnIgnore);
  end;
end;

procedure TDelphiInstall.btnIgnoreClick(Sender: TObject);
begin
  FPageVisible[1] := False;
  DefineVisiblePages;
  ModifyDefaultPythonVersion;
  NextPage;
end;

procedure TDelphiInstall.BitBtn9Click(Sender: TObject);
begin
  PreviousPage;
end;

procedure TDelphiInstall.BitBtn8Click(Sender: TObject);
begin
  NextPage;
end;

procedure TDelphiInstall.BitBtn12Click(Sender: TObject);
begin
  PreviousPage;
end;

procedure TDelphiInstall.AddToLog(const msg: String);
begin
  memLog.Lines.Add( msg );
end;

function TDelphiInstall.GetMainPath : String;
begin
  Result := '';
  with TRegistry.Create do
    try
      Access := KEY_READ;
      RootKey := HKEY_LOCAL_MACHINE;
      if KeyExists( 'Software\Open Source\Python For Delphi\3.x' ) then
        begin
          OpenKey( 'Software\Open Source\Python For Delphi\3.x', False );
          Result := ReadString('MainPath');
        end;
    finally
      Free;
    end;
end;

procedure TDelphiInstall.FormShow(Sender: TObject);

  procedure ExtractCompVersion;
  var
    i, j : Integer;
    line, ver : String;
  begin
    ver := '';
    with memChanges.Lines do
      for i := Count-1 downto 0 do
        begin
          line := Strings[i];
          if (line <> '') and (line[1] in ['0'..'9']) then
            begin
              j := 1;
              while (j <= length(line)) and (line[j] in ['0'..'9','.']) do
                begin
                  ver := ver + line[j];
                  Inc(j);
                end;
              lCompVer.Caption := ver;
              Break;
            end;
        end;
  end;

  procedure LoadFile( const FileName : String; Memo : TMemo; SeekToEnd : Boolean );
  var
    f : String;
  begin
    // Load file
    f := GetMainPath;
    if f <> '' then
      begin
        if (Length(f)>0) and (f[length(f)] <> '\') then
           f := f + '\';
        f := f + FileName;
        if FileExists(f) then
          begin
            Memo.Lines.LoadFromFile(f);
            ExtractCompVersion;
          end;
      end;
    // Scroll at the end of the text
    if SeekToEnd then
      SendMessage(Memo.Handle, EM_LINESCROLL, 0, Memo.Lines.Count);
  end;

begin
  btnCompile.SetFocus;
  LoadFile( 'changes.txt', memChanges, True );
  LoadFile( 'C++ Builder Notes.txt', memCBNotes, False );
end;

procedure TDelphiInstall.WebExec( const url : String );
begin
    ShellExecute(Self.Handle,
                 nil,
                 PChar(url),
                 nil,
                 nil,
                 SW_SHOWNORMAL);
end;

procedure TDelphiInstall.BitBtn22Click(Sender: TObject);
begin
  WebExec( 'http://mmm-experts.com/Downloads.aspx?ProductId=3' );
end;

procedure TDelphiInstall.btnWikiClick(Sender: TObject);
begin
  WebExec( 'http://py4d.pbwiki.com/' );
end;

procedure TDelphiInstall.BitBtn25Click(Sender: TObject);
begin
  WebExec( 'http://www.python.org/' );
end;

procedure TDelphiInstall.BitBtn24Click(Sender: TObject);
begin
  WebExec( 'http://www.python.org/2.3.4/' );
end;

function TDelphiInstall.CheckRegLocalKey(const key: String): Boolean;
begin
  with TRegistry.Create do
    try
      Access := KEY_READ;
      RootKey := HKEY_LOCAL_MACHINE;
      Result := KeyExists( Key );
    finally
      Free;
    end;
end;

procedure TDelphiInstall.LogNextClick(Sender: TObject);
begin
  NextPage;
end;

procedure TDelphiInstall.BitBtn15Click(Sender: TObject);
begin
  PreviousPage;
end;

procedure TDelphiInstall.NextPage;
var
  page : Integer;
begin
  page := Notebook1.PageIndex;
  if page < Notebook1.Pages.Count-1 then
    Inc(page);
  while (page < Notebook1.Pages.Count-1) and
        not FPageVisible[page] do
    Inc(Page);
  Notebook1.PageIndex := page;
end;

procedure TDelphiInstall.PreviousPage;
var
  page : Integer;
begin
  page := Notebook1.PageIndex;
  if page > 0 then
    Dec(page);
  while (page > 0) and
        not FPageVisible[page] do
    Dec(page);
  Notebook1.PageIndex := page;
end;

procedure TDelphiInstall.BitBtn11Click(Sender: TObject);
begin
  NextPage;
end;

procedure TDelphiInstall.BitBtn14Click(Sender: TObject);
begin
  NextPage;
end;

procedure TDelphiInstall.BitBtn18Click(Sender: TObject);
begin
  PreviousPage;
end;

procedure TDelphiInstall.BitBtn17Click(Sender: TObject);
begin
  NextPage;
end;

procedure TDelphiInstall.BitBtn21Click(Sender: TObject);
begin
  PreviousPage;
end;

function TDelphiInstall.CheckRegUserKey(const key: String): Boolean;
begin
  with TRegistry.Create do
    try
      Access := KEY_READ;
      RootKey := HKEY_CURRENT_USER;
      Result := KeyExists( Key );
    finally
      Free;
    end;
end;

procedure TDelphiInstall.RemovePackage(const PackageName: String;
  Version: Integer; Compiler : TCompiler);
var
  L : TStringList;
  i : Integer;
  s1, s2 : String;
begin
  s2 := UpperCase(PackageName);
  L := TStringList.Create;
  try
    with TRegistry.Create do
      try
        Access := KEY_WRITE;
        RootKey := HKEY_CURRENT_USER;
        if KeyExists( Format( 'Software\Borland\%s\%d.0\Known Packages', [GetCompiler(Compiler), Version] ) ) then
          begin
            OpenKey( Format( 'Software\Borland\%s\%d.0\Known Packages', [GetCompiler(Compiler), Version] ), False );
            GetValueNames(L);
            for i := 0 to L.Count-1 do
              begin
                s1 := UpperCase(ExtractFileName(L.Strings[i]));
                if s1 = s2 then
                  if DeleteValue( L.Strings[i] ) then
                    AddToLog( Format('Removed package %s from %s%d', [L.Strings[i], GetCompiler(Compiler), Version]) );
              end;
          end
        else
          raise Exception.CreateFmt( 'Could not get Known packages of %s version %d', [GetCompiler(Compiler), Version] );
      finally
        Free;
      end;
  finally
    L.Free;
  end;
  RemoveDisabledPackage(PackageName, Version, Compiler);
end;

procedure TDelphiInstall.RemoveDisabledPackage(const PackageName: String;
  Version: Integer; Compiler : TCompiler);
var
  L : TStringList;
  i : Integer;
  s1, s2 : String;
begin
  s2 := UpperCase(PackageName);
  L := TStringList.Create;
  try
    with TRegistry.Create do
      try
        Access := KEY_WRITE;
        RootKey := HKEY_CURRENT_USER;
        if KeyExists( Format( 'Software\Borland\%s\%d.0\Disabled Packages', [GetCompiler(Compiler), Version] ) ) then
          begin
            OpenKey( Format( 'Software\Borland\%s\%d.0\Disabled Packages', [GetCompiler(Compiler), Version] ), False );
            GetValueNames(L);
            for i := 0 to L.Count-1 do
              begin
                s1 := UpperCase(ExtractFileName(L.Strings[i]));
                if s1 = s2 then
                  if DeleteValue( L.Strings[i] ) then
                    AddToLog( Format('Removed disabled package %s from %s%d', [L.Strings[i], GetCompiler(Compiler), Version]) );
              end;
          end;
      finally
        Free;
      end;
  finally
    L.Free;
  end;
end;

procedure TDelphiInstall.SplitPath(const Path: String; Dest: TStrings);
var
  s, sp : String;
  idx : Integer;
begin
  Dest.Clear;
  sp := Path;
  repeat
    idx := Pos( ';', sp );
    if idx > 0 then
      begin
        s := Copy( sp, 1, idx-1 );
        Delete( sp, 1, idx );
        Dest.Add( s );
      end;
  until idx = 0;
  if Length(sp) > 0 then
    Dest.Add( sp );
end;

function TDelphiInstall.StringsToPath( Strings : TStrings ) : String;
var
  i : Integer;
begin
  if Strings.Count = 0 then
    begin
      Result := '';
      Exit;
    end;
  Result := Strings.Strings[0];
  for i := 1 to Strings.Count-1 do
    Result := Result + ';' + Strings.Strings[i];
end;

procedure TDelphiInstall.CheckPath(Version: Integer; Compiler : TCompiler);

  function DoCheck( const path : String ) : Boolean;
  var
    SR : TSearchRec;
    Found : Integer;
    source : String;
  begin
    Result := False;
    if not DirectoryExists( path ) then
      Exit;
    source := path;
    if (Length(source) > 0) and (source[Length(source)] <> '\') then
      source := source + '\';
    // Remove the files in the directory
    Found := FindFirst( source+'*.*', faAnyFile, SR );
    try
      while Found = 0  do
        begin
          if (SR.Name<>'.') and (SR.Name <> '..') then
            begin
              if (SR.Attr and faDirectory) = 0 then
                begin
                  if (CompareText( SR.Name, 'PythonEngine.pas' ) = 0) or
                     (CompareText( SR.Name, 'PythonVclStd.pas' ) = 0) or
                     (CompareText( SR.Name, 'PythonDatabase.pas' ) = 0) or
                     (CompareText( SR.Name, 'Python_d5.dpk' ) = 0) then
                    begin
                      Result := True;
                      Break;
                    end;
                end;
            end;
          Found := FindNext( SR );
        end;
    finally
      FindClose(SR);
    end;
  end;

var
  L : TStringList;
  i : Integer;
begin
  L := TStringList.Create;
  try
    SplitPath( GetSearchPath( Version, Compiler ), L);
    for i := L.Count-1 downto 0 do
      if DoCheck( L.Strings[i] ) then
        begin
          AddToLog( Format('Removed "%s" from %s%d path.', [L.Strings[i], GetCompiler(Compiler), Version]) );
          L.Delete(i);
        end;
    SetSearchPath( Version, StringsToPath(L), Compiler );
  finally
    L.Free;
  end;
end;

procedure TDelphiInstall.BitBtn1Click(Sender: TObject);
begin
  PreviousPage;
end;

procedure TDelphiInstall.BitBtn5Click(Sender: TObject);
begin
  NextPage;
end;

procedure TDelphiInstall.BitBtn27Click(Sender: TObject);
begin
  WebExec('mailto:pythonfordelphi-subscribe@yahoogroups.com');
end;

procedure TDelphiInstall.BitBtn28Click(Sender: TObject);
begin
  WebExec('http://groups.yahoo.com/group/pythonfordelphi/');
end;

function TDelphiInstall.GetCompiler(Compiler: TCompiler): String;
begin
  if Compiler = coDelphi then
    Result := 'Delphi'
  else if Compiler = coBuilder then
    Result := 'C++Builder'
  else
    Result := 'BDS';
end;

procedure TDelphiInstall.BitBtn29Click(Sender: TObject);
begin
  PreviousPage;
end;

procedure TDelphiInstall.BitBtn30Click(Sender: TObject);
begin
  NextPage;
end;

function TDelphiInstall.IsPythonVerXInstalled(const Version: String): Boolean;
var
  _handle : THandle;
begin
  _handle := LoadLibrary(PChar(Format('python%s.dll', [Version])));
  Result := _handle <> 0;
  if Result then
    FreeLibrary(_handle);
end;

function TDelphiInstall.IsDefaultPythonVersionSelected: Boolean;
begin
  Result := rbPython15.Checked or rbPython20.Checked or rbPython21.Checked or
            rbPython22.Checked or rbPython23.Checked or rbPython24.Checked or
            rbPython25.Checked;
end;

function TDelphiInstall.GetDefaultPythonVersion: String;
begin
  if rbPython15.Checked then
    Result := 'PYTHON15'
  else if rbPython20.Checked then
    Result := 'PYTHON20'
  else if rbPython21.Checked then
    Result := 'PYTHON21'
  else if rbPython22.Checked then
    Result := 'PYTHON22'
  else if rbPython23.Checked then
    Result := 'PYTHON23'
  else if rbPython24.Checked then
    Result := 'PYTHON24'
  else if rbPython25.Checked then
    Result := 'PYTHON25'
  else
    Result := '';
end;

function TDelphiInstall.GetDefaultPythonVersionNumber: String;
begin
  if rbPython15.Checked then
    Result := '1.5'
  else if rbPython20.Checked then
    Result := '2.0'
  else if rbPython21.Checked then
    Result := '2.1'
  else if rbPython22.Checked then
    Result := '2.2'
  else if rbPython23.Checked then
    Result := '2.3'
  else if rbPython24.Checked then
    Result := '2.4'
  else if rbPython25.Checked then
    Result := '2.5'
  else
    Result := '';
end;


procedure TDelphiInstall.ModifyDefaultPythonVersion;
const
  START_TAG = '{---<START OF DEFAULT PYTHON VERSION>---}';
  END_TAG = '{---<END OF DEFAULT PYTHON VERSION>---}';
var
  i : Integer;
  _version : String;       
  _fileName : String;
begin
  _version := GetDefaultPythonVersion;
  if _version = '' then
    Exit;
  _fileName := GetDefinitionFileName;
  if not FileExists(_fileName) then
    Exit;
  with TStringList.Create do
  try
    LoadFromFile(_fileName);
    for i := 0 to Count-2 do
      if SameText(Copy(Strings[i], 1, Length(START_TAG)), START_TAG) then
      begin
        if SameText(Copy(Strings[i+2], 1, Length(END_TAG)), END_TAG) then
        begin
          Strings[i+1] := Format('{$DEFINE %s}', [_version]);
          Break;
        end;
      end;
    SaveToFile(_fileName);
  finally
    Free;
  end;
end;

procedure TDelphiInstall.BitBtn2Click(Sender: TObject);
begin
  ShowMessage('Python for Delphi is compatible with all versions of Python, but, as there are some big differences between the versions, '+
              'you must specify a conditional symbol that will compile the code differently according to the selected version.'+#13+
              'All the symbols are defines in the file Definition.inc, and this file will be updated automatically by the installer.'+#13+
              'Note that this is only for the default version, if no symbol was already defined for a project. It means that you can specify '+
              'a Python version for each of your project by adding PYTHON15, PYTHON20, PYTHON21 or PYTHON22 to your project options, in the conditionals page.');
end;

function TDelphiInstall.GetDefinitionFileName: String;
begin
  Result := GetMainPath;
  if (Length(Result)>0) and (Result[length(Result)] <> '\') then
    Result := Result + '\';
  Result := Result + 'Components\Sources\Core\Definition.Inc';
end;

procedure TDelphiInstall.btnEditDefinitionClick(Sender: TObject);
begin
  fileExec(Format('notepad %s', [GetDefinitionFileName]), False, False);
end;

procedure TDelphiInstall.BitBtn32Click(Sender: TObject);
begin
  WebExec( 'http://mmm-experts.com/Downloads.aspx?ProductId=4' );
end;

procedure TDelphiInstall.DefineVisiblePages;
begin
  FPageVisible[2] := not CheckRegLocalKey('Software\Python\PythonCore');
  FPageVisible[3] := not CheckRegUserKey('Software\MMM-Experts\PyScripter');
  FPageVisible[7] := cbCB3.Enabled or cbCB4.Enabled or cbCB5.Enabled;
end;

end.

