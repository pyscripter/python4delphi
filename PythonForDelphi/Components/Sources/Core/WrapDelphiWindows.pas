{$I Definition.Inc}

unit WrapDelphiWindows;

interface

uses
  Windows, Classes, SysUtils, PythonEngine, WrapDelphi, WrapDelphiClasses;

implementation

{ Register the wrappers, the globals and the constants }
type
  TWindowsRegistration = class(TRegisteredUnit)
  public
    function Name : String; override;
    procedure RegisterWrappers(APyDelphiWrapper : TPyDelphiWrapper); override;
    procedure DefineVars(APyDelphiWrapper : TPyDelphiWrapper); override;
  end;

{ TWindowsRegistration }

procedure TWindowsRegistration.DefineVars(APyDelphiWrapper: TPyDelphiWrapper);
begin
  inherited;
  APyDelphiWrapper.DefineVar('MB_OK',  MB_OK);
  APyDelphiWrapper.DefineVar('MB_YESNO',  MB_YESNO);
  APyDelphiWrapper.DefineVar('MB_YESNOCANCEL',  MB_YESNOCANCEL);
  APyDelphiWrapper.DefineVar('MB_OKCANCEL',  MB_OKCANCEL);
  APyDelphiWrapper.DefineVar('MB_ABORTRETRYIGNORE',  MB_ABORTRETRYIGNORE);
  APyDelphiWrapper.DefineVar('MB_RETRYCANCEL', MB_RETRYCANCEL);
  APyDelphiWrapper.DefineVar('MB_ICONINFORMATION',  MB_ICONINFORMATION);
  APyDelphiWrapper.DefineVar('MB_ICONHAND', MB_ICONHAND);
  APyDelphiWrapper.DefineVar('MB_ICONQUESTION', MB_ICONQUESTION);
  APyDelphiWrapper.DefineVar('MB_ICONEXCLAMATION', MB_ICONEXCLAMATION);
  APyDelphiWrapper.DefineVar('MB_ICONASTERISK', MB_ICONASTERISK);
  APyDelphiWrapper.DefineVar('MB_ICONWARNING', MB_ICONWARNING);
  APyDelphiWrapper.DefineVar('MB_ICONERROR', MB_ICONERROR);
  APyDelphiWrapper.DefineVar('MB_ICONSTOP', MB_ICONSTOP);
  APyDelphiWrapper.DefineVar('MB_DEFBUTTON1', MB_DEFBUTTON1);
  APyDelphiWrapper.DefineVar('MB_DEFBUTTON2', MB_DEFBUTTON2);
  APyDelphiWrapper.DefineVar('MB_DEFBUTTON3', MB_DEFBUTTON3);
  APyDelphiWrapper.DefineVar('MB_DEFBUTTON4', MB_DEFBUTTON4);
  APyDelphiWrapper.DefineVar('MB_APPLMODAL', MB_APPLMODAL);
  APyDelphiWrapper.DefineVar('MB_SYSTEMMODAL', MB_SYSTEMMODAL);
  APyDelphiWrapper.DefineVar('MB_TASKMODAL', MB_TASKMODAL);
  APyDelphiWrapper.DefineVar('MB_HELP', MB_HELP);
  {$IFNDEF FPC}
  APyDelphiWrapper.DefineVar('MB_NOFOCUS', MB_NOFOCUS);
  {$ENDIF FPC}

  APyDelphiWrapper.DefineVar('IDOK', IDOK);
  APyDelphiWrapper.DefineVar('IDCANCEL', IDCANCEL);
  APyDelphiWrapper.DefineVar('IDABORT', IDABORT);
  APyDelphiWrapper.DefineVar('IDRETRY', IDRETRY);
  APyDelphiWrapper.DefineVar('IDIGNORE', IDIGNORE);
  APyDelphiWrapper.DefineVar('IDYES', IDYES);
  APyDelphiWrapper.DefineVar('IDNO', IDNO);
  APyDelphiWrapper.DefineVar('IDCLOSE', IDCLOSE);
  APyDelphiWrapper.DefineVar('IDHELP', IDHELP);
  {$IFNDEF FPC}
  APyDelphiWrapper.DefineVar('IDTRYAGAIN', IDTRYAGAIN);
  APyDelphiWrapper.DefineVar('IDCONTINUE', IDCONTINUE);
  {$ENDIF FPC}
end;

function TWindowsRegistration.Name: String;
begin
  Result := 'Windows';
end;

procedure TWindowsRegistration.RegisterWrappers(APyDelphiWrapper: TPyDelphiWrapper);
begin
  inherited;
end;

initialization
  RegisteredUnits.Add( TWindowsRegistration.Create );
end.

