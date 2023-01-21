{$I Definition.Inc}

unit WrapDelphiWindows;

interface

{$IFDEF MSWINDOWS}
uses
  Windows, Classes, SysUtils, PythonEngine, WrapDelphi, WrapDelphiClasses;
{$ENDIF MSWINDOWS}

implementation

{$IFDEF MSWINDOWS}

{$IFDEF DELPHI11_OR_HIGHER}
uses
  System.Win.HighDpi, Winapi.ShellScaling;
{$ENDIF DELPHI11_OR_HIGHER}

{ Register the wrappers, the globals and the constants }
type
  TWindowsRegistration = class(TRegisteredUnit)
  private
    {$IFDEF DELPHI11_OR_HIGHER}
    class function IsDpiAware_Wrapper(PySelf, AArgs: PPyObject): PPyObject; cdecl; static;
    class function SetHighDpiAware_Wrapper(PySelf, AArgs: PPyObject): PPyObject; cdecl; static;
    class function GetProcessDpiAwareness_Wrapper(PySelf, AArgs: PPyObject): PPyObject; cdecl; static;
    class function SetProcessDpiAwareness_Wrapper(PySelf, AArgs: PPyObject): PPyObject; cdecl; static;
    {$ENDIF DELPHI11_OR_HIGHER}
  public
    function Name : string; override;
    procedure RegisterWrappers(APyDelphiWrapper : TPyDelphiWrapper); override;
    procedure DefineVars(APyDelphiWrapper : TPyDelphiWrapper); override;
    procedure DefineFunctions(APyDelphiWrapper : TPyDelphiWrapper); override;
  end;

{ TWindowsRegistration }

function TWindowsRegistration.Name: string;
begin
  Result := 'Windows';
end;

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

procedure TWindowsRegistration.DefineFunctions(
  APyDelphiWrapper: TPyDelphiWrapper);
begin
  inherited;
  {$IFDEF DELPHI11_OR_HIGHER}
  APyDelphiWrapper.RegisterFunction(PAnsiChar('IsDpiAware'),
    TWindowsRegistration.IsDpiAware_Wrapper,
    PAnsiChar('IsDPIAware()'#10 +
    'Check for process DPI awareness.'));

  APyDelphiWrapper.RegisterFunction(PAnsiChar('SetHighDpiAware'),
    TWindowsRegistration.SetHighDpiAware_Wrapper,
    PAnsiChar('SetHighDpiAware()'#10 +
    'Automatically set the DPI awareness that best fits to the process.'));

  APyDelphiWrapper.RegisterFunction(PAnsiChar('GetProcessDpiAwareness'),
    TWindowsRegistration.GetProcessDpiAwareness_Wrapper,
    PAnsiChar('GetProcessDpiAwareness()'#10 +
    'Get the DPI awareness of the process.'));

  APyDelphiWrapper.RegisterFunction(PAnsiChar('SetProcessDpiAwareness'),
    TWindowsRegistration.SetProcessDpiAwareness_Wrapper,
    PAnsiChar('SetProcessDpiAwareness()'#10 +
    'Set the DPI awareness to the process.'));
  {$ENDIF DELPHI11_OR_HIGHER}
end;

procedure TWindowsRegistration.RegisterWrappers(APyDelphiWrapper: TPyDelphiWrapper);
begin
  inherited;
end;

{$IFDEF DELPHI11_OR_HIGHER}
class function TWindowsRegistration.IsDpiAware_Wrapper(PySelf, AArgs: PPyObject): PPyObject;
begin
  with GetPythonEngine() do
  begin
    if IsDpiAware() then
      Result := GetPythonEngine().ReturnTrue()
    else
      Result := GetPythonEngine().ReturnFalse();
  end;
end;

class function TWindowsRegistration.SetHighDpiAware_Wrapper(PySelf, AArgs: PPyObject): PPyObject;
begin
  with GetPythonEngine() do
  begin
    if SetHighDpiAware() then
      Result := GetPythonEngine().ReturnTrue()
    else
      Result := GetPythonEngine().ReturnFalse();
  end;
end;

class function TWindowsRegistration.GetProcessDpiAwareness_Wrapper(PySelf,
  AArgs: PPyObject): PPyObject;
var
  LErrorCode: HResult;
  LDpiAwareness: TProcessDpiAwareness;
begin
  with GetPythonEngine() do
  begin
    if (PyArg_ParseTuple(AArgs, ':GetProcessDpiAwareness') <> 0) then
    begin
      LErrorCode :=  WinAPI.ShellScaling.GetProcessDpiAwareness(GetCurrentProcess(), LDpiAwareness);

      Result := PyList_New(0);
      PyList_Append(Result, PyLong_FromLong(LErrorCode));
      PyList_Append(Result, PyLong_FromLong(Ord(LDpiAwareness)));
    end else
      Result := nil;
  end;
end;

class function TWindowsRegistration.SetProcessDpiAwareness_Wrapper(PySelf,
  AArgs: PPyObject): PPyObject;
var
  LErrorCode: HResult;
  LDpiAwareness: integer;
begin
  with GetPythonEngine() do
  begin
    if (PyArg_ParseTuple(AArgs, 'i:SetProcessDpiAwareness', @LDpiAwareness) <> 0) then
    begin
      if not (LDpiAwareness in [
        Ord(Low(TProcessDpiAwareness))
        ..
        Ord(High(TProcessDpiAwareness))]) then
      begin
        PyErr_SetString(PyExc_ValueError^, 'DPI awareness value out of range');
        Result := nil;
      end else begin
        LErrorCode :=  WinAPI.ShellScaling.SetProcessDpiAwareness(TProcessDpiAwareness(LDpiAwareness));
        Result := PyLong_FromLong(LErrorCode);
      end;
    end else
      Result := nil;
  end;
end;
{$ENDIF DELPHI11_OR_HIGHER}

initialization
  RegisteredUnits.Add(TWindowsRegistration.Create);

  {$IFDEF DELPHI11_OR_HIGHER}
  SetHighDpiAware();
  {$ENDIF DELPHI11_OR_HIGHER}

{$ENDIF MSWINDOWS}

end.

