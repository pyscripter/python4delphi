{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit P4DLaz;

interface

uses
  PythonEngine, PythonGUIInputOutput, MethodCallBack, TinyWideStrings, 
  VarPyth, WrapDelphi, WrapDelphiButtons, WrapDelphiClasses, 
  WrapDelphiControls, WrapDelphiTypes, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('PythonEngine', @PythonEngine.Register);
  RegisterUnit('PythonGUIInputOutput', @PythonGUIInputOutput.Register);
  RegisterUnit('WrapDelphi', @WrapDelphi.Register);
end;

initialization
  RegisterPackage('P4DLaz', @Register);
end.
