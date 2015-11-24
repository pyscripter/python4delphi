{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit uP4DLaz;

interface

uses
  PythonEngine, PythonGUIInputOutput, MethodCallBack, WrapDelphiWindows, 
  TinyWideStrings, VarPyth, WrapDelphi, WrapDelphiActnList, WrapDelphiButtons, 
  WrapDelphiClasses, WrapDelphiComCtrls, WrapDelphiControls, 
  WrapDelphiExtCtrls, WrapDelphiForms, WrapDelphiGraphics, WrapDelphiGrids, 
  WrapDelphiStdCtrls, WrapDelphiTypes, WrapDelphiVCL, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('PythonEngine', @PythonEngine.Register);
  RegisterUnit('PythonGUIInputOutput', @PythonGUIInputOutput.Register);
  RegisterUnit('WrapDelphi', @WrapDelphi.Register);
  RegisterUnit('uP4DLaz', @uP4DLaz.Register);
end;

initialization
  RegisterPackage('P4DLaz', @Register);
end.
