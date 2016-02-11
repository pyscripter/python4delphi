{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit P4DFpc;

interface

uses
  PythonEngine, MethodCallBack, TinyWideStrings, VarPyth, WrapDelphiClasses, 
  WrapDelphiTypes, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('PythonEngine', @PythonEngine.Register);
end;

initialization
  RegisterPackage('P4DFpc', @Register);
end.
