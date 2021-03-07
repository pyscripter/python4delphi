{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit P4DLaz;

{$warn 5023 off : no warning about unused units}
interface

uses
  PythonEngine, MethodCallBack, VarPyth, Lcl.PythonGUIInputOutput, 
  WrapDelphi, WrapDelphiClasses, WrapDelphiTypes, 
  P4DLaz_register, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('P4DLaz_register', @P4DLaz_register.Register);
end;

initialization
  RegisterPackage('P4DLaz', @Register);
end.
