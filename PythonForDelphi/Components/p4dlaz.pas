{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit P4DLaz; 

interface

uses
    PythonEngine, PythonGUIInputOutput, MethodCallBack, WrapDelphiWindows, 
  TinyWideStrings, VarPyth, WrapDelphi, WrapDelphiActnList, WrapDelphiButtons, 
  WrapDelphiClasses, WrapDelphiComCtrls, WrapDelphiControls, 
  WrapDelphiExtCtrls, WrapDelphiForms, WrapDelphiGraphics, WrapDelphiGrids, 
  WrapDelphiStdCtrls, WrapDelphiTypes, WrapDelphiVCL, LazarusPackageIntf;

implementation
{$R Sources\Core\PythonEngine.dcr}
procedure Register; 
begin
  RegisterUnit('PythonEngine', @PythonEngine.Register); 
  RegisterUnit('PythonGUIInputOutput', @PythonGUIInputOutput.Register); 
  RegisterUnit('WrapDelphi', @WrapDelphi.Register); 
end; 

initialization
  RegisterPackage('P4DLaz', @Register); 
end.
