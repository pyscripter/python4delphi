unit Vcl.WrapDelphiVCL;

{
  Helper unit that will register all the wrappers for the VCL.
  Instead of including WrapDelphiClasses, WrapDelphiControls... into your uses
  clause, simply add WrapDelphiVCL.
}
interface

implementation

uses
  Vcl.WrapDelphiTypes,
  Vcl.WrapDelphiClasses,
  Vcl.WrapDelphiWindows,
  Vcl.WrapDelphiControls,
  Vcl.WrapDelphiGraphics,
  Vcl.WrapDelphiForms,
  Vcl.WrapDelphiActnList,
  Vcl.WrapDelphiStdCtrls,
  Vcl.WrapDelphiComCtrls,
  Vcl.WrapDelphiExtCtrls,
  Vcl.WrapDelphiButtons,
  Vcl.WrapDelphiGrids,
  Vcl.WrapDelphiSamplesSpin;

end.
