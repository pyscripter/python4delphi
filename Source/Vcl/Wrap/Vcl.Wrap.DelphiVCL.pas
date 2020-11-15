unit Vcl.Wrap.DelphiVCL;

{
  Helper unit that will register all the wrappers for the VCL.
  Instead of including WrapDelphiClasses, WrapDelphiControls... into your uses
  clause, simply add WrapDelphiVCL.
}
interface

implementation

uses
  Vcl.Wrap.DelphiTypes,
  Vcl.Wrap.DelphiClasses,
  Vcl.Wrap.DelphiWindows,
  Vcl.Wrap.DelphiControls,
  Vcl.Wrap.DelphiGraphics,
  Vcl.Wrap.DelphiForms,
  Vcl.Wrap.DelphiActnList,
  Vcl.Wrap.DelphiStdCtrls,
  Vcl.Wrap.DelphiComCtrls,
  Vcl.Wrap.DelphiExtCtrls,
  Vcl.Wrap.DelphiButtons,
  Vcl.Wrap.DelphiGrids,
  Vcl.Wrap.DelphiSamplesSpin;

end.
