unit WrapDelphiVCL;

{
  Helper unit that will register all the wrappers for the VCL.
  Instead of including WrapDelphiClasses, WrapDelphiControls... into your uses
  clause, simply add WrapDelphiVCL.
}
interface

implementation

uses
  WrapDelphiTypes,
  WrapDelphiClasses,
  WrapDelphiWindows,
  WrapDelphiControls,
  WrapDelphiGraphics,
  WrapDelphiForms,
  WrapDelphiActnList,
  WrapDelphiStdCtrls,
  WrapDelphiComCtrls,
  WrapDelphiExtCtrls,
  WrapDelphiButtons,
  WrapDelphiGrids;

end.
