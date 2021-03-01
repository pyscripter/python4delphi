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
  WrapDelphiVclControls,
  WrapDelphiVclGraphics,
  WrapDelphiVclForms,
  WrapDelphiVclActnList,
  WrapDelphiVclStdCtrls,
  WrapDelphiVclComCtrls,
  WrapDelphiVclExtCtrls,
  WrapDelphiVclButtons,
  WrapDelphiVclGrids,
  WrapDelphiVclSamplesSpin;

end.
