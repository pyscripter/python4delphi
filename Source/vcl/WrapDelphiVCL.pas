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
  WrapDelphiDataBind,
  WrapActions,
  WrapVclControls,
  WrapVclGraphics,
  WrapVclForms,
  WrapVclActnList,
  WrapVclMenus,
  WrapVclStdCtrls,
  WrapVclComCtrls,
  WrapVclExtCtrls,
  WrapVclButtons,
  WrapVclGrids,
  WrapVclSamplesSpin,
  WrapVclWinXCtrls,
  WrapVclThemes,
  WrapVclDialogs,
  WrapVclMedia;

end.
