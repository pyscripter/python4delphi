unit WrapDelphiFmx;

interface

implementation

uses
  WrapDelphiTypes,
  WrapDelphiClasses,
  {$IFDEF MSWINDOWS}
  WrapDelphiWindows,
  {$ENDIF MSWINDOWS}
  WrapDelphiDataBind,
  {$IFNDEF LINUX}
  WrapFmxDataBind,
  {$ENDIF LINUX}
  WrapFmxTypes,
  WrapFmxControls,
  WrapFmxStdCtrls,
  WrapFmxEdit,
  WrapFmxListBox,
  WrapFmxListView,
  WrapFmxActnList,
  WrapFmxStdActns,
  WrapFmxComCtrls,
  WrapFmxDialogs,
  WrapFmxForms,
  WrapFmxShapes,
  WrapFmxLayouts,
  WrapFmxScrollBox,
  WrapFmxGrids,
  WrapFmxMedia,
  WrapFmxMenus,
  WrapFmxStyles,
  WrapFmxMemo,
  WrapFmxColors;

end.
