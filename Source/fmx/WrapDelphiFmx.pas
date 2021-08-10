unit WrapDelphiFmx;

interface

implementation

uses
  WrapDelphiTypes,
  WrapDelphiClasses,
  {$IFNDEF ANDROID}
  WrapDelphiWindows,
  {$ENDIF ANDROID}
  WrapFireDac,
  WrapFmxTypes,
  WrapFmxStdCtrls,
  WrapFmxEdit,
  WrapFmxListBox,
  WrapFmxActnList,
  WrapFmxComCtrls,
  WrapFmxDialogs,
  WrapFmxForms,
  WrapFmxShapes,
  WrapFmxLayouts,
  WrapFmxScrollBox,
  WrapFmxGrids;

end.
