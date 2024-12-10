(**************************************************************************)
(*  This unit is part of the Python for Delphi (P4D) library              *)
(*  Project home: https://github.com/pyscripter/python4delphi             *)
(*                                                                        *)
(*  Project Maintainer:  PyScripter (pyscripter@gmail.com)                *)
(*  Original Authors:    Dr. Dietmar Budelsky (dbudelsky@web.de)          *)
(*                       Morgan Martinet (https://github.com/mmm-experts) *)
(*  Core developer:      Lucas Belo (lucas.belo@live.com)                 *)
(*  Contributors:        See contributors.md at project home              *)
(*                                                                        *)
(*  LICENCE and Copyright: MIT (see project home)                         *)
(**************************************************************************)

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
  WrapFmxDataBind,
  WrapFmxTypes,
  WrapFmxImgList,
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
  WrapFmxColors,
  WrapFmxDateTime;

end.
