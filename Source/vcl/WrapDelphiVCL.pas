
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
  WrapVclImgList,
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
