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
{$I ..\Definition.Inc}

unit WrapVclImgList;

interface

uses
  System.Classes, System.SysUtils, Vcl.ImgList,
  PythonEngine, WrapDelphi, WrapDelphiImageList;

type
  TPyDelphiCustomImageList = class (TPyDelphiBaseImageList)
  private
    function  GetDelphiObject: TCustomImageList;
    procedure SetDelphiObject(const Value: TCustomImageList);
  public
    class function  DelphiObjectClass : TClass; override;
    // Properties
    property DelphiObject: TCustomImageList read GetDelphiObject write SetDelphiObject;
  end;

implementation

{ Register the wrappers, the globals and the constants }
type
  TVclImageListRegistration = class(TRegisteredUnit)
  public
    function Name : string; override;
    procedure RegisterWrappers(APyDelphiWrapper : TPyDelphiWrapper); override;
  end;

{ TVclImageListRegistration }

function TVclImageListRegistration.Name: string;
begin
  Result := 'VclImageList';
end;

procedure TVclImageListRegistration.RegisterWrappers(
  APyDelphiWrapper: TPyDelphiWrapper);
begin
  inherited;
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiCustomImageList);
end;

{ TPyDelphiCustomImageList }

class function TPyDelphiCustomImageList.DelphiObjectClass: TClass;
begin
  Result := TCustomImageList;
end;

function TPyDelphiCustomImageList.GetDelphiObject: TCustomImageList;
begin
  Result := TCustomImageList(inherited DelphiObject);
end;

procedure TPyDelphiCustomImageList.SetDelphiObject(const Value: TCustomImageList);
begin
  inherited DelphiObject := Value;
end;

initialization
  RegisteredUnits.Add(TVclImageListRegistration.Create());

end.
