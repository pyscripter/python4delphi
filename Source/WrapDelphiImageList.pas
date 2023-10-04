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
{$I Definition.Inc}

unit WrapDelphiImageList;

interface

uses
  System.Classes, System.SysUtils, System.ImageList,
  PythonEngine, WrapDelphi, WrapDelphiClasses;

type
  TPyDelphiBaseImageList = class (TPyDelphiComponent)
  private
    function  GetDelphiObject: TBaseImageList;
    procedure SetDelphiObject(const Value: TBaseImageList);
  public
    class function  DelphiObjectClass : TClass; override;
    // Properties
    property DelphiObject: TBaseImageList read GetDelphiObject write SetDelphiObject;
  end;

implementation

{ Register the wrappers, the globals and the constants }
type
  TImageListRegistration = class(TRegisteredUnit)
  public
    function Name : string; override;
    procedure RegisterWrappers(APyDelphiWrapper : TPyDelphiWrapper); override;
  end;

{ TImageListRegistration }

function TImageListRegistration.Name: string;
begin
  Result := 'ImageList';
end;

procedure TImageListRegistration.RegisterWrappers(
  APyDelphiWrapper: TPyDelphiWrapper);
begin
  inherited;
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiBaseImageList);
end;

{ TPyDelphiBaseImageList }

class function TPyDelphiBaseImageList.DelphiObjectClass: TClass;
begin
  Result := TBaseImageList;
end;

function TPyDelphiBaseImageList.GetDelphiObject: TBaseImageList;
begin
  Result := TBaseImageList(inherited DelphiObject);
end;

procedure TPyDelphiBaseImageList.SetDelphiObject(const Value: TBaseImageList);
begin
  inherited DelphiObject := Value;
end;

initialization
  RegisteredUnits.Add(TImageListRegistration.Create());

end.
