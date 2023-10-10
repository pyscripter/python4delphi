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

unit WrapFmxImgList;

interface

uses
  System.Classes, System.SysUtils, FMX.ImgList,
  PythonEngine, WrapDelphi, WrapDelphiImageList;

type
  TPyDelphiCustomImageList = class (TPyDelphiBaseImageList)
  private
    function GetDelphiObject: TCustomImageList;
    procedure SetDelphiObject(const Value: TCustomImageList);
  protected
    function BitmapItemByName_Wrapper(AArgs: PPyObject): PPyObject; cdecl;
    function BestSize_Wrapper(AArgs: PPyObject): PPyObject; cdecl;
    function AddOrSet_Wrapper(AArgs: PPyObject): PPyObject; cdecl;
  public
    class function DelphiObjectClass : TClass; override;
    class procedure RegisterMethods(PythonType: TPythonType); override;
    // Properties
    property DelphiObject: TCustomImageList read GetDelphiObject write SetDelphiObject;
  end;

  TPyDelphiImageList = class (TPyDelphiCustomImageList)
  private
    function  GetDelphiObject: TImageList;
    procedure SetDelphiObject(const Value: TImageList);
  public
    class function  DelphiObjectClass : TClass; override;
    // Properties
    property DelphiObject: TImageList read GetDelphiObject write SetDelphiObject;
  end;

implementation

uses
  System.Types,
  FMX.MultiResBitmap,
  WrapDelphiTypes,
  WrapFmxTypes, System.UITypes;

{ Register the wrappers, the globals and the constants }
type
  TFmxImageListRegistration = class(TRegisteredUnit)
  public
    function Name : string; override;
    procedure RegisterWrappers(APyDelphiWrapper : TPyDelphiWrapper); override;
  end;

{ TFmxImageListRegistration }

function TFmxImageListRegistration.Name: string;
begin
  Result := 'FmxImageList';
end;

procedure TFmxImageListRegistration.RegisterWrappers(
  APyDelphiWrapper: TPyDelphiWrapper);
begin
  inherited;
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiCustomImageList);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiImageList);
end;

{ TPyDelphiCustomImageList }

function TPyDelphiCustomImageList.AddOrSet_Wrapper(AArgs: PPyObject): PPyObject;
var
  LPySourceName: PPyObject;
  LPyScales: PPyObject;
  LPyFileNames: PPyObject;

  LSourceName: string;
  LScales: array of single;
  LFileNames: array of string;
  LTransparentColor: integer;
  LWidth: integer;
  LHeight: integer;
  I: integer;
begin
  Adjust(@Self);
  LTransparentColor := TColors.SysNone;
  LWidth := 0;
  LHeight := 0;
  if GetPythonEngine().PyArg_ParseTuple(AArgs, 'OOO|iii:AddOrSet',
    @LPySourceName, @LPyScales, @LPyFileNames, @LTransparentColor, @LWidth, @LHeight) <> 0 then
  begin
    if not CheckStrAttribute(LPySourceName, 'SourceName', LSourceName) then
      Exit(nil);

    if not GetPythonEngine().PyList_Check(LPyScales) then
      Exit(nil);

    if not GetPythonEngine().PyList_Check(LPyFileNames) then
      Exit(nil);

    SetLength(LScales, PythonType.Engine.PyList_Size(LPyScales));
    for I := 0 to GetPythonEngine().PyList_Size(LPyScales) - 1 do
      LScales[I] := GetPythonEngine().PyFloat_AsDouble(
        GetPythonEngine().PyList_GetItem(LPyScales, I));

    SetLength(LFileNames, PythonType.Engine.PyList_Size(LPyFileNames));
    for I := 0 to GetPythonEngine().PyList_Size(LPyFileNames) - 1 do
      LFileNames[I] := GetPythonEngine.PyUnicodeAsString(
        GetPythonEngine().PyList_GetItem(LPyFileNames, I));

    Result := GetPythonEngine().PyLong_FromLong(
      DelphiObject.AddOrSet(LSourceName, LScales, LFileNames, LTransparentColor, LWidth, LHeight));
  end else
    Result := nil;
end;

function TPyDelphiCustomImageList.BestSize_Wrapper(AArgs: PPyObject): PPyObject;

  procedure Append(const AList, AItem: PPyObject; const AIx: integer);
  begin
    with GetPythonEngine() do begin
      PyTuple_SetItem(AList, AIx, AItem);
      Py_XDecRef(AItem);
    end;
  end;

var
  LPySize: PPyObject;
  LIndex: integer;
  LSize: TSize;
  LSizeF: TSizeF;
begin
  //Signatures:
  //function BestSize(const Index: Integer; var Size: TSize): Boolean; overload;
  //function BestSize(const Index: Integer; var Size: TSizeF): Boolean; overload;

  // We adjust the transmitted self argument
  Adjust(@Self);
  if GetPythonEngine().PyArg_ParseTuple(AArgs, 'iO:BestSize', @LIndex, @LPySize) <> 0 then begin
    if (CheckSizeAttribute(LPySize, 'Size', LSize) or
      (CheckSizeFAttribute(LPySize, 'Size', LSizeF))) then
    begin
      if not CheckIndex(LIndex, DelphiObject.Count) then
        Exit(nil);

      Result := GetPythonEngine().PyTuple_New(2);

      if CheckSizeAttribute(LPySize, 'Size', LSize) then begin
        Append(Result, GetPythonEngine().PyBool_FromLong(
          Ord(DelphiObject.BestSize(LIndex, LSize))), 0);
        Append(Result, WrapSize(PyDelphiWrapper, LSize), 1);
      end
      else
      if CheckSizeFAttribute(LPySize, 'Size', LSizeF) then begin
        Append(Result, GetPythonEngine().PyBool_FromLong(
          Ord(DelphiObject.BestSize(LIndex, LSizeF))), 0);
        Append(Result, WrapSizeF(PyDelphiWrapper, LSizeF), 1);
      end;
    end else
      Result := nil;
  end else
    Result := nil;
end;

function TPyDelphiCustomImageList.BitmapItemByName_Wrapper(
  AArgs: PPyObject): PPyObject;

  procedure Append(const AList, AItem: PPyObject; const AIx: integer);
  begin
    with GetPythonEngine() do begin
      PyTuple_SetItem(AList, AIx, AItem);
      Py_XDecRef(AItem);
    end;
  end;

var
  LPyName: PPyObject;
  LPyItem: PPyObject;
  LPySize: PPyObject;
  LName: string;
  LItem: TCustomBitmapItem;
  LSize: TSize;
begin
  //Signature:
  //function BitmapItemByName(const Name: string; var Item: TCustomBitmapItem; var Size: TSize): Boolean;

  // We adjust the transmitted self argument
  Adjust(@Self);
  if GetPythonEngine().PyArg_ParseTuple(AArgs, 'OOO:BitmapItemByName', @LPyName, @LPyItem, @LPySize) <> 0 then begin
    if CheckStrAttribute(LPyName, 'Name', LName)
      and CheckObjAttribute(LPyItem, 'Item', TCustomBitmapItem, TObject(LItem))
      and CheckSizeAttribute(LPySize, 'Size', LSize) then
    begin
      Result := GetPythonEngine().PyTuple_New(3);
      Append(Result, GetPythonEngine().PyBool_FromLong(
        Ord(DelphiObject.BitmapItemByName(LName, LItem, LSize))), 0);
      Append(Result, PyDelphiWrapper.Wrap(LItem), 1);
      Append(Result, WrapSize(PyDelphiWrapper, LSize), 2);
    end else
      Result := nil;
  end else
    Result := nil;
end;

class function TPyDelphiCustomImageList.DelphiObjectClass: TClass;
begin
  Result := TCustomImageList;
end;

function TPyDelphiCustomImageList.GetDelphiObject: TCustomImageList;
begin
  Result := TCustomImageList(inherited DelphiObject);
end;

class procedure TPyDelphiCustomImageList.RegisterMethods(
  PythonType: TPythonType);
begin
  PythonType.AddMethod('BitmapItemByName',
    @TPyDelphiCustomImageList.BitmapItemByName_Wrapper,
    'TCustomImageList.BitmapItemByName()'#10 +
    'Tries to find, in the source collection, the bitmap item specified by name.');

  PythonType.AddMethod('BestSize',
    @TPyDelphiCustomImageList.BestSize_Wrapper,
    'TCustomImageList.BestSize()'#10 +
    'Tries to find, in the source collection, the bitmap item specified by name. ' +
    'This method trying to determine the maximum size of layer, which less than input size. '+
    'If TLayer.MultiResBitmap has multiple images for different scales, then the search is performed among all images.');

  PythonType.AddMethod('AddOrSet',
    @TPyDelphiCustomImageList.AddOrSet_Wrapper,
    'TCustomImageList.AddOrSet()'#10 +
    'Adds or replaces several files in the source collection, ' +
    'and adds the item to the destination collection if it does not exist.');
end;

procedure TPyDelphiCustomImageList.SetDelphiObject(
  const Value: TCustomImageList);
begin
  inherited DelphiObject := Value;
end;

{ TPyDelphiImageList }

class function TPyDelphiImageList.DelphiObjectClass: TClass;
begin
  Result := TImageList;
end;

function TPyDelphiImageList.GetDelphiObject: TImageList;
begin
  Result := TImageList(inherited DelphiObject);
end;

procedure TPyDelphiImageList.SetDelphiObject(const Value: TImageList);
begin
  inherited DelphiObject := Value;
end;

initialization
  RegisteredUnits.Add(TFmxImageListRegistration.Create());

end.
