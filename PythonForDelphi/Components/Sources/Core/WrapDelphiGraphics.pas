{$I Definition.Inc}

unit WrapDelphiGraphics;

interface

uses
  Classes, Windows, SysUtils, PythonEngine, WrapDelphi, WrapDelphiClasses, Graphics;

type
  TPyDelphiGraphic = class(TPyDelphiPersistent)
  private
    function GetDelphiObject: TGraphic;
    procedure SetDelphiObject(const Value: TGraphic);
  protected
    // Exposed Methods
    function LoadFromFile_Wrapper(args : PPyObject) : PPyObject; cdecl;
    function SaveToFile_Wrapper(args : PPyObject) : PPyObject; cdecl;
    function LoadFromStream_Wrapper(args : PPyObject) : PPyObject; cdecl;
    function SaveToStream_Wrapper(args : PPyObject) : PPyObject; cdecl;
    function LoadFromClipboardFormat_Wrapper(args : PPyObject) : PPyObject; cdecl;
    function SaveToClipboardFormat_Wrapper(args : PPyObject) : PPyObject; cdecl;

    // Property Getters
    function Get_Empty( AContext : Pointer) : PPyObject; cdecl;
    function Get_Height( AContext : Pointer) : PPyObject; cdecl;
    function Get_Modified( AContext : Pointer) : PPyObject; cdecl;
    function Get_Palette( AContext : Pointer) : PPyObject; cdecl;
    function Get_PaletteModified( AContext : Pointer) : PPyObject; cdecl;
    function Get_Transparent( AContext : Pointer) : PPyObject; cdecl;
    function Get_Width( AContext : Pointer) : PPyObject; cdecl;

    // Property Setters
    function Set_Height( AValue : PPyObject; AContext : Pointer) : integer; cdecl;
    function Set_Modified( AValue : PPyObject; AContext : Pointer) : integer; cdecl;
    function Set_Palette( AValue : PPyObject; AContext : Pointer) : integer; cdecl;
    function Set_PaletteModified( AValue : PPyObject; AContext : Pointer) : integer; cdecl;
    function Set_Transparent( AValue : PPyObject; AContext : Pointer) : integer; cdecl;
    function Set_Width( AValue : PPyObject; AContext : Pointer) : integer; cdecl;
  public
    class function  DelphiObjectClass : TClass; override;
    class procedure RegisterGetSets( PythonType : TPythonType ); override;
    class procedure RegisterMethods( PythonType : TPythonType ); override;
    // Properties
    property DelphiObject: TGraphic read GetDelphiObject write SetDelphiObject;
  end;

  TPyDelphiBitmap = class(TPyDelphiGraphic)
  private
    function GetDelphiObject: TBitmap;
    procedure SetDelphiObject(const Value: TBitmap);
  protected
   // Exposed Methods
    {$IFNDEF FPC}
    function Dormant_Wrapper(args : PPyObject) : PPyObject; cdecl;
    {$ENDIF FPC}
    function FreeImage_Wrapper(args : PPyObject) : PPyObject; cdecl;
    function LoadFromResourceName_Wrapper(args : PPyObject) : PPyObject; cdecl;
    function LoadFromResourceID_Wrapper(args : PPyObject) : PPyObject; cdecl;
    function Mask_Wrapper(args : PPyObject) : PPyObject; cdecl;
    function ReleaseHandle_Wrapper(args : PPyObject) : PPyObject; cdecl;
    function ReleaseMaskHandle_Wrapper(args : PPyObject) : PPyObject; cdecl;
    function ReleasePalette_Wrapper(args : PPyObject) : PPyObject; cdecl;

    // Property Getters
    function Get_Canvas( AContext : Pointer) : PPyObject; cdecl;
    function Get_Handle( AContext : Pointer) : PPyObject; cdecl;
    function Get_HandleAllocated( AContext : Pointer) : PPyObject; cdecl;
    function Get_HandleType( AContext : Pointer) : PPyObject; cdecl;
    {$IFNDEF FPC}
    function Get_IgnorePalette( AContext : Pointer) : PPyObject; cdecl;
    {$ENDIF FPC}
    function Get_MaskHandle( AContext : Pointer) : PPyObject; cdecl;
    function Get_Monochrome( AContext : Pointer) : PPyObject; cdecl;
    function Get_PixelFormat( AContext : Pointer) : PPyObject; cdecl;
    function Get_TransparentColor( AContext : Pointer) : PPyObject; cdecl;
    function Get_TransparentMode( AContext : Pointer) : PPyObject; cdecl;

    // Property Setters
    function Set_Handle( AValue : PPyObject; AContext : Pointer) : Integer; cdecl;
    function Set_HandleType( AValue : PPyObject; AContext : Pointer) : Integer; cdecl;
    {$IFNDEF FPC}
    function Set_IgnorePalette( AValue : PPyObject; AContext : Pointer) : Integer; cdecl;
    {$ENDIF FPC}
    function Set_MaskHandle( AValue : PPyObject; AContext : Pointer) : Integer; cdecl;
    function Set_Monochrome( AValue : PPyObject; AContext : Pointer) : Integer; cdecl;
    function Set_PixelFormat( AValue : PPyObject; AContext : Pointer) : Integer; cdecl;
    function Set_TransparentColor( AValue : PPyObject; AContext : Pointer) : Integer; cdecl;
    function Set_TransparentMode( AValue : PPyObject; AContext : Pointer) : Integer; cdecl;
  public
    class function  DelphiObjectClass : TClass; override;
    class procedure RegisterGetSets( PythonType : TPythonType ); override;
    class procedure RegisterMethods( PythonType : TPythonType ); override;
    // Properties
    property DelphiObject: TBitmap read GetDelphiObject write SetDelphiObject;
  end;

  {$IFNDEF FPC}
  TPyDelphiMetaFile = class(TPyDelphiGraphic)
  private
    function GetDelphiObject: TMetaFile;
    procedure SetDelphiObject(const Value: TMetaFile);
  protected
   // Exposed Methods
    function Clear_Wrapper(args : PPyObject) : PPyObject; cdecl;
    function ReleaseHandle_Wrapper(args : PPyObject) : PPyObject; cdecl;

    // Property Getters
    function Get_CreatedBy( AContext : Pointer) : PPyObject; cdecl;
    function Get_Description( AContext : Pointer) : PPyObject; cdecl;
    function Get_Enhanced( AContext : Pointer) : PPyObject; cdecl;
    function Get_Handle( AContext : Pointer) : PPyObject; cdecl;
    function Get_HandleAllocated( AContext : Pointer) : PPyObject; cdecl;
    function Get_MMWidth( AContext : Pointer) : PPyObject; cdecl;
    function Get_MMHeight( AContext : Pointer) : PPyObject; cdecl;
    function Get_Inch( AContext : Pointer) : PPyObject; cdecl;

    // Property Setters
    function Set_Enhanced( AValue : PPyObject; AContext : Pointer) : Integer; cdecl;
    function Set_Handle( AValue : PPyObject; AContext : Pointer) : Integer; cdecl;
    function Set_MMWidth( AValue : PPyObject; AContext : Pointer) : Integer; cdecl;
    function Set_MMHeight( AValue : PPyObject; AContext : Pointer) : Integer; cdecl;
    function Set_Inch( AValue : PPyObject; AContext : Pointer) : Integer; cdecl;
  public
    class function  DelphiObjectClass : TClass; override;
    class procedure RegisterGetSets( PythonType : TPythonType ); override;
    class procedure RegisterMethods( PythonType : TPythonType ); override;
    // Properties
    property DelphiObject: TMetaFile read GetDelphiObject write SetDelphiObject;
  end;
  {$ENDIF FPC}

  TPyDelphiIcon = class(TPyDelphiGraphic)
  private
    function GetDelphiObject: TIcon;
    procedure SetDelphiObject(const Value: TIcon);
  protected
   // Exposed Methods
    function ReleaseHandle_Wrapper(args : PPyObject) : PPyObject; cdecl;

    // Property Getters
    function Get_Handle( AContext : Pointer) : PPyObject; cdecl;
    function Get_HandleAllocated( AContext : Pointer) : PPyObject; cdecl;
    // Property Setters
    function Set_Handle( AValue : PPyObject; AContext : Pointer) : Integer; cdecl;
  public
    class function  DelphiObjectClass : TClass; override;
    class procedure RegisterGetSets( PythonType : TPythonType ); override;
    class procedure RegisterMethods( PythonType : TPythonType ); override;
    // Properties
    property DelphiObject: TIcon read GetDelphiObject write SetDelphiObject;
  end;

  TPyDelphiPicture = class(TPyDelphiPersistent)
  private
    function GetDelphiObject: TPicture;
    procedure SetDelphiObject(const Value: TPicture);
  protected
   // Exposed Methods
    function LoadFromFile_Wrapper(args : PPyObject) : PPyObject; cdecl;
    function SaveToFile_Wrapper(args : PPyObject) : PPyObject; cdecl;

    // Property Getters
    function Get_Bitmap( AContext : Pointer) : PPyObject; cdecl;
    function Get_Graphic( AContext : Pointer) : PPyObject; cdecl;
    function Get_Height( AContext : Pointer) : PPyObject; cdecl;
    function Get_Icon( AContext : Pointer) : PPyObject; cdecl;
    {$IFNDEF FPC}
    function Get_MetaFile( AContext : Pointer) : PPyObject; cdecl;
    {$ENDIF FPC}
    function Get_Width( AContext : Pointer) : PPyObject; cdecl;

    // Property Setters
    function Set_Bitmap( AValue : PPyObject; AContext : Pointer) : Integer; cdecl;
    function Set_Graphic( AValue : PPyObject; AContext : Pointer) : Integer; cdecl;
    function Set_Icon( AValue : PPyObject; AContext : Pointer) : Integer; cdecl;
    {$IFNDEF FPC}
    function Set_MetaFile( AValue : PPyObject; AContext : Pointer) : Integer; cdecl;
    {$ENDIF FPC}
  public
    class function  DelphiObjectClass : TClass; override;
    class procedure RegisterGetSets( PythonType : TPythonType ); override;
    class procedure RegisterMethods( PythonType : TPythonType ); override;
    // Properties
    property DelphiObject: TPicture read GetDelphiObject write SetDelphiObject;
  end;
  {
     PyObject wrapping TCanvas
  }
  TPyDelphiCanvas = class(TPyDelphiPersistent)
  private
    function  GetDelphiObject: TCanvas;
    procedure SetDelphiObject(const Value: TCanvas);
  protected
    // Exposed Methods
    function Arc_Wrapper(args : PPyObject) : PPyObject; cdecl;
    {$IFNDEF FPC}
    function BrushCopy_Wrapper(args : PPyObject) : PPyObject; cdecl;
    {$ENDIF FPC}
    function Chord_Wrapper(args : PPyObject) : PPyObject; cdecl;
    function CopyRect_Wrapper(args : PPyObject) : PPyObject; cdecl;
    function Draw_Wrapper(args : PPyObject) : PPyObject; cdecl;
    function DrawFocusRect_Wrapper(args : PPyObject) : PPyObject; cdecl;
    function Ellipse_Wrapper(args : PPyObject) : PPyObject; cdecl;
    function FillRect_Wrapper(args : PPyObject) : PPyObject; cdecl;
    function FloodFill_Wrapper(args : PPyObject) : PPyObject; cdecl;
    function FrameRect_Wrapper(args : PPyObject) : PPyObject; cdecl;
    function LineTo_Wrapper(args : PPyObject) : PPyObject; cdecl;
    function Lock_Wrapper(args : PPyObject) : PPyObject; cdecl;
    function MoveTo_Wrapper(args : PPyObject) : PPyObject; cdecl;
    function Pie_Wrapper(args : PPyObject) : PPyObject; cdecl;
    function Polygon_Wrapper(args : PPyObject) : PPyObject; cdecl;
    function Polyline_Wrapper(args : PPyObject) : PPyObject; cdecl;
    function PolyBezier_Wrapper(args : PPyObject) : PPyObject; cdecl;
    {$IFNDEF FPC}
    function PolyBezierTo_Wrapper(args : PPyObject) : PPyObject; cdecl;
    {$ENDIF FPC}
    function Rectangle_Wrapper(args : PPyObject) : PPyObject; cdecl;
    function Refresh_Wrapper(args : PPyObject) : PPyObject; cdecl;
    function RoundRect_Wrapper(args : PPyObject) : PPyObject; cdecl;
    function StretchDraw_Wrapper(args : PPyObject) : PPyObject; cdecl;
    function TextExtent_Wrapper(args : PPyObject) : PPyObject; cdecl;
    function TextHeight_Wrapper(args : PPyObject) : PPyObject; cdecl;
    function TextOut_Wrapper(args : PPyObject) : PPyObject; cdecl;
    function TextRect_Wrapper(args : PPyObject) : PPyObject; cdecl;
    function TextWidth_Wrapper(args : PPyObject) : PPyObject; cdecl;
    function TryLock_Wrapper(args : PPyObject) : PPyObject; cdecl;
    function Unlock_Wrapper(args : PPyObject) : PPyObject; cdecl;
    // Pixels[x,y] : TColor wrapper -> GetPixel / SetPixel
    function GetPixel(args : PPyObject) : PPyObject; cdecl;
    function SetPixel(args : PPyObject) : PPyObject; cdecl;
    // Property Getters
    function Get_HandleAllocated( AContext : Pointer) : PPyObject; cdecl;
    function Get_ClipRect( AContext : Pointer) : PPyObject; cdecl;
    function Get_Handle( AContext : Pointer) : PPyObject; cdecl;
    function Get_LockCount( AContext : Pointer) : PPyObject; cdecl;
    {$IFNDEF FPC}
    function Get_CanvasOrientation( AContext : Pointer) : PPyObject; cdecl;
    function Get_TextFlags( AContext : Pointer) : PPyObject; cdecl;
    {$ENDIF FPC}
    function Get_PenPos( AContext : Pointer) : PPyObject; cdecl;
    function Get_OnChange( AContext : Pointer) : PPyObject; cdecl;
    function Get_OnChanging( AContext : Pointer) : PPyObject; cdecl;
    // Property Setters
    function Set_Handle( AValue : PPyObject; AContext : Pointer) : integer; cdecl;
    {$IFNDEF FPC}
    function Set_TextFlags( AValue : PPyObject; AContext : Pointer) : integer; cdecl;
    {$ENDIF FPC}
    function Set_OnChange( AValue : PPyObject; AContext : Pointer) : integer; cdecl;
    function Set_OnChanging( AValue : PPyObject; AContext : Pointer) : integer; cdecl;
  public
    class function  DelphiObjectClass : TClass; override;
    class procedure RegisterGetSets( PythonType : TPythonType ); override;
    class procedure RegisterMethods( PythonType : TPythonType ); override;
    // Properties
    property DelphiObject: TCanvas read GetDelphiObject write SetDelphiObject;
  end;


implementation

uses
  Types,
  {$IFDEF FPC}
  GraphType,
  {$ENDIF FPC}
  WrapDelphiTypes;

{ Register the wrappers, the globals and the constants }
type
  TGraphicsRegistration = class(TRegisteredUnit)
  public
    function Name : String; override;
    procedure RegisterWrappers(APyDelphiWrapper : TPyDelphiWrapper); override;
    procedure DefineVars(APyDelphiWrapper : TPyDelphiWrapper); override;
  end;

{ TGraphicsRegistration }

procedure TGraphicsRegistration.DefineVars(APyDelphiWrapper: TPyDelphiWrapper);
begin
  inherited;
  APyDelphiWrapper.DefineVar('clScrollBar', clScrollBar);
  APyDelphiWrapper.DefineVar('clBackground', clBackground);
  APyDelphiWrapper.DefineVar('clActiveCaption', clActiveCaption);
  APyDelphiWrapper.DefineVar('clInactiveCaption', clInactiveCaption);
  APyDelphiWrapper.DefineVar('clMenu', clMenu);
  APyDelphiWrapper.DefineVar('clWindow', clWindow);
  APyDelphiWrapper.DefineVar('clWindowFrame', clWindowFrame);
  APyDelphiWrapper.DefineVar('clMenuText', clMenuText);
  APyDelphiWrapper.DefineVar('clWindowText', clWindowText);
  APyDelphiWrapper.DefineVar('clCaptionText', clCaptionText);
  APyDelphiWrapper.DefineVar('clActiveBorder', clActiveBorder);
  APyDelphiWrapper.DefineVar('clInactiveBorder', clInactiveBorder);
  APyDelphiWrapper.DefineVar('clAppWorkSpace', clAppWorkSpace);
  APyDelphiWrapper.DefineVar('clHighlight', clHighlight);
  APyDelphiWrapper.DefineVar('clHighlightText', clHighlightText);
  APyDelphiWrapper.DefineVar('clBtnFace', clBtnFace);
  APyDelphiWrapper.DefineVar('clBtnShadow', clBtnShadow);
  APyDelphiWrapper.DefineVar('clGrayText', clGrayText);
  APyDelphiWrapper.DefineVar('clBtnText', clBtnText);
  APyDelphiWrapper.DefineVar('clInactiveCaptionText', clInactiveCaptionText);
  APyDelphiWrapper.DefineVar('clBtnHighlight', clBtnHighlight);
  APyDelphiWrapper.DefineVar('cl3DDkShadow', cl3DDkShadow);
  APyDelphiWrapper.DefineVar('cl3DLight', cl3DLight);
  APyDelphiWrapper.DefineVar('clInfoText', clInfoText);
  APyDelphiWrapper.DefineVar('clInfoBk', clInfoBk);
  APyDelphiWrapper.DefineVar('clHotLight', clHotLight);
  APyDelphiWrapper.DefineVar('clGradientActiveCaption', clGradientActiveCaption);
  APyDelphiWrapper.DefineVar('clGradientInactiveCaption', clGradientInactiveCaption);
  APyDelphiWrapper.DefineVar('clMenuHighlight', clMenuHighlight);
  APyDelphiWrapper.DefineVar('clMenuBar', clMenuBar);

  APyDelphiWrapper.DefineVar('clBlack', clBlack);
  APyDelphiWrapper.DefineVar('clMaroon', clMaroon);
  APyDelphiWrapper.DefineVar('clGreen', clGreen);
  APyDelphiWrapper.DefineVar('clOlive', clOlive);
  APyDelphiWrapper.DefineVar('clNavy', clNavy);
  APyDelphiWrapper.DefineVar('clPurple', clPurple);
  APyDelphiWrapper.DefineVar('clTeal', clTeal);
  APyDelphiWrapper.DefineVar('clGray', clGray);
  APyDelphiWrapper.DefineVar('clSilver', clSilver);
  APyDelphiWrapper.DefineVar('clRed', clRed);
  APyDelphiWrapper.DefineVar('clLime', clLime);
  APyDelphiWrapper.DefineVar('clYellow', clYellow);
  APyDelphiWrapper.DefineVar('clBlue', clBlue);
  APyDelphiWrapper.DefineVar('clFuchsia', clFuchsia);
  APyDelphiWrapper.DefineVar('clAqua', clAqua);
  APyDelphiWrapper.DefineVar('clLtGray', clLtGray);
  APyDelphiWrapper.DefineVar('clDkGray', clDkGray);
  APyDelphiWrapper.DefineVar('clWhite', clWhite);

  APyDelphiWrapper.DefineVar('clMoneyGreen', clMoneyGreen);
  APyDelphiWrapper.DefineVar('clSkyBlue', clSkyBlue);
  APyDelphiWrapper.DefineVar('clCream', clCream);
  APyDelphiWrapper.DefineVar('clMedGray', clMedGray);

  APyDelphiWrapper.DefineVar('clNone', clNone);
  APyDelphiWrapper.DefineVar('clDefault', clDefault);

  APyDelphiWrapper.DefineVar('fsSurface', 'fsSurface');
  APyDelphiWrapper.DefineVar('fsBorder', 'fsBorder');
end;

function TGraphicsRegistration.Name: String;
begin
  Result := 'Graphics';
end;

procedure TGraphicsRegistration.RegisterWrappers(APyDelphiWrapper: TPyDelphiWrapper);
begin
  inherited;
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiGraphic);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiBitmap);
  {$IFNDEF FPC}
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiMetaFile);
  {$ENDIF FPC}
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiIcon);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiPicture);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiCanvas);
end;

{ TPyDelphiGraphic }

class function TPyDelphiGraphic.DelphiObjectClass: TClass;
begin
  Result := TGraphic;
end;

function TPyDelphiGraphic.GetDelphiObject: TGraphic;
begin
  Result := TGraphic(inherited DelphiObject);
end;

function TPyDelphiGraphic.Get_Empty(AContext: Pointer): PPyObject;
begin
  Adjust(@Self);
  Result := GetPythonEngine.VariantAsPyObject(DelphiObject.Empty);
end;

function TPyDelphiGraphic.Get_Height(AContext: Pointer): PPyObject;
begin
  Adjust(@Self);
  Result := GetPythonEngine.PyInt_FromLong(DelphiObject.Height);
end;

function TPyDelphiGraphic.Get_Modified(AContext: Pointer): PPyObject;
begin
  Adjust(@Self);
  Result := GetPythonEngine.VariantAsPyObject(DelphiObject.Modified);
end;

function TPyDelphiGraphic.Get_Palette(AContext: Pointer): PPyObject;
begin
  Adjust(@Self);
  Result := GetPythonEngine.PyInt_FromLong(DelphiObject.Palette);
end;

function TPyDelphiGraphic.Get_PaletteModified(
  AContext: Pointer): PPyObject;
begin
  Adjust(@Self);
  Result := GetPythonEngine.VariantAsPyObject(DelphiObject.PaletteModified);
end;

function TPyDelphiGraphic.Get_Transparent(AContext: Pointer): PPyObject;
begin
  Adjust(@Self);
  Result := GetPythonEngine.VariantAsPyObject(DelphiObject.Transparent);
end;

function TPyDelphiGraphic.Get_Width(AContext: Pointer): PPyObject;
begin
  Adjust(@Self);
  Result := GetPythonEngine.PyInt_FromLong(DelphiObject.Width);
end;

function TPyDelphiGraphic.LoadFromClipboardFormat_Wrapper(
  args: PPyObject): PPyObject;
var
  _format : Integer;
  {$IFNDEF FPC}
  _data : Integer;
  _palette : Integer;
  {$ENDIF FPC}
begin
  // We adjust the transmitted self argument
  Adjust(@Self);
  with GetPythonEngine do begin
    {$IFDEF FPC}
    if PyArg_ParseTuple( args, 'i:LoadFromClipboardFormat',@_format ) <> 0 then
    begin
      DelphiObject.LoadFromClipboardFormat(_format);
    {$ELSE FPC}
    if PyArg_ParseTuple( args, 'iii:LoadFromClipboardFormat',@_format, @_data, @_palette ) <> 0 then
    begin
      DelphiObject.LoadFromClipboardFormat(_format, _data, _palette);
    {$ENDIF FPC}
      Result := ReturnNone;
    end
    else
      Result := nil;
  end;
end;

function TPyDelphiGraphic.LoadFromFile_Wrapper(args: PPyObject): PPyObject;
var
  _pFileName : PPyObject;
begin
  // We adjust the transmitted self argument
  Adjust(@Self);
  with GetPythonEngine do begin
    if PyArg_ParseTuple( args, 'O:LoadFromFile',@_pFileName ) <> 0 then
    begin
      DelphiObject.LoadFromFile(PyString_AsDelphiString(_pFileName));
      Result := ReturnNone;
    end
    else
      Result := nil;
  end;
end;

function TPyDelphiGraphic.LoadFromStream_Wrapper(
  args: PPyObject): PPyObject;
var
  _obj : TObject;
  _oStream : PPyObject;
  _stream : TStream;
begin
  // We adjust the transmitted self argument
  Adjust(@Self);
  with GetPythonEngine do begin
    _oStream := nil;
    if (PyArg_ParseTuple( args, 'O:LoadFromStrea',@_oStream ) <> 0) and
       CheckObjAttribute(_oStream, 'Stream', TStream, _obj) then
    begin
      _stream := TStream(_obj);
      DelphiObject.LoadFromStream(_stream);
      Result := ReturnNone;
    end
    else
      Result := nil;
  end;
end;

class procedure TPyDelphiGraphic.RegisterGetSets(PythonType: TPythonType);
begin
  inherited;
  with PythonType do
    begin
      AddGetSet('Empty', @TPyDelphiGraphic.Get_Empty, nil,
        '', nil);
      AddGetSet('Height', @TPyDelphiGraphic.Get_Height, @TPyDelphiGraphic.Set_Height,
        '', nil);
      AddGetSet('Modified', @TPyDelphiGraphic.Get_Modified, @TPyDelphiGraphic.Set_Modified,
        '', nil);
      AddGetSet('Palette', @TPyDelphiGraphic.Get_Palette, @TPyDelphiGraphic.Set_Palette,
        '', nil);
      AddGetSet('PaletteModified', @TPyDelphiGraphic.Get_PaletteModified, @TPyDelphiGraphic.Set_PaletteModified,
        '', nil);
      AddGetSet('Transparent', @TPyDelphiGraphic.Get_Transparent, @TPyDelphiGraphic.Set_Transparent,
        '', nil);
      AddGetSet('Width', @TPyDelphiGraphic.Get_Width, @TPyDelphiGraphic.Set_Width,
        '', nil);
    end;
end;

class procedure TPyDelphiGraphic.RegisterMethods(PythonType: TPythonType);
begin
  inherited;
  PythonType.AddMethod('LoadFromFile', @TPyDelphiGraphic.LoadFromFile_Wrapper,
    'TGraphic.LoadFromFile()'#10 +
    '');
  PythonType.AddMethod('SaveToFile', @TPyDelphiGraphic.SaveToFile_Wrapper,
    'TGraphic.SaveToFile()'#10 +
    '');
  PythonType.AddMethod('LoadFromStream', @TPyDelphiGraphic.LoadFromStream_Wrapper,
    'TGraphic.LoadFromStream()'#10 +
    '');
  PythonType.AddMethod('SaveToStream', @TPyDelphiGraphic.SaveToStream_Wrapper,
    'TGraphic.SaveToStream()'#10 +
    '');
  PythonType.AddMethod('LoadFromClipboardFormat', @TPyDelphiGraphic.LoadFromClipboardFormat_Wrapper,
    'TGraphic.LoadFromClipboardFormat()'#10 +
    '');
  PythonType.AddMethod('SaveToClipboardFormat', @TPyDelphiGraphic.SaveToClipboardFormat_Wrapper,
    'TGraphic.SaveToClipboardFormat()'#10 +
    '');
end;

function TPyDelphiGraphic.SaveToClipboardFormat_Wrapper(
  args: PPyObject): PPyObject;
var
  _format : Word;
  {$IFNDEF FPC}
  _data : THandle;
  _palette : HPALETTE;
  {$ENDIF FPC}
begin
  // We adjust the transmitted self argument
  Adjust(@Self);
  with GetPythonEngine do begin
    {$IFDEF FPC}
    if PyArg_ParseTuple( args, ':SaveToClipboardFormat') <> 0 then
    begin
      DelphiObject.SaveToClipboardFormat(_format);
    {$ELSE FPC}
    if PyArg_ParseTuple( args, ':SaveToClipboardFormat') <> 0 then
    begin
      DelphiObject.SaveToClipboardFormat(_format, _data, _palette);
    {$ENDIF FPC}
      Result := PyTuple_New(3);
      PyTuple_SetItem(Result, 0, PyInt_FromLong(_format));
      {$IFNDEF FPC}
      PyTuple_SetItem(Result, 1, PyInt_FromLong(_data));
      PyTuple_SetItem(Result, 2, PyInt_FromLong(_palette));
      {$ENDIF FPC}
    end
    else
      Result := nil;
  end;
end;

function TPyDelphiGraphic.SaveToFile_Wrapper(args: PPyObject): PPyObject;
var
  _pFileName : PPyObject;
begin
  // We adjust the transmitted self argument
  Adjust(@Self);
  with GetPythonEngine do begin
    if PyArg_ParseTuple( args, 'O:SaveToFile',@_pFileName ) <> 0 then
    begin
      DelphiObject.SaveToFile(PyString_AsDelphiString(_pFileName));
      Result := ReturnNone;
    end
    else
      Result := nil;
  end;
end;

function TPyDelphiGraphic.SaveToStream_Wrapper(args: PPyObject): PPyObject;
var
  _obj : TObject;
  _oStream : PPyObject;
  _stream : TStream;
begin
  // We adjust the transmitted self argument
  Adjust(@Self);
  with GetPythonEngine do begin
    _oStream := nil;
    if (PyArg_ParseTuple( args, 'O:SaveToStream',@_oStream ) <> 0) and
       CheckObjAttribute(_oStream, 'Stream', TStream, _obj) then
    begin
      _stream := TStream(_obj);
      DelphiObject.SaveToStream(_stream);
      Result := ReturnNone;
    end
    else
      Result := nil;
  end;
end;

procedure TPyDelphiGraphic.SetDelphiObject(const Value: TGraphic);
begin
  inherited DelphiObject := Value;
end;

function TPyDelphiGraphic.Set_Height(AValue: PPyObject;
  AContext: Pointer): integer;
var
  _value : Integer;
begin
  Adjust(@Self);
  if CheckIntAttribute(AValue, 'Height', _value) then
  begin
    DelphiObject.Height := _value;
    Result := 0;
  end
  else
    Result := -1;
end;

function TPyDelphiGraphic.Set_Modified(AValue: PPyObject;
  AContext: Pointer): integer;
var
  _value : Boolean;
begin
  Adjust(@Self);
  if CheckBoolAttribute(AValue, 'Modified', _value) then
  begin
    DelphiObject.Modified := _value;
    Result := 0;
  end
  else
    Result := -1;
end;

function TPyDelphiGraphic.Set_Palette(AValue: PPyObject;
  AContext: Pointer): integer;
var
  _value : Integer;
begin
  Adjust(@Self);
  if CheckIntAttribute(AValue, 'Palette', _value) then
  begin
    DelphiObject.Palette := _value;
    Result := 0;
  end
  else
    Result := -1;
end;

function TPyDelphiGraphic.Set_PaletteModified(AValue: PPyObject;
  AContext: Pointer): integer;
var
  _value : Boolean;
begin
  Adjust(@Self);
  if CheckBoolAttribute(AValue, 'PaletteModified', _value) then
  begin
    DelphiObject.PaletteModified := _value;
    Result := 0;
  end
  else
    Result := -1;
end;

function TPyDelphiGraphic.Set_Transparent(AValue: PPyObject;
  AContext: Pointer): integer;
var
  _value : Boolean;
begin
  Adjust(@Self);
  if CheckBoolAttribute(AValue, 'Transparent', _value) then
  begin
    DelphiObject.Transparent := _value;
    Result := 0;
  end
  else
    Result := -1;
end;

function TPyDelphiGraphic.Set_Width(AValue: PPyObject;
  AContext: Pointer): integer;
var
  _value : Integer;
begin
  Adjust(@Self);
  if CheckIntAttribute(AValue, 'Width', _value) then
  begin
    DelphiObject.Width := _value;
    Result := 0;
  end
  else
    Result := -1;
end;

{ TPyDelphiBitmap }

class function TPyDelphiBitmap.DelphiObjectClass: TClass;
begin
  Result := TBitmap;
end;

{$IFNDEF FPC}
function TPyDelphiBitmap.Dormant_Wrapper(args: PPyObject): PPyObject;
begin
  // We adjust the transmitted self argument
  Adjust(@Self);
  with GetPythonEngine do begin
    if PyArg_ParseTuple( args, ':Dormant') <> 0 then
    begin
      DelphiObject.Dormant;
      Result := ReturnNone;
    end
    else
      Result := nil;
  end;
end;
{$ENDIF FPC}

function TPyDelphiBitmap.FreeImage_Wrapper(args: PPyObject): PPyObject;
begin
  // We adjust the transmitted self argument
  Adjust(@Self);
  with GetPythonEngine do begin
    if PyArg_ParseTuple( args, ':FreeImage') <> 0 then
    begin
      DelphiObject.FreeImage;
      Result := ReturnNone;
    end
    else
      Result := nil;
  end;
end;

function TPyDelphiBitmap.GetDelphiObject: TBitmap;
begin
  Result := TBitmap(inherited DelphiObject);
end;

function TPyDelphiBitmap.Get_Canvas(AContext: Pointer): PPyObject;
begin
  Adjust(@Self);
  Result := Wrap(DelphiObject.Canvas);
end;

function TPyDelphiBitmap.Get_Handle(AContext: Pointer): PPyObject;
begin
  Adjust(@Self);
  Result := GetPythonEngine.PyInt_FromLong(DelphiObject.Handle);
end;

function TPyDelphiBitmap.Get_HandleAllocated(AContext: Pointer): PPyObject;
begin
  Adjust(@Self);
  Result := GetPythonEngine.VariantAsPyObject(DelphiObject.HandleAllocated);
end;

function TPyDelphiBitmap.Get_HandleType(AContext: Pointer): PPyObject;
begin
  Adjust(@Self);
  with GetPythonEngine do begin
    case DelphiObject.HandleType of
    bmDIB:  Result := PyString_FromDelphiString('bmDIB');
    bmDDB:  Result := PyString_FromDelphiString('bmDDB');
    else
      Result := ReturnNone;
    end;
  end;
end;

{$IFNDEF FPC}
function TPyDelphiBitmap.Get_IgnorePalette(AContext: Pointer): PPyObject;
begin
  Adjust(@Self);
  Result := GetPythonEngine.VariantAsPyObject(DelphiObject.IgnorePalette);
end;
{$ENDIF FPC}

function TPyDelphiBitmap.Get_MaskHandle(AContext: Pointer): PPyObject;
begin
  Adjust(@Self);
  Result := GetPythonEngine.PyInt_FromLong(DelphiObject.MaskHandle);
end;

function TPyDelphiBitmap.Get_Monochrome(AContext: Pointer): PPyObject;
begin
  Adjust(@Self);
  Result := GetPythonEngine.VariantAsPyObject(DelphiObject.Monochrome);
end;

function TPyDelphiBitmap.Get_PixelFormat(AContext: Pointer): PPyObject;
begin
  Adjust(@Self);
  with GetPythonEngine do begin
    case DelphiObject.PixelFormat of
    pfDevice:  Result := PyString_FromDelphiString('pfDevice');
    pf1bit:    Result := PyString_FromDelphiString('pf1bit');
    pf4bit:    Result := PyString_FromDelphiString('pf4bit');
    pf8bit:    Result := PyString_FromDelphiString('pf8bit');
    pf15bit:   Result := PyString_FromDelphiString('pf15bit');
    pf16bit:   Result := PyString_FromDelphiString('pf16bit');
    pf24bit:   Result := PyString_FromDelphiString('pf24bit');
    pf32bit:   Result := PyString_FromDelphiString('pf32bit');
    pfCustom:  Result := PyString_FromDelphiString('pfCustom');
    else
      Result := ReturnNone;
    end;
  end;
end;

function TPyDelphiBitmap.Get_TransparentColor(
  AContext: Pointer): PPyObject;
begin
  Adjust(@Self);
  Result := GetPythonEngine.PyInt_FromLong(DelphiObject.TransparentColor);
end;

function TPyDelphiBitmap.Get_TransparentMode(AContext: Pointer): PPyObject;
begin
  Adjust(@Self);
  with GetPythonEngine do begin
    case DelphiObject.TransparentMode of
    tmAuto:  Result := PyString_FromDelphiString('tmAuto');
    tmFixed: Result := PyString_FromDelphiString('tmFixed');
    else
      Result := ReturnNone;
    end;
  end;
end;

function TPyDelphiBitmap.LoadFromResourceID_Wrapper(
  args: PPyObject): PPyObject;
var
  _instance : Integer;
  _resID : Integer;
begin
  // We adjust the transmitted self argument
  Adjust(@Self);
  with GetPythonEngine do begin
    if PyArg_ParseTuple( args, 'ii:LoadFromResourceID',@_instance, @_resID ) <> 0 then
    begin
      DelphiObject.LoadFromResourceID(_instance, _resID);
      Result := ReturnNone;
    end
    else
      Result := nil;
  end;
end;

function TPyDelphiBitmap.LoadFromResourceName_Wrapper(
  args: PPyObject): PPyObject;
var
  _instance : Integer;
  _resName : PAnsiChar;
begin
  // We adjust the transmitted self argument
  Adjust(@Self);
  with GetPythonEngine do begin
    _resName := nil;
    if PyArg_ParseTuple( args, 'is:LoadFromResourceName',@_instance, @_resName ) <> 0 then
    begin
      if _resName <> nil then
        DelphiObject.LoadFromResourceName(_instance, String(_resName));
      Result := ReturnNone;
    end
    else
      Result := nil;
  end;
end;

function TPyDelphiBitmap.Mask_Wrapper(args: PPyObject): PPyObject;
var
  _transpColor : Integer;
begin
  // We adjust the transmitted self argument
  Adjust(@Self);
  with GetPythonEngine do begin
    if PyArg_ParseTuple( args, 'i:Mask',@_transpColor ) <> 0 then
    begin
      DelphiObject.Mask(_transpColor);
      Result := ReturnNone;
    end
    else
      Result := nil;
  end;
end;

class procedure TPyDelphiBitmap.RegisterGetSets(PythonType: TPythonType);
begin
  inherited;
  with PythonType do
    begin
      AddGetSet('Canvas', @TPyDelphiBitmap.Get_Canvas, nil,
        '', nil);
      AddGetSet('Handle', @TPyDelphiBitmap.Get_Handle, @TPyDelphiBitmap.Set_Handle,
        '', nil);
      AddGetSet('HandleAllocated', @TPyDelphiBitmap.Get_HandleAllocated, nil,
        '', nil);
      AddGetSet('HandleType', @TPyDelphiBitmap.Get_HandleType, @TPyDelphiBitmap.Set_HandleType,
        '', nil);
      {$IFNDEF FPC}
      AddGetSet('IgnorePalette', @TPyDelphiBitmap.Get_IgnorePalette, @TPyDelphiBitmap.Set_IgnorePalette,
        '', nil);
      {$ENDIF FPC}
      AddGetSet('MaskHandle', @TPyDelphiBitmap.Get_MaskHandle, @TPyDelphiBitmap.Set_MaskHandle,
        '', nil);
      AddGetSet('Monochrome', @TPyDelphiBitmap.Get_Monochrome, @TPyDelphiBitmap.Set_Monochrome,
        '', nil);
      AddGetSet('PixelFormat', @TPyDelphiBitmap.Get_PixelFormat, @TPyDelphiBitmap.Set_PixelFormat,
        '', nil);
      AddGetSet('TransparentColor', @TPyDelphiBitmap.Get_TransparentColor, @TPyDelphiBitmap.Set_TransparentColor,
        '', nil);
      AddGetSet('TransparentMode', @TPyDelphiBitmap.Get_TransparentMode, @TPyDelphiBitmap.Set_TransparentMode,
        '', nil);
    end;
end;

class procedure TPyDelphiBitmap.RegisterMethods(PythonType: TPythonType);
begin
  inherited;
  {$IFNDEF FPC}
  PythonType.AddMethod('Dormant', @TPyDelphiBitmap.Dormant_Wrapper,
    'TBitmap.Dormant()'#10 +
    '');
  {$ENDIF FPC}
  PythonType.AddMethod('FreeImage', @TPyDelphiBitmap.FreeImage_Wrapper,
    'TBitmap.FreeImage()'#10 +
    '');
  PythonType.AddMethod('LoadFromResourceName', @TPyDelphiBitmap.LoadFromResourceName_Wrapper,
    'TBitmap.LoadFromResourceName()'#10 +
    '');
  PythonType.AddMethod('LoadFromResourceID', @TPyDelphiBitmap.LoadFromResourceID_Wrapper,
    'TBitmap.LoadFromResourceID()'#10 +
    '');
  PythonType.AddMethod('Mask', @TPyDelphiBitmap.Mask_Wrapper,
    'TBitmap.Mask()'#10 +
    '');
  PythonType.AddMethod('ReleaseHandle', @TPyDelphiBitmap.ReleaseHandle_Wrapper,
    'TBitmap.ReleaseHandle()'#10 +
    '');
  PythonType.AddMethod('ReleaseMaskHandle', @TPyDelphiBitmap.ReleaseMaskHandle_Wrapper,
    'TBitmap.ReleaseMaskHandle()'#10 +
    '');
  PythonType.AddMethod('ReleasePalette', @TPyDelphiBitmap.ReleasePalette_Wrapper,
    'TBitmap.ReleasePalette()'#10 +
    '');
end;

function TPyDelphiBitmap.ReleaseHandle_Wrapper(args: PPyObject): PPyObject;
begin
  // We adjust the transmitted self argument
  Adjust(@Self);
  with GetPythonEngine do begin
    if PyArg_ParseTuple( args, ':ReleaseHandle') <> 0 then
    begin
      Result := PyInt_FromLong(DelphiObject.ReleaseHandle);
    end
    else
      Result := nil;
  end;
end;

function TPyDelphiBitmap.ReleaseMaskHandle_Wrapper(
  args: PPyObject): PPyObject;
begin
  // We adjust the transmitted self argument
  Adjust(@Self);
  with GetPythonEngine do begin
    if PyArg_ParseTuple( args, ':ReleaseMaskHandle') <> 0 then
    begin
      Result := PyInt_FromLong(DelphiObject.ReleaseMaskHandle);
    end
    else
      Result := nil;
  end;
end;

function TPyDelphiBitmap.ReleasePalette_Wrapper(
  args: PPyObject): PPyObject;
begin
  // We adjust the transmitted self argument
  Adjust(@Self);
  with GetPythonEngine do begin
    if PyArg_ParseTuple( args, ':ReleasePalette') <> 0 then
    begin
      Result := PyInt_FromLong(DelphiObject.ReleasePalette);
    end
    else
      Result := nil;
  end;
end;

procedure TPyDelphiBitmap.SetDelphiObject(const Value: TBitmap);
begin
  inherited DelphiObject := Value;
end;

function TPyDelphiBitmap.Set_Handle(AValue: PPyObject;
  AContext: Pointer): Integer;
var
  _value : Integer;
begin
  Adjust(@Self);
  if CheckIntAttribute(AValue, 'Handle', _value) then
  begin
    DelphiObject.Handle := _value;
    Result := 0;
  end
  else
    Result := -1;
end;

function TPyDelphiBitmap.Set_HandleType(AValue: PPyObject;
  AContext: Pointer): Integer;
var
  _value : String;
begin
  Adjust(@Self);
  if CheckStrAttribute(AValue, 'HandleType', _value) then
  begin
    if SameText(_value, 'bmDIB') then
      DelphiObject.HandleType := bmDIB
    else if SameText(_value, 'bmDDB') then
      DelphiObject.HandleType := bmDDB
    else
    begin
      with GetPythonEngine do
        PyErr_SetString (PyExc_AttributeError^, PAnsiChar(AnsiString(Format('Unknown THandleType value "%s"', [_value]))));
      Result := -1;
      Exit;
    end;
    Result := 0;
  end
  else
    Result := -1;
end;

{$IFNDEF FPC}
function TPyDelphiBitmap.Set_IgnorePalette(AValue: PPyObject;
  AContext: Pointer): Integer;
var
  _value : Boolean;
begin
  Adjust(@Self);
  if CheckBoolAttribute(AValue, 'IgnorePalette', _value) then
  begin
    DelphiObject.IgnorePalette := _value;
    Result := 0;
  end
  else
    Result := -1;
end;
{$ENDIF FPC}

function TPyDelphiBitmap.Set_MaskHandle(AValue: PPyObject;
  AContext: Pointer): Integer;
var
  _value : Integer;
begin
  Adjust(@Self);
  if CheckIntAttribute(AValue, 'MaskHandle', _value) then
  begin
    DelphiObject.MaskHandle := _value;
    Result := 0;
  end
  else
    Result := -1;
end;

function TPyDelphiBitmap.Set_Monochrome(AValue: PPyObject;
  AContext: Pointer): Integer;
var
  _value : Boolean;
begin
  Adjust(@Self);
  if CheckBoolAttribute(AValue, 'Monochrome', _value) then
  begin
    DelphiObject.Monochrome := _value;
    Result := 0;
  end
  else
    Result := -1;
end;

function TPyDelphiBitmap.Set_PixelFormat(AValue: PPyObject;
  AContext: Pointer): Integer;
var
  _value : String;
begin
  with GetPythonEngine do begin
    Adjust(@Self);
    if CheckStrAttribute(AValue, 'PixelFormat', _value) then
    begin
      if SameText(_value, 'pfDevice') then
        DelphiObject.PixelFormat := pfDevice
      else if SameText(_value, 'pf1bit') then
        DelphiObject.PixelFormat := pf1bit
      else if SameText(_value, 'pf4bit') then
        DelphiObject.PixelFormat := pf4bit
      else if SameText(_value, 'pf8bit') then
        DelphiObject.PixelFormat := pf8bit
      else if SameText(_value, 'pf15bit') then
        DelphiObject.PixelFormat := pf15bit
      else if SameText(_value, 'pf16bit') then
        DelphiObject.PixelFormat := pf16bit
      else if SameText(_value, 'pf24bit') then
        DelphiObject.PixelFormat := pf24bit
      else if SameText(_value, 'pf32bit') then
        DelphiObject.PixelFormat := pf32bit
      else if SameText(_value, 'pfCustom') then
        DelphiObject.PixelFormat := pfCustom
      else
      begin
        PyErr_SetString (PyExc_AttributeError^, PAnsiChar(AnsiString(Format('Unknown TPixelFormat value "%s"', [_value]))));
        Result := -1;
        Exit;
      end;
      Result := 0;
    end
    else
      Result := -1;
  end;
end;

function TPyDelphiBitmap.Set_TransparentColor(AValue: PPyObject;
  AContext: Pointer): Integer;
var
  _value : Integer;
begin
  Adjust(@Self);
  if CheckIntAttribute(AValue, 'TransparentColor', _value) then
  begin
    DelphiObject.TransparentColor := _value;
    Result := 0;
  end
  else
    Result := -1;
end;

function TPyDelphiBitmap.Set_TransparentMode(AValue: PPyObject;
  AContext: Pointer): Integer;
var
  _value : String;
begin
  Adjust(@Self);
  if CheckStrAttribute(AValue, 'TransparentMode', _value) then
  begin
    if SameText(_value, 'tmAuto') then
      DelphiObject.TransparentMode := tmAuto
    else if SameText(_value, 'tmFixed') then
      DelphiObject.TransparentMode := tmFixed
    else
    begin
      with GetPythonEngine do
        PyErr_SetString (PyExc_AttributeError^, PAnsiChar(AnsiString(Format('Unknown TTransparentMode value "%s"', [_value]))));
      Result := -1;
      Exit;
    end;
    Result := 0;
  end
  else
    Result := -1;
end;

{ TPyDelphiCanvas }

function TPyDelphiCanvas.Arc_Wrapper(args: PPyObject): PPyObject;
var
  x1, y1, x2, y2, x3, y3, x4, y4: Integer;
begin
  // We adjust the transmitted self argument
  Adjust(@Self);
  with GetPythonEngine do begin
    if PyArg_ParseTuple( args, 'iiiiiiii:Arc',@x1, @y1, @x2, @y2, @x3, @y3, @x4, @y4 ) <> 0 then
    begin
      DelphiObject.Arc(x1, y1, x2, y2, x3, y3, x4, y4);
      Result := ReturnNone;
    end
    else
      Result := nil;
  end;
end;

{$IFNDEF FPC}
function TPyDelphiCanvas.BrushCopy_Wrapper(args: PPyObject): PPyObject;
var
  _obj : TObject;
  _oBitmap: PPyObject;
  _bitmap : TBitmap;
  _color : Integer;
  _dest : TRect;
  _source : TRect;
  _oDest : PPyObject;
  _oSource : PPyObject;
begin
  // We adjust the transmitted self argument
  Adjust(@Self);
  with GetPythonEngine do begin
    _oBitmap := nil;
    _oSource := nil;
    _oDest := nil;
    if (PyArg_ParseTuple( args, 'OOOi:BrushCopy',@_oDest, @_oBitmap, @_oSource, @_color ) <> 0) and
       CheckRectAttribute(_oDest, 'Dest', _dest) and
       CheckRectAttribute(_oSource , 'Source', _source) and
       CheckObjAttribute(_oBitmap, 'Bitmap', TBitmap, _obj) then
    begin
      _bitmap := TBitmap(_obj);
      DelphiObject.BrushCopy(_dest, _bitmap, _source, _color);
      Result := ReturnNone;
    end
    else
      Result := nil;
  end;
end;
{$ENDIF FPC}

function TPyDelphiCanvas.Chord_Wrapper(args: PPyObject): PPyObject;
var
  x1, y1, x2, y2, x3, y3, x4, y4: Integer;
begin
  // We adjust the transmitted self argument
  Adjust(@Self);
  with GetPythonEngine do begin
    if PyArg_ParseTuple( args, 'iiiiiiii:Chord',@x1, @y1, @x2, @y2, @x3, @y3, @x4, @y4 ) <> 0 then
    begin
      DelphiObject.Chord(x1, y1, x2, y2, x3, y3, x4, y4);
      Result := ReturnNone;
    end
    else
      Result := nil;
  end;
end;

function TPyDelphiCanvas.CopyRect_Wrapper(args: PPyObject): PPyObject;
var
  _obj : TObject;
  _oCanvas : PPyObject;
  _canvas : TCanvas;
  _oDest : PPyObject;
  _dest : TRect;
  _oSource : PPyObject;
  _source : TRect;
begin
  // We adjust the transmitted self argument
  Adjust(@Self);
  with GetPythonEngine do begin
    _oCanvas := nil;
    _oSource := nil;
    _oDest := nil;
    if (PyArg_ParseTuple( args, 'OOO:CopyRect',@_oDest, @_oCanvas, @_oSource ) <> 0) and
       CheckRectAttribute(_oDest, 'Dest', _dest) and
       CheckRectAttribute(_oSource, 'Source', _source) and
       CheckObjAttribute(_oCanvas, 'Canvas', TCanvas, _obj) then
    begin
      _canvas := TCanvas(_obj);
      DelphiObject.CopyRect(_dest, _canvas, _source);
      Result := ReturnNone;
    end
    else
      Result := nil;
  end;
end;

class function TPyDelphiCanvas.DelphiObjectClass: TClass;
begin
  Result := TCanvas;
end;

function TPyDelphiCanvas.Draw_Wrapper(args: PPyObject): PPyObject;
var
  x, y : Integer;
  _obj : TObject;
  _oGraphic : PPyObject;
  _graphic : TGraphic;
begin
  // We adjust the transmitted self argument
  Adjust(@Self);
  with GetPythonEngine do begin
    x := 0;
    y := 0;
    _oGraphic := nil;
    if (PyArg_ParseTuple( args, 'iiO:Draw',@x, @y, @_oGraphic ) <> 0) and
       CheckObjAttribute(_oGraphic, 'Graphic', TGraphic, _obj) then
    begin
      _graphic := TGraphic(_obj);
      DelphiObject.Draw(x, y, _graphic);
      Result := ReturnNone;
    end
    else
      Result := nil;
  end;
end;

function TPyDelphiCanvas.DrawFocusRect_Wrapper(args: PPyObject): PPyObject;
var
  _rectO : PPyObject;
  _rect : TRect;
begin
  // We adjust the transmitted self argument
  Adjust(@Self);
  with GetPythonEngine do begin
    if (PyArg_ParseTuple( args, 'O:DrawFocusRect',@_rectO ) <> 0) and
       CheckRectAttribute(_rectO, 'Rect', _rect) then
    begin
      DelphiObject.DrawFocusRect(_rect);
      Result := ReturnNone;
    end
    else
      Result := nil;
  end;
end;

function TPyDelphiCanvas.Ellipse_Wrapper(args: PPyObject): PPyObject;
var
  x1, y1, x2, y2: Integer;
begin
  // We adjust the transmitted self argument
  Adjust(@Self);
  with GetPythonEngine do begin
    if PyArg_ParseTuple( args, 'iiii:Ellipse',@x1, @y1, @x2, @y2 ) <> 0 then
    begin
      DelphiObject.Ellipse(x1, y1, x2, y2);
      Result := ReturnNone;
    end
    else
      Result := nil;
  end;
end;

function TPyDelphiCanvas.FillRect_Wrapper(args: PPyObject): PPyObject;
var
  _rectO : PPyObject;
  _rect : TRect;
begin
  // We adjust the transmitted self argument
  Adjust(@Self);
  with GetPythonEngine do begin
    if (PyArg_ParseTuple( args, 'O:FillRect',@_rectO ) <> 0) and
       CheckRectAttribute(_rectO, 'Rect', _rect) then
    begin
      DelphiObject.FillRect(_rect);
      Result := ReturnNone;
    end
    else
      Result := nil;
  end;
end;

function TPyDelphiCanvas.FloodFill_Wrapper(args: PPyObject): PPyObject;
var
  x, y, _color: Integer;
  _pFillStyle : PPyObject;
  _FillStyle : TFillStyle;
begin
  // We adjust the transmitted self argument
  Adjust(@Self);
  with GetPythonEngine do begin
    if PyArg_ParseTuple( args, 'iiiO:FloodFill',@x, @y, @_color, @_pFillStyle ) <> 0 then
    begin
      if SameText(PyString_AsDelphiString(_pFillStyle), 'fsBorder') then
        _FillStyle := fsBorder
      else
        _FillStyle := fsSurface;
      DelphiObject.FloodFill(x, y, _color, _fillStyle);
      Result := ReturnNone;
    end
    else
      Result := nil;
  end;
end;

function TPyDelphiCanvas.FrameRect_Wrapper(args: PPyObject): PPyObject;
var
  _rectO : PPyObject;
  _rect : TRect;
begin
  // We adjust the transmitted self argument
  Adjust(@Self);
  with GetPythonEngine do begin
    if (PyArg_ParseTuple( args, 'O:FrameRect',@_rectO ) <> 0) and
       CheckRectAttribute(_rectO, 'Rect', _rect) then
    begin
      DelphiObject.FrameRect(_rect);
      Result := ReturnNone;
    end
    else
      Result := nil;
  end;
end;

{$IFNDEF FPC}
function TPyDelphiCanvas.Get_CanvasOrientation(
  AContext: Pointer): PPyObject;
begin
  Adjust(@Self);
  with GetPythonEngine do begin
    if DelphiObject.CanvasOrientation = coRightToLeft then
      Result := PyString_FromDelphiString('coRightToLeft')
    else
      Result := PyString_FromDelphiString('coLeftToRight');
  end;
end;
{$ENDIF FPC}

function TPyDelphiCanvas.Get_ClipRect(AContext: Pointer): PPyObject;
begin
  Adjust(@Self);
  Result := WrapRect(PyDelphiWrapper, DelphiObject.ClipRect);
end;

function TPyDelphiCanvas.Get_Handle(AContext: Pointer): PPyObject;
begin
  Adjust(@Self);
  Result := GetPythonEngine.PyInt_FromLong(DelphiObject.Handle);
end;

function TPyDelphiCanvas.Get_HandleAllocated(AContext: Pointer): PPyObject;
begin
  Adjust(@Self);
  Result := GetPythonEngine.VariantAsPyObject(DelphiObject.HandleAllocated);
end;

function TPyDelphiCanvas.Get_LockCount(AContext: Pointer): PPyObject;
begin
  Adjust(@Self);
  Result := GetPythonEngine.PyInt_FromLong(DelphiObject.LockCount);
end;

function TPyDelphiCanvas.Get_OnChange(AContext: Pointer): PPyObject;
begin
  //todo
  Result := GetPythonEngine.ReturnNone;
end;

function TPyDelphiCanvas.Get_OnChanging(AContext: Pointer): PPyObject;
begin
  //todo
  Result := GetPythonEngine.ReturnNone;
end;

function TPyDelphiCanvas.Get_PenPos(AContext: Pointer): PPyObject;
begin
  Adjust(@Self);
  Result := WrapPoint(PyDelphiWrapper, DelphiObject.PenPos);
end;

{$IFNDEF FPC}
function TPyDelphiCanvas.Get_TextFlags(AContext: Pointer): PPyObject;
begin
  Adjust(@Self);
  Result := GetPythonEngine.PyInt_FromLong(DelphiObject.TextFlags);
end;
{$ENDIF FPC}

function TPyDelphiCanvas.GetDelphiObject: TCanvas;
begin
  Result := TCanvas(inherited DelphiObject);
end;

function TPyDelphiCanvas.GetPixel(args: PPyObject): PPyObject;
var
  x, y : Integer;
begin
  // We adjust the transmitted self argument
  Adjust(@Self);
  with GetPythonEngine do begin
    x := 0;
    y := 0;
    if PyArg_ParseTuple( args, 'ii:GetPixel',@x, @y ) <> 0 then
    begin
      Result := PyInt_FromLong(DelphiObject.Pixels[x, y]);
    end
    else
      Result := nil;
  end;
end;

function TPyDelphiCanvas.LineTo_Wrapper(args: PPyObject): PPyObject;
var
  x, y : Integer;
begin
  // We adjust the transmitted self argument
  Adjust(@Self);
  with GetPythonEngine do begin
    x := 0;
    y := 0;
    if PyArg_ParseTuple( args, 'ii:LineTo',@x, @y ) <> 0 then
    begin
      DelphiObject.LineTo(x, y);
      Result := ReturnNone;
    end
    else
      Result := nil;
  end;
end;

function TPyDelphiCanvas.Lock_Wrapper(args: PPyObject): PPyObject;
begin
  // We adjust the transmitted self argument
  Adjust(@Self);
  with GetPythonEngine do begin
    if PyArg_ParseTuple( args, ':Lock') <> 0 then
    begin
      DelphiObject.Lock;
      Result := ReturnNone;
    end
    else
      Result := nil;
  end;
end;

function TPyDelphiCanvas.MoveTo_Wrapper(args: PPyObject): PPyObject;
var
  x, y : Integer;
begin
  // We adjust the transmitted self argument
  Adjust(@Self);
  with GetPythonEngine do begin
    x := 0;
    y := 0;
    if PyArg_ParseTuple( args, 'ii:MoveTo',@x, @y ) <> 0 then
    begin
      DelphiObject.MoveTo(x, y);
      Result := ReturnNone;
    end
    else
      Result := nil;
  end;
end;

function TPyDelphiCanvas.Pie_Wrapper(args: PPyObject): PPyObject;
var
  x1, y1, x2, y2, x3, y3, x4, y4: Integer;
begin
  // We adjust the transmitted self argument
  Adjust(@Self);
  with GetPythonEngine do begin
    if PyArg_ParseTuple( args, 'iiiiiiii:Pie',@x1, @y1, @x2, @y2, @x3, @y3, @x4, @y4 ) <> 0 then
    begin
      DelphiObject.Pie(x1, y1, x2, y2, x3, y3, x4, y4);
      Result := ReturnNone;
    end
    else
      Result := nil;
  end;
end;

function TPyDelphiCanvas.PolyBezier_Wrapper(args: PPyObject): PPyObject;
var
  i : Integer;
  p : TPoint;
  _points : array of TPoint;
  _oPoints : PPyObject;
  _oPoint : PPyObject;
begin
  // We adjust the transmitted self argument
  Adjust(@Self);
  with GetPythonEngine do begin
    _oPoints := nil;
    if PyArg_ParseTuple( args, 'O:PolyBezier',@_oPoints ) <> 0 then
    begin
      if PySequence_Check(_oPoints) <> 0 then
      begin
        SetLength(_points, PySequence_Length(_oPoints));
        for i := 0 to Length(_points)-1 do
        begin
          _oPoint := PySequence_GetItem(_oPoints, i);
          try
            if CheckPointAttribute(_oPoint, Format('Item #%d of Point sequence', [i]), p) then
              _points[i] := p
            else
            begin
              Result := nil;
              Exit;
            end;
          finally
            Py_XDecRef(_oPoint);
          end;
        end;
        DelphiObject.PolyBezier(_points);
        Result := GetPythonEngine.ReturnNone;
      end
      else
      begin
        Result := nil;
        PyErr_SetString (PyExc_AttributeError^, 'PolyBezier accepts only a sequence of points as single parameter');
      end;
    end
    else
      Result := nil;
  end;
end;

{$IFNDEF FPC}
function TPyDelphiCanvas.PolyBezierTo_Wrapper(args: PPyObject): PPyObject;
var
  i : Integer;
  p : TPoint;
  _points : array of TPoint;
  _oPoints : PPyObject;
  _oPoint : PPyObject;
begin
  // We adjust the transmitted self argument
  Adjust(@Self);
  with GetPythonEngine do begin
    _oPoints := nil;
    if PyArg_ParseTuple( args, 'O:PolyBezierTo',@_oPoints ) <> 0 then
    begin
      if PySequence_Check(_oPoints) <> 0 then
      begin
        SetLength(_points, PySequence_Length(_oPoints));
        for i := 0 to Length(_points)-1 do
        begin
          _oPoint := PySequence_GetItem(_oPoints, i);
          try
            if CheckPointAttribute(_oPoint, Format('Item #%d of Point sequence', [i]), p) then
              _points[i] := p
            else
            begin
              Result := nil;
              Exit;
            end;
          finally
            Py_XDecRef(_oPoint);
          end;
        end;
        DelphiObject.PolyBezierTo(_points);
        Result := GetPythonEngine.ReturnNone;
      end
      else
      begin
        Result := nil;
        PyErr_SetString (PyExc_AttributeError^, 'PolyBezierTo accepts only a sequence of points as single parameter');
      end;
    end
    else
      Result := nil;
  end;
end;
{$ENDIF FPC}

function TPyDelphiCanvas.Polygon_Wrapper(args: PPyObject): PPyObject;
var
  i : Integer;
  p : TPoint;
  _points : array of TPoint;
  _oPoints : PPyObject;
  _oPoint : PPyObject;
begin
  // We adjust the transmitted self argument
  Adjust(@Self);
  with GetPythonEngine do begin
    _oPoints := nil;
    if PyArg_ParseTuple( args, 'O:Polygon',@_oPoints ) <> 0 then
    begin
      if PySequence_Check(_oPoints) <> 0 then
      begin
        SetLength(_points, PySequence_Length(_oPoints));
        for i := 0 to Length(_points)-1 do
        begin
          _oPoint := PySequence_GetItem(_oPoints, i);
          try
            if CheckPointAttribute(_oPoint, Format('Item #%d of Point sequence', [i]), p) then
              _points[i] := p
            else
            begin
              Result := nil;
              Exit;
            end;
          finally
            Py_XDecRef(_oPoint);
          end;
        end;
        DelphiObject.Polygon(_points);
        Result := GetPythonEngine.ReturnNone;
      end
      else
      begin
        Result := nil;
        PyErr_SetString (PyExc_AttributeError^, 'Polygon accepts only a sequence of points as single parameter');
      end;
    end
    else
      Result := nil;
  end;
end;

function TPyDelphiCanvas.Polyline_Wrapper(args: PPyObject): PPyObject;
var
  i : Integer;
  p : TPoint;
  _points : array of TPoint;
  _oPoints : PPyObject;
  _oPoint : PPyObject;
begin
  // We adjust the transmitted self argument
  Adjust(@Self);
  with GetPythonEngine do begin
    _oPoints := nil;
    if PyArg_ParseTuple( args, 'O:Polyline',@_oPoints ) <> 0 then
    begin
      if PySequence_Check(_oPoints) <> 0 then
      begin
        SetLength(_points, PySequence_Length(_oPoints));
        for i := 0 to Length(_points)-1 do
        begin
          _oPoint := PySequence_GetItem(_oPoints, i);
          try
            if CheckPointAttribute(_oPoint, Format('Item #%d of Point sequence', [i]), p) then
              _points[i] := p
            else
            begin
              Result := nil;
              Exit;
            end;
          finally
            Py_XDecRef(_oPoint);
          end;
        end;
        DelphiObject.Polyline(_points);
        Result := GetPythonEngine.ReturnNone;
      end
      else
      begin
        Result := nil;
        PyErr_SetString (PyExc_AttributeError^, 'Polyline accepts only a sequence of points as single parameter');
      end;
    end
    else
      Result := nil;
  end;
end;

function TPyDelphiCanvas.Rectangle_Wrapper(args: PPyObject): PPyObject;
var
  x1, y1, x2, y2: Integer;
begin
  // We adjust the transmitted self argument
  Adjust(@Self);
  with GetPythonEngine do begin
    if PyArg_ParseTuple( args, 'iiii:Rectangle',@x1, @y1, @x2, @y2 ) <> 0 then
    begin
      DelphiObject.Rectangle(x1, y1, x2, y2);
      Result := ReturnNone;
    end
    else
      Result := nil;
  end;
end;

function TPyDelphiCanvas.Refresh_Wrapper(args: PPyObject): PPyObject;
begin
  // We adjust the transmitted self argument
  Adjust(@Self);
  with GetPythonEngine do begin
    if PyArg_ParseTuple( args, ':Refresh') <> 0 then
    begin
      DelphiObject.Refresh;
      Result := ReturnNone;
    end
    else
      Result := nil;
  end;
end;

class procedure TPyDelphiCanvas.RegisterGetSets(PythonType: TPythonType);
begin
  inherited;
  with PythonType do
    begin
      AddGetSet('HandleAllocated', @TPyDelphiCanvas.Get_HandleAllocated, nil,
        '', nil);
      AddGetSet('ClipRect', @TPyDelphiCanvas.Get_ClipRect, nil,
        'Specifies the boundaries of the clipping rectangle.', nil);
      AddGetSet('Handle', @TPyDelphiCanvas.Get_Handle, @TPyDelphiCanvas.Set_Handle,
        'Specifies the handle for this canvas.', nil);
      AddGetSet('LockCount', @TPyDelphiCanvas.Get_LockCount,nil,
        'Indicates the number of times the canvas has been locked to prevent interference from other threads.', nil);
      {$IFNDEF FPC}
      AddGetSet('CanvasOrientation', @TPyDelphiCanvas.Get_CanvasOrientation, nil,
        'Determines the orientation of the canvas as left-to-right or right-to-left.', nil);
      {$ENDIF FPC}
      AddGetSet('PenPos', @TPyDelphiCanvas.Get_PenPos, nil,
        'Specifies the current drawing position of the Pen. ', nil);
      {$IFNDEF FPC}
      AddGetSet('TextFlags', @TPyDelphiCanvas.Get_TextFlags, @TPyDelphiCanvas.Set_TextFlags,
        'Specifies how text is written to the canvas.', nil);
      {$ENDIF FPC}
      AddGetSet('OnChange', @TPyDelphiCanvas.Get_OnChange, @TPyDelphiCanvas.Set_OnChange,
        'Occurs when the image has just changed.', nil);
      AddGetSet('OnChanging', @TPyDelphiCanvas.Get_OnChanging, @TPyDelphiCanvas.Set_OnChanging,
        'Occurs just before a change is made to the image.', nil);
    end;
end;

class procedure TPyDelphiCanvas.RegisterMethods(PythonType: TPythonType);
begin
  inherited;
  PythonType.AddMethod('Arc', @TPyDelphiCanvas.Arc_Wrapper,
    'TCanvas.Arc()'#10 +
    '');
  {$IFNDEF FPC}
  PythonType.AddMethod('BrushCopy', @TPyDelphiCanvas.BrushCopy_Wrapper,
    'TCanvas.BrushCopy()'#10 +
    '');
  {$ENDIF FPC}
  PythonType.AddMethod('Chord', @TPyDelphiCanvas.Chord_Wrapper,
    'TCanvas.Chord()'#10 +
    '');
  PythonType.AddMethod('CopyRect', @TPyDelphiCanvas.CopyRect_Wrapper,
    'TCanvas.CopyRect()'#10 +
    '');
  PythonType.AddMethod('Draw', @TPyDelphiCanvas.Draw_Wrapper,
    'TCanvas.Draw()'#10 +
    '');
  PythonType.AddMethod('DrawFocusRect', @TPyDelphiCanvas.DrawFocusRect_Wrapper,
    'TCanvas.DrawFocusRect()'#10 +
    '');
  PythonType.AddMethod('Ellipse', @TPyDelphiCanvas.Ellipse_Wrapper,
    'TCanvas.Ellipse()'#10 +
    '');
  PythonType.AddMethod('FillRect', @TPyDelphiCanvas.FillRect_Wrapper,
    'TCanvas.FillRect()'#10 +
    '');
  PythonType.AddMethod('FloodFill', @TPyDelphiCanvas.FloodFill_Wrapper,
    'TCanvas.FloodFill()'#10 +
    '');
  PythonType.AddMethod('FrameRect', @TPyDelphiCanvas.FrameRect_Wrapper,
    'TCanvas.FrameRect()'#10 +
    '');
  PythonType.AddMethod('LineTo', @TPyDelphiCanvas.LineTo_Wrapper,
    'TCanvas.LineTo()'#10 +
    '');
  PythonType.AddMethod('Lock', @TPyDelphiCanvas.Lock_Wrapper,
    'TCanvas.Lock()'#10 +
    '');
  PythonType.AddMethod('MoveTo', @TPyDelphiCanvas.MoveTo_Wrapper,
    'TCanvas.MoveTo()'#10 +
    '');
  PythonType.AddMethod('Pie', @TPyDelphiCanvas.Pie_Wrapper,
    'TCanvas.Pie()'#10 +
    '');
  PythonType.AddMethod('Polygon', @TPyDelphiCanvas.Polygon_Wrapper,
    'TCanvas.Polygon()'#10 +
    '');
  PythonType.AddMethod('Polyline', @TPyDelphiCanvas.Polyline_Wrapper,
    'TCanvas.Polyline()'#10 +
    '');
  PythonType.AddMethod('PolyBezier', @TPyDelphiCanvas.PolyBezier_Wrapper,
    'TCanvas.PolyBezier()'#10 +
    '');
  {$IFNDEF FPC}
  PythonType.AddMethod('PolyBezierTo', @TPyDelphiCanvas.PolyBezierTo_Wrapper,
    'TCanvas.PolyBezierTo()'#10 +
    '');
  {$ENDIF FPC}
  PythonType.AddMethod('Rectangle', @TPyDelphiCanvas.Rectangle_Wrapper,
    'TCanvas.Rectangle()'#10 +
    '');
  PythonType.AddMethod('Refresh', @TPyDelphiCanvas.Refresh_Wrapper,
    'TCanvas.Refresh()'#10 +
    '');
  PythonType.AddMethod('RoundRect', @TPyDelphiCanvas.RoundRect_Wrapper,
    'TCanvas.RoundRect()'#10 +
    '');
  PythonType.AddMethod('StretchDraw', @TPyDelphiCanvas.StretchDraw_Wrapper,
    'TCanvas.StretchDraw()'#10 +
    '');
  PythonType.AddMethod('TextExtent', @TPyDelphiCanvas.TextExtent_Wrapper,
    'TCanvas.TextExtent()'#10 +
    '');
  PythonType.AddMethod('TextHeight', @TPyDelphiCanvas.TextHeight_Wrapper,
    'TCanvas.TextHeight()'#10 +
    '');
  PythonType.AddMethod('TextOut', @TPyDelphiCanvas.TextOut_Wrapper,
    'TCanvas.TextOut()'#10 +
    '');
  PythonType.AddMethod('TextRect', @TPyDelphiCanvas.TextRect_Wrapper,
    'TCanvas.TextRect()'#10 +
    '');
  PythonType.AddMethod('TextWidth', @TPyDelphiCanvas.TextWidth_Wrapper,
    'TCanvas.TextWidth()'#10 +
    '');
  PythonType.AddMethod('TryLock', @TPyDelphiCanvas.TryLock_Wrapper,
    'TCanvas.TryLock()'#10 +
    '');
  PythonType.AddMethod('Unlock', @TPyDelphiCanvas.Unlock_Wrapper,
    'TCanvas.Unlock()'#10 +
    '');
  PythonType.AddMethod('GetPixel', @TPyDelphiCanvas.GetPixel,
    'TCanvas.GetPixel(x, y) -> TColor'#10 +
    'This is the same as TCanvas.Pixels[x, y].'#13+
    'Returns the color of the pixels within the current ClipRect.');
  PythonType.AddMethod('SetPixel', @TPyDelphiCanvas.SetPixel,
    'TCanvas.SetPixel(x, y, color)'#10 +
    'This is the same as TCanvas.Pixels[x, y] := color'#13+
    'Specifies the color of the pixels within the current ClipRect.');
end;

function TPyDelphiCanvas.RoundRect_Wrapper(args: PPyObject): PPyObject;
var
  x1, y1, x2, y2, x3, y3: Integer;
begin
  // We adjust the transmitted self argument
  Adjust(@Self);
  with GetPythonEngine do begin
    if PyArg_ParseTuple( args, 'iiiiii:RoundRect',@x1, @y1, @x2, @y2, @x3, @y3 ) <> 0 then
    begin
      DelphiObject.RoundRect(x1, y1, x2, y2, x3, y3);
      Result := ReturnNone;
    end
    else
      Result := nil;
  end;
end;

function TPyDelphiCanvas.Set_Handle(AValue: PPyObject;
  AContext: Pointer): integer;
var
  _value : Integer;
begin
  Adjust(@Self);
  if CheckIntAttribute(AValue, 'Handle', _value) then
  begin
    DelphiObject.Handle := _value;
    Result := 0;
  end
  else
    Result := -1;
end;

function TPyDelphiCanvas.Set_OnChange(AValue: PPyObject;
  AContext: Pointer): integer;
begin
  //todo
  Result := 0;
end;

function TPyDelphiCanvas.Set_OnChanging(AValue: PPyObject;
  AContext: Pointer): integer;
begin
  //todo
  Result := 0;
end;

{$IFNDEF FPC}
function TPyDelphiCanvas.Set_TextFlags(AValue: PPyObject;
  AContext: Pointer): integer;
var
  _value : Integer;
begin
  Adjust(@Self);
  if CheckIntAttribute(AValue, 'TextFlags', _value) then
  begin
    DelphiObject.TextFlags := _value;
    Result := 0;
  end
  else
    Result := -1;
end;
{$ENDIF FPC}

procedure TPyDelphiCanvas.SetDelphiObject(const Value: TCanvas);
begin
  inherited DelphiObject := Value;
end;

function TPyDelphiCanvas.SetPixel(args: PPyObject): PPyObject;
var
  x, y : Integer;
  _color : TColor;
begin
  // We adjust the transmitted self argument
  Adjust(@Self);
  with GetPythonEngine do begin
    x := 0;
    y := 0;
    if PyArg_ParseTuple( args, 'iii:SetPixel',@x, @y, @_color ) <> 0 then
    begin
      DelphiObject.Pixels[x, y] := _color;
      Result := GetPythonEngine.ReturnNone;
    end
    else
      Result := nil;
  end;
end;

function TPyDelphiCanvas.StretchDraw_Wrapper(args: PPyObject): PPyObject;
var
  _obj : TObject;
  _oGraphic : PPyObject;
  _graphic : TGraphic;
  _oRect : PPyObject;
  _rect : TRect;
begin
  // We adjust the transmitted self argument
  Adjust(@Self);
  with GetPythonEngine do begin
    _oGraphic := nil;
    _oRect := nil;
    if (PyArg_ParseTuple( args, 'iiO:StretchDraw',@_oRect, @_oGraphic ) <> 0) and
       CheckRectAttribute(_oRect, 'Rect', _rect) and
       CheckObjAttribute(_oGraphic, 'Graphic', TGraphic, _obj) then
    begin
      _graphic := TGraphic(_obj);
      DelphiObject.StretchDraw(_rect, _graphic);
      Result := ReturnNone;
    end
    else
      Result := nil;
  end;
end;

function TPyDelphiCanvas.TextExtent_Wrapper(args: PPyObject): PPyObject;
var
  _pText : PPyObject;
begin
  // We adjust the transmitted self argument
  Adjust(@Self);
  with GetPythonEngine do begin
    if PyArg_ParseTuple( args, 'O:TextExtent',@_pText ) <> 0 then
    begin
      Result := WrapSize(PyDelphiWrapper, DelphiObject.TextExtent(PyString_AsDelphiString(_pText)));
    end
    else
      Result := nil;
  end;
end;

function TPyDelphiCanvas.TextHeight_Wrapper(args: PPyObject): PPyObject;
var
  _pText : PPyObject;
begin
  // We adjust the transmitted self argument
  Adjust(@Self);
  with GetPythonEngine do begin
    if PyArg_ParseTuple( args, 'O:TextHeight',@_pText ) <> 0 then
    begin
      Result := PyInt_FromLong(DelphiObject.TextHeight(PyString_AsDelphiString(_pText)));
    end
    else
      Result := nil;
  end;
end;

function TPyDelphiCanvas.TextOut_Wrapper(args: PPyObject): PPyObject;
var
  x, y : Integer;
  _text : PAnsiChar;
begin
  // We adjust the transmitted self argument
  Adjust(@Self);
  with GetPythonEngine do begin
    x := 0;
    y := 0;
    if PyArg_ParseTuple( args, 'iis:TextRect',@x, @y, @_text ) <> 0 then
    begin
      if _text <> nil then
        DelphiObject.TextOut(x, y, String(_text));
      Result := GetPythonEngine.ReturnNone;
    end
    else
      Result := nil;
  end;
end;

function TPyDelphiCanvas.TextRect_Wrapper(args: PPyObject): PPyObject;
var
  x, y : Integer;
  _rectO : PPyObject;
  _rect : TRect;
  _text : PAnsiChar;
begin
  // We adjust the transmitted self argument
  Adjust(@Self);
  with GetPythonEngine do begin
    x := 0;
    y := 0;
    _rectO := nil;
    _text := nil;
    if (PyArg_ParseTuple( args, 'Oiis:TextRect',@_rectO, @x, @y, @_text ) <> 0) and
       CheckRectAttribute(_rectO, 'Rect', _rect) then
    begin
      if _text <> nil then
        DelphiObject.TextRect(_rect, x, y, String(_text));
      Result := GetPythonEngine.ReturnNone;
    end
    else
      Result := nil;
  end;
end;

function TPyDelphiCanvas.TextWidth_Wrapper(args: PPyObject): PPyObject;
var
  _pText : PPyObject;
begin
  // We adjust the transmitted self argument
  Adjust(@Self);
  with GetPythonEngine do begin
    if PyArg_ParseTuple( args, 'O:TextWidth',@_pText ) <> 0 then
    begin
      Result := PyInt_FromLong(DelphiObject.TextWidth(PyString_AsDelphiString(_pText)));
    end
    else
      Result := nil;
  end;
end;

function TPyDelphiCanvas.TryLock_Wrapper(args: PPyObject): PPyObject;
begin
  // We adjust the transmitted self argument
  Adjust(@Self);
  with GetPythonEngine do begin
    if PyArg_ParseTuple( args, ':Unlock') <> 0 then
    begin
      Result := VariantAsPyObject( DelphiObject.TryLock );
    end
    else
      Result := nil;
  end;
end;

function TPyDelphiCanvas.Unlock_Wrapper(args: PPyObject): PPyObject;
begin
  // We adjust the transmitted self argument
  Adjust(@Self);
  with GetPythonEngine do begin
    if PyArg_ParseTuple( args, ':Unlock') <> 0 then
    begin
      DelphiObject.Unlock;
      Result := ReturnNone;
    end
    else
      Result := nil;
  end;
end;

{$IFNDEF FPC}
{ TPyDelphiMetaFile }

function TPyDelphiMetaFile.Clear_Wrapper(args: PPyObject): PPyObject;
begin
  // We adjust the transmitted self argument
  Adjust(@Self);
  with GetPythonEngine do begin
    if PyArg_ParseTuple( args, ':Clear') <> 0 then
    begin
      DelphiObject.Clear;
      Result := ReturnNone;
    end
    else
      Result := nil;
  end;
end;

class function TPyDelphiMetaFile.DelphiObjectClass: TClass;
begin
  Result := TMetaFile;
end;

function TPyDelphiMetaFile.GetDelphiObject: TMetaFile;
begin
  Result := TMetaFile(inherited DelphiObject);
end;

function TPyDelphiMetaFile.Get_CreatedBy(AContext: Pointer): PPyObject;
begin
  Adjust(@Self);
  Result := GetPythonEngine.PyString_FromDelphiString(DelphiObject.CreatedBy);
end;

function TPyDelphiMetaFile.Get_Description(AContext: Pointer): PPyObject;
begin
  Adjust(@Self);
  Result := GetPythonEngine.PyString_FromDelphiString(DelphiObject.Description);
end;

function TPyDelphiMetaFile.Get_Enhanced(AContext: Pointer): PPyObject;
begin
  with GetPythonEngine do begin
    Adjust(@Self);
    Result := VariantAsPyObject(DelphiObject.Enhanced);
  end;
end;

function TPyDelphiMetaFile.Get_Handle(AContext: Pointer): PPyObject;
begin
  Adjust(@Self);
  Result := GetPythonEngine.PyInt_FromLong(DelphiObject.Handle);
end;

function TPyDelphiMetaFile.Get_HandleAllocated(
  AContext: Pointer): PPyObject;
begin
  Adjust(@Self);
  Result := GetPythonEngine.VariantAsPyObject(DelphiObject.HandleAllocated);
end;

function TPyDelphiMetaFile.Get_Inch(AContext: Pointer): PPyObject;
begin
  Adjust(@Self);
  Result := GetPythonEngine.PyInt_FromLong(DelphiObject.Inch);
end;

function TPyDelphiMetaFile.Get_MMHeight(AContext: Pointer): PPyObject;
begin
  Adjust(@Self);
  Result := GetPythonEngine.PyInt_FromLong(DelphiObject.MMHeight);
end;

function TPyDelphiMetaFile.Get_MMWidth(AContext: Pointer): PPyObject;
begin
  Adjust(@Self);
  Result := GetPythonEngine.PyInt_FromLong(DelphiObject.MMWidth);
end;

class procedure TPyDelphiMetaFile.RegisterGetSets(PythonType: TPythonType);
begin
  inherited;
  with PythonType do
    begin
      AddGetSet('CreatedBy', @TPyDelphiMetaFile.Get_CreatedBy, nil,
        '', nil);
      AddGetSet('Description', @TPyDelphiMetaFile.Get_Description, nil,
        '', nil);
      AddGetSet('Enhanced', @TPyDelphiMetaFile.Get_Enhanced, @TPyDelphiMetaFile.Set_Enhanced,
        '', nil);
      AddGetSet('Handle', @TPyDelphiMetaFile.Get_Handle, @TPyDelphiMetaFile.Set_Handle,
        '', nil);
      AddGetSet('HandleAllocated', @TPyDelphiMetaFile.Get_HandleAllocated, nil,
        '', nil);
      AddGetSet('MMWidth', @TPyDelphiMetaFile.Get_MMWidth, @TPyDelphiMetaFile.Set_MMWidth,
        '', nil);
      AddGetSet('MMHeight', @TPyDelphiMetaFile.Get_MMHeight, @TPyDelphiMetaFile.Set_MMHeight,
        '', nil);
      AddGetSet('Inch', @TPyDelphiMetaFile.Get_Inch, @TPyDelphiMetaFile.Set_Inch,
        '', nil);
    end;
end;

class procedure TPyDelphiMetaFile.RegisterMethods(PythonType: TPythonType);
begin
  inherited;
  PythonType.AddMethod('Clear', @TPyDelphiMetaFile.Clear_Wrapper,
    'TMetaFile.Clear()'#10 +
    '');
  PythonType.AddMethod('ReleaseHandle', @TPyDelphiMetaFile.ReleaseHandle_Wrapper,
    'TMetaFile.ReleaseHandle()'#10 +
    '');
end;

function TPyDelphiMetaFile.ReleaseHandle_Wrapper(
  args: PPyObject): PPyObject;
begin
  // We adjust the transmitted self argument
  Adjust(@Self);
  with GetPythonEngine do begin
    if PyArg_ParseTuple( args, ':ReleaseHandle') <> 0 then
    begin
      Result := PyInt_FromLong(DelphiObject.ReleaseHandle);
    end
    else
      Result := nil;
  end;
end;

procedure TPyDelphiMetaFile.SetDelphiObject(const Value: TMetaFile);
begin
  inherited DelphiObject := Value;
end;

function TPyDelphiMetaFile.Set_Enhanced(AValue: PPyObject;
  AContext: Pointer): Integer;
var
  _value : Boolean;
begin
  Adjust(@Self);
  if CheckBoolAttribute(AValue, 'Enhanced', _value) then
  begin
    DelphiObject.Enhanced := _value;
    Result := 0;
  end
  else
    Result := -1;
end;

function TPyDelphiMetaFile.Set_Handle(AValue: PPyObject;
  AContext: Pointer): Integer;
var
  _value : Integer;
begin
  Adjust(@Self);
  if CheckIntAttribute(AValue, 'Handle', _value) then
  begin
    DelphiObject.Handle := _value;
    Result := 0;
  end
  else
    Result := -1;
end;

function TPyDelphiMetaFile.Set_Inch(AValue: PPyObject;
  AContext: Pointer): Integer;
var
  _value : Integer;
begin
  Adjust(@Self);
  if CheckIntAttribute(AValue, 'Inch', _value) then
  begin
    DelphiObject.Inch := _value;
    Result := 0;
  end
  else
    Result := -1;
end;

function TPyDelphiMetaFile.Set_MMHeight(AValue: PPyObject;
  AContext: Pointer): Integer;
var
  _value : Integer;
begin
  Adjust(@Self);
  if CheckIntAttribute(AValue, 'MMHeight', _value) then
  begin
    DelphiObject.MMHeight := _value;
    Result := 0;
  end
  else
    Result := -1;
end;

function TPyDelphiMetaFile.Set_MMWidth(AValue: PPyObject;
  AContext: Pointer): Integer;
var
  _value : Integer;
begin
  Adjust(@Self);
  if CheckIntAttribute(AValue, 'MMWidth', _value) then
  begin
    DelphiObject.MMWidth := _value;
    Result := 0;
  end
  else
    Result := -1;
end;
{$ENDIF FPC}

{ TPyDelphiIcon }

class function TPyDelphiIcon.DelphiObjectClass: TClass;
begin
  Result := TIcon;
end;

function TPyDelphiIcon.GetDelphiObject: TIcon;
begin
  Result := TIcon(inherited DelphiObject);
end;

function TPyDelphiIcon.Get_Handle(AContext: Pointer): PPyObject;
begin
  Adjust(@Self);
  Result := GetPythonEngine.PyInt_FromLong(DelphiObject.Handle);
end;

function TPyDelphiIcon.Get_HandleAllocated(AContext: Pointer): PPyObject;
begin
  Adjust(@Self);
  Result := GetPythonEngine.VariantAsPyObject(DelphiObject.HandleAllocated);
end;

class procedure TPyDelphiIcon.RegisterGetSets(PythonType: TPythonType);
begin
  inherited;
  with PythonType do
    begin
      AddGetSet('Handle', @TPyDelphiIcon.Get_Handle, @TPyDelphiIcon.Set_Handle,
        '', nil);
      AddGetSet('HandleAllocated', @TPyDelphiIcon.Get_HandleAllocated, nil,
        '', nil);
    end;
end;

class procedure TPyDelphiIcon.RegisterMethods(PythonType: TPythonType);
begin
  inherited;
  PythonType.AddMethod('ReleaseHandle', @TPyDelphiIcon.ReleaseHandle_Wrapper,
    'TIcon.ReleaseHandle()'#10 +
    '');
end;

function TPyDelphiIcon.ReleaseHandle_Wrapper(args: PPyObject): PPyObject;
begin
  // We adjust the transmitted self argument
  Adjust(@Self);
  with GetPythonEngine do begin
    if PyArg_ParseTuple( args, ':ReleaseHandle') <> 0 then
    begin
      Result := PyInt_FromLong(DelphiObject.ReleaseHandle);
    end
    else
      Result := nil;
  end;
end;

procedure TPyDelphiIcon.SetDelphiObject(const Value: TIcon);
begin
  inherited DelphiObject := Value;
end;

function TPyDelphiIcon.Set_Handle(AValue: PPyObject;
  AContext: Pointer): Integer;
var
  _value : Integer;
begin
  Adjust(@Self);
  if CheckIntAttribute(AValue, 'Handle', _value) then
  begin
    DelphiObject.Handle := _value;
    Result := 0;
  end
  else
    Result := -1;
end;

{ TPyDelphiPicture }

class function TPyDelphiPicture.DelphiObjectClass: TClass;
begin
  Result := TPicture;
end;

function TPyDelphiPicture.Get_Bitmap(AContext: Pointer): PPyObject;
begin
  Adjust(@Self);
  Result := Wrap(DelphiObject.Bitmap);
end;

function TPyDelphiPicture.Get_Graphic(AContext: Pointer): PPyObject;
begin
  Adjust(@Self);
  Result := Wrap(DelphiObject.Graphic);
end;

function TPyDelphiPicture.Get_Height(AContext: Pointer): PPyObject;
begin
    Adjust(@Self);
    Result := GetPythonEngine.PyInt_FromLong(DelphiObject.Height);
end;

function TPyDelphiPicture.Get_Icon(AContext: Pointer): PPyObject;
begin
  Adjust(@Self);
  Result := Wrap(DelphiObject.Icon);
end;

{$IFNDEF FPC}
function TPyDelphiPicture.Get_MetaFile(AContext: Pointer): PPyObject;
begin
  Adjust(@Self);
  Result := Wrap(DelphiObject.MetaFile);
end;
{$ENDIF FPC}

function TPyDelphiPicture.Get_Width(AContext: Pointer): PPyObject;
begin
  Adjust(@Self);
  Result := GetPythonEngine.PyInt_FromLong(DelphiObject.Width);
end;

function TPyDelphiPicture.GetDelphiObject: TPicture;
begin
  Result := TPicture(inherited DelphiObject);
end;

function TPyDelphiPicture.LoadFromFile_Wrapper(args: PPyObject): PPyObject;
var
  _pFileName : PPyObject;
begin
  // We adjust the transmitted self argument
  Adjust(@Self);
  with GetPythonEngine do begin
    if PyArg_ParseTuple( args, 'O:LoadFromFile',@_pFileName ) <> 0 then
    begin
      DelphiObject.LoadFromFile(PyString_AsDelphiString(_pFileName));
      Result := ReturnNone;
    end
    else
      Result := nil;
  end;
end;

class procedure TPyDelphiPicture.RegisterGetSets(PythonType: TPythonType);
begin
  inherited;
  with PythonType do
    begin
      AddGetSet('Bitmap', @TPyDelphiPicture.Get_Bitmap, @TPyDelphiPicture.Set_Bitmap,
        '', nil);
      AddGetSet('Graphic', @TPyDelphiPicture.Get_Graphic, @TPyDelphiPicture.Set_Graphic,
        '', nil);
      AddGetSet('Height', @TPyDelphiPicture.Get_Height, nil,
        '', nil);
      AddGetSet('Icon', @TPyDelphiPicture.Get_Icon, @TPyDelphiPicture.Set_Icon,
        '', nil);
      {$IFNDEF FPC}
      AddGetSet('Metafile', @TPyDelphiPicture.Get_Metafile, @TPyDelphiPicture.Set_Metafile,
        '', nil);
      {$ENDIF FPC}
      AddGetSet('Width', @TPyDelphiPicture.Get_Width, nil,
        '', nil);
    end;
end;

class procedure TPyDelphiPicture.RegisterMethods(PythonType: TPythonType);
begin
  inherited;
  PythonType.AddMethod('LoadFromFile', @TPyDelphiPicture.LoadFromFile_Wrapper,
    'TPicture.LoadFromFile()'#10 +
    '');
  PythonType.AddMethod('SaveToFile', @TPyDelphiPicture.SaveToFile_Wrapper,
    'TPicture.SaveToFile()'#10 +
    '');
end;

function TPyDelphiPicture.SaveToFile_Wrapper(args: PPyObject): PPyObject;
var
  _pFileName : PPyObject;
begin
  // We adjust the transmitted self argument
  Adjust(@Self);
  with GetPythonEngine do begin
    if PyArg_ParseTuple( args, 'O:SaveToFile',@_pFileName ) <> 0 then
    begin
      DelphiObject.SaveToFile(PyString_AsDelphiString(_pFileName));
      Result := ReturnNone;
    end
    else
      Result := nil;
  end;
end;

function TPyDelphiPicture.Set_Bitmap(AValue: PPyObject;
  AContext: Pointer): Integer;
var
  _object : TObject;
begin
  Adjust(@Self);
  if CheckObjAttribute(AValue, 'Bitmap', TBitmap, _object) then
  begin
    Self.DelphiObject.Bitmap := TBitmap(_object);
    Result := 0;
  end
  else
    Result := -1;
end;

function TPyDelphiPicture.Set_Graphic(AValue: PPyObject;
  AContext: Pointer): Integer;
var
  _object : TObject;
begin
  Adjust(@Self);
  if CheckObjAttribute(AValue, 'Graphic', TGraphic, _object) then
  begin
    Self.DelphiObject.Graphic := TGraphic(_object);
    Result := 0;
  end
  else
    Result := -1;
end;

function TPyDelphiPicture.Set_Icon(AValue: PPyObject;
  AContext: Pointer): Integer;
var
  _object : TObject;
begin
  Adjust(@Self);
  if CheckObjAttribute(AValue, 'Icon', TIcon, _object) then
  begin
    Self.DelphiObject.Icon := TIcon(_object);
    Result := 0;
  end
  else
    Result := -1;
end;

{$IFNDEF FPC}
function TPyDelphiPicture.Set_MetaFile(AValue: PPyObject;
  AContext: Pointer): Integer;
var
  _object : TObject;
begin
  Adjust(@Self);
  if CheckObjAttribute(AValue, 'MetaFile', TMetaFile, _object) then
  begin
    Self.DelphiObject.MetaFile := TMetaFile(_object);
    Result := 0;
  end
  else
    Result := -1;
end;
{$ENDIF FPC}

procedure TPyDelphiPicture.SetDelphiObject(const Value: TPicture);
begin
  inherited DelphiObject := Value;
end;

initialization
  RegisteredUnits.Add(TGraphicsRegistration.Create);
end.
