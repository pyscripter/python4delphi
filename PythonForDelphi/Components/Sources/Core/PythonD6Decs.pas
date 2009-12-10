{The following types are implemented in this unit:
  TPersistent
  TControl
  TForm
  TPicture
  TLabel
  TGraphicControl
  TImage
  TPen
  TBrush
  TCanvas
  TGraphic
  TBitmap
  
  Usage:
  To use this unit, you have to add some initialization code:
    First, create a module to encapsule all these classes (after creating a
    TPythonEngine or placing one on a form)
    
      MyClasses:= TPythonModule.Create(Self);
      MyClasses.Engine:= GetPythonEngine;
      MyClasses.ModuleName:= 'MyClasses';
      MyClasses.Initialize; //You can place a Python Module on a form and simply
      //set its properties to skip these four steps.
      
      PyClassMod:= MyClasses; //Assign this module to PyClassMod
      
    Now add all classes and constants implemented in this unit:

      AddConstants;
      AddClasses;
      
  To create and link a Delphi Class to a Python variable:
  
      MyLabel:= TLabel.Create(Self);//Create your label, if you haven't yet

      MyPyLabel:= PyLabelType.CreateInstance;
      //Create a instance of TPyLabel to use
      //Class names correspond to Delphi names

      ((PythonToDelphi(MyPyLabel) as TPyLabel).Control:= MyLabel;
      //Assign your control to this Python variable instance
      
      PyClassMod.SetVar('MyLabel', MyPyLabel);
      //Now give the Python Variable a name in the module of your choice
      
  Remember to import the Python Classes module when you initialize your program
  and each time after you have added a variable.  You can do this with the line
    GetPythonEngine.ExecString('import * from MyClasses');
}
unit PythonD6Decs;

interface

uses
  SysUtils, Graphics, PythonEngine, Classes, Controls, ExtCtrls,
  StdCtrls, Forms;

type
  {shorter for defines}
  int = integer;
  PPO = PPyObject;
  TPT = TPythonType;

const
  {needed Identity Maps}
  TBrushStyleMap: array[0..7] of TIdentMapEntry =
    ( (Value: Integer(bsSolid); Name: 'bsSolid'),
      (Value: Integer(bsClear); Name: 'bsClear'),
      (Value: Integer(bsHorizontal); Name: 'bsHorizontal'),
      (Value: Integer(bsVertical); Name: 'bsVertical'),
      (Value: Integer(bsFDiagonal); Name: 'bsFDiagonal'),
      (Value: Integer(bsBDiagonal); Name: 'bsBDiagonal'),
      (Value: Integer(bsCross); Name: 'bsCross'),
      (Value: Integer(bsDiagCross); Name: 'bsDiagCross'));

  TPenModeMap: array[0..15] of TIdentMapEntry =
    ( (Value: Integer(pmBlack); Name: 'pmBlack'),
      (Value: Integer(pmWhite); Name: 'pmWhite'),
      (Value: Integer(pmNop); Name: 'pmNop'),
      (Value: Integer(pmNot); Name: 'pmNot'),
      (Value: Integer(pmCopy); Name: 'pmCopy'),
      (Value: Integer(pmNotCopy); Name: 'pmNotCopy'),
      (Value: Integer(pmMergePenNot); Name: 'pmMergePenNot'),
      (Value: Integer(pmMaskPenNot); Name: 'pmMaskPenNot'),
      (Value: Integer(pmMergeNotPen); Name: 'pmMergeNotPen'),
      (Value: Integer(pmMaskNotPen); Name: 'pmMaskNotPen'),
      (Value: Integer(pmMerge); Name: 'pmMerge'),
      (Value: Integer(pmNotMerge); Name: 'pmNotMerge'),
      (Value: Integer(pmMask); Name: 'pmMask'),
      (Value: Integer(pmNotMask); Name: 'pmNotMask'),
      (Value: Integer(pmXor); Name: 'pmXor'),
      (Value: Integer(pmNotXor); Name: 'pmNotXor'));

  TPenStyleMap: array[48..{$IFDEF FPC}53{$ELSE}54{$ENDIF}] of TIdentMapEntry =
    ( (Value: Integer(psSolid); Name: 'psSolid'),
      (Value: Integer(psDash); Name: 'psDash'),
      (Value: Integer(psDot); Name: 'psDot'),
      (Value: Integer(psDashDot); Name: 'psDashDot'),
      (Value: Integer(psDashDotDot);Name: 'psDashDotDot'),
      (Value: Integer(psClear); Name: 'psClear')
{$IFNDEF FPC}
      ,(Value: Integer(psInsideFrame);Name: 'psInsideFrame')
{$ENDIF}
      );

type
  TPyPersistent = class(TPyObject)
  private
    obj: TPersistent;
  public
    class procedure RegisterMethods (PythonType: TPythonType); override;
    class procedure RegisterGetSets (PythonType: TPythonType); override;

    function DoGetObj(c: int): PPO; cdecl;
    function DoSetObj(v: PPO; c: int): int; cdecl;

    function DoAssign(args: PPO): PPO; cdecl;

    property Control: TPersistent read obj write obj;
  end;

  TPyControl = class(TPyPersistent)
  private
    obj: TControl;
    procedure SetControl(value: TControl);
  public
    class procedure RegisterMethods (PythonType: TPythonType); override;
    class procedure RegisterGetSets (PythonType: TPythonType); override;

    function DoBringToFront (args: PPyObject): PPyObject; cdecl;
    function DoGetTop (context: Pointer): PPyObject; cdecl;
    function DoGetLeft (context: Pointer): PPyObject; cdecl;
    function DoGetHeight (context: Pointer): PPyObject; cdecl;
    function DoGetWidth (context: Pointer): PPyObject; cdecl;
    function DoGetRight (context: Pointer): PPyObject; cdecl;
    function DoGetBottom (context: Pointer): PPyObject; cdecl;
    function DoGetVisible (context: Pointer): PPyObject; cdecl;
    function DoSendToBack (args: PPyObject): PPyObject; cdecl;
    function DoSetTop (value: PPyObject; context: Pointer): integer; cdecl;
    function DoSetLeft (value: PPyObject; context: Pointer): integer; cdecl;
    function DoSetHeight (value: PPyObject; context: Pointer): integer; cdecl;
    function DoSetWidth (value: PPyObject; context: Pointer): integer; cdecl;
    function DoSetRight (value: PPyObject; context: Pointer): integer; cdecl;
    function DoSetBottom (value: PPyObject; context: Pointer): integer; cdecl;
    function DoSetVisible (value: PPyObject; context: Pointer): integer; cdecl;
    function DoShow (args: PPyObject): PPyObject; cdecl;
    function DoHide (args: PPyObject): PPyObject; cdecl;
    function DoInvalidate (args: PPyObject): PPyObject; cdecl;

    property Control: TControl read obj write SetControl;
  end;

  TPyForm = class(TPyControl)
  private
    fForm: TForm;
    procedure SetForm(Value: TForm);
  public
    class procedure RegisterGetSets(PythonType: TPythonType); override;

    function DoGetCaption(Context: Pointer): PPyObject; cdecl;
    function DoSetCaption(Value: PPyObject; Context: Pointer): integer; cdecl;

    property Control: TForm read fForm write SetForm;
  end;

  TPyPicture = class(TPyPersistent)
  private
    obj: TPicture;
  public
    class procedure RegisterMethods (PythonType: TPythonType); override;
    class procedure RegisterGetSets (PythonType: TPythonType); override;

    function DoGetHeight(Context: pointer): PPyObject; cdecl;
    function DoGetWidth(Context: pointer): PPyObject; cdecl;
    function DoLoadFromFile(args: PPyObject): PPyObject; cdecl;
    function DoSaveToFile(args: PPyObject): PPyObject; cdecl;

    property Control: TPicture read obj write obj;
  end;

  TPyLabel = class(TPyControl)
  private
    fLabel: TLabel;
    procedure SetLabel(Value: TLabel);
  public
    class procedure RegisterGetSets (PythonType: TPythonType); override;

    function DoGetAlignment(Context: Pointer): PPyObject; cdecl;
    function DoGetCaption(Context: Pointer): PPyObject; cdecl;
    function DoGetLayout(Context: Pointer): PPyObject; cdecl;
    function DoGetTransparent(Context: Pointer): PPyObject; cdecl;
    function DoGetWordWrap(Context: Pointer): PPyObject; cdecl;
    function DoSetAlignment(Value: PPyObject; Context: Pointer): integer; cdecl;
    function DoSetCaption(Value: PPyObject; Context: Pointer): integer; cdecl;
    function DoSetLayout(Value: PPyObject; Context: Pointer): integer; cdecl;
    function DoSetTransparent(Value: PPyObject; Context: Pointer): integer; cdecl;
    function DoSetWordWrap(Value: PPyObject; Context: Pointer): integer; cdecl;

    property Control: TLabel read fLabel write SetLabel;
  end;

  TPyGraphicControl = class(TPyControl)
  private
    fGC: TGraphicControl;
    procedure SetGC(const Value: TGraphicControl);
  public
    class procedure RegisterGetSets(PT: TPT); override;

    function DoGetCanvas(c: int): PPO; cdecl;

    property Control: TGraphicControl read fGC write SetGC;
  end;

  TPyImage = class(TPyGraphicControl)
    fImage: TImage;
    fPicture: PPyObject;
    procedure SetImage(value: TImage);
  public
    class procedure RegisterMethods (PythonType: TPythonType); override;
    class procedure RegisterGetSets (PythonType: TPythonType); override;

    function DoGetPicture(Context: Pointer): PPyObject; cdecl;
    function DoGetProportional (Context: Pointer): PPyObject; cdecl;
    function DoGetCenter (Context: Pointer): PPyObject; cdecl;
    function DoGetStretch (Context: Pointer): PPyObject; cdecl;
    function DoGetTransparent (Context: Pointer): PPyObject; cdecl;

    function DoSetTransparent (Value: PPyObject; Context: Pointer): integer; cdecl;
    function DoSetStretch (Value: PPyObject; Context: Pointer): integer; cdecl;
    function DoSetProportional (Value: PPyObject; Context: Pointer): integer; cdecl;
    function DoSetCenter (Value: PPyObject; Context: Pointer): integer; cdecl;

    function DoLoadFromFile(Args: PPyObject): PPyObject; cdecl;
    function DoSaveToFile(Args: PPyObject): PPyObject; cdecl;

    property Control: TImage read fImage write SetImage;
  end;

  TPyPen = class(TPyPersistent)
  private
    fPen: TPen;
    procedure SetPen(val: TPen);
  public
    class procedure RegisterGetSets(PythonType: TPT); override;

    function DoGetColor(c: int): PPO; cdecl;
    function DoGetHandle(c: int): PPO; cdecl;
    function DoGetMode(c: int): PPO; cdecl;
    function DoGetStyle(c: int): PPO; cdecl;
    function DoGetWidth(c: int): PPO; cdecl;
    function DoSetColor(val: PPO; c: int): int; cdecl;
    function DoSetHandle(val: PPO; c: int): int; cdecl;
    function DoSetMode(val: PPO; c: int): int; cdecl;
    function DoSetStyle(val: PPO; c: int): int; cdecl;
    function DoSetWidth(val: PPO; c: int): int; cdecl;

    property Control: TPen read fPen write SetPen;
  end;

  TPyBrush = class(TPyPersistent)
  private
    fBrush: TBrush;
    procedure SetBrush(value: TBrush);
  public
    class procedure RegisterGetSets(PythonType: TPythonType); override;

    function DoGetBitmap(Context: Pointer): PPyObject; cdecl;
    function DoGetColor(Context: Pointer): PPyObject; cdecl;
    function DoGetHandle(Context: Pointer): PPyObject; cdecl;
    function DoGetStyle(Context: Pointer): PPyObject; cdecl;
    function DoSetBitmap(Value: PPyObject; Context: Pointer): integer; cdecl;
    function DoSetColor(Value: PPyObject; Context: Pointer): integer; cdecl;
    function DoSetHandle(Value: PPyObject; Context: Pointer): integer; cdecl;
    function DoSetStyle(Value: PPyObject; Context: Pointer): integer; cdecl;

    property Control: TBrush read fBrush write SetBrush;
  end;

  TPyCanvas = class(TPyObject)
  private
    fCanvas: TCanvas;
  public
    class procedure RegisterMethods(PythonType: TPythonType); override;
    class procedure RegisterGetSets(PythonType: TPythonType); override;

    function DoGetBrush(Context: Pointer): PPyObject; cdecl;
    function DoGetPen(Context: Pointer): PPyObject; cdecl;

    function DoArc(Args: PPyObject): PPyObject; cdecl;
    function DoChord(Args: PPyObject): PPyObject; cdecl;
    function DoDraw(Args: PPyObject): PPyObject; cdecl;
    function DoEllipse(Args: PPyObject): PPyObject; cdecl;
    function DoFillRect(Args: PPyObject): PPyObject; cdecl;
    function DoFloodFill(Args: PPyObject): PPyObject; cdecl;
    function DoFrameRect(Args: PPyObject): PPyObject; cdecl;
    function DoLineTo(Args: PPyObject): PPyObject; cdecl;
    function DoLine(Args: PPyObject): PPyObject; cdecl;
    function DoMoveTo(Args: PPyObject): PPyObject; cdecl;
    function DoPie(Args: PPyObject): PPyObject; cdecl;
    function DoRect(Args: PPyObject): PPyObject; cdecl;
    function DoRoundRect(Args: PPyObject): PPyObject; cdecl;
    function DoStretchDraw(Args: PPyObject): PPyObject; cdecl;
    function DoTextHeight(Args: PPyObject): PPyObject; cdecl;
    function DoTextOut(Args: PPyObject): PPyObject; cdecl;
    function DoTextWidth(Args: PPyObject): PPyObject; cdecl;

    property Control: TCanvas read fCanvas write fCanvas;
  end;

  TPyGraphic = class(TPyPersistent)
  private
    fGraphic: TGraphic;
    procedure SetGraphic(const Value: TGraphic);
  public
    class procedure RegisterMethods(PT: TPT); override;
    class procedure RegisterGetSets(PT: TPT); override;

    function DoGetEmpty(c: int): PPO; cdecl;
    function DoGetHeight(c: int): PPO; cdecl;
    function DoGetModified(c: int): PPO; cdecl;
    function DoGetTransparent(c: int): PPO; cdecl;
    function DoGetWidth(c: int): PPO; cdecl;

    function DoLoadFromFile(args: PPO): PPO; cdecl;
    function DoSaveToFile(args: PPO): PPO; cdecl;

    property Control: TGraphic read fGraphic write SetGraphic;
  end;

  TPyBitmap = class(TPyGraphic)
  private
    fBmp: Graphics.TBitmap;
    procedure SetBitmap(const Value: Graphics.TBitmap);
  public
    class procedure RegisterGetSets(PT: TPT); override;

    function DoGetCanvas(c: int): PPO; cdecl;

    property Control: Graphics.TBitmap read fBmp write SetBitmap;
  end;

{misc functions}
function BrushStyleToString(BrushStyle: TBrushStyle): string;
function PenModeToString(PenMode: TPenMode): string;
function PenStyleToString(PenStyle: TPenStyle): string;
function StringToBrushStyle(s: string): TBrushStyle;
function StringToPenMode(s: string): TPenMode;
function StringToPenStyle(s: string): TPenStyle;

{Python functions}
function ApplicationTerminate(Self, Args: PPyObject): PPyObject; cdecl;
function VarPyObj(v: Variant): PPO;
function PyObjVar(p: PPyObject): Variant;
function PyObjInt(p: PPyObject): integer;
function PyObjStr(p: PPyObject): string;
procedure PyAddClasses;
procedure PyAddConstants;

var
  PyBitmapType: TPT;
  PyBrushType: TPT;
  PyCanvasType: TPT;
  PyControlType: TPT;
  PyFormType: TPT;
  PyGraphicType: TPT;
  PyGraphicControlType: TPT;
  PyImageType: TPT;
  PyLabelType: TPT;
  PyPenType: TPT;
  PyPersistent: TPT;
  PyPictureType: TPT;
  PyClassMod: TPythonModule;

implementation

{$IFDEF FPC}
uses
  GraphType;
{$ENDIF}

//class needed to bypass Delphi Canvas Class regulation
type
  TGraphicArea = class(TGraphicControl)
  public
    property Canvas;
  end;

{misc functions}

function BrushStyleToString(BrushStyle: TBrushStyle): string;
begin
  IntToIdent(Integer(BrushStyle), result, TBrushStyleMap);
end;

function PenModeToString(PenMode: TPenMode): string;
begin
  IntToIdent(Integer(PenMode), result, TPenModeMap);
end;

function PenStyleToString(PenStyle: TPenStyle): string;
begin
  IntToIdent(Integer(PenStyle), result, TPenStyleMap);
end;

function StringToBrushStyle(s: string): TBrushStyle;
var
  r: integer;
begin
  if(not IdentToInt(s, r, TBrushStyleMap)) then
    raise EConvertError.CreateFmt('"%s" is not a TBrushStyle', [s]);
  result:= TBrushStyle(r);
end;

function StringToPenMode(s: string): TPenMode;
var
  r: integer;
begin
  if(not IdentToInt(s, r, TPenModeMap)) then
    raise EConvertError.CreateFmt('"%s" is not a TPenMode', [s]);
  result:= TPenMode(r);
end;

function StringToPenStyle(s: string): TPenStyle;
var
  r: integer;
begin
  if(not IdentToInt(s, r, TPenStyleMap)) then
    raise EConvertError.CreateFmt('"%s" is not a TPenStyle', [s]);
  result:= TPenStyle(r);
end;

{------------------------------------------------------------------------------}

function PyNewType: TPythonType;
begin
  result:= TPythonType.Create(GetPythonEngine);
  result.Module:= PyClassMod;
  result.Engine:= GetPythonEngine;
  result.Services.Basic:= [bsStr, bsRepr, bsGetAttrO, bsSetAttrO];
  result.TypeFlags:= result.TypeFlags + [tpfBaseType];
end;

function ApplicationTerminate(Self, Args: PPyObject): PPyObject; cdecl;
begin
  result:= GetPythonEngine.ReturnNone;
  Application.Terminate;
end;

function VarPyObj(v: Variant): PPO;
begin with GetPythonEngine do begin
  result:= VariantAsPyObject(v);
end; end;

function PyObjVar(p: PPyObject): Variant;
begin with GetPythonEngine do begin
  result:= PyObjectAsVariant(p);
end; end;

function PyObjInt(p: PPyObject): integer;
begin with GetPythonEngine do begin
  result:= Integer(PyObjectAsVariant(p));
end; end;

function PyObjStr(p: PPyObject): string;
begin
  result:= GetPythonEngine.PyObjectAsString(p);
end;

procedure PyAddClasses;
  function NewPyType(name: string; oc: TPyObjectClass): TPythonType;
  begin
    result:= PyNewType;
    result.TypeName:= name;
    result.PyObjectClass:= oc;
    result.Initialize;
  end;
begin
  PyPersistent:= NewPyType('TPersistent', TPyPersistent);
    PyPictureType:= NewPyType('TPicture', TPyPicture);
    PyBrushType:= NewPyType('TBrush', TPyBrush);
    PyPenType:= NewPyType('TPen', TPyPen);
    PyCanvasType:= NewPyType('TCanvas', TPyCanvas);
    PyGraphicType:= NewPyType('TGraphic', TPyGraphic);
      PyBitmapType:= NewPyType('TBitmap', TPyBitmap);
    PyControlType:= NewPyType('TControl', TPyControl);
      PyLabelType:= NewPyType('TLabel', TPyLabel);
      PyFormType:= NewPyType('TForm', TPyForm);
    PyGraphicControlType:= NewPyType('TGraphicControl', TPyGraphicControl);
      PyImageType:= NewPyType('TImage', TPyImage);

  PyClassMod.AddMethod('Terminate', ApplicationTerminate, 'Terminate the application');
end;

procedure PyAddConstants;
begin with GetPythonEngine, PyClassMod do begin
  SetVar('taLeftJustify', VariantAsPyObject(Integer(taLeftJustify)));
  SetVar('taRightJustify', VariantAsPyObject(Integer(taRightJustify)));
  SetVar('taCenter', VariantAsPyObject(Integer(taCenter)));
  SetVar('tlTop', VariantAsPyObject(Integer(tlTop)));
  SetVar('tlBottom', VariantAsPyObject(Integer(tlBottom)));
  SetVar('tlCenter', VariantAsPyObject(Integer(tlCenter)));
end; end;

{ TPyControl }

function TPyControl.DoBringToFront(args: PPyObject): PPyObject;
begin Adjust(@Self);
  result:= GetPythonEngine.ReturnNone;
  if(obj=nil) then exit;
  obj.BringToFront;
end;

function TPyControl.DoGetBottom(context: Pointer): PPyObject;
begin with GetPythonEngine do begin Adjust(@Self);
  if(obj=nil) then
    result:= ReturnNone
  else
    result:= VariantAsPyObject(obj.Top + obj.Height);
end; end;

function TPyControl.DoGetHeight(context: Pointer): PPyObject;
begin with GetPythonEngine do begin Adjust(@Self);
  if(obj=nil) then
    result:= ReturnNone
  else
    result:= VariantAsPyObject(obj.Height);
end; end;

function TPyControl.DoGetLeft(context: Pointer): PPyObject;
begin with GetPythonEngine do begin Adjust(@Self);
  if(obj=nil) then
    result:= ReturnNone
  else
    result:= VariantAsPyObject(obj.Left);
end; end;

function TPyControl.DoGetRight(context: Pointer): PPyObject;
begin with GetPythonEngine do begin Adjust(@Self);
  if(obj=nil) then
    result:= ReturnNone
  else
    result:= VariantAsPyObject(obj.Left + obj.Width);
end; end;

function TPyControl.DoGetTop(context: Pointer): PPyObject;
begin with GetPythonEngine do begin Adjust(@self);
  if(obj=nil) then begin
    result:= ReturnNone;
    exit;
  end;
  result:= VariantAsPyObject(obj.Top);
end; end;

function TPyControl.DoGetVisible(context: Pointer): PPyObject;
begin with GetPythonEngine do begin Adjust(@self);
  if(obj=nil) then
    result:= ReturnNone
  else
    result:= VariantAsPyObject(obj.Visible);
end; end;

function TPyControl.DoGetWidth(context: Pointer): PPyObject;
begin with GetPythonEngine do begin Adjust(@Self);
  if(obj=nil) then
    result:= ReturnNone
  else
    result:= VariantAsPyObject(obj.Width);
end; end;

function TPyControl.DoHide(args: PPyObject): PPyObject;
begin Adjust(@Self);
  result:= GetPythonEngine.ReturnNone;
  if(obj=nil) then exit;
  obj.Hide;
end;

function TPyControl.DoInvalidate(args: PPyObject): PPyObject;
begin Adjust(@Self);
  result:= GetPythonEngine.ReturnNone;
  if(obj=nil) then exit;
  obj.Invalidate;
end;

function TPyControl.DoSendToBack(args: PPyObject): PPyObject;
begin Adjust(@Self);
  result:= GetPythonEngine.ReturnNone;
  if(obj=nil) then exit;
  obj.SendToBack;
end;

function TPyControl.DoSetBottom(value: PPyObject;
  context: Pointer): integer;
begin with GetPythonEngine do begin Adjust(@Self);
  result:= 1;
  if(obj=nil) then exit;
  result:= 0;
  obj.Top:= PyObjectAsVariant(value) - obj.Height; 
end; end;

function TPyControl.DoSetHeight(value: PPyObject;
  context: Pointer): integer;
begin with GetPythonEngine do begin Adjust(@Self);
  result:= 1;
  if(obj=nil) then exit;
  result:= 0;
  obj.Height:= PyObjectAsVariant(value);
end; end;

function TPyControl.DoSetLeft(value: PPyObject; context: Pointer): integer;
begin with GetPythonEngine do begin Adjust(@Self);
  result:= 1;
  if(obj=nil) then exit;
  result:= 0;
  obj.Left:= PyObjectAsVariant(value);
end; end;

function TPyControl.DoSetRight(value: PPyObject;
  context: Pointer): integer;
begin with GetPythonEngine do begin Adjust(@Self);
  result:= 1;
  if(obj=nil) then exit;
  result:= 0;
  obj.Left:= PyObjectAsVariant(value) - obj.Width;
end; end;

function TPyControl.DoSetTop(value: PPyObject;
  context: Pointer): integer;
begin with GetPythonEngine do begin Adjust(@Self);
  result:= 1;
  if(obj=nil) then exit;
  result:= 0;
  obj.Top:= PyObjectAsVariant(value);
end; end;

function TPyControl.DoSetVisible(value: PPyObject;
  context: Pointer): integer;
begin with GetPythonEngine do begin Adjust(@Self);
  result:= 1;
  if(obj=nil) then exit;
  result:= 0;
  obj.Visible:= PyObjectAsVariant(value);
end; end;

function TPyControl.DoSetWidth(value: PPyObject;
  context: Pointer): integer;
begin with GetPythonEngine do begin Adjust(@Self);
  result:= 1;
  if(obj=nil) then exit;
  result:= 0;
  obj.Width:= PyObjectAsVariant(value);
end; end;

function TPyControl.DoShow(args: PPyObject): PPyObject;
begin Adjust(@Self);
  result:= GetPythonEngine.ReturnNone;
  if(obj=nil) then exit;
  obj.Show;
end;

class procedure TPyControl.RegisterGetSets(PythonType: TPythonType);
begin
  inherited;
  with PythonType do begin
    AddGetSet('Top', @TPyControl.DoGetTop, @TPyControl.DoSetTop, 'Top of the object', nil);
    AddGetSet('Left', @TPyControl.DoGetLeft, @TPyControl.DoSetLeft, 'Left of the object', nil);
    AddGetSet('Height', @TPyControl.DoGetHeight, @TPyControl.DoSetHeight, 'Height of the object', nil);
    AddGetSet('Width', @TPyControl.DoGetWidth, @TPyControl.DoSetWidth, 'Width of the object', nil);
    AddGetSet('Right', @TPyControl.DoGetRight, @TPyControl.DoSetRight, 'Right of the object', nil);
    AddGetSet('Bottom', @TPyControl.DoGetBottom, @TPyControl.DoSetBottom, 'Bottom of the object', nil);
    AddGetSet('Visible', @TPyControl.DoGetVisible, @TPyControl.DoSetVisible, 'Is the object visible?', nil);
  end;
end;

class procedure TPyControl.RegisterMethods(PythonType: TPythonType);
begin
  inherited;
  with PythonType do begin
    AddMethod('BringToFront', @TPyControl.DoBringToFront, 'Bring the object to the front');
    AddMethod('SendToBack', @TPyControl.DoSendToBack, 'Send the object to the back');
    AddMethod('Show', @TPyControl.DoShow, 'Show the object');
    AddMethod('Hide', @TPyControl.DoHide, 'Hide the object');
    AddMethod('Invalidate', @TPyControl.DoInvalidate, 'Repaint the object');
  end;
end;

procedure TPyControl.SetControl(value: TControl);
begin
  inherited Control:= value;
  obj:= value;
end;

{ TPyImage }

function TPyImage.DoGetCenter(Context: Pointer): PPyObject;
begin with GetPythonEngine do begin Adjust(@Self);
  if(fImage=nil) then
    result:= ReturnNone
  else
    result:= VariantAsPyObject(fImage.Center); 
end; end;

function TPyImage.DoGetPicture(Context: Pointer): PPyObject;
begin with GetPythonEngine do begin Adjust(@Self);
  if(fImage<>nil) then
    TPyPicture(PythonToDelphi(fPicture)).obj:= fImage.Picture
  else
    TPyPicture(PythonToDelphi(fPicture)).obj:= nil;

  result:= fPicture;
end; end;

function TPyImage.DoGetProportional(Context: Pointer): PPyObject;
begin with GetPythonEngine do begin Adjust(@Self);
  if(fImage=nil) then
    result:= ReturnNone
  else
    result:= VariantAsPyObject(fImage.Proportional);
end; end;

function TPyImage.DoGetStretch(Context: Pointer): PPyObject;
begin with GetPythonEngine do begin Adjust(@Self);
  if(fImage=nil) then
    result:= ReturnNone
  else
    result:= VariantAsPyObject(fImage.Stretch);
end; end;

function TPyImage.DoGetTransparent(Context: Pointer): PPyObject;
begin with GetPythonEngine do begin Adjust(@Self);
  if(fImage=nil) then
    result:= ReturnNone
  else
    result:= VariantAsPyObject(fImage.Transparent);
end; end;

function TPyImage.DoLoadFromFile(Args: PPyObject): PPyObject;
var
  s: PAnsiChar;
begin with GetPythonEngine do begin Adjust(@Self);
  if(PyArg_ParseTuple(args, 's:LoadFromFile', [@s])<>0) then begin
    result:= ReturnNone;
    if(fImage=nil)or(fImage.Picture=nil) then exit;
    fImage.Picture.LoadFromFile(s);
  end else
    result:= nil;
end; end;

function TPyImage.DoSaveToFile(Args: PPyObject): PPyObject;
var
  s: PAnsiChar;
begin with GetPythonEngine do begin Adjust(@Self);
  if(PyArg_ParseTuple(args, 's:SaveToFile', [@s])<>0) then begin
    result:= ReturnNone;
    if(fImage=nil)or(fImage.Picture=nil) then exit;
    fImage.Picture.SaveToFile(s);
  end else
    result:= nil;
end; end;

function TPyImage.DoSetCenter(Value: PPyObject; Context: Pointer): integer;
begin with GetPythonEngine do begin Adjust(@Self);
  result:= 1;
  if(fImage=nil) then exit;
  result:= 0;
  fImage.Center:= PyObjectAsVariant(Value);
end; end;

function TPyImage.DoSetProportional(Value: PPyObject;
  Context: Pointer): integer;
begin with GetPythonEngine do begin Adjust(@Self);
  result:= 1;
  if(fImage=nil) then exit;
  result:= 0;
  fImage.Proportional:= PyObjectAsVariant(Value);
end; end;

function TPyImage.DoSetStretch(Value: PPyObject;
  Context: Pointer): integer;
begin with GetPythonEngine do begin Adjust(@Self);
  result:= 1;
  if(fImage=nil) then exit;
  result:= 0;
  fImage.Stretch:= PyObjectAsVariant(Value);
end; end;

function TPyImage.DoSetTransparent(Value: PPyObject;
  Context: Pointer): integer;
begin with GetPythonEngine do begin Adjust(@Self);
  result:= 1;
  if(fImage=nil) then exit;
  result:= 0;
  fImage.Transparent:= PyObjectAsVariant(Value);
end; end;

class procedure TPyImage.RegisterGetSets(PythonType: TPythonType);
begin
  with PythonType do begin
    AddGetSet('Transparent', @TPyImage.DoGetTransparent, @TPyImage.DoSetTransparent, 'Is the image transparent?', nil);
    AddGetSet('Stretch', @TPyImage.DoGetStretch, @TPyImage.DoSetStretch, 'Should the image be stretched?', nil);
    AddGetSet('Proportional', @TPyImage.DoGetProportional, @TPyImage.DoSetProportional, 'Should be image be rendered proportionally?', nil);
    AddGetSet('Center', @TPyImage.DoGetCenter, @TPyImage.DoSetCenter, 'Should the image be centered?', nil);
    AddGetSet('Picture', @TPyImage.DoGetPicture, nil, 'TImage Picture property : TPicture', nil);
  end;
  inherited;
end;

class procedure TPyImage.RegisterMethods(PythonType: TPythonType);
begin
  with PythonType do begin
    AddMethod('LoadFromFile', @TPyImage.DoLoadFromFile, 'Load a picture from a file');
    AddMethod('SaveToFile', @TPyImage.DoSaveToFile, 'Save the picture to a file');
  end;
  inherited;
end;

procedure TPyImage.SetImage(value: TImage);
begin
  inherited Control:= value;
  fImage:= Value;
  if(fImage<>nil) then begin
    fPicture:= PyPictureType.CreateInstance;
    TPyPicture(PythonToDelphi(fPicture)).obj:= fImage.Picture;
    GetPythonEngine.Py_INCREF(fPicture);
  end;
end;

{ TPyPicture }

function TPyPicture.DoGetHeight(Context: pointer): PPyObject;
begin with GetPythonEngine do begin Adjust(@Self);
  if(obj=nil) then begin
    result:= ReturnNone;
    exit;
  end;
  result:= VariantAsPyObject(obj.Height);
end; end;

function TPyPicture.DoGetWidth(Context: pointer): PPyObject;
begin with GetPythonEngine do begin Adjust(@Self);
  if(obj=nil) then begin
    result:= ReturnNone;
    exit;
  end;
  result:= VariantAsPyObject(obj.Width);
end; end;

function TPyPicture.DoLoadFromFile(args: PPyObject): PPyObject;
var
  s: PAnsiChar;
begin with GetPythonEngine do begin Adjust(@Self);
  if(PyArg_ParseTuple(args, 's:LoadFromFile', [@s])<>0) then begin
    result:= ReturnNone;
    if(obj=nil) then exit;
    obj.LoadFromFile(s);
  end else
    result:= nil;
end; end;

function TPyPicture.DoSaveToFile(args: PPyObject): PPyObject;
var
  s: PAnsiChar;
begin with GetPythonEngine do begin Adjust(@Self);
  if(PyArg_ParseTuple(args, 's:SaveToFile', [@s])<>0) then begin
    result:= ReturnNone;
    if(obj=nil) then exit;
    obj.SaveToFile(s);
  end else
    result:= nil;
end; end;

class procedure TPyPicture.RegisterGetSets(PythonType: TPythonType);
begin
  inherited;
  with PythonType do begin
    AddGetSet('Height', @TPyPicture.DoGetHeight, nil, 'Height of the picture', nil);
    AddGetSet('Width', @TPyPicture.DoGetWidth, nil, 'Width of the picture', nil);
  end;
end;

class procedure TPyPicture.RegisterMethods(PythonType: TPythonType);
begin
  inherited;
  with PythonType do begin
    AddMethod('LoadFromFile', @TPyPicture.DoLoadFromFile, 'Load a picture from a file');
    AddMethod('SaveToFile', @TPyPicture.DoSaveToFile, 'Save the picture to a file');
  end;
end;

{ TPyLabel }

function TPyLabel.DoGetAlignment(Context: Pointer): PPyObject;
begin with GetPythonEngine do begin Adjust(@Self);
  if(fLabel=nil) then begin
    result:= ReturnNone;
    exit;
  end;
  result:= VariantAsPyObject(Integer(fLabel.Alignment));
end; end;

function TPyLabel.DoGetCaption(Context: Pointer): PPyObject;
begin with GetPythonEngine do begin Adjust(@Self);
  if(fLabel=nil) then begin
    result:= ReturnNone;
    exit;
  end;
  result:= VariantAsPyObject(fLabel.Caption);
end; end;

function TPyLabel.DoGetLayout(Context: Pointer): PPyObject;
begin with GetPythonEngine do begin Adjust(@Self);
  if(fLabel=nil) then begin
    result:= ReturnNone;
    exit;
  end;
  result:= VariantAsPyObject(Integer(fLabel.Layout));
end; end;

function TPyLabel.DoGetTransparent(Context: Pointer): PPyObject;
begin with GetPythonEngine do begin Adjust(@Self);
  if(fLabel=nil) then begin
    result:= ReturnNone;
    exit;
  end;
  result:= VariantAsPyObject(fLabel.Transparent);
end; end;

function TPyLabel.DoGetWordWrap(Context: Pointer): PPyObject;
begin with GetPythonEngine do begin Adjust(@Self);
  if(fLabel=nil) then begin
    result:= ReturnNone;
    exit;
  end;
  result:= VariantAsPyObject(fLabel.WordWrap);
end; end;

function TPyLabel.DoSetAlignment(Value: PPyObject;
  Context: Pointer): integer;
begin with GetPythonEngine do begin Adjust(@Self);
  result:= 1;
  if(fLabel=nil) then exit;
  result:= 0;
  fLabel.Alignment:= TAlignment(Integer(PyObjectAsVariant(Value)));
end; end;

function TPyLabel.DoSetCaption(Value: PPyObject;
  Context: Pointer): integer;
begin with GetPythonEngine do begin Adjust(@Self);
  result:= 1;
  if(fLabel=nil) then exit;
  result:= 0;
  fLabel.Caption:= PyObjectAsVariant(Value);
end; end;

function TPyLabel.DoSetLayout(Value: PPyObject; Context: Pointer): integer;
begin with GetPythonEngine do begin Adjust(@Self);
  result:= 1;
  if(fLabel=nil) then exit;
  result:= 0;
  fLabel.Layout:= TTextLayout(Integer(PyObjectAsVariant(Value)));
end; end;

function TPyLabel.DoSetTransparent(Value: PPyObject;
  Context: Pointer): integer;
begin with GetPythonEngine do begin Adjust(@Self);
  result:= 1;
  if(fLabel=nil) then exit;
  result:= 0;
  fLabel.Transparent:= PyObjectAsVariant(Value);
end; end;

function TPyLabel.DoSetWordWrap(Value: PPyObject;
  Context: Pointer): integer;
begin with GetPythonEngine do begin Adjust(@Self);
  result:= 1;
  if(fLabel=nil) then exit;
  result:= 0;
  fLabel.WordWrap:= PyObjectAsVariant(Value);
end; end;

class procedure TPyLabel.RegisterGetSets(PythonType: TPythonType);
begin
  with PythonType do begin
    AddGetSet('Transparent', @TPyLabel.DoGetTransparent, @TPyLabel.DoSetTransparent, 'Label transparency', nil);
    AddGetSet('Alignment', @TPyLabel.DoGetAlignment, @TPyLabel.DoSetAlignment, 'Alignment = taLeftJustify or taRightJustify or taCenter', nil);
    AddGetSet('WordWrap', @TPyLabel.DoGetWordWrap, @TPyLabel.DoSetWordWrap, 'WordWrap = (boolean)', nil);
    AddGetSet('Layout', @TPyLabel.DoGetLayout, @TPyLabel.DoSetLayout, 'Layout = tlTop or tlCenter or tlBottom', nil);
    AddGetSet('Caption', @TPyLabel.DoGetCaption, @TPyLabel.DoSetCaption, 'Caption = string', nil);
  end;
  inherited;
end;

procedure TPyLabel.SetLabel(Value: TLabel);
begin
  inherited Control:= Value;
  fLabel:= Value;
end;

{ TPyForm }

function TPyForm.DoGetCaption(Context: Pointer): PPyObject;
begin with GetPythonEngine do begin Adjust(@Self);
  if(fForm=nil) then begin
    result:= ReturnNone;
    exit;
  end;
  result:= VariantAsPyObject(fForm.Caption);
end; end;

function TPyForm.DoSetCaption(Value: PPyObject; Context: Pointer): integer;
begin with GetPythonEngine do begin Adjust(@Self);
  result:= 1;
  if(fForm=nil) then exit;
  result:= 0;
  fForm.Caption:= PyObjectAsVariant(Value);
end; end;

class procedure TPyForm.RegisterGetSets(PythonType: TPythonType);
begin
  with PythonType do begin
    AddGetSet('Caption', @TPyForm.DoGetCaption, @TPyForm.DoSetCaption, 'Form''s Caption', nil);
  end;
  inherited;
end;

procedure TPyForm.SetForm(Value: TForm);
begin
  fForm:= Value;
  inherited Control:= Value;
end;

{ TPyCanvas }

function TPyCanvas.DoArc(Args: PPyObject): PPyObject;
var
  x1,y1,x2,y2,x3,y3,x4,y4: integer;
begin with GetPythonEngine do begin Adjust(@Self);
  if(PyArg_ParseTuple(args, 'iiiiiiii:Arc',
    [@x1,@y1,@x2,@y2,@x3,@y3,@x4,@y4])<>0)
  then begin
    result:= ReturnNone;
    if(fCanvas<>nil) then
      fCanvas.Arc(x1, y1, x2, y2, x3, y3, x4, y4);
  end else
    result:= nil;
end; end;

function TPyCanvas.DoChord(Args: PPyObject): PPyObject;
var
  x1, y1, x2, y2, x3, y3, x4, y4: integer;
begin with GetPythonEngine do begin Adjust(@Self);
  if(PyArg_ParseTuple(args, 'iiiiiiii:Chord',
    [@x1, @y1, @x2, @y2, @x3, @y3, @x4, @y4]) <> 0)
  then begin
    result:= ReturnNone;
    if(fCanvas<>nil) then
      fCanvas.Chord(x1, y1, x2, y2, x3, y3, x4, y4);
  end else
    result:= nil;
end; end;

function TPyCanvas.DoDraw(Args: PPyObject): PPyObject;
var
  x,y,graph: integer;
begin with GetPythonEngine do begin Adjust(@Self);
  if(PyArg_ParseTuple(args, 'iii:Draw', [@x, @y, @graph])<>0) then begin
    result:= ReturnNone;
    if(fCanvas<>nil) then
      fCanvas.Draw(x,y, TGraphic(graph));
  end else
    result:= nil;
end; end;

function TPyCanvas.DoEllipse(Args: PPyObject): PPyObject;
var
  x1, y1, x2, y2: integer;
begin with GetPythonEngine do begin Adjust(@Self);
  if(PyArg_ParseTuple(args, 'iiii:Ellipse',
    [@x1, @y1, @x2, @y2])<>0)
  then begin
    result:= ReturnNone;
    if(fCanvas<>nil) then
      fCanvas.Ellipse(x1, y1, x2, y2);
  end else
    result:= nil;
end; end;

function TPyCanvas.DoFillRect(Args: PPyObject): PPyObject;
var
  x1, y1, x2, y2: integer;
begin with GetPythonEngine do begin Adjust(@Self);
  if(PyArg_ParseTuple(args, 'iiii:FillRect',
    [@x1, @y1, @x2, @y2])<>0)
  then begin
    result:= ReturnNone;
    if(fCanvas<>nil) then
      fCanvas.FillRect({$IFNDEF FPC}Rect({$ENDIF}x1, y1, x2, y2{$IFNDEF FPC}){$ENDIF});
  end else
    result:= nil;
end; end;

function TPyCanvas.DoFloodFill(Args: PPyObject): PPyObject;
var
  x,y: integer;
  col: PAnsiChar;
begin with GetPythonEngine do begin Adjust(@Self);
  if(PyArg_ParseTuple(args, 'iis:FloodFill', [@x, @y, @col])<>0) then begin
    result:= ReturnNone;
    if(fCanvas<>nil) then
      fCanvas.FloodFill(x,y,StringToColor(col), fsSurface);
  end else
    result:= nil;
end; end;

function TPyCanvas.DoFrameRect(Args: PPyObject): PPyObject;
var
  x1, y1, x2, y2: integer;
begin with GetPythonEngine do begin Adjust(@Self);
  if(PyArg_ParseTuple(args, 'iiii:FrameRect', [@x1, @y1, @x2, @y2])<>0) then begin
    result:= ReturnNone;
    if(fCanvas<>nil) then
      fCanvas.FrameRect(
      {$IFNDEF FPC}
        Rect(x1, y1, x2, y2)
      {$ELSE}
        x1, y1, x2, y1
      {$ENDIF});
  end else
    result:= nil;
end; end;

function TPyCanvas.DoGetBrush(Context: Pointer): PPyObject;
begin with GetPythonEngine do begin Adjust(@Self);
  if(fCanvas=nil) or (fCanvas.Brush=nil) then begin
    result:= ReturnNone;
    exit;
  end;
  result:= PyBrushType.CreateInstance;
  (PythonToDelphi(result) as TPyBrush).Control:= fCanvas.Brush;
end; end;

function TPyCanvas.DoGetPen(Context: Pointer): PPyObject;
begin with GetPythonEngine do begin Adjust(@Self);
  if(fCanvas=nil) then begin
    result:= ReturnNone;
    exit;
  end;
  result:= PyPenType.CreateInstance;
  TPyPen(PythonToDelphi(result)).fPen:= fCanvas.Pen;
end; end;

function TPyCanvas.DoLine(Args: PPyObject): PPyObject;
var
  x1, y1, x2, y2: integer;
begin with GetPythonEngine do begin Adjust(@Self);
  if(PyArg_ParseTuple(args, 'iiii:Line', [@x1, @y1, @x2, @y2])<>0) then begin
    result:= ReturnNone;
    if(fCanvas<>nil) then begin
      fCanvas.MoveTo(x1, y1);
      fCanvas.LineTo(x2, y2);
    end;
  end else
    result:= nil;
end; end;

function TPyCanvas.DoLineTo(Args: PPyObject): PPyObject;
var
  x,y: integer;
begin with GetPythonEngine do begin Adjust(@Self);
  if(PyArg_ParseTuple(args, 'ii:LineTo', [@x, @y])<>0) then begin
    result:= ReturnNone;
    if(fCanvas<>nil) then
      fCanvas.LineTo(x, y);
  end else
    result:= nil;
end; end;

function TPyCanvas.DoMoveTo(Args: PPyObject): PPyObject;
var
  x,y: integer;
begin with GetPythonEngine do begin Adjust(@Self);
  if(PyArg_ParseTuple(args, 'ii:MoveTo', [@x, @y])<>0) then begin
    result:= ReturnNone;
    if(fCanvas<>nil) then
      fCanvas.MoveTo(x, y);
  end else
    result:= nil;
end; end;

function TPyCanvas.DoPie(Args: PPyObject): PPyObject;
var
  x1, y1, x2, y2, x3, y3, x4, y4: integer;
begin with GetPythonEngine do begin Adjust(@Self);
  if(PyArg_ParseTuple(args, 'iiiiiiii:Pie',
    [@x1, @y1, @x2, @y2, @x3, @y3, @x4, @y4])<>0)
  then begin
    result:= ReturnNone;
    if(fCanvas<>nil) then
      fCanvas.Pie(x1, y1, x2, y2, x3, y3, x4, y4);
  end else
    result:= nil;
end; end;

function TPyCanvas.DoRect(Args: PPyObject): PPyObject;
var
  x1, y1, x2, y2: integer;
begin with GetPythonEngine do begin Adjust(@Self);
  if(PyArg_ParseTuple(args, 'iiii:Rectangle',
    [@x1, @y1, @x2, @y2])<>0)
  then begin
    result:= ReturnNone;
    if(fCanvas<>nil) then
      fCanvas.Rectangle(x1, y1, x2, y2);
  end else
    result:= nil;
end; end;

function TPyCanvas.DoRoundRect(Args: PPyObject): PPyObject;
var
  x1, y1, x2, y2, x3, y3: integer;
begin with GetPythonEngine do begin Adjust(@Self);
  if(PyArg_ParseTuple(args, 'iiiiii:RoundRect',
    [@x1, @y1, @x2, @y2, @x3, @y3])<>0)
  then begin
    result:= ReturnNone;
    if(fCanvas<>nil) then
      fCanvas.RoundRect(x1, y1, x2, y2, x3, y3);
  end else
    result:= nil;
end; end;

function TPyCanvas.DoStretchDraw(Args: PPyObject): PPyObject;
var
  x1, y1, x2, y2: integer;
  graph: TGraphic;
begin with GetPythonEngine do begin Adjust(@Self);
  if(PyArg_ParseTuple(args, 'iiiii:StretchDraw',
    [@x1, @y1, @x2, @y2, @graph])<>0)
  then begin
    result:= ReturnNone;
    if(fCanvas<>nil) then
      fCanvas.StretchDraw(Classes.Rect(x1, y1, x2, y2), graph);
  end else
    result:= nil;
end; end;

function TPyCanvas.DoTextHeight(Args: PPyObject): PPyObject;
var
  s: PAnsiChar;
begin with GetPythonEngine do begin Adjust(@Self);
  if(PyArg_ParseTuple(args, 's:TextHeight', [@s])<>0) then begin
    result:= ReturnNone;
    if(fCanvas<>nil) then
      result:= VariantAsPyObject(fCanvas.TextHeight(s));
  end else
    result:= nil;
end; end;

function TPyCanvas.DoTextOut(Args: PPyObject): PPyObject;
var
  x,y: integer;
  s: PAnsiChar;
begin with GetPythonEngine do begin Adjust(@Self);
  if(PyArg_ParseTuple(args, 'iis:TextOut', [@x, @y, @s])<>0) then begin
    result:= ReturnNone;
    if(fCanvas<>nil) then
      fCanvas.TextOut(x, y, s);
  end else
    result:= nil;
end; end;

function TPyCanvas.DoTextWidth(Args: PPyObject): PPyObject;
var
  s: PAnsiChar;
begin with GetPythonEngine do begin Adjust(@Self);
  if(PyArg_ParseTuple(args, 's:TextWidth', [@s])<>0) then begin
    result:= ReturnNone;
    if(fCanvas<>nil) then
      result:= VariantAsPyObject(fCanvas.TextWidth(s));
  end else
    result:= nil;
end; end;

class procedure TPyCanvas.RegisterGetSets(PythonType: TPythonType);
begin
  with PythonType do begin
    AddGetSet('Brush', @TPyCanvas.DoGetBrush, nil, 'TBrush assosciated with the TCanvas', nil);
    AddGetSet('Pen', @TPyCanvas.DoGetPen, nil, 'TPen assosciated with the TCanvas', nil);
  end;
  inherited;
end;

class procedure TPyCanvas.RegisterMethods(PythonType: TPythonType);
begin
  with PythonType do begin
    AddMethod('Arc', @TPyCanvas.DoArc, 'Arc(X1, Y1, X2, Y2, X3, Y3, X4, Y4: integer)');
    AddMethod('Chord', @TPyCanvas.DoChord, 'Chord(X1, Y1, X2, Y2, X3, Y3, X4, Y4: integer)');
    AddMethod('Draw', @TPyCanvas.DoDraw, 'Draw(X, Y: integer; Graphic: integer) -> Graphic is the Delphi object ID for the graphic');
    AddMethod('Ellipse', @TPyCanvas.DoEllipse, 'Ellipse(X1, Y1, X2, Y2: integer)');
    AddMethod('FillRect', @TPyCanvas.DoFillRect, 'FillRect(X1, Y1, X2, Y2: integer)');
    AddMethod('FloodFill', @TPyCanvas.DoFloodFill, 'FloodFill(X1, Y1: integer; color: string');
    AddMethod('FrameRect', @TPyCanvas.DoFrameRect, 'FrameRect(X1, Y1, X2, Y2: integer)');
    AddMethod('LineTo', @TPyCanvas.DoLineTo, 'LineTo(X, Y: integer)');
    AddMethod('Line', @TPyCanvas.DoLine, 'Line(X1, Y1, X2, Y2: integer)');
    AddMethod('MoveTo', @TPyCanvas.DoMoveTo, 'MoveTo(X, Y: integer)');
    AddMethod('Pie', @TPyCanvas.DoPie, 'Pie(X1, Y1, X2, Y2, X3, Y3, X4, Y4: integer)');
    AddMethod('Rectangle', @TPyCanvas.DoRect, 'Rectangle(X1, Y1, X2, Y2: integer)');
    AddMethod('Rect', @TPyCanvas.DoRect, 'Rect(X1, Y1, X2, Y2: integer)');
    AddMethod('RoundRect', @TPyCanvas.DoRoundRect, 'RoundRect(X1, Y1, X2, Y2, X3, Y3: integer)');
    AddMethod('StretchDraw', @TPyCanvas.DoStretchDraw, 'StretchDraw(X1, Y1, X2, Y2: integer; Graphic: integer) -> Graphic is the Delphi object ID for the graphic');
    AddMethod('TextHeight', @TPyCanvas.DoTextHeight, 'TextHeight(s: string): integer');
    AddMethod('TextOut', @TPyCanvas.DoTextOut, 'TextOut(X, Y: integer; Text: string)');
    AddMethod('TextWidth', @TPyCanvas.DoTextWidth, 'TextWidth(Text: string): integer');
  end;
  inherited;
end;

{ TPyBrush }

function TPyBrush.DoGetBitmap(Context: Pointer): PPyObject;
begin with GetPythonEngine do begin Adjust(@Self);
  if(fBrush=nil) or (fBrush.Bitmap=nil) then begin
    result:= ReturnNone;
    exit;
  end;
  result:= VariantAsPyObject(Integer(fBrush.Bitmap));
end; end;

function TPyBrush.DoGetColor(Context: Pointer): PPyObject;
begin with GetPythonEngine do begin Adjust(@Self);
  if(fBrush=nil) then begin
    result:= ReturnNone;
    exit;
  end;
  result:= VariantAsPyObject(ColorToString(fBrush.Color));
end; end;

function TPyBrush.DoGetHandle(Context: Pointer): PPyObject;
begin with GetPythonEngine do begin Adjust(@Self);
  if(fBrush=nil) then begin
    result:= ReturnNone;
    exit;
  end;
  result:= VariantAsPyObject(Integer(fBrush.Handle));
end; end;

function TPyBrush.DoGetStyle(Context: Pointer): PPyObject;
var
  s: string;
begin with GetPythonEngine do begin Adjust(@Self);
  if(fBrush=nil) then begin
    result:= ReturnNone;
    exit;
  end;
  IntToIdent(Integer(fBrush.Style), s, TBrushStyleMap);
  result:= VariantAsPyObject(s);
end; end;

function TPyBrush.DoSetBitmap(Value: PPyObject; Context: Pointer): integer;
begin with GetPythonEngine do begin Adjust(@Self);
  result:= 1;
  if(fBrush=nil) then exit;
  result:= 0;
  fBrush.Bitmap:= Graphics.TBitmap(Integer(PyObjectAsVariant(Value)));
end; end;

function TPyBrush.DoSetColor(Value: PPyObject; Context: Pointer): integer;
begin with GetPythonEngine do begin Adjust(@Self);
  result:= 1;
  if(fBrush=nil) then exit;
  result:= 0;
  fBrush.Color:= StringToColor(PyObjectAsVariant(Value));
end; end;

function TPyBrush.DoSetHandle(Value: PPyObject; Context: Pointer): integer;
begin with GetPythonEngine do begin Adjust(@Self);
  result:= 1;
  if(fBrush=nil) then exit;
  result:= 0;
  fBrush.Handle:= PyObjectAsVariant(Value);
end; end;

function TPyBrush.DoSetStyle(Value: PPyObject; Context: Pointer): integer;
begin with GetPythonEngine do begin Adjust(@Self);
  result:= 1;
  if(fBrush=nil) then exit;
  IdentToInt(PyObjectAsVariant(Value), result, TBrushStyleMap);
  fBrush.Style:= TBrushStyle(result);
  result:= 0;
end; end;

class procedure TPyBrush.RegisterGetSets(PythonType: TPythonType);
begin
  with PythonType do begin
    AddGetSet('Bitmap', @TPyBrush.DoGetBitmap, @TPyBrush.DoSetBitmap, 'ID of Bitmap assosciated with Brush style', nil);
    AddGetSet('Color', @TPyBrush.DoGetColor, @TPyBrush.DoSetColor, 'String ID of Brush color', nil);
    AddGetSet('Handle', @TPyBrush.DoGetHandle, @TPyBrush.DoSetHandle, 'Windows Brush Handle', nil);
    AddGetSet('Style', @TPyBrush.DoGetStyle, @TPyBrush.DoSetStyle, 'The Brush Style', nil);
  end;
  inherited;
end;

procedure TPyBrush.SetBrush(value: TBrush);
begin
  inherited Control:= value;
  fBrush:= value;
end;

{ TPyPen }

class procedure TPyPen.RegisterGetSets(PythonType: TPythonType);
begin
  with PythonType do begin
    AddGetSet('Color', @TPyPen.DoGetColor, @TPyPen.DoSetColor, 'String ID of current TPen color', nil);
    AddGetSet('Handle', @TPyPen.DoGetHandle, @TPyPen.DoSetHandle, 'Windows HPen object', nil);
    AddGetSet('Mode', @TPyPen.DoGetMode, @TPyPen.DoSetMode, 'String representing TPenMode', nil);
    AddGetSet('Style', @TPyPen.DoGetStyle, @TPyPen.DoSetStyle, 'String representing TPenStyle', nil);
    AddGetSet('Width', @TPyPen.DoGetWidth, @TPyPen.DoSetWidth, 'Width of the current pen', nil);
  end;
  inherited;
end;

function TPyPen.DoGetColor(c: int): PPO;
begin with GetPythonEngine do begin Adjust(@Self);
  if(fPen=nil) then
    result:= ReturnNone
  else
    result:= VarPyObj(ColorToString(fPen.Color));
end; end;

function TPyPen.DoGetHandle(c: int): PPO;
begin with GetPythonEngine do begin Adjust(@Self);
  if(fPen=nil) then
    result:= ReturnNone
  else
    result:= VarPyObj(int(fPen.Handle));
end; end;

function TPyPen.DoGetMode(c: int): PPO;
begin with GetPythonEngine do begin Adjust(@Self);
  if(fPen=nil) then
    result:= ReturnNone
  else
    result:= VariantAsPyObject(PenModeToString(fPen.Mode));
end; end;

function TPyPen.DoGetStyle(c: int): PPO;
begin with GetPythonEngine do begin Adjust(@Self);
  if(fPen=nil) then
    result:= ReturnNone
  else
    result:= VariantAsPyObject(PenStyleToString(fPen.Style));
end; end;

function TPyPen.DoGetWidth(c: int): PPO;
begin with GetPythonEngine do begin Adjust(@Self);
  if(fPen=nil) then
    result:= ReturnNone
  else
    result:= VarPyObj(fPen.Width);
end; end;

function TPyPen.DoSetColor(val: PPO; c: int): int;
begin with GetPythonEngine do begin Adjust(@Self);
  result:= 1;
  if(fPen=nil) then exit;
  result:= 0;
  fPen.Color:= StringToColor(PyObjStr(val));
end; end;

function TPyPen.DoSetHandle(val: PPO; c: int): int;
begin with GetPythonEngine do begin Adjust(@Self);
  result:= 1;
  if(fPen=nil) then exit;
  result:= 0;
  fPen.Handle:= PyObjInt(val);
end; end;

function TPyPen.DoSetMode(val: PPO; c: int): int;
begin with GetPythonEngine do begin Adjust(@Self);
  result:= 1;
  if(fPen=nil) then exit;
  result:= 0;
  fPen.Mode:= StringToPenMode(PyObjectAsString(val));
end; end;

function TPyPen.DoSetStyle(val: PPO; c: int): int;
begin with GetPythonEngine do begin Adjust(@Self);
  result:= 1;
  if(fPen=nil) then exit;
  result:= 0;
  fPen.Style:= StringToPenStyle(PyObjectAsString(val));
end; end;

function TPyPen.DoSetWidth(val: PPO; c: int): int;
begin with GetPythonEngine do begin Adjust(@Self);
  result:= 1;
  if(fPen=nil) then exit;
  result:= 0;
  fPen.Width:= PyObjInt(val);
end; end;

procedure TPyPen.SetPen(val: TPen);
begin
  inherited Control:= val;
  fPen:= val;
end;

{ TPyPersistent }

function TPyPersistent.DoAssign(args: PPO): PPO;
var
  i: int;
begin with GetPythonEngine do begin Adjust(@Self);
  if(PyArg_ParseTuple(args, 'i:Assign', [@i])<>0) then begin
    result:= ReturnNone;
    if(obj=nil) then exit;
    obj.Assign(TPersistent(i));
  end else
    result:= nil;
end; end;

function TPyPersistent.DoGetObj(c: int): PPO;
begin with GetPythonEngine do begin Adjust(@Self);
  result:= VarPyObj(int(obj));
end; end;

function TPyPersistent.DoSetObj(v: PPO; c: int): int;
begin with GetPythonEngine do begin Adjust(@Self);
  result:= 0;
  Control:= TPersistent(PyObjInt(v));
end; end;

class procedure TPyPersistent.RegisterGetSets(PythonType: TPythonType);
begin
  with PythonType do begin
    AddGetSet('obj', @TPyPersistent.DoGetObj, @TPyPersistent.DoSetObj, 'Delphi internal TControl object ID', nil);
  end;
  inherited;
end;

class procedure TPyPersistent.RegisterMethods(PythonType: TPythonType);
begin
  with PythonType do begin
    AddMethod('Assign', @TPyPersistent.DoAssign, 'Assign another object(ID) to this object');
  end;
  inherited;
end;

{ TPyBitmap }

function TPyBitmap.DoGetCanvas(c: int): PPO;
begin with GetPythonEngine do begin Adjust(@Self);
  if(fBmp=nil) then begin
    result:= ReturnNone;
    exit;
  end;
  result:= PyCanvasType.CreateInstance;
  TPyCanvas(PythonToDelphi(result)).fCanvas:= fBmp.Canvas;
end; end;

class procedure TPyBitmap.RegisterGetSets(PT: TPT);
begin
  with PT do begin
    AddGetSet('Canvas', @TPyBitmap.DoGetCanvas, nil, 'TCanvas of the TBitmap', nil);
  end;
  inherited;
end;


procedure TPyBitmap.SetBitmap(const Value: Graphics.TBitmap);
begin
  inherited Control:= Value;
  fBmp := Value;
end;

{ TPyGraphic }

function TPyGraphic.DoGetEmpty(c: int): PPO;
begin with GetPythonEngine do begin Adjust(@Self);
  if(fGraphic=nil) then
    result:= ReturnNone
  else
    result:= VarPyObj(fGraphic.Empty);
end; end;

function TPyGraphic.DoGetHeight(c: int): PPO;
begin with GetPythonEngine do begin Adjust(@Self);
  if(fGraphic=nil) then
    result:= ReturnNone
  else
    result:= VarPyObj(fGraphic.Height);
end; end;

function TPyGraphic.DoGetModified(c: int): PPO;
begin with GetPythonEngine do begin Adjust(@Self);
  if(fGraphic=nil) then
    result:= ReturnNone
  else
    result:= VarPyObj(fGraphic.Modified);
end; end;

function TPyGraphic.DoGetTransparent(c: int): PPO;
begin with GetPythonEngine do begin Adjust(@Self);
  if(fGraphic=nil) then
    result:= ReturnNone
  else
    result:= VarPyObj(fGraphic.Transparent);
end; end;

function TPyGraphic.DoGetWidth(c: int): PPO;
begin with GetPythonEngine do begin Adjust(@Self);
  if(fGraphic=nil) then
    result:= ReturnNone
  else
    result:= VarPyObj(fGraphic.Width);
end; end;

function TPyGraphic.DoLoadFromFile(args: PPO): PPO;
var
  s: PAnsiChar;
begin with GetPythonEngine do begin Adjust(@Self);
  if(PyArg_ParseTuple(args, 's:LoadFromFile', [@s])<>0) then begin
    result:= ReturnNone;
    if(fGraphic=nil) then exit;
    fGraphic.LoadFromFile(s);
  end else
    result:= nil;
end; end;

function TPyGraphic.DoSaveToFile(args: PPO): PPO;
var
  s: PAnsiChar;
begin with GetPythonEngine do begin Adjust(@Self);
  if(PyArg_ParseTuple(args, 's:SaveToFile', [@s])<>0) then begin
    result:= ReturnNone;
    if(fGraphic=nil) then exit;
    fGraphic.SaveToFile(s);
  end else
    result:= nil;
end; end;

class procedure TPyGraphic.RegisterGetSets(PT: TPT);
begin
  with PT do begin
    AddGetSet('Empty', @TPyGraphic.DoGetEmpty, nil, 'Is the graphic empty?', nil);
    AddGetSet('Height', @TPyGraphic.DoGetHeight, nil, 'Height of the graphic', nil);
    AddGetSet('Modified', @TPyGraphic.DoGetModified, nil, 'Has the graphic been modified?', nil);
    AddGetSet('Transparent', @TPyGraphic.DoGetTransparent, nil, 'Is the graphic transparent?', nil);
    AddGetSet('Width', @TPyGraphic.DoGetWidth, nil, 'Width of the graphic', nil);
  end;
  inherited;
end;

class procedure TPyGraphic.RegisterMethods(PT: TPT);
begin
  with PT do begin
    AddMethod('LoadFromFile', @TPyGraphic.DoLoadFromFile, 'Loads the graphic from a file');
    AddMethod('SaveToFile', @TPyGraphic.DoSaveToFile, 'Save the graphic to a file');
  end;
  inherited;
end;

procedure TPyGraphic.SetGraphic(const Value: TGraphic);
begin
  inherited Control:= Value;
  fGraphic := Value;
end;

{ TPyGraphicControl }

function TPyGraphicControl.DoGetCanvas(c: int): PPO;
begin with GetPythonEngine do begin Adjust(@Self);
  if(fGC=nil) then begin
    result:= ReturnNone;
    exit;
  end;

  result:= PyCanvasType.CreateInstance;
  TPyCanvas(PythonToDelphi(result)).fCanvas:= TGraphicArea(fGC).Canvas;
  //bypass Delphi class regulations -
  //this method will have to overriden to implement classes
  //like TImage which implement their own TCanvas, but will
  //always grant access to the control's canvas.
end; end;

class procedure TPyGraphicControl.RegisterGetSets(PT: TPT);
begin
  with PT do begin
    AddGetSet('Canvas', @TPyGraphicControl.DoGetCanvas, nil, 'TCanvas of this control', nil);
  end;
  inherited;
end;

procedure TPyGraphicControl.SetGC(const Value: TGraphicControl);
begin
  inherited Control:= Value;
  fGC := Value;
end;

end.
