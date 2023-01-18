{$I ..\Definition.Inc}

unit WrapFmxShapes;

interface

uses
  FMX.Objects, WrapFmxControls;

type
  TPyDelphiShape = class(TPyDelphiControl)
  private
    function  GetDelphiObject: TShape;
    procedure SetDelphiObject(const Value: TShape);
  public
    class function  DelphiObjectClass : TClass; override;
    // Properties
    property DelphiObject: TShape read GetDelphiObject write SetDelphiObject;
  end;

  TPyDelphiLine = class(TPyDelphiShape)
  private
    function  GetDelphiObject: TLine;
    procedure SetDelphiObject(const Value: TLine);
  public
    class function  DelphiObjectClass : TClass; override;
    // Properties
    property DelphiObject: TLine read GetDelphiObject write SetDelphiObject;
  end;

  TPyDelphiRectangle = class(TPyDelphiShape)
  private
    function  GetDelphiObject: TRectangle;
    procedure SetDelphiObject(const Value: TRectangle);
  public
    class function  DelphiObjectClass : TClass; override;
    // Properties
    property DelphiObject: TRectangle read GetDelphiObject write SetDelphiObject;
  end;

  TPyDelphiCaretRectangle = class(TPyDelphiRectangle)
  private
    function  GetDelphiObject: TCaretRectangle;
    procedure SetDelphiObject(const Value: TCaretRectangle);
  public
    class function  DelphiObjectClass : TClass; override;
    // Properties
    property DelphiObject: TCaretRectangle read GetDelphiObject write SetDelphiObject;
  end;

  TPyDelphiRoundRect = class(TPyDelphiShape)
  private
    function  GetDelphiObject: TRoundRect;
    procedure SetDelphiObject(const Value: TRoundRect);
  public
    class function  DelphiObjectClass : TClass; override;
    // Properties
    property DelphiObject: TRoundRect read GetDelphiObject write SetDelphiObject;
  end;

  TPyDelphiCalloutRectangle = class(TPyDelphiRectangle)
    function  GetDelphiObject: TCalloutRectangle;
    procedure SetDelphiObject(const Value: TCalloutRectangle);
  public
    class function  DelphiObjectClass : TClass; override;
    // Properties
    property DelphiObject: TCalloutRectangle read GetDelphiObject write SetDelphiObject;
  end;

  TPyDelphiEllipse = class(TPyDelphiShape)
    function  GetDelphiObject: TEllipse;
    procedure SetDelphiObject(const Value: TEllipse);
  public
    class function  DelphiObjectClass : TClass; override;
    // Properties
    property DelphiObject: TEllipse read GetDelphiObject write SetDelphiObject;
  end;

  TPyDelphiCircle = class(TPyDelphiEllipse)
    function  GetDelphiObject: TCircle;
    procedure SetDelphiObject(const Value: TCircle);
  public
    class function  DelphiObjectClass : TClass; override;
    // Properties
    property DelphiObject: TCircle read GetDelphiObject write SetDelphiObject;
  end;

  TPyDelphiPie = class(TPyDelphiEllipse)
    function  GetDelphiObject: TPie;
    procedure SetDelphiObject(const Value: TPie);
  public
    class function  DelphiObjectClass : TClass; override;
    // Properties
    property DelphiObject: TPie read GetDelphiObject write SetDelphiObject;
  end;

  TPyDelphiArc = class(TPyDelphiEllipse)
    function  GetDelphiObject: TArc;
    procedure SetDelphiObject(const Value: TArc);
  public
    class function  DelphiObjectClass : TClass; override;
    // Properties
    property DelphiObject: TArc read GetDelphiObject write SetDelphiObject;
  end;

  TPyDelphiCustomPath = class(TPyDelphiShape)
    function  GetDelphiObject: TCustomPath;
    procedure SetDelphiObject(const Value: TCustomPath);
  public
    class function  DelphiObjectClass : TClass; override;
    // Properties
    property DelphiObject: TCustomPath read GetDelphiObject write SetDelphiObject;
  end;

  TPyDelphiPath = class(TPyDelphiCustomPath)
    function  GetDelphiObject: TPath;
    procedure SetDelphiObject(const Value: TPath);
  public
    class function  DelphiObjectClass : TClass; override;
    // Properties
    property DelphiObject: TPath read GetDelphiObject write SetDelphiObject;
  end;

  TPyDelphiText = class(TPyDelphiControl)
    function  GetDelphiObject: TText;
    procedure SetDelphiObject(const Value: TText);
  public
    class function  DelphiObjectClass : TClass; override;
    // Properties
    property DelphiObject: TText read GetDelphiObject write SetDelphiObject;
  end;

  TPyDelphiImage = class(TPyDelphiControl)
    function  GetDelphiObject: TImage;
    procedure SetDelphiObject(const Value: TImage);
  public
    class function  DelphiObjectClass : TClass; override;
    // Properties
    property DelphiObject: TImage read GetDelphiObject write SetDelphiObject;
  end;

  TPyDelphiPaintBox = class(TPyDelphiControl)
    function  GetDelphiObject: TPaintBox;
    procedure SetDelphiObject(const Value: TPaintBox);
  public
    class function  DelphiObjectClass : TClass; override;
    // Properties
    property DelphiObject: TPaintBox read GetDelphiObject write SetDelphiObject;
  end;

  TPyDelphiSelection = class(TPyDelphiControl)
    function  GetDelphiObject: TSelection;
    procedure SetDelphiObject(const Value: TSelection);
  public
    class function  DelphiObjectClass : TClass; override;
    // Properties
    property DelphiObject: TSelection read GetDelphiObject write SetDelphiObject;
  end;

  TPyDelphiSelectionPoint = class(TPyDelphiStyledControl)
    function  GetDelphiObject: TSelectionPoint;
    procedure SetDelphiObject(const Value: TSelectionPoint);
  public
    class function  DelphiObjectClass : TClass; override;
    // Properties
    property DelphiObject: TSelectionPoint read GetDelphiObject write SetDelphiObject;
  end;

implementation

uses
  WrapDelphi;

{ Register the wrappers, the globals and the constants }
type
  TShapesRegistration = class(TRegisteredUnit)
  public
    function Name : string; override;
    procedure RegisterWrappers(APyDelphiWrapper : TPyDelphiWrapper); override;
    procedure DefineVars(APyDelphiWrapper : TPyDelphiWrapper); override;
  end;

{ TShapesRegistration }

procedure TShapesRegistration.DefineVars(APyDelphiWrapper: TPyDelphiWrapper);
begin
  inherited;
end;

function TShapesRegistration.Name: string;
begin
  Result := 'Shapes';
end;

procedure TShapesRegistration.RegisterWrappers(
  APyDelphiWrapper: TPyDelphiWrapper);
begin
  inherited;
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiShape);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiLine);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiRectangle);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiCaretRectangle);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiRoundRect);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiCalloutRectangle);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiEllipse);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiCircle);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiPie);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiArc);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiCustomPath);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiPath);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiText);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiImage);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiPaintBox);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiSelection);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiSelectionPoint);
end;

{ TPyDelphiShape }

class function TPyDelphiShape.DelphiObjectClass: TClass;
begin
  Result := TShape;
end;

function TPyDelphiShape.GetDelphiObject: TShape;
begin
  Result := TShape(inherited DelphiObject);
end;

procedure TPyDelphiShape.SetDelphiObject(const Value: TShape);
begin
  inherited DelphiObject := Value;
end;

{ TPyDelphiLine }

class function TPyDelphiLine.DelphiObjectClass: TClass;
begin
  Result := TLine;
end;

function TPyDelphiLine.GetDelphiObject: TLine;
begin
  Result := TLine(inherited DelphiObject);
end;

procedure TPyDelphiLine.SetDelphiObject(const Value: TLine);
begin
  inherited DelphiObject := Value;
end;

{ TPyDelphiRectangle }

class function TPyDelphiRectangle.DelphiObjectClass: TClass;
begin
  Result := TRectangle;
end;

function TPyDelphiRectangle.GetDelphiObject: TRectangle;
begin
  Result := TRectangle(inherited DelphiObject);
end;

procedure TPyDelphiRectangle.SetDelphiObject(const Value: TRectangle);
begin
  inherited DelphiObject := Value;
end;

{ TPyDelphiCaretRectangle }

class function TPyDelphiCaretRectangle.DelphiObjectClass: TClass;
begin
  Result := TCaretRectangle;
end;

function TPyDelphiCaretRectangle.GetDelphiObject: TCaretRectangle;
begin
  Result := TCaretRectangle(inherited DelphiObject);
end;

procedure TPyDelphiCaretRectangle.SetDelphiObject(const Value: TCaretRectangle);
begin
  inherited DelphiObject := Value;
end;

{ TPyDelphiRoundRect }

class function TPyDelphiRoundRect.DelphiObjectClass: TClass;
begin
  Result := TRoundRect;
end;

function TPyDelphiRoundRect.GetDelphiObject: TRoundRect;
begin
  Result := TRoundRect(inherited DelphiObject);
end;

procedure TPyDelphiRoundRect.SetDelphiObject(const Value: TRoundRect);
begin
  inherited DelphiObject := Value;
end;

{ TPyDelphiCalloutRectangle }

class function TPyDelphiCalloutRectangle.DelphiObjectClass: TClass;
begin
  Result := TCalloutRectangle;
end;

function TPyDelphiCalloutRectangle.GetDelphiObject: TCalloutRectangle;
begin
  Result := TCalloutRectangle(inherited DelphiObject);
end;

procedure TPyDelphiCalloutRectangle.SetDelphiObject(
  const Value: TCalloutRectangle);
begin
  inherited DelphiObject := Value;
end;

{ TPyDelphiEllipse }

class function TPyDelphiEllipse.DelphiObjectClass: TClass;
begin
  Result := TEllipse;
end;

function TPyDelphiEllipse.GetDelphiObject: TEllipse;
begin
  Result := TEllipse(inherited DelphiObject);
end;

procedure TPyDelphiEllipse.SetDelphiObject(const Value: TEllipse);
begin
  inherited DelphiObject := Value;
end;

{ TPyDelphiCircle }

class function TPyDelphiCircle.DelphiObjectClass: TClass;
begin
  Result := TCircle;
end;

function TPyDelphiCircle.GetDelphiObject: TCircle;
begin
  Result := TCircle(inherited DelphiObject);
end;

procedure TPyDelphiCircle.SetDelphiObject(const Value: TCircle);
begin
  inherited DelphiObject := Value;
end;

{ TPyDelphiPie }

class function TPyDelphiPie.DelphiObjectClass: TClass;
begin
  Result := TPie;
end;

function TPyDelphiPie.GetDelphiObject: TPie;
begin
  Result := TPie(inherited DelphiObject);
end;

procedure TPyDelphiPie.SetDelphiObject(const Value: TPie);
begin
  inherited DelphiObject := Value;
end;

{ TPyDelphiArc }

class function TPyDelphiArc.DelphiObjectClass: TClass;
begin
  Result := TArc;
end;

function TPyDelphiArc.GetDelphiObject: TArc;
begin
  Result := TArc(inherited DelphiObject);
end;

procedure TPyDelphiArc.SetDelphiObject(const Value: TArc);
begin
  inherited DelphiObject := Value;
end;

{ TPyDelphiCustomPath }

class function TPyDelphiCustomPath.DelphiObjectClass: TClass;
begin
  Result := TCustomPath;
end;

function TPyDelphiCustomPath.GetDelphiObject: TCustomPath;
begin
  Result := TCustomPath(inherited DelphiObject);
end;

procedure TPyDelphiCustomPath.SetDelphiObject(const Value: TCustomPath);
begin
  inherited DelphiObject := Value;
end;

{ TPyDelphiPath }

class function TPyDelphiPath.DelphiObjectClass: TClass;
begin
  Result := TPath;
end;

function TPyDelphiPath.GetDelphiObject: TPath;
begin
  Result := TPath(inherited DelphiObject);
end;

procedure TPyDelphiPath.SetDelphiObject(const Value: TPath);
begin
  inherited DelphiObject := Value;
end;

{ TPyDelphiText }

class function TPyDelphiText.DelphiObjectClass: TClass;
begin
  Result := TText;
end;

function TPyDelphiText.GetDelphiObject: TText;
begin
  Result := TText(inherited DelphiObject);
end;

procedure TPyDelphiText.SetDelphiObject(const Value: TText);
begin
  inherited DelphiObject := Value;
end;

{ TPyDelphiImage }

class function TPyDelphiImage.DelphiObjectClass: TClass;
begin
  Result := TImage;
end;

function TPyDelphiImage.GetDelphiObject: TImage;
begin
  Result := TImage(inherited DelphiObject);
end;

procedure TPyDelphiImage.SetDelphiObject(const Value: TImage);
begin
  inherited DelphiObject := Value;
end;

{ TPyDelphiPaintBox }

class function TPyDelphiPaintBox.DelphiObjectClass: TClass;
begin
  Result := TPaintBox;
end;

function TPyDelphiPaintBox.GetDelphiObject: TPaintBox;
begin
  Result := TPaintBox(inherited DelphiObject);
end;

procedure TPyDelphiPaintBox.SetDelphiObject(const Value: TPaintBox);
begin
  inherited DelphiObject := Value;
end;

{ TPyDelphiSelection }

class function TPyDelphiSelection.DelphiObjectClass: TClass;
begin
  Result := TSelection;
end;

function TPyDelphiSelection.GetDelphiObject: TSelection;
begin
  Result := TSelection(inherited DelphiObject);
end;

procedure TPyDelphiSelection.SetDelphiObject(const Value: TSelection);
begin
  inherited DelphiObject := Value;
end;

{ TPyDelphiSelectionPoint }

class function TPyDelphiSelectionPoint.DelphiObjectClass: TClass;
begin
  Result := TSelectionPoint;
end;

function TPyDelphiSelectionPoint.GetDelphiObject: TSelectionPoint;
begin
  Result := TSelectionPoint(inherited DelphiObject);
end;

procedure TPyDelphiSelectionPoint.SetDelphiObject(const Value: TSelectionPoint);
begin
  inherited DelphiObject := Value;
end;

initialization
  RegisteredUnits.Add(TShapesRegistration.Create);

end.
