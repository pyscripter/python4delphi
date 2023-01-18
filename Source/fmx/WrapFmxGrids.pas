unit WrapFmxGrids;

interface

uses
  FMX.Grid, FMX.Grid.Style, WrapFmxControls, WrapFmxScrollBox;

type
  TPyDelphiColumn = class(TPyDelphiControl)
  private
    function  GetDelphiObject: TColumn;
    procedure SetDelphiObject(const Value: TColumn);
  public
    class function  DelphiObjectClass : TClass; override;
    // Properties
    property DelphiObject: TColumn read GetDelphiObject write SetDelphiObject;
  end;

  TPyDelphiStringColumn = class(TPyDelphiColumn)
  private
    function  GetDelphiObject: TStringColumn;
    procedure SetDelphiObject(const Value: TStringColumn);
  public
    class function  DelphiObjectClass : TClass; override;
    // Properties
    property DelphiObject: TStringColumn read GetDelphiObject write SetDelphiObject;
  end;

  TPyDelphiProgressColumn = class(TPyDelphiColumn)
  private
    function  GetDelphiObject: TProgressColumn;
    procedure SetDelphiObject(const Value: TProgressColumn);
  public
    class function  DelphiObjectClass : TClass; override;
    // Properties
    property DelphiObject: TProgressColumn read GetDelphiObject write SetDelphiObject;
  end;

  TPyDelphiCheckColumn = class(TPyDelphiColumn)
  private
    function  GetDelphiObject: TCheckColumn;
    procedure SetDelphiObject(const Value: TCheckColumn);
  public
    class function  DelphiObjectClass : TClass; override;
    // Properties
    property DelphiObject: TCheckColumn read GetDelphiObject write SetDelphiObject;
  end;

  TPyDelphiDateTimeColumnBase = class(TPyDelphiColumn)
  private
    function  GetDelphiObject: TDateTimeColumnBase;
    procedure SetDelphiObject(const Value: TDateTimeColumnBase);
  public
    class function  DelphiObjectClass : TClass; override;
    // Properties
    property DelphiObject: TDateTimeColumnBase read GetDelphiObject write SetDelphiObject;
  end;

  TPyDelphiDateTimeColumn = class(TPyDelphiDateTimeColumnBase)
  private
    function  GetDelphiObject: TDateTimeColumn;
    procedure SetDelphiObject(const Value: TDateTimeColumn);
  public
    class function  DelphiObjectClass : TClass; override;
    // Properties
    property DelphiObject: TDateTimeColumn read GetDelphiObject write SetDelphiObject;
  end;

  TPyDelphiTimeColumn = class(TPyDelphiDateTimeColumnBase)
  private
    function  GetDelphiObject: TTimeColumn;
    procedure SetDelphiObject(const Value: TTimeColumn);
  public
    class function  DelphiObjectClass : TClass; override;
    // Properties
    property DelphiObject: TTimeColumn read GetDelphiObject write SetDelphiObject;
  end;

  TPyDelphiDateColumn = class(TPyDelphiDateTimeColumnBase)
  private
    function  GetDelphiObject: TDateColumn;
    procedure SetDelphiObject(const Value: TDateColumn);
  public
    class function  DelphiObjectClass : TClass; override;
    // Properties
    property DelphiObject: TDateColumn read GetDelphiObject write SetDelphiObject;
  end;

  TPyDelphiPopupColumn = class(TPyDelphiColumn)
  private
    function  GetDelphiObject: TPopupColumn;
    procedure SetDelphiObject(const Value: TPopupColumn);
  public
    class function  DelphiObjectClass : TClass; override;
    // Properties
    property DelphiObject: TPopupColumn read GetDelphiObject write SetDelphiObject;
  end;

  TPyDelphiImageColumn = class(TPyDelphiColumn)
  private
    function  GetDelphiObject: TImageColumn;
    procedure SetDelphiObject(const Value: TImageColumn);
  public
    class function  DelphiObjectClass : TClass; override;
    // Properties
    property DelphiObject: TImageColumn read GetDelphiObject write SetDelphiObject;
  end;

  TPyDelphiCustomNumberColumn = class(TPyDelphiColumn)
  private
    function  GetDelphiObject: TCustomNumberColumn;
    procedure SetDelphiObject(const Value: TCustomNumberColumn);
  public
    class function  DelphiObjectClass : TClass; override;
    // Properties
    property DelphiObject: TCustomNumberColumn read GetDelphiObject write SetDelphiObject;
  end;

  TPyDelphiCurrencyColumn = class(TPyDelphiCustomNumberColumn)
  private
    function  GetDelphiObject: TCurrencyColumn;
    procedure SetDelphiObject(const Value: TCurrencyColumn);
  public
    class function  DelphiObjectClass : TClass; override;
    // Properties
    property DelphiObject: TCurrencyColumn read GetDelphiObject write SetDelphiObject;
  end;

  TPyDelphiFloatColumn = class(TPyDelphiCustomNumberColumn)
  private
    function  GetDelphiObject: TFloatColumn;
    procedure SetDelphiObject(const Value: TFloatColumn);
  public
    class function  DelphiObjectClass : TClass; override;
    // Properties
    property DelphiObject: TFloatColumn read GetDelphiObject write SetDelphiObject;
  end;

  TPyDelphiIntegerColumn = class(TPyDelphiCustomNumberColumn)
  private
    function  GetDelphiObject: TIntegerColumn;
    procedure SetDelphiObject(const Value: TIntegerColumn);
  public
    class function  DelphiObjectClass : TClass; override;
    // Properties
    property DelphiObject: TIntegerColumn read GetDelphiObject write SetDelphiObject;
  end;

  TPyDelphiGlyphColumn = class(TPyDelphiColumn)
  private
    function  GetDelphiObject: TGlyphColumn;
    procedure SetDelphiObject(const Value: TGlyphColumn);
  public
    class function  DelphiObjectClass : TClass; override;
    // Properties
    property DelphiObject: TGlyphColumn read GetDelphiObject write SetDelphiObject;
  end;

  TPyDelphiCustomGrid = class(TPyDelphiCustomPresentedScrollBox)
  private
    function  GetDelphiObject: TCustomGrid;
    procedure SetDelphiObject(const Value: TCustomGrid);
  public
    class function  DelphiObjectClass : TClass; override;
    // Properties
    property DelphiObject: TCustomGrid read GetDelphiObject write SetDelphiObject;
  end;

  TPyDelphiGrid = class(TPyDelphiCustomGrid)
  private
    function GetDelphiObject: TGrid;
    procedure SetDelphiObject(const Value: TGrid);
  public
    class function  DelphiObjectClass : TClass; override;
    // Properties
    property DelphiObject: TGrid read GetDelphiObject write SetDelphiObject;
  end;

  TPyDelphiStringGrid = class(TPyDelphiCustomGrid)
  private
    function GetDelphiObject: TStringGrid;
    procedure SetDelphiObject(const Value: TStringGrid);
  public
    class function  DelphiObjectClass : TClass; override;
    // Properties
    property DelphiObject: TStringGrid read GetDelphiObject write SetDelphiObject;
  end;

implementation

uses
  WrapDelphi;

{ Register the wrappers, the globals and the constants }
type
  TGridsRegistration = class(TRegisteredUnit)
  public
    function Name : string; override;
    procedure RegisterWrappers(APyDelphiWrapper : TPyDelphiWrapper); override;
    procedure DefineVars(APyDelphiWrapper : TPyDelphiWrapper); override;
  end;

{ TGridsRegistration }

procedure TGridsRegistration.DefineVars(APyDelphiWrapper: TPyDelphiWrapper);
begin
  inherited;
end;

function TGridsRegistration.Name: string;
begin
  Result := 'Grids';
end;

procedure TGridsRegistration.RegisterWrappers(
  APyDelphiWrapper: TPyDelphiWrapper);
begin
  inherited;
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiColumn);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiStringColumn);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiProgressColumn);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiCheckColumn);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiDateTimeColumnBase);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiDateTimeColumn);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiTimeColumn);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiDateColumn);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiPopupColumn);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiImageColumn);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiCustomNumberColumn);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiCurrencyColumn);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiFloatColumn);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiIntegerColumn);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiGlyphColumn);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiCustomGrid);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiGrid);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiStringGrid);
end;

{ TPyDelphiColumn }

class function TPyDelphiColumn.DelphiObjectClass: TClass;
begin
  Result := TColumn;
end;

function TPyDelphiColumn.GetDelphiObject: TColumn;
begin
  Result := TColumn(inherited DelphiObject);
end;

procedure TPyDelphiColumn.SetDelphiObject(const Value: TColumn);
begin
  inherited DelphiObject := Value;
end;

{ TPyDelphiStringColumn }

class function TPyDelphiStringColumn.DelphiObjectClass: TClass;
begin
  Result := TStringColumn;
end;

function TPyDelphiStringColumn.GetDelphiObject: TStringColumn;
begin
  Result := TStringColumn(inherited DelphiObject);
end;

procedure TPyDelphiStringColumn.SetDelphiObject(const Value: TStringColumn);
begin
  inherited DelphiObject := Value;
end;

{ TPyDelphiProgressColumn }

class function TPyDelphiProgressColumn.DelphiObjectClass: TClass;
begin
  Result := TProgressColumn;
end;

function TPyDelphiProgressColumn.GetDelphiObject: TProgressColumn;
begin
  Result := TProgressColumn(inherited DelphiObject);
end;

procedure TPyDelphiProgressColumn.SetDelphiObject(const Value: TProgressColumn);
begin
  inherited DelphiObject := Value;
end;

{ TPyDelphiCheckColumn }

class function TPyDelphiCheckColumn.DelphiObjectClass: TClass;
begin
  Result := TCheckColumn;
end;

function TPyDelphiCheckColumn.GetDelphiObject: TCheckColumn;
begin
  Result := TCheckColumn(inherited DelphiObject);
end;

procedure TPyDelphiCheckColumn.SetDelphiObject(const Value: TCheckColumn);
begin
  inherited DelphiObject := Value;
end;

{ TPyDelphiDateTimeColumnBase }

class function TPyDelphiDateTimeColumnBase.DelphiObjectClass: TClass;
begin
  Result := TDateTimeColumnBase;
end;

function TPyDelphiDateTimeColumnBase.GetDelphiObject: TDateTimeColumnBase;
begin
  Result := TDateTimeColumnBase(inherited DelphiObject);
end;

procedure TPyDelphiDateTimeColumnBase.SetDelphiObject(
  const Value: TDateTimeColumnBase);
begin
  inherited DelphiObject := Value;
end;

{ TPyDelphiDateTimeColumn }

class function TPyDelphiDateTimeColumn.DelphiObjectClass: TClass;
begin
  Result := TDateTimeColumn;
end;

function TPyDelphiDateTimeColumn.GetDelphiObject: TDateTimeColumn;
begin
  Result := TDateTimeColumn(inherited DelphiObject);
end;

procedure TPyDelphiDateTimeColumn.SetDelphiObject(const Value: TDateTimeColumn);
begin
  inherited DelphiObject := Value;
end;

{ TPyDelphiTimeColumn }

class function TPyDelphiTimeColumn.DelphiObjectClass: TClass;
begin
  Result := TTimeColumn;
end;

function TPyDelphiTimeColumn.GetDelphiObject: TTimeColumn;
begin
  Result := TTimeColumn(inherited DelphiObject);
end;

procedure TPyDelphiTimeColumn.SetDelphiObject(const Value: TTimeColumn);
begin
  inherited DelphiObject := Value;
end;

{ TPyDelphiDateColumn }

class function TPyDelphiDateColumn.DelphiObjectClass: TClass;
begin
  Result := TDateColumn;
end;

function TPyDelphiDateColumn.GetDelphiObject: TDateColumn;
begin
  Result := TDateColumn(inherited DelphiObject);
end;

procedure TPyDelphiDateColumn.SetDelphiObject(const Value: TDateColumn);
begin
  inherited DelphiObject := Value;
end;

{ TPyDelphiPopupColumn }

class function TPyDelphiPopupColumn.DelphiObjectClass: TClass;
begin
  Result := TPopupColumn;
end;

function TPyDelphiPopupColumn.GetDelphiObject: TPopupColumn;
begin
  Result := TPopupColumn(inherited DelphiObject);
end;

procedure TPyDelphiPopupColumn.SetDelphiObject(const Value: TPopupColumn);
begin
  inherited DelphiObject := Value;
end;

{ TPyDelphiImageColumn }

class function TPyDelphiImageColumn.DelphiObjectClass: TClass;
begin
  Result := TImageColumn;
end;

function TPyDelphiImageColumn.GetDelphiObject: TImageColumn;
begin
  Result := TImageColumn(inherited DelphiObject);
end;

procedure TPyDelphiImageColumn.SetDelphiObject(const Value: TImageColumn);
begin
  inherited DelphiObject := Value;
end;

{ TPyDelphiCustomNumberColumn }

class function TPyDelphiCustomNumberColumn.DelphiObjectClass: TClass;
begin
  Result := TCustomNumberColumn;
end;

function TPyDelphiCustomNumberColumn.GetDelphiObject: TCustomNumberColumn;
begin
  Result := TCustomNumberColumn(inherited DelphiObject);
end;

procedure TPyDelphiCustomNumberColumn.SetDelphiObject(
  const Value: TCustomNumberColumn);
begin
  inherited DelphiObject := Value;
end;

{ TPyDelphiCurrencyColumn }

class function TPyDelphiCurrencyColumn.DelphiObjectClass: TClass;
begin
  Result := TCurrencyColumn;
end;

function TPyDelphiCurrencyColumn.GetDelphiObject: TCurrencyColumn;
begin
  Result := TCurrencyColumn(inherited DelphiObject);
end;

procedure TPyDelphiCurrencyColumn.SetDelphiObject(const Value: TCurrencyColumn);
begin
  inherited DelphiObject := Value;
end;

{ TPyDelphiFloatColumn }

class function TPyDelphiFloatColumn.DelphiObjectClass: TClass;
begin
  Result := TFloatColumn;
end;

function TPyDelphiFloatColumn.GetDelphiObject: TFloatColumn;
begin
  Result := TFloatColumn(inherited DelphiObject);
end;

procedure TPyDelphiFloatColumn.SetDelphiObject(const Value: TFloatColumn);
begin
  inherited DelphiObject := Value;
end;

{ TPyDelphiIntegerColumn }

class function TPyDelphiIntegerColumn.DelphiObjectClass: TClass;
begin
  Result := TIntegerColumn;
end;

function TPyDelphiIntegerColumn.GetDelphiObject: TIntegerColumn;
begin
  Result := TIntegerColumn(inherited DelphiObject);
end;

procedure TPyDelphiIntegerColumn.SetDelphiObject(const Value: TIntegerColumn);
begin
  inherited DelphiObject := Value;
end;

{ TPyDelphiGlyphColumn }

class function TPyDelphiGlyphColumn.DelphiObjectClass: TClass;
begin
  Result := TGlyphColumn;
end;

function TPyDelphiGlyphColumn.GetDelphiObject: TGlyphColumn;
begin
  Result := TGlyphColumn(inherited DelphiObject);
end;

procedure TPyDelphiGlyphColumn.SetDelphiObject(const Value: TGlyphColumn);
begin
  inherited DelphiObject := Value;
end;

{ TPyDelphiGlyphCustomGrid }

class function TPyDelphiCustomGrid.DelphiObjectClass: TClass;
begin
  Result := TCustomGrid;
end;

function TPyDelphiCustomGrid.GetDelphiObject: TCustomGrid;
begin
  Result := TCustomGrid(inherited DelphiObject);
end;

procedure TPyDelphiCustomGrid.SetDelphiObject(const Value: TCustomGrid);
begin
  inherited DelphiObject := Value;
end;

{ TPyDelphiGrid }

class function TPyDelphiGrid.DelphiObjectClass: TClass;
begin
  Result := TGrid;
end;

function TPyDelphiGrid.GetDelphiObject: TGrid;
begin
  Result := TGrid(inherited DelphiObject);
end;

procedure TPyDelphiGrid.SetDelphiObject(const Value: TGrid);
begin
  inherited DelphiObject := Value;
end;

{ TPyDelphiStringGrid }

class function TPyDelphiStringGrid.DelphiObjectClass: TClass;
begin
  Result := TStringGrid;
end;

function TPyDelphiStringGrid.GetDelphiObject: TStringGrid;
begin
  Result := TStringGrid(inherited DelphiObject);
end;

procedure TPyDelphiStringGrid.SetDelphiObject(const Value: TStringGrid);
begin
  inherited DelphiObject := Value;
end;

initialization
  RegisteredUnits.Add(TGridsRegistration.Create);

end.
