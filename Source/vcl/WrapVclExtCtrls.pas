{$I ..\Definition.Inc}

unit WrapVclExtCtrls;

interface

uses
  Classes, SysUtils, PythonEngine, WrapDelphi, WrapDelphiClasses, WrapVclControls,
  Windows, ExtCtrls;

type
  TPyDelphiShape = class (TPyDelphiGraphicControl)
  private
    function  GetDelphiObject: TShape;
    procedure SetDelphiObject(const Value: TShape);
  public
    class function  DelphiObjectClass : TClass; override;
    // Properties
    property DelphiObject: TShape read GetDelphiObject write SetDelphiObject;
  end;

  TPyDelphiPaintBox = class (TPyDelphiGraphicControl)
  private
    function  GetDelphiObject: TPaintBox;
    procedure SetDelphiObject(const Value: TPaintBox);
  public
    class function  DelphiObjectClass : TClass; override;
    // Properties
    property DelphiObject: TPaintBox read GetDelphiObject write SetDelphiObject;
  end;

  TPyDelphiImage = class (TPyDelphiGraphicControl)
  private
    function  GetDelphiObject: TImage;
    procedure SetDelphiObject(const Value: TImage);
  public
    class function  DelphiObjectClass : TClass; override;
    // Properties
    property DelphiObject: TImage read GetDelphiObject write SetDelphiObject;
  end;

  TPyDelphiBevel = class (TPyDelphiGraphicControl)
  private
    function  GetDelphiObject: TBevel;
    procedure SetDelphiObject(const Value: TBevel);
  public
    class function  DelphiObjectClass : TClass; override;
    // Properties
    property DelphiObject: TBevel read GetDelphiObject write SetDelphiObject;
  end;

  TPyDelphiTimer = class (TPyDelphiComponent)
  private
    function  GetDelphiObject: TTimer;
    procedure SetDelphiObject(const Value: TTimer);
  public
    class function  DelphiObjectClass : TClass; override;
    // Properties
    property DelphiObject: TTimer read GetDelphiObject write SetDelphiObject;
  end;

  TPyDelphiPanel = class (TPyDelphiWinControl)
  private
    function  GetDelphiObject: TPanel;
    procedure SetDelphiObject(const Value: TPanel);
  public
    class function  DelphiObjectClass : TClass; override;
    // Properties
    property DelphiObject: TPanel read GetDelphiObject write SetDelphiObject;
  end;

  TPyDelphiPage = class (TPyDelphiWinControl)
  private
    function  GetDelphiObject: TPage;
    procedure SetDelphiObject(const Value: TPage);
  public
    class function  DelphiObjectClass : TClass; override;
    // Properties
    property DelphiObject: TPage read GetDelphiObject write SetDelphiObject;
  end;

  TPyDelphiNotebook = class (TPyDelphiWinControl)
  private
    function  GetDelphiObject: TNotebook;
    procedure SetDelphiObject(const Value: TNotebook);
  public
    class function  DelphiObjectClass : TClass; override;
    // Properties
    property DelphiObject: TNotebook read GetDelphiObject write SetDelphiObject;
  end;

  {$IFNDEF FPC}
  TPyDelphiHeader = class (TPyDelphiWinControl)
  private
    function  GetDelphiObject: THeader;
    procedure SetDelphiObject(const Value: THeader);
  public
    class function  DelphiObjectClass : TClass; override;
    // Properties
    property DelphiObject: THeader read GetDelphiObject write SetDelphiObject;
  end;
  {$ENDIF FPC}

  TPyDelphiRadioGroup = class (TPyDelphiWinControl)
  private
    function  GetDelphiObject: TRadioGroup;
    procedure SetDelphiObject(const Value: TRadioGroup);
  public
    class function  DelphiObjectClass : TClass; override;
    // Properties
    property DelphiObject: TRadioGroup read GetDelphiObject write SetDelphiObject;
  end;

  {$IFDEF FPC}
  TPyDelphiSplitter = class (TPyDelphiWinControl)
  {$ELSE FPC}
  TPyDelphiSplitter = class (TPyDelphiGraphicControl)
  {$ENDIF FPC}
  private
    function  GetDelphiObject: TSplitter;
    procedure SetDelphiObject(const Value: TSplitter);
  public
    class function  DelphiObjectClass : TClass; override;
    // Properties
    property DelphiObject: TSplitter read GetDelphiObject write SetDelphiObject;
  end;

  {$IFNDEF FPC}
  TPyDelphiControlBar = class (TPyDelphiWinControl)
  private
    function  GetDelphiObject: TControlBar;
    procedure SetDelphiObject(const Value: TControlBar);
  public
    class function  DelphiObjectClass : TClass; override;
    // Properties
    property DelphiObject: TControlBar read GetDelphiObject write SetDelphiObject;
  end;
  {$ENDIF FPC}

  TPyDelphiBoundLabel = class (TPyDelphiControl)
  private
    function  GetDelphiObject: TBoundLabel;
    procedure SetDelphiObject(const Value: TBoundLabel);
  public
    class function  DelphiObjectClass : TClass; override;
    // Properties
    property DelphiObject: TBoundLabel read GetDelphiObject write SetDelphiObject;
  end;

  TPyDelphiLabeledEdit = class (TPyDelphiWinControl)
  private
    function  GetDelphiObject: TLabeledEdit;
    procedure SetDelphiObject(const Value: TLabeledEdit);
  public
    class function  DelphiObjectClass : TClass; override;
    // Properties
    property DelphiObject: TLabeledEdit read GetDelphiObject write SetDelphiObject;
  end;

  {$IFNDEF FPC}
  TPyDelphiColorBox = class (TPyDelphiWinControl)
  private
    function  GetDelphiObject: TColorBox;
    procedure SetDelphiObject(const Value: TColorBox);
  public
    class function  DelphiObjectClass : TClass; override;
    // Properties
    property DelphiObject: TColorBox read GetDelphiObject write SetDelphiObject;
  end;
  {$ENDIF FPC}

implementation

{ Register the wrappers, the globals and the constants }
type
  TExtCtrlsRegistration = class(TRegisteredUnit)
  public
    function Name : string; override;
    procedure RegisterWrappers(APyDelphiWrapper : TPyDelphiWrapper); override;
    procedure DefineVars(APyDelphiWrapper : TPyDelphiWrapper); override;
  end;

{ TExtCtrlsRegistration }

procedure TExtCtrlsRegistration.DefineVars(APyDelphiWrapper: TPyDelphiWrapper);
begin
  inherited;
end;

function TExtCtrlsRegistration.Name: string;
begin
  Result := 'ExtCtrls';
end;

procedure TExtCtrlsRegistration.RegisterWrappers(APyDelphiWrapper: TPyDelphiWrapper);
begin
  inherited;
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiShape);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiPaintBox);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiImage);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiBevel);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiTimer);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiPanel);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiPage);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiNotebook);
  {$IFNDEF FPC}
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiHeader);
  {$ENDIF FPC}
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiRadioGroup);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiSplitter);
  {$IFNDEF FPC}
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiControlBar);
  {$ENDIF FPC}
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiBoundLabel);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiLabeledEdit);
  {$IFNDEF FPC}
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiColorBox);
  {$ENDIF FPC}
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


{ TPyDelphiBevel }

class function TPyDelphiBevel.DelphiObjectClass: TClass;
begin
  Result := TBevel;
end;

function TPyDelphiBevel.GetDelphiObject: TBevel;
begin
  Result := TBevel(inherited DelphiObject);
end;

procedure TPyDelphiBevel.SetDelphiObject(const Value: TBevel);
begin
  inherited DelphiObject := Value;
end;


{ TPyDelphiTimer }

class function TPyDelphiTimer.DelphiObjectClass: TClass;
begin
  Result := TTimer;
end;

function TPyDelphiTimer.GetDelphiObject: TTimer;
begin
  Result := TTimer(inherited DelphiObject);
end;

procedure TPyDelphiTimer.SetDelphiObject(const Value: TTimer);
begin
  inherited DelphiObject := Value;
end;


{ TPyDelphiPanel }

class function TPyDelphiPanel.DelphiObjectClass: TClass;
begin
  Result := TPanel;
end;

function TPyDelphiPanel.GetDelphiObject: TPanel;
begin
  Result := TPanel(inherited DelphiObject);
end;

procedure TPyDelphiPanel.SetDelphiObject(const Value: TPanel);
begin
  inherited DelphiObject := Value;
end;


{ TPyDelphiPage }

class function TPyDelphiPage.DelphiObjectClass: TClass;
begin
  Result := TPage;
end;

function TPyDelphiPage.GetDelphiObject: TPage;
begin
  Result := TPage(inherited DelphiObject);
end;

procedure TPyDelphiPage.SetDelphiObject(const Value: TPage);
begin
  inherited DelphiObject := Value;
end;


{ TPyDelphiNotebook }

class function TPyDelphiNotebook.DelphiObjectClass: TClass;
begin
  Result := TNotebook;
end;

function TPyDelphiNotebook.GetDelphiObject: TNotebook;
begin
  Result := TNotebook(inherited DelphiObject);
end;

procedure TPyDelphiNotebook.SetDelphiObject(const Value: TNotebook);
begin
  inherited DelphiObject := Value;
end;


{$IFNDEF FPC}
{ TPyDelphiHeader }

class function TPyDelphiHeader.DelphiObjectClass: TClass;
begin
  Result := THeader;
end;

function TPyDelphiHeader.GetDelphiObject: THeader;
begin
  Result := THeader(inherited DelphiObject);
end;

procedure TPyDelphiHeader.SetDelphiObject(const Value: THeader);
begin
  inherited DelphiObject := Value;
end;
{$ENDIF FPC}


{ TPyDelphiRadioGroup }

class function TPyDelphiRadioGroup.DelphiObjectClass: TClass;
begin
  Result := TRadioGroup;
end;

function TPyDelphiRadioGroup.GetDelphiObject: TRadioGroup;
begin
  Result := TRadioGroup(inherited DelphiObject);
end;

procedure TPyDelphiRadioGroup.SetDelphiObject(const Value: TRadioGroup);
begin
  inherited DelphiObject := Value;
end;


{ TPyDelphiSplitter }

class function TPyDelphiSplitter.DelphiObjectClass: TClass;
begin
  Result := TSplitter;
end;

function TPyDelphiSplitter.GetDelphiObject: TSplitter;
begin
  Result := TSplitter(inherited DelphiObject);
end;

procedure TPyDelphiSplitter.SetDelphiObject(const Value: TSplitter);
begin
  inherited DelphiObject := Value;
end;


{$IFNDEF FPC}
{ TPyDelphiControlBar }

class function TPyDelphiControlBar.DelphiObjectClass: TClass;
begin
  Result := TControlBar;
end;

function TPyDelphiControlBar.GetDelphiObject: TControlBar;
begin
  Result := TControlBar(inherited DelphiObject);
end;

procedure TPyDelphiControlBar.SetDelphiObject(const Value: TControlBar);
begin
  inherited DelphiObject := Value;
end;
{$ENDIF FPC}

{ TPyDelphiBoundLabel }

class function TPyDelphiBoundLabel.DelphiObjectClass: TClass;
begin
  Result := TBoundLabel;
end;

function TPyDelphiBoundLabel.GetDelphiObject: TBoundLabel;
begin
  Result := TBoundLabel(inherited DelphiObject);
end;

procedure TPyDelphiBoundLabel.SetDelphiObject(const Value: TBoundLabel);
begin
  inherited DelphiObject := Value;
end;


{ TPyDelphiLabeledEdit }

class function TPyDelphiLabeledEdit.DelphiObjectClass: TClass;
begin
  Result := TLabeledEdit;
end;

function TPyDelphiLabeledEdit.GetDelphiObject: TLabeledEdit;
begin
  Result := TLabeledEdit(inherited DelphiObject);
end;

procedure TPyDelphiLabeledEdit.SetDelphiObject(const Value: TLabeledEdit);
begin
  inherited DelphiObject := Value;
end;


{$IFNDEF FPC}
{ TPyDelphiColorBox }

class function TPyDelphiColorBox.DelphiObjectClass: TClass;
begin
  Result := TColorBox;
end;

function TPyDelphiColorBox.GetDelphiObject: TColorBox;
begin
  Result := TColorBox(inherited DelphiObject);
end;

procedure TPyDelphiColorBox.SetDelphiObject(const Value: TColorBox);
begin
  inherited DelphiObject := Value;
end;
{$ENDIF FPC}


initialization
  RegisteredUnits.Add( TExtCtrlsRegistration.Create );
  {$IFDEF FPC}
  Classes.RegisterClasses([TShape, TPaintBox, TImage, TBevel, TTimer, TPanel, TPage, TNotebook,
                           TRadioGroup, TSplitter, TBoundLabel, TLabeledEdit]);
  {$ELSE FPC}
  Classes.RegisterClasses([TShape, TPaintBox, TImage, TBevel, TTimer, TPanel, TPage, TNotebook, THeader,
                           TRadioGroup, TSplitter, TControlBar, TBoundLabel, TLabeledEdit, TColorBox]);
  {$ENDIF FPC}
end.
