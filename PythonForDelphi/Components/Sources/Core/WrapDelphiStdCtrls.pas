{$I Definition.Inc}

unit WrapDelphiStdCtrls;

interface

uses
  Classes, SysUtils, PythonEngine, WrapDelphi, WrapDelphiClasses,
  WrapDelphiControls, Windows, StdCtrls;

type
  TPyDelphiButton = class (TPyDelphiWinControl)
  private
    function  GetDelphiObject: TButton;
    procedure SetDelphiObject(const Value: TButton);
  public
    class function  DelphiObjectClass : TClass; override;
    // Properties
    property DelphiObject: TButton read GetDelphiObject write SetDelphiObject;
  end;

  TPyDelphiCheckBox = class (TPyDelphiWinControl)
  private
    function  GetDelphiObject: TCheckBox;
    procedure SetDelphiObject(const Value: TCheckBox);
  public
    class function  DelphiObjectClass : TClass; override;
    // Properties
    property DelphiObject: TCheckBox read GetDelphiObject write SetDelphiObject;
  end;

  TPyDelphiComboBox = class (TPyDelphiWinControl)
  private
    function  GetDelphiObject: TComboBox;
    procedure SetDelphiObject(const Value: TComboBox);
  public
    class function  DelphiObjectClass : TClass; override;
    // Properties
    property DelphiObject: TComboBox read GetDelphiObject write SetDelphiObject;
  end;

  TPyDelphiCustomEdit = class (TPyDelphiWinControl)
  private
    function  GetDelphiObject: TCustomEdit;
    procedure SetDelphiObject(const Value: TCustomEdit);
  protected
    // Property Getters
    function Get_SelStart( AContext : Pointer) : PPyObject; cdecl;
    // Property Setters
    function Set_SelStart( AValue : PPyObject; AContext : Pointer) : integer; cdecl;
  public
    class function  DelphiObjectClass : TClass; override;
    class procedure RegisterGetSets( PythonType : TPythonType ); override;
    // Properties
    property DelphiObject: TCustomEdit read GetDelphiObject write SetDelphiObject;
  end;

  TPyDelphiEdit = class (TPyDelphiCustomEdit)
  private
    function  GetDelphiObject: TEdit;
    procedure SetDelphiObject(const Value: TEdit);
  public
    class function  DelphiObjectClass : TClass; override;
    // Properties
    property DelphiObject: TEdit read GetDelphiObject write SetDelphiObject;
  end;

  TPyDelphiCustomMemo = class (TPyDelphiCustomEdit)
  private
    function  GetDelphiObject: TCustomMemo;
    procedure SetDelphiObject(const Value: TCustomMemo);
  public
    class function  DelphiObjectClass : TClass; override;
    // Properties
    property DelphiObject: TCustomMemo read GetDelphiObject write SetDelphiObject;
  end;

  TPyDelphiMemo = class (TPyDelphiCustomMemo)
  private
    function  GetDelphiObject: TMemo;
    procedure SetDelphiObject(const Value: TMemo);
  public
    class function  DelphiObjectClass : TClass; override;
    // Properties
    property DelphiObject: TMemo read GetDelphiObject write SetDelphiObject;
  end;

  TPyDelphiGroupBox = class (TPyDelphiWinControl)
  private
    function  GetDelphiObject: TGroupBox;
    procedure SetDelphiObject(const Value: TGroupBox);
  public
    class function  DelphiObjectClass : TClass; override;
    // Properties
    property DelphiObject: TGroupBox read GetDelphiObject write SetDelphiObject;
  end;

  TPyDelphiLabel = class (TPyDelphiControl)
  private
    function  GetDelphiObject: TLabel;
    procedure SetDelphiObject(const Value: TLabel);
  public
    class function  DelphiObjectClass : TClass; override;
    // Properties
    property DelphiObject: TLabel read GetDelphiObject write SetDelphiObject;
  end;

  TPyDelphiListBox = class (TPyDelphiWinControl)
  private
    function  GetDelphiObject: TListBox;
    procedure SetDelphiObject(const Value: TListBox);
  protected
    // Property Getters
    function Get_ItemIndex( AContext : Pointer) : PPyObject; cdecl;
    // Property Setters
    function Set_ItemIndex( AValue : PPyObject; AContext : Pointer) : integer; cdecl;
  public
    class function  DelphiObjectClass : TClass; override;
    class procedure RegisterGetSets( PythonType : TPythonType ); override;
    // Properties
    property DelphiObject: TListBox read GetDelphiObject write SetDelphiObject;
  end;

  TPyDelphiRadioButton = class (TPyDelphiWinControl)
  private
    function  GetDelphiObject: TRadioButton;
    procedure SetDelphiObject(const Value: TRadioButton);
  public
    class function  DelphiObjectClass : TClass; override;
    // Properties
    property DelphiObject: TRadioButton read GetDelphiObject write SetDelphiObject;
  end;

  TPyDelphiStaticText = class (TPyDelphiWinControl)
  private
    function  GetDelphiObject: TStaticText;
    procedure SetDelphiObject(const Value: TStaticText);
  public
    class function  DelphiObjectClass : TClass; override;
    // Properties
    property DelphiObject: TStaticText read GetDelphiObject write SetDelphiObject;
  end;

  TPyDelphiScrollBar = class (TPyDelphiWinControl)
  private
    function  GetDelphiObject: TScrollBar;
    procedure SetDelphiObject(const Value: TScrollBar);
  public
    class function  DelphiObjectClass : TClass; override;
    // Properties
    property DelphiObject: TScrollBar read GetDelphiObject write SetDelphiObject;
  end;

implementation

{ Register the wrappers, the globals and the constants }
type
  TStdCtrlsRegistration = class(TRegisteredUnit)
  public
    function Name : String; override;
    procedure RegisterWrappers(APyDelphiWrapper : TPyDelphiWrapper); override;
    procedure DefineVars(APyDelphiWrapper : TPyDelphiWrapper); override;
  end;

{ TStdCtrlsRegistration }

procedure TStdCtrlsRegistration.DefineVars(APyDelphiWrapper: TPyDelphiWrapper);
begin
  inherited;
end;

function TStdCtrlsRegistration.Name: String;
begin
  Result := 'StdCtrls';
end;

procedure TStdCtrlsRegistration.RegisterWrappers(APyDelphiWrapper: TPyDelphiWrapper);
begin
  inherited;
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiButton);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiCheckBox);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiComboBox);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiCustomEdit);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiEdit);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiCustomMemo);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiMemo);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiGroupBox);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiLabel);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiListBox);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiRadioButton);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiStaticText);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiScrollBar);
end;

{ TPyDelphiButton }

class function TPyDelphiButton.DelphiObjectClass: TClass;
begin
  Result := TButton;
end;

function TPyDelphiButton.GetDelphiObject: TButton;
begin
  Result := TButton(inherited DelphiObject);
end;

procedure TPyDelphiButton.SetDelphiObject(const Value: TButton);
begin
  inherited DelphiObject := Value;
end;

{ TPyDelphiCheckBox }

class function TPyDelphiCheckBox.DelphiObjectClass: TClass;
begin
  Result := TCheckBox;
end;

function TPyDelphiCheckBox.GetDelphiObject: TCheckBox;
begin
  Result := TCheckBox(inherited DelphiObject);
end;

procedure TPyDelphiCheckBox.SetDelphiObject(const Value: TCheckBox);
begin
  inherited DelphiObject := Value;
end;

{ TPyDelphiComboBox }

class function TPyDelphiComboBox.DelphiObjectClass: TClass;
begin
  Result := TComboBox;
end;

function TPyDelphiComboBox.GetDelphiObject: TComboBox;
begin
  Result := TComboBox(inherited DelphiObject);
end;

procedure TPyDelphiComboBox.SetDelphiObject(const Value: TComboBox);
begin
  inherited DelphiObject := Value;
end;

{ TPyDelphiEdit }

class function TPyDelphiCustomEdit.DelphiObjectClass: TClass;
begin
  Result := TCustomEdit;
end;

function TPyDelphiCustomEdit.GetDelphiObject: TCustomEdit;
begin
  Result := TCustomEdit(inherited DelphiObject);
end;

procedure TPyDelphiCustomEdit.SetDelphiObject(const Value: TCustomEdit);
begin
  inherited DelphiObject := Value;
end;

function TPyDelphiCustomEdit.Get_SelStart(AContext: Pointer): PPyObject;
begin
  Adjust(@Self);
  Result := GetPythonEngine.PyInt_FromLong(DelphiObject.SelStart);
end;

function TPyDelphiCustomEdit.Set_SelStart(AValue: PPyObject; AContext: Pointer): integer;
var
  _SelStart : Integer;
begin
  Adjust(@Self);
  if CheckIntAttribute(AValue, 'SelStart', _SelStart) then
  begin
    DelphiObject.SelStart := _SelStart;
    Result := 0;
  end
  else
    Result := -1;
end;

class procedure TPyDelphiCustomEdit.RegisterGetSets(PythonType: TPythonType);
begin
  inherited;
  with PythonType do
    begin
      AddGetSet('SelStart', @TPyDelphiCustomEdit.Get_SelStart, @TPyDelphiCustomEdit.Set_SelStart,
        'Returns/sets the position of the cursor.', nil);
    end;
end;

{ TPyDelphiEdit }

class function TPyDelphiEdit.DelphiObjectClass: TClass;
begin
  Result := TEdit;
end;

function TPyDelphiEdit.GetDelphiObject: TEdit;
begin
  Result := TEdit(inherited DelphiObject);
end;

procedure TPyDelphiEdit.SetDelphiObject(const Value: TEdit);
begin
  inherited DelphiObject := Value;
end;

{ TPyDelphiCustomMemo }

class function TPyDelphiCustomMemo.DelphiObjectClass: TClass;
begin
  Result := TCustomMemo;
end;

function TPyDelphiCustomMemo.GetDelphiObject: TCustomMemo;
begin
  Result := TCustomMemo(inherited DelphiObject);
end;

procedure TPyDelphiCustomMemo.SetDelphiObject(const Value: TCustomMemo);
begin
  inherited DelphiObject := Value;
end;

{ TPyDelphiMemo }

class function TPyDelphiMemo.DelphiObjectClass: TClass;
begin
  Result := TMemo;
end;

function TPyDelphiMemo.GetDelphiObject: TMemo;
begin
  Result := TMemo(inherited DelphiObject);
end;

procedure TPyDelphiMemo.SetDelphiObject(const Value: TMemo);
begin
  inherited DelphiObject := Value;
end;


{ TPyDelphiGroupBox }

class function TPyDelphiGroupBox.DelphiObjectClass: TClass;
begin
  Result := TGroupBox;
end;

function TPyDelphiGroupBox.GetDelphiObject: TGroupBox;
begin
  Result := TGroupBox(inherited DelphiObject);
end;

procedure TPyDelphiGroupBox.SetDelphiObject(const Value: TGroupBox);
begin
  inherited DelphiObject := Value;
end;


{ TPyDelphiLabel }

class function TPyDelphiLabel.DelphiObjectClass: TClass;
begin
  Result := TLabel;
end;

function TPyDelphiLabel.GetDelphiObject: TLabel;
begin
  Result := TLabel(inherited DelphiObject);
end;

procedure TPyDelphiLabel.SetDelphiObject(const Value: TLabel);
begin
  inherited DelphiObject := Value;
end;


{ TPyDelphiListBox }

class function TPyDelphiListBox.DelphiObjectClass: TClass;
begin
  Result := TListBox;
end;

function TPyDelphiListBox.GetDelphiObject: TListBox;
begin
  Result := TListBox(inherited DelphiObject);
end;

procedure TPyDelphiListBox.SetDelphiObject(const Value: TListBox);
begin
  inherited DelphiObject := Value;
end;

function TPyDelphiListBox.Set_ItemIndex(AValue: PPyObject; AContext: Pointer): integer;
var
  _index : Integer;
begin
  Adjust(@Self);
  if CheckIntAttribute(AValue, 'ItemIndex', _index) then
  begin
    DelphiObject.ItemIndex := _index;
    Result := 0;
  end
  else
    Result := -1;
end;

function TPyDelphiListBox.Get_ItemIndex(AContext: Pointer): PPyObject;
begin
  Adjust(@Self);
  Result := GetPythonEngine.PyInt_FromLong(DelphiObject.ItemIndex);
end;

class procedure TPyDelphiListBox.RegisterGetSets(PythonType: TPythonType);
begin
  inherited;
  with PythonType do
    begin
      AddGetSet('ItemIndex', @TPyDelphiListBox.Get_ItemIndex, @TPyDelphiListBox.Set_ItemIndex,
        'Indicates the position of the selected item.', nil);
    end;
end;


{ TPyDelphiRadioButton }

class function TPyDelphiRadioButton.DelphiObjectClass: TClass;
begin
  Result := TRadioButton;
end;

function TPyDelphiRadioButton.GetDelphiObject: TRadioButton;
begin
  Result := TRadioButton(inherited DelphiObject);
end;

procedure TPyDelphiRadioButton.SetDelphiObject(const Value: TRadioButton);
begin
  inherited DelphiObject := Value;
end;


{ TPyDelphiStaticText }

class function TPyDelphiStaticText.DelphiObjectClass: TClass;
begin
  Result := TStaticText;
end;

function TPyDelphiStaticText.GetDelphiObject: TStaticText;
begin
  Result := TStaticText(inherited DelphiObject);
end;

procedure TPyDelphiStaticText.SetDelphiObject(const Value: TStaticText);
begin
  inherited DelphiObject := Value;
end;


{ TPyDelphiScrollBar }

class function TPyDelphiScrollBar.DelphiObjectClass: TClass;
begin
  Result := TScrollBar;
end;

function TPyDelphiScrollBar.GetDelphiObject: TScrollBar;
begin
  Result := TScrollBar(inherited DelphiObject);
end;

procedure TPyDelphiScrollBar.SetDelphiObject(const Value: TScrollBar);
begin
  inherited DelphiObject := Value;
end;

initialization
  RegisteredUnits.Add( TStdCtrlsRegistration.Create );
  Classes.RegisterClasses([TButton, TCheckBox, TComboBox, TEdit, TMemo, TGroupBox,
                           TLabel, TListBox, TRadioButton, TStaticText, TScrollBar]);
end.
