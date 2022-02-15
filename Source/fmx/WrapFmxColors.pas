unit WrapFmxColors;

interface

uses
  FMX.Colors,
  WrapFmxControls, WrapFmxStdCtrls, WrapFmxListBox;

type
  TPyDelphiColorBox = class(TPyDelphiControl)
	private
		function GetDelphiObject: TColorBox;
		procedure SetDelphiObject(const Value: TColorBox);
	public
		class function DelphiObjectClass: TClass; override;
		// Properties
		property DelphiObject: TColorBox read GetDelphiObject
			write SetDelphiObject;
	end;

  TPyDelphiColorQuad = class(TPyDelphiControl)
	private
		function GetDelphiObject: TColorQuad;
		procedure SetDelphiObject(const Value: TColorQuad);
	public
		class function DelphiObjectClass: TClass; override;
		// Properties
		property DelphiObject: TColorQuad read GetDelphiObject
			write SetDelphiObject;
	end;

  TPyDelphiColorPicker = class(TPyDelphiControl)
	private
		function GetDelphiObject: TColorPicker;
		procedure SetDelphiObject(const Value: TColorPicker);
	public
		class function DelphiObjectClass: TClass; override;
		// Properties
		property DelphiObject: TColorPicker read GetDelphiObject
			write SetDelphiObject;
	end;

  TPyDelphiGradientEdit = class(TPyDelphiControl)
	private
		function GetDelphiObject: TGradientEdit;
		procedure SetDelphiObject(const Value: TGradientEdit);
	public
		class function DelphiObjectClass: TClass; override;
		// Properties
		property DelphiObject: TGradientEdit read GetDelphiObject
			write SetDelphiObject;
	end;

  TPyDelphiColorPanel = class(TPyDelphiControl)
	private
		function GetDelphiObject: TColorPanel;
		procedure SetDelphiObject(const Value: TColorPanel);
	public
		class function DelphiObjectClass: TClass; override;
		// Properties
		property DelphiObject: TColorPanel read GetDelphiObject
			write SetDelphiObject;
	end;

  TPyDelphiComboColorBox = class(TPyDelphiStyledControl)
	private
		function GetDelphiObject: TComboColorBox;
		procedure SetDelphiObject(const Value: TComboColorBox);
	public
		class function DelphiObjectClass: TClass; override;
		// Properties
		property DelphiObject: TComboColorBox read GetDelphiObject
			write SetDelphiObject;
	end;

  TPyDelphiColorButton = class(TPyDelphiCustomButton)
	private
		function GetDelphiObject: TColorButton;
		procedure SetDelphiObject(const Value: TColorButton);
	public
		class function DelphiObjectClass: TClass; override;
		// Properties
		property DelphiObject: TColorButton read GetDelphiObject
			write SetDelphiObject;
	end;

  TPyDelphiColorListBox = class(TPyDelphiCustomListBox)
	private
		function GetDelphiObject: TColorListBox;
		procedure SetDelphiObject(const Value: TColorListBox);
	public
		class function DelphiObjectClass: TClass; override;
		// Properties
		property DelphiObject: TColorListBox read GetDelphiObject
			write SetDelphiObject;
	end;

  TPyDelphiCustomColorComboBox = class(TPyDelphiCustomComboBox)
	private
		function GetDelphiObject: TCustomColorComboBox;
		procedure SetDelphiObject(const Value: TCustomColorComboBox);
	public
		class function DelphiObjectClass: TClass; override;
		// Properties
		property DelphiObject: TCustomColorComboBox read GetDelphiObject
			write SetDelphiObject;
	end;

  TPyDelphiColorComboBox = class(TPyDelphiCustomColorComboBox)
	private
		function GetDelphiObject: TColorComboBox;
		procedure SetDelphiObject(const Value: TColorComboBox);
	public
		class function DelphiObjectClass: TClass; override;
		// Properties
		property DelphiObject: TColorComboBox read GetDelphiObject
			write SetDelphiObject;
	end;

implementation

uses
  WrapDelphi;

{ Register the wrappers, the globals and the constants }
type
	TColorsRegistration = class(TRegisteredUnit)
  public
    function Name: string; override;
    procedure RegisterWrappers(APyDelphiWrapper: TPyDelphiWrapper); override;
    procedure DefineVars(APyDelphiWrapper: TPyDelphiWrapper); override;
  end;

{ TColorsRegistration }

procedure TColorsRegistration.DefineVars(APyDelphiWrapper: TPyDelphiWrapper);
begin
  inherited;
end;

function TColorsRegistration.Name: string;
begin
	Result := 'Colors';
end;

procedure TColorsRegistration.RegisterWrappers(
  APyDelphiWrapper: TPyDelphiWrapper);
begin
  inherited;
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiColorBox);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiColorQuad);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiColorPicker);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiGradientEdit);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiColorPanel);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiComboColorBox);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiColorButton);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiColorListBox);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiCustomColorComboBox);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiColorComboBox);
end;

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

{ TPyDelphiColorQuad }

class function TPyDelphiColorQuad.DelphiObjectClass: TClass;
begin
  Result := TColorQuad;
end;

function TPyDelphiColorQuad.GetDelphiObject: TColorQuad;
begin
	Result := TColorQuad(inherited DelphiObject);
end;

procedure TPyDelphiColorQuad.SetDelphiObject(const Value: TColorQuad);
begin
	inherited DelphiObject := Value;
end;

{ TPyDelphiColorPicker }

class function TPyDelphiColorPicker.DelphiObjectClass: TClass;
begin
  Result := TColorPicker;
end;

function TPyDelphiColorPicker.GetDelphiObject: TColorPicker;
begin
	Result := TColorPicker(inherited DelphiObject);
end;

procedure TPyDelphiColorPicker.SetDelphiObject(const Value: TColorPicker);
begin
	inherited DelphiObject := Value;
end;

{ TPyDelphiGradientEdit }

class function TPyDelphiGradientEdit.DelphiObjectClass: TClass;
begin
  Result := TGradientEdit;
end;

function TPyDelphiGradientEdit.GetDelphiObject: TGradientEdit;
begin
	Result := TGradientEdit(inherited DelphiObject);
end;

procedure TPyDelphiGradientEdit.SetDelphiObject(
  const Value: TGradientEdit);
begin
	inherited DelphiObject := Value;
end;

{ TPyDelphiColorPanel }

class function TPyDelphiColorPanel.DelphiObjectClass: TClass;
begin
  Result := TColorPanel;
end;

function TPyDelphiColorPanel.GetDelphiObject: TColorPanel;
begin
	Result := TColorPanel(inherited DelphiObject);
end;

procedure TPyDelphiColorPanel.SetDelphiObject(const Value: TColorPanel);
begin
	inherited DelphiObject := Value;
end;

{ TPyDelphiComboColorBox }

class function TPyDelphiComboColorBox.DelphiObjectClass: TClass;
begin
  Result := TComboColorBox;
end;

function TPyDelphiComboColorBox.GetDelphiObject: TComboColorBox;
begin
	Result := TComboColorBox(inherited DelphiObject);
end;

procedure TPyDelphiComboColorBox.SetDelphiObject(const Value: TComboColorBox);
begin
	inherited DelphiObject := Value;
end;

{ TPyDelphiColorButton }

class function TPyDelphiColorButton.DelphiObjectClass: TClass;
begin
  Result := TColorButton;
end;

function TPyDelphiColorButton.GetDelphiObject: TColorButton;
begin
	Result := TColorButton(inherited DelphiObject);
end;

procedure TPyDelphiColorButton.SetDelphiObject(const Value: TColorButton);
begin
	inherited DelphiObject := Value;
end;

{ TPyDelphiColorListBox }

class function TPyDelphiColorListBox.DelphiObjectClass: TClass;
begin
  Result := TColorListBox;
end;

function TPyDelphiColorListBox.GetDelphiObject: TColorListBox;
begin
	Result := TColorListBox(inherited DelphiObject);
end;

procedure TPyDelphiColorListBox.SetDelphiObject(const Value: TColorListBox);
begin
	inherited DelphiObject := Value;
end;

{ TPyDelphiCustomColorComboBox }

class function TPyDelphiCustomColorComboBox.DelphiObjectClass: TClass;
begin
  Result := TCustomColorComboBox;
end;

function TPyDelphiCustomColorComboBox.GetDelphiObject: TCustomColorComboBox;
begin
	Result := TCustomColorComboBox(inherited DelphiObject);
end;

procedure TPyDelphiCustomColorComboBox.SetDelphiObject(
  const Value: TCustomColorComboBox);
begin
	inherited DelphiObject := Value;
end;

{ TPyDelphiColorComboBox }

class function TPyDelphiColorComboBox.DelphiObjectClass: TClass;
begin
  Result := TColorComboBox;
end;

function TPyDelphiColorComboBox.GetDelphiObject: TColorComboBox;
begin
	Result := TColorComboBox(inherited DelphiObject);
end;

procedure TPyDelphiColorComboBox.SetDelphiObject(const Value: TColorComboBox);
begin
	inherited DelphiObject := Value;
end;

initialization
  RegisteredUnits.Add(TColorsRegistration.Create());

end.
