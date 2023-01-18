unit WrapFmxDataBind;

interface

uses
  FMX.Bind.Navigator,
  WrapFmxActnList;

type
  TPyDelphiFMXBindNavigateAction = class(TPyDelphiAction)
	private
		function GetDelphiObject: TFMXBindNavigateAction;
		procedure SetDelphiObject(const Value: TFMXBindNavigateAction);
	public
		class function DelphiObjectClass: TClass; override;
		property DelphiObject: TFMXBindNavigateAction read GetDelphiObject
			write SetDelphiObject;
	end;

  TPyDelphiFMXBindNavigateFirst = class(TPyDelphiFMXBindNavigateAction)
	private
		function GetDelphiObject: TFMXBindNavigateFirst;
		procedure SetDelphiObject(const Value: TFMXBindNavigateFirst);
	public
		class function DelphiObjectClass: TClass; override;
		property DelphiObject: TFMXBindNavigateFirst read GetDelphiObject
			write SetDelphiObject;
	end;

  TPyDelphiFMXBindNavigatePrior = class(TPyDelphiFMXBindNavigateAction)
	private
		function GetDelphiObject: TFMXBindNavigatePrior;
		procedure SetDelphiObject(const Value: TFMXBindNavigatePrior);
	public
		class function DelphiObjectClass: TClass; override;
		property DelphiObject: TFMXBindNavigatePrior read GetDelphiObject
			write SetDelphiObject;
	end;

  TPyDelphiFMXBindNavigateNext = class(TPyDelphiFMXBindNavigateAction)
	private
		function GetDelphiObject: TFMXBindNavigateNext;
		procedure SetDelphiObject(const Value: TFMXBindNavigateNext);
	public
		class function DelphiObjectClass: TClass; override;
		property DelphiObject: TFMXBindNavigateNext read GetDelphiObject
			write SetDelphiObject;
	end;

  TPyDelphiFMXBindNavigateLast = class(TPyDelphiFMXBindNavigateAction)
	private
		function GetDelphiObject: TFMXBindNavigateLast;
		procedure SetDelphiObject(const Value: TFMXBindNavigateLast);
	public
		class function DelphiObjectClass: TClass; override;
		property DelphiObject: TFMXBindNavigateLast read GetDelphiObject
			write SetDelphiObject;
	end;

  TPyDelphiFMXBindNavigateInsert = class(TPyDelphiFMXBindNavigateAction)
	private
		function GetDelphiObject: TFMXBindNavigateInsert;
		procedure SetDelphiObject(const Value: TFMXBindNavigateInsert);
	public
		class function DelphiObjectClass: TClass; override;
		property DelphiObject: TFMXBindNavigateInsert read GetDelphiObject
			write SetDelphiObject;
	end;

  TPyDelphiFMXBindNavigateDelete = class(TPyDelphiFMXBindNavigateAction)
	private
		function GetDelphiObject: TFMXBindNavigateDelete;
		procedure SetDelphiObject(const Value: TFMXBindNavigateDelete);
	public
		class function DelphiObjectClass: TClass; override;
		property DelphiObject: TFMXBindNavigateDelete read GetDelphiObject
			write SetDelphiObject;
	end;

  TPyDelphiFMXBindNavigateEdit = class(TPyDelphiFMXBindNavigateAction)
	private
		function GetDelphiObject: TFMXBindNavigateEdit;
		procedure SetDelphiObject(const Value: TFMXBindNavigateEdit);
	public
		class function DelphiObjectClass: TClass; override;
		property DelphiObject: TFMXBindNavigateEdit read GetDelphiObject
			write SetDelphiObject;
	end;

  TPyDelphiFMXBindNavigatePost = class(TPyDelphiFMXBindNavigateAction)
	private
		function GetDelphiObject: TFMXBindNavigatePost;
		procedure SetDelphiObject(const Value: TFMXBindNavigatePost);
	public
		class function DelphiObjectClass: TClass; override;
		property DelphiObject: TFMXBindNavigatePost read GetDelphiObject
			write SetDelphiObject;
	end;

  TPyDelphiFMXBindNavigateCancel = class(TPyDelphiFMXBindNavigateAction)
	private
		function GetDelphiObject: TFMXBindNavigateCancel;
		procedure SetDelphiObject(const Value: TFMXBindNavigateCancel);
	public
		class function DelphiObjectClass: TClass; override;
		property DelphiObject: TFMXBindNavigateCancel read GetDelphiObject
			write SetDelphiObject;
	end;

  TPyDelphiFMXBindNavigateRefresh = class(TPyDelphiFMXBindNavigateAction)
	private
		function GetDelphiObject: TFMXBindNavigateRefresh;
		procedure SetDelphiObject(const Value: TFMXBindNavigateRefresh);
	public
		class function DelphiObjectClass: TClass; override;
		property DelphiObject: TFMXBindNavigateRefresh read GetDelphiObject
			write SetDelphiObject;
	end;

  TPyDelphiFMXBindNavigateApplyUpdates = class(TPyDelphiFMXBindNavigateAction)
	private
		function GetDelphiObject: TFMXBindNavigateApplyUpdates;
		procedure SetDelphiObject(const Value: TFMXBindNavigateApplyUpdates);
	public
		class function DelphiObjectClass: TClass; override;
		property DelphiObject: TFMXBindNavigateApplyUpdates read GetDelphiObject
			write SetDelphiObject;
	end;

  TPyDelphiFMXBindNavigateCancelUpdates = class(TPyDelphiFMXBindNavigateAction)
	private
		function GetDelphiObject: TFMXBindNavigateCancelUpdates;
		procedure SetDelphiObject(const Value: TFMXBindNavigateCancelUpdates);
	public
		class function DelphiObjectClass: TClass; override;
		property DelphiObject: TFMXBindNavigateCancelUpdates read GetDelphiObject
			write SetDelphiObject;
	end;

implementation

uses
  WrapDelphi;

{ Register the wrappers, the globals and the constants }
type
  TFmxDataBindRegistration = class(TRegisteredUnit)
  public
    function Name : string; override;
    procedure RegisterWrappers(APyDelphiWrapper : TPyDelphiWrapper); override;
    procedure DefineVars(APyDelphiWrapper : TPyDelphiWrapper); override;
  end;

{ TFmxDataBindRegistration }

procedure TFmxDataBindRegistration.DefineVars(
  APyDelphiWrapper: TPyDelphiWrapper);
begin
  inherited;
end;

function TFmxDataBindRegistration.Name: string;
begin
  Result := 'FMX.DataBind';
end;

procedure TFmxDataBindRegistration.RegisterWrappers(
  APyDelphiWrapper: TPyDelphiWrapper);
begin
  inherited;
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiFMXBindNavigateAction);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiFMXBindNavigateFirst);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiFMXBindNavigatePrior);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiFMXBindNavigateNext);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiFMXBindNavigateLast);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiFMXBindNavigateInsert);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiFMXBindNavigateDelete);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiFMXBindNavigateEdit);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiFMXBindNavigatePost);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiFMXBindNavigateCancel);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiFMXBindNavigateRefresh);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiFMXBindNavigateApplyUpdates);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiFMXBindNavigateCancelUpdates);
end;

{ TPyDelphiFMXBindNavigateAction }

class function TPyDelphiFMXBindNavigateAction.DelphiObjectClass: TClass;
begin
  Result := TFMXBindNavigateAction;
end;

function TPyDelphiFMXBindNavigateAction.GetDelphiObject: TFMXBindNavigateAction;
begin
  Result := TFMXBindNavigateAction(inherited DelphiObject);
end;

procedure TPyDelphiFMXBindNavigateAction.SetDelphiObject(
  const Value: TFMXBindNavigateAction);
begin
  inherited DelphiObject := Value;
end;

{ TPyDelphiFMXBindNavigateFirst }

class function TPyDelphiFMXBindNavigateFirst.DelphiObjectClass: TClass;
begin
  Result := TFMXBindNavigateFirst;
end;

function TPyDelphiFMXBindNavigateFirst.GetDelphiObject: TFMXBindNavigateFirst;
begin
  Result := TFMXBindNavigateFirst(inherited DelphiObject);
end;

procedure TPyDelphiFMXBindNavigateFirst.SetDelphiObject(
  const Value: TFMXBindNavigateFirst);
begin
  inherited DelphiObject := Value;
end;

{ TPyDelphiFMXBindNavigatePrior }

class function TPyDelphiFMXBindNavigatePrior.DelphiObjectClass: TClass;
begin
  Result := TFMXBindNavigatePrior;
end;

function TPyDelphiFMXBindNavigatePrior.GetDelphiObject: TFMXBindNavigatePrior;
begin
  Result := TFMXBindNavigatePrior(inherited DelphiObject);
end;

procedure TPyDelphiFMXBindNavigatePrior.SetDelphiObject(
  const Value: TFMXBindNavigatePrior);
begin
  inherited DelphiObject := Value;
end;

{ TPyDelphiFMXBindNavigateNext }

class function TPyDelphiFMXBindNavigateNext.DelphiObjectClass: TClass;
begin
  Result := TFMXBindNavigateNext;
end;

function TPyDelphiFMXBindNavigateNext.GetDelphiObject: TFMXBindNavigateNext;
begin
  Result := TFMXBindNavigateNext(inherited DelphiObject);
end;

procedure TPyDelphiFMXBindNavigateNext.SetDelphiObject(
  const Value: TFMXBindNavigateNext);
begin
  inherited DelphiObject := Value;
end;

{ TPyDelphiFMXBindNavigateLast }

class function TPyDelphiFMXBindNavigateLast.DelphiObjectClass: TClass;
begin
  Result := TFMXBindNavigateLast;
end;

function TPyDelphiFMXBindNavigateLast.GetDelphiObject: TFMXBindNavigateLast;
begin
  Result := TFMXBindNavigateLast(inherited DelphiObject);
end;

procedure TPyDelphiFMXBindNavigateLast.SetDelphiObject(
  const Value: TFMXBindNavigateLast);
begin
  inherited DelphiObject := Value;
end;

{ TPyDelphiFMXBindNavigateInsert }

class function TPyDelphiFMXBindNavigateInsert.DelphiObjectClass: TClass;
begin
  Result := TFMXBindNavigateInsert;
end;

function TPyDelphiFMXBindNavigateInsert.GetDelphiObject: TFMXBindNavigateInsert;
begin
  Result := TFMXBindNavigateInsert(inherited DelphiObject);
end;

procedure TPyDelphiFMXBindNavigateInsert.SetDelphiObject(
  const Value: TFMXBindNavigateInsert);
begin
  inherited DelphiObject := Value;
end;

{ TPyDelphiFMXBindNavigateDelete }

class function TPyDelphiFMXBindNavigateDelete.DelphiObjectClass: TClass;
begin
  Result := TFMXBindNavigateDelete;
end;

function TPyDelphiFMXBindNavigateDelete.GetDelphiObject: TFMXBindNavigateDelete;
begin
  Result := TFMXBindNavigateDelete(inherited DelphiObject);
end;

procedure TPyDelphiFMXBindNavigateDelete.SetDelphiObject(
  const Value: TFMXBindNavigateDelete);
begin
  inherited DelphiObject := Value;
end;

{ TPyDelphiFMXBindNavigateEdit }

class function TPyDelphiFMXBindNavigateEdit.DelphiObjectClass: TClass;
begin
  Result := TFMXBindNavigateEdit;
end;

function TPyDelphiFMXBindNavigateEdit.GetDelphiObject: TFMXBindNavigateEdit;
begin
  Result := TFMXBindNavigateEdit(inherited DelphiObject);
end;

procedure TPyDelphiFMXBindNavigateEdit.SetDelphiObject(
  const Value: TFMXBindNavigateEdit);
begin
  inherited DelphiObject := Value;
end;

{ TPyDelphiFMXBindNavigatePost }

class function TPyDelphiFMXBindNavigatePost.DelphiObjectClass: TClass;
begin
  Result := TFMXBindNavigatePost;
end;

function TPyDelphiFMXBindNavigatePost.GetDelphiObject: TFMXBindNavigatePost;
begin
  Result := TFMXBindNavigatePost(inherited DelphiObject);
end;

procedure TPyDelphiFMXBindNavigatePost.SetDelphiObject(
  const Value: TFMXBindNavigatePost);
begin
  inherited DelphiObject := Value;
end;

{ TPyDelphiFMXBindNavigateCancel }

class function TPyDelphiFMXBindNavigateCancel.DelphiObjectClass: TClass;
begin
  Result := TFMXBindNavigateCancel;
end;

function TPyDelphiFMXBindNavigateCancel.GetDelphiObject: TFMXBindNavigateCancel;
begin
  Result := TFMXBindNavigateCancel(inherited DelphiObject);
end;

procedure TPyDelphiFMXBindNavigateCancel.SetDelphiObject(
  const Value: TFMXBindNavigateCancel);
begin
  inherited DelphiObject := Value;
end;

{ TPyDelphiFMXBindNavigateRefresh }

class function TPyDelphiFMXBindNavigateRefresh.DelphiObjectClass: TClass;
begin
  Result := TFMXBindNavigateRefresh;
end;

function TPyDelphiFMXBindNavigateRefresh.GetDelphiObject: TFMXBindNavigateRefresh;
begin
  Result := TFMXBindNavigateRefresh(inherited DelphiObject);
end;

procedure TPyDelphiFMXBindNavigateRefresh.SetDelphiObject(
  const Value: TFMXBindNavigateRefresh);
begin
  inherited DelphiObject := Value;
end;

{ TPyDelphiFMXBindNavigateApplyUpdates }

class function TPyDelphiFMXBindNavigateApplyUpdates.DelphiObjectClass: TClass;
begin
  Result := TFMXBindNavigateApplyUpdates;
end;

function TPyDelphiFMXBindNavigateApplyUpdates.GetDelphiObject: TFMXBindNavigateApplyUpdates;
begin
  Result := TFMXBindNavigateApplyUpdates(inherited DelphiObject);
end;

procedure TPyDelphiFMXBindNavigateApplyUpdates.SetDelphiObject(
  const Value: TFMXBindNavigateApplyUpdates);
begin
  inherited DelphiObject := Value;
end;

{ TPyDelphiFMXBindNavigateCancelUpdates }

class function TPyDelphiFMXBindNavigateCancelUpdates.DelphiObjectClass: TClass;
begin
  Result := TFMXBindNavigateCancelUpdates;
end;

function TPyDelphiFMXBindNavigateCancelUpdates.GetDelphiObject: TFMXBindNavigateCancelUpdates;
begin
  Result := TFMXBindNavigateCancelUpdates(inherited DelphiObject);
end;

procedure TPyDelphiFMXBindNavigateCancelUpdates.SetDelphiObject(
  const Value: TFMXBindNavigateCancelUpdates);
begin
  inherited DelphiObject := Value;
end;

initialization
  RegisteredUnits.Add(TFmxDataBindRegistration.Create);

end.
