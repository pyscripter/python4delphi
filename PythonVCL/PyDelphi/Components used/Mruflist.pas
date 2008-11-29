unit MRUFList;

(*    Implements a Recently-Used Files List     *)

(*  Constructed by Robert R. Marsh, S.J., 1995  *)
(* Use freely, distribute widely, charge nothing*)
(* If you like it you could always give some    *)
(* money to your favorite charity.              *)

(*  Comments, bug-reports, praise and blame to: *)
(*              RobMarsh@AOL.COM                *)

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, Menus, Registry;

type
  TRecentFileEvent = procedure(Sender: TObject; LatestFile: string) of object;

type
  TRecentFiles = class(TComponent)
  private
    FMenu       : TMenuItem;
    MDivider    : TMenuItem;
    FMaxFiles   : integer;
    FStAlone    : boolean;
    FDelete     : boolean;
    FHKey       : String;
    FLatestFile : String;
    FOnClick    : TRecentFileEvent;
    procedure SetLatestFile(value: string);
    procedure SetMenu(value: TMenuItem);
    procedure SetMaxFiles(value: integer);
    procedure MenuOnClick(Sender: TObject);
    procedure SetAlone(value: boolean);
    function DividerPlace: integer;
  protected
    procedure Click(RecentFile: string);
  public
    constructor Create(AOwner: TComponent); override;
    procedure SaveToRegistry;
    procedure LoadFromRegistry;
    property LatestFile: string read FLatestFile write SetLatestFile;
  published
    property Menu           : TMenuItem read FMenu write SetMenu;
    property MaxFiles       : integer read FMaxFiles write SetMaxFiles;
    property RegistryKey    : string read FHKey write FHKey;
    property StandAloneMenu : boolean read FStAlone write SetAlone;
    property DeleteEntry    : boolean read FDelete write FDelete;
    property OnClick        : TRecentFileEvent read FOnClick write FOnClick;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Samples', [TRecentFiles]);
end;

{we frequently need to know both if the Divider}
{has been added and, if so, where it is}
function TRecentFiles.DividerPlace: integer;
begin
  Result:=-1;
  if FMenu <> nil then
    Result:=Menu.IndexOf(MDivider);
end;

procedure TRecentFiles.SetMenu(value: TMenuItem);
begin
  FMenu:=value;
end;

procedure TRecentFiles.SetAlone(value: boolean);
begin
  if Menu<>nil then
    Menu.Enabled:=Menu.Count<0;
  FStAlone:=value;
end;

procedure TRecentFiles.SetMaxFiles(value: integer);
begin
{the Max value of MaxFiles is 9}
  if value <=9 then
    FMaxFiles:=value
  else
    FMaxFiles:=9;
  if (FMenu <> nil) and (DividerPlace <> -1) then begin
    {trim off any entries more MaxFiles}
    while Menu.Count > DividerPlace + FMaxFiles + 1 do
      Menu.Delete(Menu.Count-1);
    {if neccesary delete the divider too}
    if (FMaxFiles=0) and (not FStAlone) then
      Menu.Delete(Menu.Count-1);
  end;
end;

procedure TRecentFiles.SetLatestFile(value: string);
var
  NewMenuItem: TMenuItem;
  n          : integer;
  Thiscaption: string;
  OldPlace   : integer;
  DividerPos : integer;
begin
  FLatestFile:=value;
  if (Menu <> nil) and (MaxFiles > 0) and (FLatestFile <> '') and FileExists(FLatestFile) then begin
    { No need to check, must always be enabled }
    Menu.Enabled:=true;
    {special case - the divider}
    if (DividerPlace<0) and (not FStAlone) then
      Menu.Add(MDivider);
    {is the new Name already there?}
    DividerPos:=DividerPlace;
    n:=DividerPos+1;
    OldPlace:=n;
    while (n < Menu.Count) do begin
      if (FLatestFile=Copy(Menu.Items[n].Caption,4,256)) then begin
        OldPlace:=n;
        Break;
      end else
        Inc(n);
    end;
    if n >= Menu.Count then begin
      NewMenuItem:=TMenuItem.Create(Self);
      NewMenuItem.Caption:='&1 '+FLatestFile;
      {what happens if we click it}
      NewMenuItem.OnClick:=MenuOnClick;
      Menu.Insert(DividerPos+1,NewMenuItem);
    end else begin
      NewMenuItem:=Menu.Items[OldPlace];
      Menu.Delete(OldPlace);
      Menu.Insert(DividerPos+1,NewMenuItem);
    end;
    {now change the 'hot' keys}
    for n:=DividerPos+1 to Menu.Count - 1 do begin
      ThisCaption:=Menu.Items[n].Caption;
      ThisCaption[2]:=Chr(n - DividerPos + Ord('1') -1);
      Menu.Items[n].Caption:=ThisCaption;
    end;
    {delete any excess items}
    if Menu.Count > DividerPos + MaxFiles + 1 then
      Menu.Delete(Menu.Count-1);
  end;
end;

procedure TRecentFiles.Click(RecentFile: string);
begin
  if Assigned(FOnClick) then FOnClick(Self,RecentFile);
end;

procedure TRecentFiles.SaveToRegistry;
var
  Registry  : TRegistry;
  n         : integer;
  DividerPos: integer;
begin
  if Menu <> nil then begin
    if (RegistryKey='') then
      RegistryKey:='\Software\'+Application.Title;
    Registry:=TRegistry.Create;
    with Registry do begin
      RootKey:=HKEY_CURRENT_USER;
      if KeyExists(RegistryKey+'\FileHistory') then
        DeleteKey(RegistryKey+'\FileHistory');
      if OpenKey(RegistryKey+'\FileHistory',true) then begin
        if (Menu <> nil) then begin
          DividerPos:=DividerPlace;
          for n:=DividerPos+1 to Menu.Count-1 do
            WriteString('File'+Chr(n+Ord('1')-1-DividerPos),Copy(Menu.Items[n].Caption,4,256));
        end;
        CloseKey;
      end;
      Free;
    end;
  end;
end;

procedure TRecentFiles.LoadFromRegistry;
var
  Registry: TRegistry;
  n       : integer;
  Name,S  : String;
begin
  if (Menu<>nil) then begin
    if (RegistryKey='') then
      RegistryKey:='\Software\'+Application.Title;
    Registry:=TRegistry.Create;
    with Registry do begin
      RootKey:=HKEY_CURRENT_USER;
      if OpenKey(RegistryKey+'\FileHistory',false) then begin
        n:=MaxFiles;
        repeat
          S:='File'+Chr(n+Ord('1')-1);
          if ValueExists(S) then
            Name:=ReadString(S)
          else
            Name:='';
          if Name<>'' then
            LatestFile:=Name;
          Dec(n);
        until (n=0);
        CloseKey;
      end;
      Free;
    end;
    if FStAlone then
      Menu.Enabled:=Menu.Count>0;
  end;
end;

constructor TRecentFiles.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FMaxFiles:=0;
  FMenu:=nil;
  MDivider:=TMenuItem.Create(Self);
  MDivider.Caption:='-';
  FStAlone:=false;
end;

procedure TRecentFiles.MenuOnClick(Sender: TObject);
var
  Name: string;
  n: integer;
  Thiscaption: string;
  DividerPos : integer;
begin
  Name:=Copy(TMenuItem(Sender).Caption,4,256);
  if FDelete then
    Menu.Delete(Menu.IndexOf(TMenuItem(Sender)));
  if (Menu<>nil) and FStAlone then
    Menu.Enabled:=Menu.Count>0;
  DividerPos:=DividerPlace;
  if (Menu.Count-1=DividerPos) and (not FStAlone) then
    Menu.Delete(DividerPos)
  else begin
    for n:=DividerPos+1 to Menu.Count-1 do begin
      ThisCaption:=Menu.Items[n].Caption;
      ThisCaption[2]:=Chr(n - DividerPos + Ord('1') -1);
      Menu.Items[n].Caption:=ThisCaption;
    end;
  end;
  Click(Name);
end;

end.
