unit WrapVclMedia;

interface

uses
  Vcl.MPlayer, WrapVclControls;

type
  TPyDelphiMediaPlayer = class(TPyDelphiCustomControl)
	private
		function GetDelphiObject: TMediaPlayer;
		procedure SetDelphiObject(const Value: TMediaPlayer);
	public
		class function DelphiObjectClass: TClass; override;
		property DelphiObject: TMediaPlayer read GetDelphiObject
			write SetDelphiObject;
	end;

implementation

uses
  TypInfo,
  WrapDelphi;

type
  TVCLMediaRegistration = class(TRegisteredUnit)
  public
    function Name: string; override;
    procedure RegisterWrappers(APyDelphiWrapper: TPyDelphiWrapper); override;
    procedure DefineVars(APyDelphiWrapper: TPyDelphiWrapper); override;
  end;


{ TVCLMediaRegistration }

procedure TVCLMediaRegistration.DefineVars(APyDelphiWrapper: TPyDelphiWrapper);
begin
  inherited;
  APyDelphiWrapper.DefineVar('btPlay', 'btPlay');
  APyDelphiWrapper.DefineVar('btPause', 'btPause');
  APyDelphiWrapper.DefineVar('btStop', 'btStop');
  APyDelphiWrapper.DefineVar('btNext', 'btNext');
  APyDelphiWrapper.DefineVar('btPrev', 'btPrev');
  APyDelphiWrapper.DefineVar('btStep', 'btStep');
  APyDelphiWrapper.DefineVar('btBack', 'btBack');
  APyDelphiWrapper.DefineVar('btRecord', 'btRecord');
  APyDelphiWrapper.DefineVar('btEject', 'btRecord');

  APyDelphiWrapper.DefineVar('mgEnabled', 'mgEnabled');
  APyDelphiWrapper.DefineVar('mgDisabled', 'mgDisabled');
  APyDelphiWrapper.DefineVar('mgColored', 'mgColored');

  APyDelphiWrapper.DefineVar('dtAutoSelect', 'dtAutoSelect');
  APyDelphiWrapper.DefineVar('dtAVIVideo', 'dtAVIVideo');
  APyDelphiWrapper.DefineVar('dtCDAudio', 'dtCDAudio');
  APyDelphiWrapper.DefineVar('dtDAT', 'dtDAT');
  APyDelphiWrapper.DefineVar('dtDigitalVideo', 'dtDigitalVideo');
  APyDelphiWrapper.DefineVar('dtMMMovie', 'dtMMMovie');
  APyDelphiWrapper.DefineVar('dtOther', 'dtOther');
  APyDelphiWrapper.DefineVar('dtOverlay', 'dtOverlay');
  APyDelphiWrapper.DefineVar('dtScanner', 'dtScanner');
  APyDelphiWrapper.DefineVar('dtSequencer', 'dtSequencer');
  APyDelphiWrapper.DefineVar('dtVCR', 'dtVCR');
  APyDelphiWrapper.DefineVar('dtVideodisc', 'dtVideodisc');
  APyDelphiWrapper.DefineVar('dtWaveAudio', 'dtWaveAudio');

  APyDelphiWrapper.DefineVar('tfMilliseconds', 'tfMilliseconds');
  APyDelphiWrapper.DefineVar('tfHMS', 'tfHMS');
  APyDelphiWrapper.DefineVar('tfMSF', 'tfMSF');
  APyDelphiWrapper.DefineVar('tfFrames', 'tfFrames');
  APyDelphiWrapper.DefineVar('tfSMPTE24', 'tfSMPTE24');
  APyDelphiWrapper.DefineVar('tfSMPTE25', 'tfSMPTE25');
  APyDelphiWrapper.DefineVar('tfSMPTE30', 'tfSMPTE30');
  APyDelphiWrapper.DefineVar('tfSMPTE30Drop', 'tfSMPTE30Drop');
  APyDelphiWrapper.DefineVar('tfBytes', 'tfBytes');
  APyDelphiWrapper.DefineVar('tfSamples', 'tfSamples');
  APyDelphiWrapper.DefineVar('tfTMSF', 'tfTMSF');

  APyDelphiWrapper.DefineVar('mpNotReady', 'mpNotReady');
  APyDelphiWrapper.DefineVar('mpStopped',' mpStopped');
  APyDelphiWrapper.DefineVar('mpPlaying', 'mpPlaying');
  APyDelphiWrapper.DefineVar('mpRecording', 'mpRecording');
  APyDelphiWrapper.DefineVar('mpSeeking', 'mpSeeking');
  APyDelphiWrapper.DefineVar('mpPaused', 'mpPaused');
  APyDelphiWrapper.DefineVar('mpOpen', 'mpOpen');

  APyDelphiWrapper.DefineVar('nvSuccessful', 'nvSuccessful');
  APyDelphiWrapper.DefineVar('nvSuperseded', 'nvSuperseded');
  APyDelphiWrapper.DefineVar('nvAborted', 'nvAborted');
  APyDelphiWrapper.DefineVar('nvFailure', 'nvFailure');

  APyDelphiWrapper.DefineVar('mpCanStep', 'mpCanStep');
  APyDelphiWrapper.DefineVar('mpCanEject', 'mpCanEject');
  APyDelphiWrapper.DefineVar('mpCanPlay', 'mpCanPlay');
  APyDelphiWrapper.DefineVar('mpCanRecord', 'mpCanRecord');
  APyDelphiWrapper.DefineVar('mpUsesWindow', 'mpUsesWindow');
end;

function TVCLMediaRegistration.Name: string;
begin
  Result := 'Media';
end;

procedure TVCLMediaRegistration.RegisterWrappers(
  APyDelphiWrapper: TPyDelphiWrapper);
begin
  inherited;
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiMediaPlayer);
end;

{ TPyDelphiMediaPlayer }

class function TPyDelphiMediaPlayer.DelphiObjectClass: TClass;
begin
  Result := TMediaPlayer;
end;

function TPyDelphiMediaPlayer.GetDelphiObject: TMediaPlayer;
begin
  Result := TMediaPlayer(inherited DelphiObject);
end;

procedure TPyDelphiMediaPlayer.SetDelphiObject(
  const Value: TMediaPlayer);
begin
  inherited DelphiObject := Value;
end;

initialization
  RegisteredUnits.Add(TVCLMediaRegistration.Create());

end.
