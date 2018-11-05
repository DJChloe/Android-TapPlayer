unit UnitAudio;

interface

uses
System.SysUtils,
System.Classes,
System.SyncObjs,
System.Generics.Collections,
FMX.Dialogs,
Androidapi.Helpers,
AndroidApi.JNI, Androidapi.JNI.Media,
Androidapi.JNI.GraphicsContentViewText,
Androidapi.JNI.Net,
Androidapi.JNIBridge,
Androidapi.JNI.JavaTypes,
Androidapi.JNI.Telephony;

type
{Thread Audio }
TPlayState = (psStopped, psPlaying, psPaused, psInterrupted,psNil);
TState = (sUnitialized, sInitialized,sNoStaticData,sNil);

TPlayStateChangedEvent = procedure (APlayState: TPlayState) of object;
TStateChangedEvent = procedure (AState: TState) of object;
TTimeChangedEvent = procedure (ATime: single) of object;
TDurationChangedEvent = procedure (ADuration: single) of object;
TCompleteEvent=procedure of object;

TAudioCap=record
    SampleRate:integer;
    BufferSize:integer;
end;
TPCM_Player=class;

TPlaybackPositionUpdateListener = class(TJavaLocal, JAudioTrack_OnPlaybackPositionUpdateListener)
  private
    //Objet pour lequel il faut transmettre  la notification
    FOwner : TPCM_Player;
  public
    constructor Create(AOwner : TPCM_Player);
    //Note : la convention d'appel cdecl;
    //les implémentation doivent utiliser cette convention autrement
    //on aura erreurs de compilation.
    procedure OnMarkerReached(track: JAudioTrack); cdecl;
    procedure OnPeriodicNotification(track: JAudioTrack); cdecl;
end;

TPhoneStateListener=class(TJavaLocal, JPhoneStateListener)
    private
    FOwner : TPCM_Player;
  public
    constructor Create(AOwner : TPCM_Player);
    procedure OnCallStateChanged(state: Integer; incomingNumber: JString); cdecl;
end;

TPCM_Player=class
  private
     FSampleRate: Integer;
     FPlayState:TPlayState;
     FState:TState;
     FTime:single;
     FDuration:single;
     FComplete:boolean;
     TelephonyManager:JTelephonyManager;
     AudioTrack: JAudioTrack;
     AudioStream: TJavaArray<Byte>;
     AudioDataSize: Integer;
     AudioPausePosition : Integer;
     FOnPlayStateChange:TPlayStateChangedEvent;
     FOnStateChange:TStateChangedEvent;
     FOnTimeChange:TTimeChangedEvent;
     FOnDurationChange:TDurationChangedEvent;
     FOnComplete:TCompleteEvent;
     PlaybackPositionUpdateListener:TPlaybackPositionUpdateListener;
     PhoneStateListener:TPhoneStateListener;
     function GetPlayState:TPlayState;
     function GetState:TState;
     procedure SetPlayState(const APlayState: TPlayState);
     procedure SetState(const AState: TState);
     procedure SetTime(const ATime: Single);
     procedure SetDuration(const ADuration: Single);
     function GetTime: Single;
     function GetDuration: Single;
  protected
    procedure Release;
    procedure DoMarkerReached;
    procedure DoPeriodicNotification;
    procedure DoOnCallStateChanged(state: Integer);
    procedure Updates;
  public
    constructor Create(SampleRate: integer);
    destructor Destroy; override;
    procedure LoadStream(AStream:TMemoryStream);
    procedure FirstPlay;
    procedure Play;
    procedure Pause;
    procedure PlayPause;
    procedure Stop;
    property PlayState:TPlayState read FPlayState;
    property State:TState read FState;
    property Time: Single read FTime;
    property Duration: Single read FDuration;
    property Complete:boolean read FComplete;
    property OnPlayStateChange:TPlayStateChangedEvent read FOnPlayStateChange write FOnPlayStateChange;
    property OnStateChange:TStateChangedEvent read FOnStateChange write FOnStateChange;
    property OnTimeChange:TTimeChangedEvent read FOnTimeChange write FOnTimeChange;
    property OnDurationChange:TDurationChangedEvent read FOnDurationChange write FOnDurationChange;
    property OnComplete:TCompleteEvent read FOnComplete write FOnComplete;
end;

function getMinSupportedSampleRate(const AnArrayOfSampleRatesToTest: array of Integer):TAudioCap;


implementation

function getMinSupportedSampleRate(const AnArrayOfSampleRatesToTest: array of Integer):TAudioCap;
var i:integer;
    bufsize:integer;
begin
  result.SampleRate:=-1;
  result.BufferSize:=-1;
  i:=Low(AnArrayOfSampleRatesToTest);
  while ((result.SampleRate<0) and (i<=High(AnArrayOfSampleRatesToTest))) do
  begin
    bufsize := TJAudioRecord.JavaClass.getMinBufferSize(AnArrayOfSampleRatesToTest[i],
            TJAudioFormat.JavaClass.CHANNEL_IN_MONO,
            TJAudioFormat.JavaClass.ENCODING_PCM_8BIT);
    if ((bufsize <> TJAudioRecord.JavaClass.ERROR)
       and (bufsize <> TJAudioRecord.JavaClass.ERROR_BAD_VALUE)
       and (bufsize > 0))
    then begin
           result.SampleRate:= AnArrayOfSampleRatesToTest[i];
           result.BufferSize:=bufsize;
    end;
    inc(i);
  end;
end;

constructor TPCM_Player.Create(SampleRate: integer);
var
  TelephonyManagerObj: JObject;
begin
  inherited Create;
  FSampleRate:=SampleRate;
  FPlayState:=psNil;
  FState:=sNil;
  FTime:=0;
  FDuration:=0;
  AudioDataSize:=0;
  FComplete:=false;
  TelephonyManagerObj:= TAndroidHelper.Context.getSystemService(
    TJContext.JavaClass.TELEPHONY_SERVICE);
  if TelephonyManagerObj <> nil then
    TelephonyManager := TJTelephonyManager.Wrap(TelephonyManagerObj);


  PlaybackPositionUpdateListener:=TPlaybackPositionUpdateListener.Create(self);
  PhoneStateListener:=TPhoneStateListener.Create(self);
  if TelephonyManager <> nil then
      TelephonyManager.listen(PhoneStateListener, TJPhoneStateListener.JavaClass.LISTEN_CALL_STATE);
  updates;
end;

destructor TPCM_Player.Destroy;
begin
  inherited;
end;

procedure TPCM_Player.LoadStream(AStream:TMemoryStream);
begin
  if Assigned(AudioTrack) then
  begin
    if AudioTrack.getPlayState<>TJAudioTrack.JavaClass.PLAYSTATE_STOPPED then exit;
    AudioTrack.release;
  end;
  FComplete:=false;
  //AudioDataSize := TJAudioTrack.JavaClass.getMinBufferSize(FSampleRate,
  //                   TJAudioFormat.JavaClass.CHANNEL_OUT_MONO,
  //                   TJAudioFormat.JavaClass.ENCODING_PCM_16BIT);
  AudioDataSize:=AStream.Size;
  AudioStream := TJavaArray<Byte>.Create(AudioDataSize);
  AStream.Position := 0;
  AStream.Read(AudioStream.Data^, AudioDataSize);
  if (not Assigned(AudioStream)) then exit;
  AudioTrack := TJAudioTrack.JavaClass.init(TJAudioManager.JavaClass.STREAM_MUSIC,
              FSampleRate, TJAudioFormat.JavaClass.CHANNEL_OUT_MONO,
              TJAudioFormat.JavaClass.ENCODING_PCM_8BIT, AudioDataSize,
              TJAudioTrack.JavaClass.MODE_STATIC);
  if ((not Assigned(AudioTrack)) or (AudioDataSize=0)) then exit;
  try
    AudioTrack.setVolume(0);
    AudioTrack.setPlaybackPositionUpdateListener(PlaybackPositionUpdateListener);
    updates;
  except
   // TODO: handle exception
   //showmessage(???);
  end;
end;

procedure TPCM_Player.Play;
begin
  if ((not Assigned(AudioTrack)) or (AudioDataSize=0)) then exit;
  try
    if (AudioTrack.getPlayState=TJAudioTrack.JavaClass.PLAYSTATE_PLAYING)
    then Exit;
    if AudioTrack.getPlayState=TJAudioTrack.JavaClass.PLAYSTATE_STOPPED
    then begin
            AudioPausePosition:=0;
            AudioTrack.reloadStaticData();
            AudioTrack.write(AudioStream, 0, AudioDataSize);
            AudioTrack.setNotificationMarkerPosition(AudioDataSize);
            AudioTrack.setPositionNotificationPeriod(FSampleRate/2); //notification toutes les 500ms
    end;
    AudioTrack.setPlaybackHeadPosition(AudioPausePosition);
    AudioTrack.play;
    updates;
  except
   // TODO: handle exception
   //showmessage(???);
  end;
end;

procedure TPCM_Player.Pause;
begin
  if (not Assigned(AudioTrack)) then exit;
  try
    if (AudioTrack.getPlayState=TJAudioTrack.JavaClass.PLAYSTATE_PAUSED)
    then Exit;
    if AudioTrack.getPlayState=TJAudioTrack.JavaClass.PLAYSTATE_PLAYING
    then begin
      AudioTrack.setPlaybackHeadPosition(AudioPausePosition);
      AudioTrack.play;
      updates;
    end;
  except
   // TODO: handle exception
   //showmessage(???);
  end;
end;

procedure TPCM_Player.PlayPause;
begin
  if ((not Assigned(AudioTrack)) or (AudioDataSize=0)) then exit;
  try
    case AudioTrack.getPlayState of
      TJAudioTrack.JavaClass.PLAYSTATE_PLAYING:
      begin
        AudioPausePosition:=AudioTrack.getPlaybackHeadPosition;
        AudioTrack.pause;
      end;
      TJAudioTrack.JavaClass.PLAYSTATE_PAUSED:
      begin
        AudioTrack.setPlaybackHeadPosition(AudioPausePosition);
        AudioTrack.play;
      end;
      TJAudioTrack.JavaClass.PLAYSTATE_STOPPED:
      begin
        AudioPausePosition:=0;
        AudioTrack.reloadStaticData();
        AudioTrack.write(AudioStream, 0, AudioDataSize);
        AudioTrack.setNotificationMarkerPosition(AudioDataSize);
        AudioTrack.setPositionNotificationPeriod(FSampleRate/2); //notification toutes les 500ms
        AudioTrack.play;
      end;
    end;
    updates;
  except
     // TODO: handle exception
     //showmessage(???);
  end;
  updates;
end;

procedure TPCM_Player.Stop;
begin
  if not Assigned(AudioTrack) then exit;
  AudioTrack.pause;
  AudioTrack.stop;
  updates;
end;

function TPCM_Player.GetPlayState:TPlayState;
begin
  Result:=TPlayState.psNil;
  if Assigned(AudioTrack) then
  case AudioTrack.getPlayState of
    TJAudioTrack.JavaClass.PLAYSTATE_STOPPED:Result:=TPlayState.psStopped;
    TJAudioTrack.JavaClass.PLAYSTATE_PAUSED:Result:=TPlayState.psPaused;
    TJAudioTrack.JavaClass.PLAYSTATE_PLAYING:Result:=TPlayState.psPlaying;
  end;
end;

function TPCM_Player.GetState:TState;
begin
  Result:=TState.sNil;
  if Assigned(AudioTrack) then
  case AudioTrack.getState of
    TJAudioTrack.JavaClass.STATE_UNINITIALIZED:Result:=TState.sUnitialized;
    TJAudioTrack.JavaClass.STATE_INITIALIZED:Result:=TState.sInitialized;
    TJAudioTrack.JavaClass.STATE_NO_STATIC_DATA:Result:=TState.sNoStaticData;
  end;
end;

procedure TPCM_Player.SetPlayState(const APlayState: TPlayState);
begin
  if FPlayState=APlayState then exit;
  FPlayState:=APlayState;
  if Assigned(FOnPlayStateChange) then
     FOnPlayStateChange(APlayState);
end;

procedure TPCM_Player.SetState(const AState: TState);
begin
  if FState=AState then exit;
  FState:=AState;
  if Assigned(FOnStateChange) then
     FOnStateChange(AState);
end;

procedure TPCM_Player.SetTime(const ATime: Single);
begin
  if FTime=ATime then exit;
  FTime:=ATime;
  if Assigned(FOnTimeChange) then
     FOnTimeChange(ATime);
end;

procedure TPCM_Player.SetDuration(const ADuration: Single);
begin
  if FDuration=ADuration then exit;
  FDuration:=ADuration;
  if Assigned(FOnDurationChange) then
     FOnDurationChange(ADuration);
end;

procedure TPCM_Player.release;
begin
  if Assigned(AudioTrack) then
  begin
    AudioTrack.stop;
    AudioTrack.release;
  end;
end;

function TPCM_Player.GetTime: Single;
begin
  if Assigned(AudioTrack)
  then result:=1000*AudioTrack.getPlaybackHeadPosition/FSampleRate
  else result:=0;
end;

function TPCM_Player.GetDuration: Single;
begin
  if Assigned(AudioTrack)
  then result:=1000*AudioDataSize/FSampleRate
  else result:=0;
end;

procedure TPCM_Player.Updates;
begin
  SetState(GetState);
  SetPlayState(GetPlayState);
  SetTime(GetTime);
  SetDuration(GetDuration);
end;

procedure TPCM_Player.DoMarkerReached;
begin
  FComplete:=true;
  if Assigned(FOnComplete) then FOnComplete;
end;

procedure TPCM_Player.DoPeriodicNotification;
begin
  updates;
end;

procedure TPCM_Player.DoOnCallStateChanged(state: Integer);
begin
   case state of
      TJTelephonyManager.Javaclass.CALL_STATE_IDLE:
        begin
          //on revient à l'application, on ne fait rien..
        end;
      TJTelephonyManager.Javaclass.CALL_STATE_RINGING:stop;
      TJTelephonyManager.Javaclass.CALL_STATE_OFFHOOK:
      begin
        //on passe hors-ligne, le téléphone ne sonnera pas, rien ne se passe.
      end;
   end;
end;

constructor TPlaybackPositionUpdateListener.Create(AOwner : TPCM_Player);
begin
  FOwner := AOwner;
end;

procedure TPlaybackPositionUpdateListener.OnMarkerReached(track: JAudioTrack);
begin
  FOwner.DoMarkerReached;
end;

procedure TPlaybackPositionUpdateListener.OnPeriodicNotification(track: JAudioTrack);
begin
  FOwner.DoPeriodicNotification;
end;

constructor TPhoneStateListener.Create(AOwner : TPCM_Player);
begin
  FOwner := AOwner;
end;

procedure TPhoneStateListener.OnCallStateChanged(state: Integer; incomingNumber: JString);
begin
  FOwner.DoOnCallStateChanged(state);
end;
