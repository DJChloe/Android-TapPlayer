
//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc. 
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------
unit MusicPlayer.Android;

interface
uses
  FMX.Graphics,
  MusicPlayer.Utils,
  System.IoUtils, System.SysUtils, System.Classes,
  FMX.Types, FMX.Platform.Android,
  Androidapi.JNI.Os, Androidapi.JNI.Net,
  Androidapi.JNIBridge, Androidapi.JNI.JavaTypes, Androidapi.JNI.GraphicsContentViewText,
  Androidapi.JNI.Media, Androidapi.JNI.Provider, Androidapi.Helpers, Androidapi.JNI.App;
type
  TMusicPlayer = class
  private
  type
    TProcessThread = class (TThread)
    private
      [weak] FMusicPlayer: TMusicPlayer;
      FOnProcessPlay: TOnProcessPlayEvent;
      procedure DoProcessPlay;
    public
      constructor Create(CreateSuspended: Boolean; AMusicPlayer: TMusicPlayer; processHandler: TOnProcessPlayEvent);
      destructor Destroy; override;
      procedure Execute; override;
    end;
  protected
    class var FInstance: TMusicPlayer;
  private
    FCurrentIndex: Integer;
    FMusicPlayer: JMediaPlayer;
    FPlayBackState: TMPPlaybackState;
    FOnProcessPlay: TOnProcessPlayEvent;
    constructor Create(AType: TMPControllerType = TMPControllerType.App);
    procedure DoOnProcessPlay(newPos: Single);
    procedure SetVolume(const Value: Single);
    procedure SetTime(const Value: Single);
    function GetVolume: Single;
    function GetTime: Single;
    function GetDuration: Single;
    function GetPlaybackState: TMPPlaybackState;
  public
    destructor Destroy; override;
    class procedure SetPlayerType(AType: TMPControllerType);
    class function DefaultPlayer: TMusicPlayer;
    property CurrentIndex: Integer read FCurrentIndex;
    property Volume: Single read GetVolume write SetVolume;
    property Time: Single read GetTime write SetTime;
    property Duration: Single read GetDuration;
    property PlaybackState: TMPPlaybackState read GetPlaybackState;
    property OnProcessPlay: TOnProcessPlayEvent read FOnProcessPlay write FOnProcessPlay;
    function IsPlaying: Boolean;
    procedure PlaySong(path: string);
    procedure LoadSong(path: string);
    procedure Play;
    procedure Stop;
    procedure Pause;
  end;
var
  NoArtBitmap: TBitmap;
implementation
{ TMusicPlayer }
constructor TMusicPlayer.Create(AType: TMPControllerType);
begin
  MainActivity.setVolumeControlStream(TJAudioManager.JavaClass.STREAM_MUSIC);
  FMusicPlayer := TJMediaPlayer.Create;
  FPlayBackState := TMPPlaybackState.Stopped;
  TProcessThread.Create(True,self,DoOnProcessPlay).Start;
end;

class function TMusicPlayer.DefaultPlayer: TMusicPlayer;
begin
  if not Assigned(FInstance) then
    FInstance := TMusicPlayer.Create;
  Result := FInstance;
end;

destructor TMusicPlayer.Destroy;
begin
  FMusicPlayer.release;
end;

procedure TMusicPlayer.DoOnProcessPlay(newPos: Single);
begin
  if Assigned(FOnProcessPlay) then
    TThread.Queue(TThread.CurrentThread, procedure
      begin
        FOnProcessPlay(newPos);
      end);
end;


function TMusicPlayer.GetDuration: Single;
begin
  Result := FMusicPlayer.getDuration;
end;

function TMusicPlayer.GetPlaybackState: TMPPlaybackState;
begin
  Result := FPlayBackState;
end;


function TMusicPlayer.GetTime: Single;
begin
  Result := FMusicPlayer.getCurrentPosition;
end;

function TMusicPlayer.GetVolume: Single;
var
  AudioManager: JAudioManager;
begin
  AudioManager := TJAudioManager.Wrap(MainActivity.getSystemService(TJContext.JavaClass.AUDIO_SERVICE));
  Result := AudioManager.getStreamVolume(TJAudioManager.JavaClass.STREAM_MUSIC);
  Result := Result / AudioManager.getStreamMaxVolume(TJAudioManager.JavaClass.STREAM_MUSIC);
end;

procedure TMusicPlayer.Pause;
begin
  FMusicPlayer.pause;
  FPlayBackState := TMPPlaybackState.Paused;
end;

procedure TMusicPlayer.Play;
begin
  if FPlayBackState = TMPPlaybackState.Stopped  then
    FMusicPlayer.prepare;
  FMusicPlayer.start;
  FPlayBackState := TMPPlaybackState.Playing;
end;

function TMusicPlayer.IsPlaying: Boolean;
begin
  Result := FMusicPlayer.isPlaying;
end;

procedure TMusicPlayer.LoadSong(path: string);
begin
  Stop;
  FMusicPlayer.reset;
  FMusicPlayer.setDataSource(StringToJString(path));
  FMusicPlayer.prepare;
end;

procedure TMusicPlayer.PlaySong(path: string);
begin
  Stop;
  FMusicPlayer.reset;
  FMusicPlayer.setDataSource(StringToJString(path));
  Play;
end;


class procedure TMusicPlayer.SetPlayerType(AType: TMPControllerType);
begin
  // Do nothing
end;


procedure TMusicPlayer.SetTime(const Value: Single);
begin
  FMusicPlayer.seekTo(Trunc(Value));
end;

procedure TMusicPlayer.SetVolume(const Value: Single);
var
  AudioManager: JAudioManager;
begin
  AudioManager := TJAudioManager.Wrap(MainActivity.getSystemService(TJContext.JavaClass.AUDIO_SERVICE));
  AudioManager.setStreamVolume(TJAudioManager.JavaClass.STREAM_MUSIC,
    Round(AudioManager.getStreamMaxVolume(TJAudioManager.JavaClass.STREAM_MUSIC) * Value), 0);
end;

procedure TMusicPlayer.Stop;
begin
  if FPlayBackState in [TMPPlaybackState.Playing, TMPPlaybackState.Paused] then
    FMusicPlayer.seekTo(0);
  FPlayBackState := TMPPlaybackState.Stopped;
  FMusicPlayer.stop;
  while FMusicPlayer.isPlaying do
    sleep(10);
  DoOnProcessPlay(0);
end;

{ TMusicPlayer.TProcessThread }

constructor TMusicPlayer.TProcessThread.Create(CreateSuspended: Boolean;
  AMusicPlayer: TMusicPlayer; processHandler: TOnProcessPlayEvent);
begin
  inherited Create(CreateSuspended);
  FOnProcessPlay := processHandler;
  FMusicPlayer := AMusicPlayer;
end;

destructor TMusicPlayer.TProcessThread.Destroy;
begin
  FMusicPlayer := nil;
  inherited;
end;

procedure TMusicPlayer.TProcessThread.Execute;
begin
  inherited;
  while Assigned(FMusicPlayer) do
  begin
    case FMusicPlayer.PlaybackState of
      Playing: DoProcessPlay;
      Stopped,
      Paused,
      Interrupted,
      SeekingForward,
      SeekingBackward: sleep(200);
    end;
  end;
end;

procedure TMusicPlayer.TProcessThread.DoProcessPlay;
var
  currentPos: Single;
begin
  currentPos := FMusicPlayer.Time;
  if Assigned(FOnProcessPlay) then
    FOnProcessPlay((currentPos/FMusicPlayer.Duration) * 100);
  if FMusicPlayer.IsPlaying then Sleep(200);
end;
end.
