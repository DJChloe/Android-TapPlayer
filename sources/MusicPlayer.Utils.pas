
//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc. 
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------
unit MusicPlayer.Utils;

interface
uses
  Androidapi.JNI.JavaTypes, Androidapi.JNI.GraphicsContentViewText, Androidapi.Helpers,
   Androidapi.JNI.Os, Androidapi.JNI.Net, Androidapi.JNI.App,
  Androidapi.JNI.Media,
  Androidapi.JNIBridge,
  System.SysUtils,
  FMX.Graphics;

type
  TMPControllerType = (App, Ipod);
  TMPRepeatMode = (Default, None, One, All);
  TMPPlaybackState = (Stopped, Playing, Paused, Interrupted, SeekingForward, SeekingBackward);
  TAudioCap=record
    SampleRate:integer;
    BufferSize:integer;
  end;
  TAudioCap2=record
    SampleRate:string;
    BufferSize:string;
  end;
  TOnProcessPlayEvent = procedure (newPos: Single) of object;
function getMinSupportedSampleRate(const AnArrayOfSampleRatesToTest: array of Integer):TAudioCap;
function getMinSupportedSampleRate2:TAudioCap2;
function DurationToString(duration: Single): string;

implementation

{ TMPAlbum }

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

function getMinSupportedSampleRate2:TAudioCap2;
var
audioManager : JAudioManager;
 AudioObj: JObject;

begin
AudioObj := TAndroidHelper.Activity.getSystemService( TJContext.JavaClass.AUDIO_SERVICE );
audioManager := TJAudioManager.Wrap( ( AudioObj as ILocalObject ).GetObjectID );

//AudioManager audioManager = (AudioManager) this.getSystemService(Context.AUDIO_SERVICE);
result.SampleRate := Jstringtostring(audioManager.getProperty(TJAudioManager.JavaClass.PROPERTY_OUTPUT_SAMPLE_RATE));
result.BufferSize:= Jstringtostring(audioManager.getProperty(TJAudioManager.JavaClass.PROPERTY_OUTPUT_FRAMES_PER_BUFFER));
end;

function DurationToString(duration: Single): string;
var
  hours,
  minutes,
  seconds: Integer;
  secondsStr: string;
begin
  Result := '';
  hours := Trunc(duration) div (1000*60*60);
  minutes := (Trunc(duration) mod (1000*60*60)) div (1000*60);
  seconds :=  ((Trunc(duration) mod (1000*60*60)) mod (1000*60)) div 1000;
  if hours > 0 then
    if minutes < 10 then
      Result := Result + hours.ToString + ':0'
    else
      Result := Result + hours.ToString + ':';

  if seconds < 10 then
    secondsStr := '0' + seconds.ToString
  else
    secondsStr := seconds.ToString;

  Result := Result + minutes.ToString + ':' + secondsStr;
end;

end.
