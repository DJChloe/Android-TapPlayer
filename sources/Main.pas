unit Main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.TabControl,
  FMX.StdCtrls, FMX.Gestures, FMX.Controls.Presentation, FMX.Layouts,
  FMX.ListBox, System.ImageList, FMX.ImgList, FMX.Edit, FMX.SearchBox,
  FMX.Objects,UnitFileFrame,unitTapFiles, unittapfileslist,
  FMX.ComboEdit, FMX.EditBox, FMX.SpinBox,UnitAudio,FMX.Platform, FMX.PhoneDialer;

type
  TMainForm = class(TForm)
    HeaderToolBar: TToolBar;
    ToolBarLabel: TLabel;
    StyleBook1: TStyleBook;
    ListBox1: TListBox;
    ListBoxGroupHeader1: TListBoxGroupHeader;
    lbiFileSelect: TListBoxItem;
    ToolBar1: TToolBar;
    sbPlay: TSpeedButton;
    sbNext: TSpeedButton;
    sbPrevious: TSpeedButton;
    sbStop: TSpeedButton;
    sbPause: TSpeedButton;
    FramedVertScrollBox1: TFramedVertScrollBox;
    GridLayout1: TGridLayout;
    procedure FormCreate(Sender: TObject);
    procedure lbiFileSelectClick(Sender: TObject);
    procedure sbNextClick(Sender: TObject);
    procedure sbPauseClick(Sender: TObject);
    procedure sbPlayClick(Sender: TObject);
    procedure sbPreviousClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FileFrameClick(Sender: TObject);
    procedure PlayStateChanged(APlayState: TPlayState);
    procedure StateChanged(AState: TPlayState);
  private
    fsout:TMemoryStream;
    file_size:integer;
    ccurrent_level:byte;
    FCurrentFile: string;
    FCurrentindex: integer;
    FPaused: boolean;
    FPlayState:TPlayState;
    FPCMPlayer:TPCM_Player;
    PhoneDialerService: IFMXPhoneDialerService;
    procedure MyOnCallStateChanged(const ACallID: String; const ACallState: TCallState);
    procedure SetCurrentFile(const Value: string);
    procedure SetCurrentIndex(const Value: integer);
    procedure SetPaused(const Value: boolean);
    procedure emit_level(size:integer);
    procedure emit_bit(bit:boolean);
    procedure emit_byte(val:byte);
    procedure emit_gap;
    procedure emit_silence(size:integer);
    function init:boolean;
    procedure DoUpdateUI(newPos: Single);
    { Private declarations }
  public
    { Public declarations }
    //AppPath:string;
    OricTape:TOricTape;
    FileFrames:array of TframeFile;
    WavDurations:array of integer;
    PNotify:boolean;
    function ComputeDuration(OTF:TOricFile):integer;
    function ComputeTotalTime:integer;
    function ComputePlayedTime(UpToIndex:integer;Supplement:int64):integer;
    procedure UpdatePlayedTime(UpToIndex:integer;Supplement:int64);
    procedure OpenFile(FileName:string);
    procedure updatetimeinfo;
    procedure inittimeinfos;
    procedure Inittimeinfo(fileindex:integer);
    procedure Maxtimeinfo(fileindex:integer);
    procedure UpdateFileFrames;
    procedure FreeFileFrames;
    procedure EncodeAndGetReady;
    procedure TapStart;
    procedure TapPlay;
    procedure TapPause;
    procedure TapPrevious;
    procedure TapNext;
    procedure TapStop;
    procedure TapStopBetween;
    procedure PStop;
    procedure PPlay;
    procedure PPause;
    Function Convertir:boolean;
    property CurrentFile:string read fCurrentFile write SetCurrentFile;
    property CurrentIndex:integer read fCurrentIndex write SetCurrentIndex;
    property Paused:boolean read fPaused write SetPaused;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.fmx}
uses unitOpenDiag,System.IOUtils,UnitProgressDialog,UnitPermissions;

var Speed:integer;//8000;

procedure TMainForm.FormCreate(Sender: TObject);
var
ac1:TAudioCap;
const
   validSampleRates:array[0..5] of integer=(4800, 8000, 11025, 16000, 44100, 48000);
begin
  { This defines the default active tab at runtime }
 //LPermissions := TJavaObjectArray<JString>.Create(1);
//  LPermissions.Items[0] := StringToJString('android.permission.READ_PHONE_STATE');

//  TAndroidHelper.Activity.requestPermissions(LPermissions, 0);
   CurrentFile:='';
  //AppPath:=IncludeTrailingBackslash(ExtractFilePath(application.ExeName));
  OricTape:=TOricTape.Create;
  OricTape.OwnsObjects:=true;

  TPlatformServices.Current.SupportsPlatformService(IFMXPhoneDialerService, IInterface(PhoneDialerService));
  if Assigned(PhoneDialerService) then
    PhoneDialerService.OnCallStateChanged := MyOnCallStateChanged;

  ac1:=getMinSupportedSampleRate(validSampleRates);
  //Speed:=ac1.SampleRate;
  speed:=44100;
  FPCMPlayer:=TPCM_Player.Create(speed);
  FPCMPlayer.OnPlayStateChange:=PlayStateChanged;
  FPCMPlayer.OnTimeChange:=DoUpdateUI;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FPCMPlayer.Free;
  FreeFileFrames;
  if Assigned(fsout) then fsout.Free;
  OricTape.Free;
end;

procedure TMainForm.lbiFileSelectClick(Sender: TObject);
begin
  FormOpenFile.Show;
  ///if FormOpenFile.ModalResult=mrOk then
  //OpenDialog1.Filter := 'Text Files (*.txt)|*.txt';
  //OpenDialog1.Execute;
end;

procedure TMainForm.sbNextClick(Sender: TObject);
begin
  TapNext;
end;

procedure TMainForm.sbPauseClick(Sender: TObject);
begin
  TapPause;
end;

procedure TMainForm.sbPlayClick(Sender: TObject);
begin
  TapPlay;
end;

procedure TMainForm.sbPreviousClick(Sender: TObject);
begin
  TapPrevious;
end;

procedure TMainForm.SetCurrentFile(const Value: string);
begin
  if fCurrentFile=value then Exit;
  FCurrentFile := Value;
  lbiFileSelect.Text:='Aucun fichier selectionné';
  if FCurrentFile<>''
  then if FileExists(FCurrentFile)
       then lbiFileSelect.Text:=ExtractFileName(FCurrentFile);
  Application.ProcessMessages;
end;

procedure TMainForm.OpenFile(FileName: string);
begin
    CurrentFile:=FileName;
    EncodeAndGetReady;
end;

procedure TMainForm.PPause;
begin
  FPCMPlayer.Pause;
end;

procedure TMainForm.PPlay;
begin
  FPCMPlayer.Play;
end;

procedure TMainForm.PStop;
begin
  FPCMPlayer.Stop;
end;

procedure TMainForm.SetCurrentindex(const Value: integer);
begin
  if FCurrentindex<>value then
  begin
    if ((FCurrentindex>=0) and (FCurrentindex<=High(FileFrames)))
    then if assigned(FileFrames[FCurrentindex])
         then FileFrames[FCurrentindex].Actif:=False;
    FCurrentindex := Value;
  end;
  if ((FCurrentindex>=0) and (FCurrentindex<=High(FileFrames)))
    then if assigned(FileFrames[FCurrentindex])
         then FileFrames[FCurrentindex].Actif:=true;
end;

function TMainForm.ComputeDuration(OTF: TOricFile): integer;
begin
  result:=(OTF.WavLength*1000) div Speed;
end;

function TMainForm.ComputeTotalTime: integer;
begin
  Result:=(OricTape.TotalWavLength*1000) div Speed;
end;

function TMainForm.ComputePlayedTime(UpToIndex: Integer; Supplement: Int64):integer;
var i:integer;
begin
  result:=0;
  if not Assigned(OricTape) then exit;
  for i := 0 to UpToIndex-1 do
  begin
    Result:=Result+(OricTape.Items[i].WavLength*1000) div Speed;
  end;
  Result:=Result+Supplement;
end;

procedure TMainForm.emit_bit(bit:boolean);
begin
  //44100Hz
  case speed of
    4800:begin
            emit_level(1);
            if bit then emit_level(1)
                     else emit_level(2);
         end;
    8000:begin
            if bit then begin
                          emit_level(1);
                          emit_level(2);
                        end
            else begin
                   emit_level(2);
                   emit_level(3);
                 end;
          end;
    11025:begin
          if bit then begin
                   emit_level(2);
                   emit_level(2);
                 end
            else begin
                   emit_level(3);
                   emit_level(4);
                 end;
          end;
    16000:begin
            if bit then begin
                          emit_level(2);
                          emit_level(4);
                        end
            else begin
                   emit_level(4);
                   emit_level(6);
                 end;
          end;
    44100:begin
            if bit then begin
                  emit_level(8);
                  emit_level(8);
                end
            else begin
                   emit_level(12);
                   emit_level(16);
                 end;
          end;
    48000:begin
            emit_level(10);
            if bit then emit_level(10)
                     else emit_level(20);
         end;
  end;
end;

procedure TMainForm.emit_byte(val: byte);
var i:byte;
    parity:byte;
begin
  parity:=1;
  emit_bit(false);
  for i:=0 to 7 do
  begin
    parity:=parity+(val and 1);
    emit_bit((val and 1)=1);
    val:=val shr 1;
  end;
  emit_bit((parity and 1)=1);
  emit_bit(true);
  emit_bit(true);
  emit_bit(true);
  emit_bit(true);  // 4 bits stop au lieu de 3.5 pour être sûr que les routines aient du temps
end;

procedure TMainForm.emit_gap;
var i:integer;
begin
 //un paquet de bits stop pour laisser le temps d'afficher la ligne de statut
  for i:=0 to 99 do emit_bit(true);
end;

procedure TMainForm.emit_level(size: integer);
var i:integer;
begin
   ccurrent_level:=256-ccurrent_level;
   for i:=0 to size-1 do fsout.Write(ccurrent_level,1);
   inc(file_size,size);
end;


procedure TMainForm.emit_silence(size: integer);
var i:integer;
    b:byte;
begin
   b:=128;
   for i:=0 to size-1 do fsout.Write(b,1);
   inc(file_size,size);
end;

procedure TMainForm.EncodeAndGetReady;
begin
  ToolBar1.Enabled:=false;
  if FPCMPlayer.PlayState=TPlayState.psPlaying
  then TapStop
  else PStop;
  if Convertir
  then begin
         ToolBar1.Enabled:=true;
         TapStart;
       end
  else begin
         ToolBar1.Enabled:=true;
         sbPlay.Enabled:=False;
         sbPrevious.Enabled:=False;
         sbNext.Enabled:=False;
         sbStop.Enabled:=False;
         sbPause.Enabled:=False;
       end;
  Application.ProcessMessages;
end;

function TMainForm.init:boolean;
begin
  if not FileExists(CurrentFile)
  then begin
         Result:=false;
         exit;
       end;
  Result:=true;
end;

procedure TMainForm.InitTimeInfos;
var
  L,tsec:integer;
  Min,Sec: Word;
  i:integer;
begin
  for i:=0 to OricTape.Count - 1 do
      InitTimeInfo(i);
  L:=ComputeTotalTime;
  tsec:= L div 1000;
  Min := tsec div 60;
  Sec := tsec-min*60;
  lbiFileSelect.ItemData.Detail:=Format('00:00 of %.2d:%.2d', [Min,Sec]);
end;

procedure TMainForm.InitTimeInfo(fileindex:integer);
var
  L,tsec:integer;
  Min,Sec: Word;
begin
    if fileindex>=OricTape.Count then Exit;

    L:=ComputeDuration(OricTape.Items[fileindex]);
    tsec:= L div 1000;
    Min := tsec div 60;
    Sec := tsec-min*60;
    if ((fileindex>=0) and (fileindex<High(FileFrames)))
    then if assigned(FileFrames[fileindex])
         then begin
                FileFrames[fileindex].ProgressValue:=0;
                FileFrames[fileindex].Time:=Format('00:00 of %.2d:%.2d', [Min,Sec]);
         end;
    UpdatePlayedTime(fileindex,0);
end;

procedure TMainForm.Maxtimeinfo(fileindex:integer);
var
  L,tsec:integer;
  Min,Sec: Word;
begin
    if fileindex>=OricTape.Count then Exit;
    L:=ComputeDuration(OricTape.Items[fileindex]);
    tsec:= L div 1000;
    Min := tsec div 60;
    Sec := tsec-min*60;
    if ((fileindex>=0) and (fileindex<High(FileFrames)))
    then if assigned(FileFrames[fileindex])
         then begin
                FileFrames[fileindex].ProgressValue:=L;
                FileFrames[fileindex].Time:=Format('%.2d:%.2d of %.2d:%.2d', [Min,Sec,Min,Sec]);
         end;
    UpdatePlayedTime(fileindex+1,0);
end;

procedure TMainForm.SetPaused(const Value: boolean);
begin
  FPaused := Value;
end;

procedure TMainForm.PlayStateChanged(APlayState: TPlayState);
begin
  if not PNotify then exit;

  FPlayState:=APlayState;
  if FPlayState=TPlayState.psStopped then
  if currentindex<(OricTape.Count-1)
  then begin
         if ((FCurrentindex>=0) and (FCurrentindex<=High(FileFrames)))
         then if assigned(FileFrames[FCurrentindex])
         then UpdateTimeInfo;
         paused:=FileFrames[Currentindex].DoPause;
         Currentindex:=Currentindex+1;
         FPCMPlayer.LoadStream(OricTape[FCurrentindex].PCMStream);
         //TempFiles.Strings[currentindex]);
         if paused then TapStopBetween else TapPlay;
  end else TapStop;
end;

procedure TMainForm.StateChanged(AState: TPlayState);
begin
  //
end;

procedure TMainForm.TapNext;
var LastMode:TPlayState;
begin
  LastMode:=FPlayState;
  PStop;
  MaxTimeInfo(Currentindex);
  if LastMode<>TPlayState.psStopped then
  begin
    MaxTimeInfo(Currentindex);
    if currentindex<OricTape.Count then Currentindex:=Currentindex+1;
    InitTimeInfo(Currentindex);
  end;
  sbPlay.Enabled:=true;
  sbPause.Enabled:=false;
  sbPrevious.Enabled:=(Currentindex>0);
  sbNext.Enabled:=(Currentindex<=OricTape.Count);
  sbStop.Enabled:=false;
  if Currentindex<OricTape.Count then
  begin
    FPCMPlayer.LoadStream(OricTape[Currentindex].PCMStream);
    FileFrames[Currentindex].MaxValue := FPCMPlayer.Duration;
    if (LastMode=TPlayState.psPlaying)
    then begin
           sbPlay.Enabled:=false;
           sbPause.Enabled:=true;
           sbPrevious.Enabled:=true;
           sbNext.Enabled:=true;
           sbStop.Enabled:=true;
           PPlay;
           PNotify:=true;
         end;
  end;
end;

procedure TMainForm.TapPause;
var IsPaused:boolean;
begin
  IsPaused:=(FPlayState=TPlayState.psStopped);
  lbiFileSelect.Enabled:=not IsPaused;
  if paused then TapPlay
  else begin
  if IsPaused
  then PPlay
  else begin
         PStop;
         if ((FCurrentindex>=0) and (FCurrentindex<=High(FileFrames)))
         then if assigned(FileFrames[FCurrentindex])
         then UpdateTimeInfo;
       end;
  end;
end;

procedure TMainForm.TapPlay;
begin
  paused:=false;
  lbiFileSelect.Enabled:=false;
  sbPlay.Enabled:=false;
  sbPause.Enabled:=true;
  sbPrevious.Enabled:=true;
  sbNext.Enabled:=(currentindex<=OricTape.Count);
  sbStop.Enabled:=true;
  FileFrames[Currentindex].MaxValue := FPCMPlayer.Duration;
  UpdateTimeInfo;
  PPlay;
  PNotify:=true;
end;

procedure TMainForm.TapPrevious;
var LastMode:TPlayState;
begin
  LastMode:=FPlayState;
  PStop;

  InitTimeInfo(Currentindex);
  if LastMode<>TPlayState.psStopped then
  begin
    if currentindex>0 then Currentindex:=Currentindex-1;
    InitTimeInfo(Currentindex);
  end;
  sbPlay.Enabled:=true;
  sbPause.Enabled:=false;
  sbPrevious.Enabled:=(Currentindex>0);
  sbNext.Enabled:=true;
  sbStop.Enabled:=false;
  FPCMPlayer.LoadStream(OricTape[Currentindex].PCMStream);
  FileFrames[Currentindex].MaxValue := FPCMPlayer.Duration;

  if LastMode=TPlayState.psPlaying then begin
                               sbPlay.Enabled:=false;
                               sbPause.Enabled:=true;
                               sbPrevious.Enabled:=true;
                               sbNext.Enabled:=true;
                               sbStop.Enabled:=true;
                               PPlay;
                               PNotify:=true;
                             end;
end;

procedure TMainForm.TapStart;
var s:string;
begin
  Currentindex:=0;
  InitTimeInfos;

  sbPlay.Enabled:=true;
  sbPause.Enabled:=false;
  sbPrevious.Enabled:=false;
  sbNext.Enabled:=true;
  sbStop.Enabled:=false;

  FPCMPlayer.LoadStream(OricTape[Currentindex].PCMStream);
end;

procedure TMainForm.TapStop;
begin
  PStop;
  UpdateTimeInfo;
  lbiFileSelect.Enabled:=true;
  TapStart;
end;

procedure TMainForm.TapStopBetween;
begin
  UpdateTimeInfo;
  PStop;
  lbiFileSelect.Enabled:=true;
  sbPlay.Enabled:=true;
  sbPause.Enabled:=false;
  sbPrevious.Enabled:=false;
  sbNext.Enabled:=true;
  sbStop.Enabled:=true;
end;

procedure TMainForm.FreeFileFrames;
var i:integer;
begin
  for i:=0 to High(FileFrames) do
  begin
    if Assigned(FileFrames[i]) then begin
                                      FileFrames[i].Parent:=nil;
                                      FileFrames[i].Free;
                                    end;
  end;
  SetLength(FileFrames,0);
end;

procedure TMainForm.UpdateFileFrames;
var i:integer;
begin
  FreeFileFrames;
  SetLength(FileFrames,OricTape.Count+1);
  GridLayout1.ItemHeight:=90;
  GridLayout1.ItemWidth:=GridLayout1.Width;
  GridLayout1.height := (OricTape.Count+1)*90;
  for i:=0 to OricTape.Count-1 do
  begin
    FileFrames[i]:=TframeFile.Create(GridLayout1);
    GridLayout1.AddObject(FileFrames[i]);
      OricTape.Items[i].TapeIndex:=i;
      FileFrames[i].Parent:=GridLayout1;
      FileFrames[i].TabOrder:=i;
      FileFrames[i].Name:='FF'+trim(IntToStr(i));
      if not OricTape.Items[i].RawData then
         FileFrames[i].OricFileName:=OricTape.Items[i].CatalogName+'    '+OricTape.Items[i].FileKindDesc
      else FileFrames[i].OricFileName:='Raw Data';
      FileFrames[i].tag:=i;
      FileFrames[i].DoPause:=False;
      FileFrames[i].Actif:=False;
      FileFrames[i].SpeedButton1.OnClick:=FileFrameClick;
      FileFrames[i].Repaint;
      Application.ProcessMessages;
  end;

  FileFrames[OricTape.Count]:=TframeFile.Create(GridLayout1);
   GridLayout1.AddObject(FileFrames[OricTape.Count]);
    FileFrames[OricTape.Count].TabOrder:=OricTape.Count;
    FileFrames[OricTape.Count].Name:='FF'+trim(IntToStr(OricTape.Count));
    FileFrames[OricTape.Count].OricFileName:='End of Tape';
    FileFrames[OricTape.Count].tag:=OricTape.Count;
    FileFrames[OricTape.Count].Label1.Visible:=true;
    FileFrames[OricTape.Count].Label2.Visible:=false;
    FileFrames[OricTape.Count].SpeedButton1.Visible:=False;
    FileFrames[OricTape.Count].Rectangle1.Visible:=false;
    FileFrames[OricTape.Count].CBPause.Visible:=false;
    FileFrames[OricTape.Count].DoPause:=False;
    FileFrames[OricTape.Count].Actif:=False;
    FileFrames[OricTape.Count].Repaint;
    Application.ProcessMessages;

  FramedVertScrollBox1.Repaint;
  Application.ProcessMessages;
end;

procedure TMainForm.UpdatePlayedTime(UpToIndex: integer; Supplement: int64);
var
  L,tsec:integer;
  Min,Sec: Word;
  Min2,Sec2: Word;
begin
      L:=ComputePlayedTime(Currentindex,Supplement);
      tsec:= L div 1000;
      Min := tsec div 60;
      Sec := tsec-min*60;
      L:=ComputeTotalTime;
      tsec:= L div 1000;
      Min2 := tsec div 60;
      Sec2 := tsec-min2*60;
      lbiFileSelect.Itemdata.detail:=Format('%.2d:%.2d of %.2d:%.2d', [Min, Sec,Min2,Sec2]);
end;

procedure TMainForm.UpdateTimeInfo;
var
  L,tsec:integer;
  Min,Sec: Word;
  Min2,Sec2: Word;
begin
  L:=Trunc(FPCMPlayer.Time);
  if FPCMPlayer.Duration<L then L:=0;
  tsec:= L div 1000;
  Min := tsec div 60;
  Sec := tsec-min*60;
  L:=Trunc(FPCMPlayer.Duration);
  tsec:= L div 1000;
  Min2 := tsec div 60;
  Sec2 := tsec-min2*60;
    if ((FCurrentindex>=0) and (FCurrentindex<=High(FileFrames)))
    then if assigned(FileFrames[FCurrentindex])
       then begin
              FileFrames[Currentindex].ProgressValue := FPCMPlayer.Time;
              FileFrames[Currentindex].Time:=Format('%.2d:%.2d of %.2d:%.2d', [Min, Sec,Min2,Sec2]);
       end;
  UpdatePlayedTime(Currentindex,trunc(FPCMPlayer.Time));
end;

//Opérations sur fichiers cassette
procedure TMainForm.FileFrameClick(Sender: TObject);
var AFileFrame:TComponent;
    LastMode:TPlayState;
    i:integer;
begin
  AFileFrame:=(sender as TPanel).Parent;
  LastMode:=FPlayState;
  PStop;

  InitTimeInfo(Currentindex);
  if LastMode<>TPlayState.psStopped then
  begin
    Currentindex:=AFileFrame.Tag;
    for I := 0 to Currentindex-1 do MaxTimeInfo(i);
    for I := Currentindex to high(FileFrames) do
    InitTimeInfo(i);
  end;
  sbPlay.Enabled:=true;
  sbPause.Enabled:=false;
  sbPrevious.Enabled:=(Currentindex>0);
  sbNext.Enabled:=true;
  sbStop.Enabled:=false;
  FPCMPlayer.LoadStream(OricTape[Currentindex].PCMStream);
  FileFrames[Currentindex].ProgressValue := FPCMPlayer.Duration;

  if LastMode=TPlayState.psPlaying then begin
                               sbPlay.Enabled:=false;
                               sbPause.Enabled:=true;
                               sbPrevious.Enabled:=true;
                               sbNext.Enabled:=true;
                               sbStop.Enabled:=true;
                               PPlay;
                               PNotify:=true;
                             end;
end;

function TMainForm.Convertir:boolean;
var i,ii:integer;
    b:byte;
    readsize:int64;
    Astream:TMemoryStream;
begin
  OricTape.LoadfromFile(CurrentFile,lmDistinctRawData);
  if OricTape.ContainsRawData then
        OricTape.LoadfromFile(CurrentFile,lmMergeRawData);
  UpdateFileFrames;
  result:=false;
  if not init then exit;
  Cursor:=crHourGlass;
  readsize:=0;
  FormProgressDialog.ProgressBar1.Max:=OricTape.TotalSize;
  FormProgressDialog.ProgressBar1.Value:=0;
  FormProgressDialog.Show;
  Application.ProcessMessages;

  for ii := 0 to OricTape.Count - 1 do
  begin
    FormProgressDialog.ProgressBar1.Value:=readsize;
    Application.ProcessMessages;

    fsout:=TMemoryStream.Create;
    try
      for i:=0 to 255 do emit_byte($16);

      OricTape.Items[ii].Header.Position:=0;
      for i:=0 to OricTape.Items[ii].Header.Size-1 do
      begin
        OricTape.Items[ii].Header.Read(b,1);
        inc(readsize);
        emit_byte(b);
        FormProgressDialog.ProgressBar1.Value:=readsize;
        Application.ProcessMessages;
      end;
      emit_gap;
      OricTape.Items[ii].Data.Position:=0;
      for i:=0 to OricTape.Items[ii].Data.Size-1 do
      begin
        OricTape.Items[ii].Data.Read(b,1);
        inc(readsize);
        emit_byte(b);
        FormProgressDialog.ProgressBar1.Value:=readsize;
        Application.ProcessMessages;
      end;
      emit_silence(Speed); //Speed*1 seconde
      //finalize wav
      //sample_riff.Size2:=fsout.Size-44;
      //sample_riff.Size1:=fsout.Size-8;
      //fsout.Seek(0,soFromBeginning);
      //fsout.Write(sample_riff.asbytes,44);
      OricTape.Items[ii].wavLength:=fsout.Size;
      OricTape[ii].PCMStream.Size:=0;
      OricTape[ii].PCMStream.CopyFrom(fsout,0);
    finally
      fsout.Free;
    end;
  end;
  Cursor:=crDefault;
  result:=true;
  FormProgressDialog.Hide;
  Application.ProcessMessages;
end;

procedure TMainForm.DoUpdateUI(newPos: Single);
begin
  UpdateTimeInfo;
end;

procedure TMainForm.MyOnCallStateChanged(const ACallID: String; const ACallState: TCallState);
var outText: String;
begin
  case ACallState of
         TCallState.Connected,TCallState.None,TCallState.Disconnected:
         begin
           // on revient à l'application, on ne fait rien..
           //on passe hors-ligne, le téléphone ne sonnera pas, rien ne se passe.
         end;
         TCallState.Incoming,TCallState.Dialing:FPCMPlayer.Stop;
     end;
end;

end.
