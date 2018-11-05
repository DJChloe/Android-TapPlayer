unit UnitFileFrame;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation, FMX.Objects, FMX.ImgList;

type
  TframeFile = class(TFrame)
    cbPause: TCheckBox;
    SpeedButton1: TSpeedButton;
    Rectangle1: TRectangle;
    Rectangle2: TRectangle;
    Rectangle3: TRectangle;
    Label1: TLabel;
    Label2: TLabel;
    procedure Rectangle1Resize(Sender: TObject);
  private
    FOricFileName: string;
    FPause: boolean;
    FTime: string;
    FActif: boolean;
    FMax: single;
    FMin: single;
    FValue: single;
    FFileStream: TMemoryStream;
    FFileNameOnDisk: string;
    FFileIndex: integer;
    function GetDoPause: boolean;
    function GetOricFileName: string;
    function GetTime: string;
    procedure SetActif(const Value: boolean);
    procedure SetDoPause(const Value: boolean);
    procedure SetOricFileName(const Value: string);
    procedure SetTime(const Value: string);
    procedure SetMax(const Value: single);
    procedure SetMin(const Value: single);
    procedure SetValue(const Value: single);
    procedure UpdateWidths;
    procedure SetFileIndex(const Value: integer);
    procedure SetFileNameOnDisk(const Value: string);
    procedure SetFileStream(const Value: TMemoryStream);
    { Déclarations privées }
  public
    { Déclarations publiques }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property OricFileName:string read GetOricFileName write SetOricFileName;
    property DoPause:boolean read GetDoPause write SetDoPause;
    property Time:string read GetTime write SetTime;
    property Actif:boolean read FActif write SetActif;
    property MinValue:single read FMin write SetMin;
    property MaxValue:single read FMax write SetMax;
    property ProgressValue:single read FValue write SetValue;
    property FileNameOnDisk:string read FFileNameOnDisk write SetFileNameOnDisk;
    property FileIndex:integer read FFileIndex write SetFileIndex;
    property FileStream:TMemoryStream read FFileStream write SetFileStream;
  end;

implementation

{$R *.fmx}
{ TFileFrame }
uses System.Math,main;

constructor TframeFile.Create(AOwner: TComponent);
begin
  inherited;
  fMin:=0;
  FMax:=100;
  FValue:=0;
  FFileStream:=TMemoryStream.Create;
end;

destructor TframeFile.Destroy;
begin
  inherited;
  FFileStream.Free;
end;

function TframeFile.GetDoPause: boolean;
begin
  result:=(cbPause.IsChecked);
end;

function TframeFile.GetOricFileName: string;
begin
  result:=Label1.Text;
end;

function TframeFile.GetTime: string;
begin
  Result:=Label2.Text;
end;

procedure TframeFile.Rectangle1Resize(Sender: TObject);
begin
  UpdateWidths;
end;

procedure TframeFile.SetActif(const Value: boolean);
begin
  FActif := Value;
  if value
  then SpeedButton1.StyleLookup:='playtoolbutton'
  else SpeedButton1.StyleLookup:='speedbuttonstyle';
  SpeedButton1.Width:=48;
  Application.ProcessMessages;
end;

procedure TframeFile.SetDoPause(const Value: boolean);
begin
  if FPause<>Value then
  begin
    FPause := Value;
    if value
    then cbPause.IsChecked:=true
    else cbPause.IsChecked:=false;
  end;
end;

procedure TframeFile.SetFileIndex(const Value: integer);
begin
  FFileIndex := Value;
end;

procedure TframeFile.SetFileNameOnDisk(const Value: string);
begin
  FFileNameOnDisk := Value;
end;

procedure TframeFile.SetFileStream(const Value: TMemoryStream);
begin
  FFileStream := Value;
end;

procedure TframeFile.SetMax(const Value: single);
begin
  if fmax=value then exit;
  if value<fmin then exit;
  FMax := Value;
  updatewidths;
end;

procedure TframeFile.SetMin(const Value: single);
begin
  if fmin=value then exit;
  if fmax<value then exit;
  fmin := Value;
  updatewidths;
end;

procedure TframeFile.SetOricFileName(const Value: string);
begin
  if FOricFileName<>Value then
  begin
    FOricFileName := Value;
    Label1.Text:=Value;
    Application.ProcessMessages;
  end;
end;

procedure TframeFile.SetTime(const Value: string);
begin
  if FTime<>Value then
  begin
    FTime := Value;
    Label2.Text:=Value;
    Application.ProcessMessages;
  end;
end;

procedure TframeFile.SetValue(const Value: single);
begin
  FValue := System.Math.Min(System.Math.Max(Value, 0), 100);
  UpdateWidths;
end;

procedure TframeFile.UpdateWidths;
begin
  Rectangle2.Width := Rectangle1.Width * (FValue-fmin) / (fmax-fmin);
  Application.ProcessMessages;
end;

end.
