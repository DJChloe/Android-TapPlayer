unit UnitTapFiles;

interface
uses Classes;

Const BASIC_FILE=$00;
      ARRRAY_FILE=$40;
      LM_FILE=$80;
type

TOricFile=class
  private
    Fnamehex: string;
    FName: string;
    FNamebytes:array of byte;
    FHeader: TMemoryStream;
    FAddrFin: word;
    FTapeIndex: integer;
    FFileKind: byte;
    Fsize: Integer;
    Fspecialname: boolean;
    FAddrDeb: word;
    FData: TMemoryStream;
    FAutoFlag: byte;
    FWavLength: int64;
    FRawData: boolean;
    procedure SetAddrDeb(const Value: word);
    procedure SetAddrFin(const Value: word);
    procedure SetName(const Value: string);
    procedure Setsize(const Value: integer);
    procedure UpdateNames;
    function GetCatalogName: string;
    function GetFileKindDesc: string;
    function GetAuto: boolean;
    function GetAutoDesc: string;
    function GetAddrDebDesc: string;
    function GetAddrFinDesc: string;
    function GetSizeDesc: string;
    procedure SetWavLength(const Value: int64);
    procedure SetRawData(const Value: boolean);
  public
    constructor Create;
    destructor Destroy; override;
    procedure ClearName;
    procedure AddToName(b:byte); overload;
    procedure AddToName(s:string); overload;
    procedure SaveToFile(FileName:string);
    procedure SaveToStream(AStream:TStream);
    procedure StreamToProps;
    procedure PropsToStream;
    property Name:string read FName write SetName;
    property size:integer read Fsize write Setsize;
    property namehex:string read Fnamehex;
    property TapeIndex:integer read FTapeIndex write FTapeIndex;
    property AddrDeb:word read FAddrDeb write SetAddrDeb;
    property AddrFin:word read FAddrFin write SetAddrFin;
    property specialname:boolean read Fspecialname;
    property Header:TMemoryStream read FHeader write FHeader;
    property Data:TMemoryStream read FData write FData;
    property FileKind:byte read FFileKind write FFileKind;
    property FileKindDesc:string read GetFileKindDesc;
    property CatalogName:string read GetCatalogName;
    property AutoFlag:byte read FAutoFlag write FAutoFlag;
    property Auto:boolean read GetAuto;
    property AutoDesc:string read GetAutoDesc;
    property SizeDesc:string read GetSizeDesc;
    property AddrDebDesc:string read GetAddrDebDesc;
    property AddrFinDesc:string read GetAddrFinDesc;
    property WavLength:int64 read FWavLength write SetWavLength;
    property RawData:boolean read FRawData write SetRawData;
end;
implementation
uses SysUtils;
{ TOricFile }

procedure TOricFile.AddToName(b: byte);
var i:integer;
begin
  i:=length(FNamebytes)+1;
  SetLength(FNamebytes,i);
  FNamebytes[i-1]:=b;
  UpdateNames;
end;

procedure TOricFile.AddToName(s: string);
var i,j:integer;
begin
  j:=length(FNamebytes);
  i:=j+Length(s);
  SetLength(FNamebytes,i);
  for i:=0 to length(s) - 1 do FNamebytes[i+j]:=byte(s[i+1]);
  UpdateNames;
end;

procedure TOricFile.ClearName;
begin
  SetLength(FNamebytes,0);
  UpdateNames;
end;

constructor TOricFile.Create;
begin
  RawData:=False;
  FHeader:=TMemoryStream.Create;
  FData:=TMemoryStream.Create;
end;

destructor TOricFile.Destroy;
begin
  FHeader.Free;
  FData.Free;
end;

function TOricFile.GetAddrDebDesc: string;
begin
  result:='#'+IntToHex(AddrDeb,4);
end;

function TOricFile.GetAddrFinDesc: string;
begin
  result:='#'+IntToHex(AddrFin,4);
end;

function TOricFile.GetAuto: boolean;
begin
  Result:=(AutoFlag<>0);
end;

function TOricFile.GetAutoDesc: string;
begin
  if AutoFlag=$00
  then result:='No'
  else result:='Yes (#'+inttohex(AutoFlag,2)+')';
end;

function TOricFile.GetCatalogName: string;
begin
  result:=trim(name);
  if specialname then result:=result+'('+namehex+')';
end;

function TOricFile.GetFileKindDesc: string;
begin
  case FFileKind of
    $00:result:='BASIC';
    $40:result:='Array';
    $80:result:='Mach. code/Mem. bloc'
    else result:='#'+inttohex(FFileKind,2);
  end;
end;

function TOricFile.GetSizeDesc: string;
begin
  result:=IntToStr(size);
end;

procedure TOricFile.PropsToStream;
var b:byte;
begin
  if FRawData then exit;  
  Header.Clear;
  b:=$16;
  Header.Write(b,1);
  Header.Write(b,1);
  Header.Write(b,1);
  b:=$24;
  Header.Write(b,1);
  //header
  b:=0;
  Header.Write(b,1);
  Header.Write(b,1);
  Header.Write(FFileKind,1);
  Header.Write(FAutoFlag,1);
  b:=Hi(FAddrFin);
  Header.Write(b,1);
  b:=Lo(FAddrFin);
  Header.Write(b,1);
  b:=Hi(FAddrDeb);
  Header.Write(b,1);
  b:=Lo(FAddrDeb);
  Header.Write(b,1);
  b:=0;
  Header.Write(b,1);
  //Name
  if length(FNamebytes)>0 then
     Header.Write(FNamebytes[0],length(FNamebytes));
  b:=0;
  Header.Write(b,1);
end;

procedure TOricFile.StreamToProps;
var b:byte;
    hheader:array[0..8] of byte;
    i:Integer;
begin
  if FRawData then begin
                     Fsize:=Data.Size;
                     exit;
                   end;
  Header.Position:=0;
  Fspecialname:=false;
  ClearName;
  FAddrDeb:=0;
  FAddrFin:=0;
  Fsize:=0;
  b:=$16;
  while (b=$16) do Header.Read(b,1); // read synchro (0x24 included)
  if (Header.Position>=Header.size) then exit;

  //header
  for i:=0 to 8 do
  begin
    Header.Read(b,1);
    hheader[i]:=b;
  end;
  //Name

  repeat
    i:=Header.Read(b,1);
    if ((b<>0) and (i<>0)) then AddToName(b);
  until ((b=0) or (i=0));

  FAutoFlag:=hheader[3];

  FAddrDeb:=hheader[6]*256+hheader[7];
  FAddrFin:=hheader[4]*256+hheader[5];
  Fsize:=AddrFin-AddrDeb+1;

  FFileKind:=hheader[2];
end;

procedure TOricFile.UpdateNames;
var i:integer;
    b,c:byte;
begin
  FName:='';
  Fnamehex:='';
  Fspecialname:=false;
  for I := 0 to length(FNamebytes) - 1 do
  begin
    b:=FNamebytes[i];
    Fnamehex:=Fnamehex+IntToHex(b,2)+' ';
    c:=b;
    if ((b<32) or (b>=128)) then
    begin
       Fspecialname:=true;
       if b<32 then c:=32 else c:=b-128;
    end;
    FName:=FName+chr(c);
  end;
end;

procedure TOricFile.SaveToFile(FileName: string);
begin
  Header.SaveToFile(filename);
  Data.SaveToFile(FileName);
end;

procedure TOricFile.SaveToStream(AStream: TStream);
begin
  PropsToStream;
  Header.SaveToStream(AStream);
  Data.SaveToStream(AStream);
end;

procedure TOricFile.SetAddrDeb(const Value: word);
begin
  if FAddrDeb<>Value then
  begin
    FAddrDeb:=Value;
    //size:=AddrFin-AddrDeb+1;
    FAddrFin:=FAddrDeb+size-1;
  end;
end;

procedure TOricFile.SetAddrFin(const Value: word);
begin
  if FAddrFin<>Value then
  begin
    FAddrFin := Value;
    FAddrDeb:=FAddrFin-size+1;
  end;
end;

procedure TOricFile.SetName(const Value: string);
begin
  if FName<>Value then
  begin
    ClearName;
    AddToName(Value);
  end;
end;

procedure TOricFile.SetRawData(const Value: boolean);
begin
  FRawData := Value;
end;

procedure TOricFile.Setsize(const Value: integer);
begin
  if FSize<>Value then
  begin
    Fsize:=Value;
    if not FRawData then
    begin
      FAddrFin:=FAddrDeb+size-1;
      Data.SetSize(Fsize);
    end;
  end;
end;

procedure TOricFile.SetWavLength(const Value: int64);
begin
  FWavLength := Value;
end;

end.
